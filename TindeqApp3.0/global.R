## global.R ---------------------------------------------------------------

## Packages
library(tidyverse)
library(shiny)
library(bslib)
library(reticulate)
library(shinyWidgets)
library(shinytoastr)

## -----------------------------------------------------------------------
## Themes and Formatting
## -----------------------------------------------------------------------

custom_theme <- bslib::bs_theme(
  version = 5,
  preset = "bootstrap",
  spacer = "0.5rem",
  # bg = "#FFF",
  # fg = "#000000",
  primary = "#000000"
)








## Reticulate / Python env
use_condaenv("tindeq", required = TRUE)

## (Optional) sanity check: print which python is being used
# message("Python: ", py_config()$python)

## -----------------------------------------------------------------------
## Python BLE streaming engine (Bleak in a background thread)
## Defines: start_stream(), stop_stream(), get_samples(clear=TRUE), is_streaming()
## -----------------------------------------------------------------------

py_run_string("
import asyncio, struct, threading
from bleak import BleakScanner, BleakClient

# Tindeq UUIDs
CTRL_UUID = '7e4e1703-1ea6-40c9-9dcc-13d34ffead57'
DATA_UUID = '7e4e1702-1ea6-40c9-9dcc-13d34ffead57'

# Opcodes
OP_TARE  = 0x64
OP_START = 0x65
OP_STOP  = 0x66

def tlv_cmd(opcode, value=b''):
    return bytes([opcode, len(value)]) + value

# Shared state (Python-side)
_stream = {
  'running': False,
  'thread': None,
  'loop': None,
  'client': None,
  'samples': [],      # list of (t_us, weight)
  'lock': threading.Lock(),
  'name_prefix': 'Progressor_1827',
  'auto_tare': True
}

def _append_sample(t_us, w):
    with _stream['lock']:
        _stream['samples'].append((t_us, w))
        # keep buffer bounded
        if len(_stream['samples']) > 5000:
            _stream['samples'] = _stream['samples'][-5000:]

async def _run_stream():
    devices = await BleakScanner.discover(timeout=2.0)
    target = None
    for d in devices:
        if (d.name or '').startswith(_stream['name_prefix']):
            target = d
            break
    if target is None:
        _stream['running'] = False
        return

    def handle_notify(sender, data: bytearray):
        b = bytes(data)
        if len(b) < 2: return
        code, ln = b[0], b[1]
        val = b[2:2+ln]
        if code == 0x01 and ln >= 8:
            w = struct.unpack('<f', val[0:4])[0]
            t = struct.unpack('<I', val[4:8])[0]
            _append_sample(t, w)

    async with BleakClient(target) as client:
        _stream['client'] = client
        await client.start_notify(DATA_UUID, handle_notify)

        if _stream['auto_tare']:
            await client.write_gatt_char(CTRL_UUID, tlv_cmd(OP_TARE), response=False)
            await asyncio.sleep(0.2)

        await client.write_gatt_char(CTRL_UUID, tlv_cmd(OP_START), response=False)

        while _stream['running']:
            await asyncio.sleep(0.05)

        await client.write_gatt_char(CTRL_UUID, tlv_cmd(OP_STOP), response=False)
        await asyncio.sleep(0.1)
        await client.stop_notify(DATA_UUID)

    _stream['client'] = None

def _thread_main():
    loop = asyncio.new_event_loop()
    _stream['loop'] = loop
    asyncio.set_event_loop(loop)
    try:
        loop.run_until_complete(_run_stream())
    finally:
        try:
            loop.stop()
        except:
            pass
        loop.close()
        _stream['loop'] = None
        _stream['running'] = False

def start_stream(name_prefix='Progressor_1827', auto_tare=True, reset_buffer=True):
    if _stream['running']:
        return True
    _stream['name_prefix'] = name_prefix
    _stream['auto_tare'] = bool(auto_tare)
    if reset_buffer:
        with _stream['lock']:
            _stream['samples'] = []
    _stream['running'] = True
    th = threading.Thread(target=_thread_main, daemon=True)
    _stream['thread'] = th
    th.start()
    return True

def stop_stream():
    _stream['running'] = False
    return True

def get_samples(clear=True):
    with _stream['lock']:
        out = list(_stream['samples'])
        if clear:
            _stream['samples'] = []
    return out

def is_streaming():
    return bool(_stream['running'])
")

## -----------------------------------------------------------------------
## R helpers for server.R
## -----------------------------------------------------------------------

# Convert python list of (t_us, weight) into an R data.frame
samples_to_df <- function(samps) {
  if (length(samps) == 0) {
    return(data.frame(t_us = numeric(0), weight = numeric(0), t_s = numeric(0)))
  }
  df <- data.frame(
    t_us   = vapply(samps, function(x) x[[1]], numeric(1)),
    weight = vapply(samps, function(x) x[[2]], numeric(1))
  )
  df$t_s <- df$t_us / 1e6
  df
}

# Append new samples to an existing df and keep last `window_s` seconds
append_and_trim <- function(cur_df, new_df, window_s = 20) {
  out <- rbind(cur_df, new_df)
  if (nrow(out) >= 2) {
    tmax <- max(out$t_s)
    out <- out[out$t_s >= (tmax - window_s), , drop = FALSE]
  }
  out
}

#filename slug helper
slugify <- function(x) {
  x <- trimws(x)
  x <- gsub("[^A-Za-z0-9]+", "-", x)   # non-alnum -> hyphen
  x <- gsub("(^-+|-+$)", "", x)        # trim hyphens
  if (nchar(x) == 0) "NA" else x
}

