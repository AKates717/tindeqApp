## global.R ---------------------------------------------------------------

## Packages
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(shiny)
library(bslib)
library(bsicons)
library(reticulate)
library(shinyWidgets)
library(gt)

source("helpers.R")

## -----------------------------------------------------------------------
## Reticulate / Python env
## -----------------------------------------------------------------------

use_condaenv("tindeq", required = TRUE)

## -----------------------------------------------------------------------
## Python BLE streaming engine (Bleak in a background thread)
## Defines:
##   start_stream(), stop_stream(), get_samples(clear=TRUE),
##   is_streaming(), get_debug()
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

# --------------------------------------------------------------------
# Shared state (Python-side)
# --------------------------------------------------------------------

_stream = {
  'running': False,
  'thread': None,
  'loop': None,
  'client': None,
  'samples': [],      # list of (t_us, weight)
  'lock': threading.Lock(),
  'name_prefix': 'Progressor_3177',
  'auto_tare': True
}

# Debug counters to diagnose effective sampling rate
_debug = {
  'packets': 0,     # BLE notifications received
  'samples': 0,     # weight samples decoded
  'last_len': 0,    # last notification payload length
  'last_t_us': None
}

def _append_sample(t_us, w):
    with _stream['lock']:
        _stream['samples'].append((t_us, w))
        if len(_stream['samples']) > 5000:
            _stream['samples'] = _stream['samples'][-5000:]

    _debug['samples'] += 1
    _debug['last_t_us'] = t_us

async def _run_stream():
    # Scan for device (fast startup)
    devices = await BleakScanner.discover(timeout=2.0)

    target = None
    for d in devices:
        if (d.name or '').startswith(_stream['name_prefix']):
            target = d
            break

    if target is None:
        _stream['running'] = False
        return

    # IMPORTANT:
    # Parse *all* TLVs in a single notification payload
    def handle_notify(sender, data: bytearray):
        b = bytes(data)
        _debug['packets'] += 1
        _debug['last_len'] = len(b)

        i = 0
        while i + 2 <= len(b):
            code = b[i]
            ln   = b[i + 1]
            j = i + 2 + ln
            if j > len(b):
                break

            val = b[i + 2 : j]

            # Weight measurement TLV (0x01):
            # value may contain MULTIPLE samples back-to-back:
            # each sample = float32 weight + uint32 timestamp (8 bytes)
            if code == 0x01 and ln >= 8:
                n = (ln // 8) * 8
                for k in range(0, n, 8):
                    w = struct.unpack('<f', val[k:k+4])[0]
                    t = struct.unpack('<I', val[k+4:k+8])[0]
                    _append_sample(t, w)

            i = j
            
            
            
            

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

def start_stream(name_prefix='Progressor_3177', auto_tare=True, reset_buffer=True):
    if _stream['running']:
        return True

    _stream['name_prefix'] = name_prefix
    _stream['auto_tare'] = bool(auto_tare)

    if reset_buffer:
        with _stream['lock']:
            _stream['samples'] = []

        _debug['packets'] = 0
        _debug['samples'] = 0
        _debug['last_len'] = 0
        _debug['last_t_us'] = None

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

def get_debug():
    return dict(_debug)
")




