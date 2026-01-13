library(reticulate)
library(tidyverse)
use_condaenv("tindeq", required = TRUE)




py_run_string("
import asyncio, struct, threading, time
from bleak import BleakScanner, BleakClient

CTRL_UUID = '7e4e1703-1ea6-40c9-9dcc-13d34ffead57'
DATA_UUID = '7e4e1702-1ea6-40c9-9dcc-13d34ffead57'

OP_TARE  = 0x64
OP_START = 0x65
OP_STOP  = 0x66

def tlv_cmd(opcode, value=b''):
    return bytes([opcode, len(value)]) + value

# ---- shared state (lives in Python interpreter) ----
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
        # keep last ~5000 samples to avoid unbounded growth
        if len(_stream['samples']) > 5000:
            _stream['samples'] = _stream['samples'][-5000:]

async def _run_stream():
    # scan + pick device by name prefix
    devices = await BleakScanner.discover(timeout=6.0)
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

        # idle while running
        while _stream['running']:
            await asyncio.sleep(0.05)

        # stop
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




















tindeq_capture <- function(name_prefix = "Progressor_1827",
                           n_samples   = 400,
                           timeout_s   = 10,
                           auto_tare   = TRUE) {
  
  # ---- pass R values safely into Python (no quoting issues) ----
  py$TARGET_NAME_PREFIX <- name_prefix
  py$N_SAMPLES <- as.integer(n_samples)
  py$TIMEOUT_S <- as.numeric(timeout_s)
  py$AUTO_TARE <- isTRUE(auto_tare)
  
  py_run_string("
import asyncio, struct
from bleak import BleakScanner, BleakClient

CTRL_UUID = '7e4e1703-1ea6-40c9-9dcc-13d34ffead57'
DATA_UUID = '7e4e1702-1ea6-40c9-9dcc-13d34ffead57'

def tlv_cmd(opcode, value=b''):
    # opcode (1 byte) + length (1 byte) + value (n bytes)
    return bytes([opcode, len(value)]) + value

# Tindeq opcodes
OP_TARE  = 0x64
OP_START = 0x65
OP_STOP  = 0x66

async def capture():
    # --- scan ---
    devices = await BleakScanner.discover(timeout=6.0)
    target = None
    for d in devices:
        if (d.name or '').startswith(TARGET_NAME_PREFIX):
            target = d
            break
    if target is None:
        return []

    samples = []

    # --- notification handler ---
    def handle_notify(sender, data: bytearray):
        b = bytes(data)
        if len(b) < 2:
            return

        code = b[0]
        ln   = b[1]
        val  = b[2:2+ln]

        # Weight measurement: float32 weight + uint32 timestamp (us)
        if code == 0x01 and ln >= 8:
            w = struct.unpack('<f', val[0:4])[0]
            t = struct.unpack('<I', val[4:8])[0]
            samples.append((t, w))

    # --- connect + capture ---
    async with BleakClient(target) as client:
        await client.start_notify(DATA_UUID, handle_notify)

        if AUTO_TARE:
            await client.write_gatt_char(CTRL_UUID, tlv_cmd(OP_TARE), response=False)
            await asyncio.sleep(0.2)

        await client.write_gatt_char(CTRL_UUID, tlv_cmd(OP_START), response=False)

        t0 = asyncio.get_event_loop().time()
        while len(samples) < N_SAMPLES and (asyncio.get_event_loop().time() - t0) < TIMEOUT_S:
            await asyncio.sleep(0.01)

        await client.write_gatt_char(CTRL_UUID, tlv_cmd(OP_STOP), response=False)
        await asyncio.sleep(0.1)
        await client.stop_notify(DATA_UUID)

    return samples

samples = asyncio.run(capture())
")
  
  # ---- convert Python samples to R data.frame ----
  samps <- py$samples
  if (length(samps) == 0) return(data.frame())
  
  df <- data.frame(
    t_us   = vapply(samps, function(x) x[[1]], numeric(1)),
    weight = vapply(samps, function(x) x[[2]], numeric(1))
  )
  
  df$t_s <- df$t_us / 1e6
  df
}



tindeq_tare <- function(name_prefix = "Progressor_1827") {
  
  py$TARGET_NAME_PREFIX <- name_prefix
  
  py_run_string("
import asyncio
from bleak import BleakScanner, BleakClient

CTRL_UUID = '7e4e1703-1ea6-40c9-9dcc-13d34ffead57'

def tlv_cmd(opcode, value=b''):
    return bytes([opcode, len(value)]) + value

OP_TARE = 0x64

async def do_tare():
    devices = await BleakScanner.discover(timeout=6.0)
    target = None
    for d in devices:
        if (d.name or '').startswith(TARGET_NAME_PREFIX):
            target = d
            break
    if target is None:
        return False

    async with BleakClient(target) as client:
        await client.write_gatt_char(CTRL_UUID, tlv_cmd(OP_TARE), response=False)
        await asyncio.sleep(0.2)

    return True

tare_ok = asyncio.run(do_tare())
")
  isTRUE(py$tare_ok)
}


tindeq_battery <- function(name_prefix = "Progressor_1827", timeout_s = 3) {
  
  py$TARGET_NAME_PREFIX <- name_prefix
  py$TIMEOUT_S <- timeout_s
  
  py_run_string("
import asyncio, struct
from bleak import BleakScanner, BleakClient

CTRL_UUID = '7e4e1703-1ea6-40c9-9dcc-13d34ffead57'
DATA_UUID = '7e4e1702-1ea6-40c9-9dcc-13d34ffead57'

def tlv_cmd(opcode, value=b''):
    return bytes([opcode, len(value)]) + value

OP_BATT = 0x6F  # sample battery voltage

async def get_batt():
    devices = await BleakScanner.discover(timeout=6.0)
    target = None
    for d in devices:
        if (d.name or '').startswith(TARGET_NAME_PREFIX):
            target = d
            break
    if target is None:
        return None

    batt_mv = None

    def handle_notify(sender, data: bytearray):
        nonlocal batt_mv
        b = bytes(data)
        if len(b) < 2:
            return
        code, ln = b[0], b[1]
        val = b[2:2+ln]

        # Battery response code = 0x00, value = uint32 millivolts
        if code == 0x00 and ln >= 4:
            batt_mv = struct.unpack('<I', val[0:4])[0]

    async with BleakClient(target) as client:
        await client.start_notify(DATA_UUID, handle_notify)

        # request battery sample
        await client.write_gatt_char(CTRL_UUID, tlv_cmd(OP_BATT), response=False)

        t0 = asyncio.get_event_loop().time()
        while batt_mv is None and (asyncio.get_event_loop().time() - t0) < TIMEOUT_S:
            await asyncio.sleep(0.05)

        await client.stop_notify(DATA_UUID)

    return batt_mv

batt_mv = asyncio.run(get_batt())
")
  
  mv <- py$batt_mv
  if (is.null(mv)) return(NA_real_)
  as.numeric(mv)
}










