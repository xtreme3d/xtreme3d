import ctypes
from ctypes import wintypes

def HWNDFromDouble(cdouble_hwnd):
    as_float = float(cdouble_hwnd)
    as_int = int(as_float)
    return wintypes.HWND(as_int)

user32 = ctypes.WinDLL("user32", use_last_error=True)

SetFocus = user32.SetFocus
SetFocus.argtypes = [wintypes.HWND]
SetFocus.restype  = wintypes.HWND

SetForegroundWindow = user32.SetForegroundWindow
SetForegroundWindow.argtypes = [wintypes.HWND]
SetForegroundWindow.restype  = wintypes.BOOL

ShowWindow = user32.ShowWindow
ShowWindow.argtypes = [wintypes.HWND, ctypes.c_int]
ShowWindow.restype = wintypes.BOOL

SW_RESTORE = 9

def RestoreWindow(handle):
    hwnd = HWNDFromDouble(handle)
    ShowWindow(hwnd, SW_RESTORE)
    SetForegroundWindow(hwnd)
