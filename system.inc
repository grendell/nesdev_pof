.segment "CODE"
; https://www.nesdev.org/wiki/PPU_registers
PPU_CTRL = $2000
PPU_MASK = $2001
PPU_STATUS = $2002
PPU_SCROLL = $2005
PPU_ADDR = $2006
PPU_DATA = $2007
OAM_DMA = $4014

; https://www.nesdev.org/wiki/APU_registers
SQ1_VOL = $4000
SQ1_SWEEP = $4001
SQ1_LO = $4002
SQ1_HI = $4003

SQ2_VOL = $4004
SQ2_SWEEP = $4005
SQ2_LO = $4006
SQ2_HI = $4007

TRI_LINEAR = $4008
TRI_LO = $400a
TRI_HI = $400b

NOISE_VOL = $400c
NOISE_LO = $400e
NOISE_HI = $400f

DMC_FREQ = $4010
DMC_RAW = $4011
DMC_START = $4012
DMC_LEN = $4013

SND_CHN = $4015
APU_FRAME_COUNTER = $4017

; https://www.nesdev.org/wiki/Controller_reading_code
JOY1 = $4016
JOY_STROBE = $4016
BUTTON_A = %10000000
BUTTON_B = %01000000
BUTTON_SELECT = %00100000
BUTTON_START = %00010000
BUTTON_UP = %00001000
BUTTON_DOWN = %00000100
BUTTON_LEFT = %00000010
BUTTON_RIGHT = %00000001