.segment "HEADER"
; https://www.nesdev.org/wiki/INES
  .byte $4e, $45, $53, $1a ; iNES header identifier
  .byte $02                ; 2x 16KB PRG code
  .byte $01                ; 1x  8KB CHR data
  .byte $01                ; mapper 0 and vertical mirroring
  .byte $00                ; mapper 0

.segment "VECTORS"
  .addr nmi, reset, 0

.segment "ZEROPAGE"
joy1_curr: .res 1
joy1_prev_inv: .res 1

selection: .res 1
immediate: .res 1
focus: .res 1
asm_ptr: .res 2
mac_ptr: .res 2

result_a: .res 1
result_x: .res 1
result_y: .res 1

waiting_for_nmi: .res 1

OP_RTS = $60
EXE_RAM = $0200

.struct instruction
  mnemonic .byte 3
  opcode .byte
  length .byte
  padding .byte 3
.endstruct

.enum
  FOCUS_NONE = 0
  FOCUS_OP = 1
  FOCUS_IMM = 2
  FOCUS_RES = 3
.endenum

.enum
  TILE_A = 10
  TILE_NUMBER = 36
  TILE_DOLLAR = 37
  TILE_COLON = 38
  TILE_SPACE = $ff
.endenum

.include "system.inc"
.include "chars.inc"

.segment "CODE"
.proc reset
  ; https://www.nesdev.org/wiki/Init_code
  sei                    ; ignore IRQs
  cld                    ; disable decimal mode
  ldx #$40
  stx APU_FRAME_COUNTER  ; disable APU frame IRQ
  ldx #$ff
  txs                    ; Set up stack
  inx                    ; now X = 0
  stx PPU_CTRL           ; disable NMI
  stx PPU_MASK           ; disable rendering
  stx DMC_FREQ           ; disable DMC IRQs

  ; clear vblank flag
  bit PPU_STATUS

  ; wait for first vblank
: bit PPU_STATUS
  bpl :-

  ; initialize cpu variables
  lda #0
  sta joy1_curr
  sta waiting_for_nmi

  jsr clear

  ; wait for second vblank
: bit PPU_STATUS
  bpl :-

  ; initialize ppu
  jsr init_palettes
  jsr init_nametables

  ; initialize background scroll position
  lda #0
  sta PPU_SCROLL
  sta PPU_SCROLL

  ; enable NMI and select pattern tables
  lda #%10000000
  sta PPU_CTRL

game_loop:
  lda focus
  cmp #FOCUS_RES
  beq game_loop

  jsr handle_input

  ; wait for frame to be completed
  inc waiting_for_nmi
: lda waiting_for_nmi
  bne :-

  jmp game_loop
.endproc

.proc nmi
  ; retain previous value of a and x on the stack
  pha
  txa
  pha

  ; clear vblank flag
  bit PPU_STATUS

  lda focus
  cmp #FOCUS_NONE
  beq done

  cmp #FOCUS_RES
  beq show_results

  ; update current instruction
  lda asm_ptr + 1
  sta PPU_ADDR
  lda asm_ptr
  sta PPU_ADDR

  lda selection
  asl
  asl
  asl
  tax

  lda instructions, x
  sta PPU_DATA
  lda instructions + 1, x
  sta PPU_DATA
  lda instructions + 2, x
  sta PPU_DATA

  lda instructions + instruction::length, x
  cmp #1
  beq done

  lda focus
  cmp #FOCUS_OP
  beq done

  lda #TILE_SPACE
  sta PPU_DATA
  lda #TILE_NUMBER
  sta PPU_DATA
  lda #TILE_DOLLAR
  sta PPU_DATA

  lda immediate
  lsr
  lsr
  lsr
  lsr
  sta PPU_DATA

  lda immediate
  and #$f
  sta PPU_DATA

done:
  ; show backgrounds
  lda #%00001000
  sta PPU_MASK

  ; update background scroll position
  lda #0
  sta PPU_SCROLL
  sta PPU_SCROLL

  ; allow game loop to continue after interrupt
  lda #0
  sta waiting_for_nmi

  ; restore previous value of x and a before interrupt
  pla
  tax
  pla
  rti

show_results:
.macro show_result ppu_addr, register, result
  lda #$20
  sta PPU_ADDR
  lda #ppu_addr
  sta PPU_ADDR

  lda #register
  sta PPU_DATA
  lda #TILE_COLON
  sta PPU_DATA
  lda #TILE_DOLLAR
  sta PPU_DATA

  lda result
  lsr
  lsr
  lsr
  lsr
  sta PPU_DATA

  lda result
  and #$f
  sta PPU_DATA
.endmacro

  show_result $34, 10, result_a
  show_result $54, 33, result_x
  show_result $74, 34, result_y

  jmp done
.endproc

.proc init_palettes
  ; set ppu address to palette entries ($3f00)
  lda #$3f
  sta PPU_ADDR
  lda #0
  sta PPU_ADDR

  ; loop through each palette entry, 32 total
  ldx #0
: lda palettes, x
  sta PPU_DATA
  inx
  cpx #32
  bne :-

  rts
.endproc

.proc init_nametables
  ; set ppu address to first nametable ($2000)
  lda #$20
  sta PPU_ADDR
  lda #0
  sta PPU_ADDR

  ; set all tiles to $ff (blank)
  lda #$ff
  ldy #30
: ldx #32
: sta PPU_DATA

  dex
  bne :-

  dey
  bne :--

  ; set all attrs to 0
  lda #0
  ldx #64
: sta PPU_DATA

  dex
  bne :-

  ; set ppu address to second nametable ($2800)
  lda #$28
  sta PPU_ADDR
  lda #0
  sta PPU_ADDR

  ; set all tiles to $ff (blank)
  lda #$ff
  ldy #30
: ldx #32
: sta PPU_DATA

  dex
  bne :-

  dey
  bne :--

  ; set all attrs to 0
  lda #0
  ldx #64
: sta PPU_DATA

  dex
  bne :-

  rts
.endproc

.proc read_joypad
  ; https://www.nesdev.org/wiki/Controller_reading_code
  ; progress previous button state
  lda joy1_curr
  eor #%11111111
  sta joy1_prev_inv

  ; initialize ring buffer
  lda #1
  sta joy1_curr

  ; strobe joypad to record latest state
  sta JOY_STROBE
  lsr
  sta JOY_STROBE

: lda JOY1      ; read next button state
  lsr           ; bit 0 -> carry
  rol joy1_curr ; carry -> bit 0, bit 7 -> carry
  bcc :-

  rts
.endproc

.proc handle_input
  jsr read_joypad

  ; was A just pressed?
  lda joy1_prev_inv
  and joy1_curr
  and #BUTTON_A
  beq :+

  jmp make_selection

  ; was start just pressed?
: lda joy1_prev_inv
  and joy1_curr
  and #BUTTON_START
  beq :+

  jmp execute

  ; was up just pressed?
: lda joy1_prev_inv
  and joy1_curr
  and #BUTTON_UP
  beq :+

  jmp prev_choice

  ; was down just pressed?
: lda joy1_prev_inv
  and joy1_curr
  and #BUTTON_DOWN
  beq :+

  jmp next_choice

: rts
.endproc

.proc clear
  lda #FOCUS_NONE
  sta focus

  lda #$20
  sta asm_ptr + 1
  lda #$24
  sta asm_ptr

  lda #>EXE_RAM
  sta mac_ptr + 1
  lda #<EXE_RAM
  sta mac_ptr

  lda #OP_RTS
  sta EXE_RAM

  rts
.endproc

.proc make_selection
  lda focus
  bne :+

  lda #FOCUS_OP
  sta focus

  lda #0
  sta selection

  rts

  ; find selection op code
: lda selection
  asl
  asl
  asl
  tay

  lda focus
  cmp #FOCUS_IMM
  beq select_immediate

select_op:
  ; find instruction length
  lda instructions + instruction::length, y
  cmp #1
  bne move_focus

write_opcode:
  lda #FOCUS_NONE
  sta focus

  ; write op code to execution ram
  lda instructions + instruction::opcode, y
  ldy #0
  sta (mac_ptr), y
  iny

  ; append rts op code to execution ram
  lda #OP_RTS
  sta (mac_ptr), y

  ; progress op code write pointer
  inc mac_ptr
  bne update_ppu_ptr

  inc mac_ptr + 1
  jmp update_ppu_ptr

move_focus:
  lda #FOCUS_IMM
  sta focus

  lda #0
  sta immediate

  rts

select_immediate:
  lda #FOCUS_NONE
  sta focus

  lda instructions + instruction::opcode, y

  ; write op code to execution ram
  ldy #0
  sta (mac_ptr), y
  iny

  ; write immediate to execution ram
  lda immediate
  sta (mac_ptr), y
  iny

  ; append rts op code to execution ram
  lda #OP_RTS
  sta (mac_ptr), y

  ; progress op code write pointer
  lda mac_ptr
  clc
  adc #2
  sta mac_ptr
  bcc update_ppu_ptr

  inc mac_ptr + 1

update_ppu_ptr:
  ; update ppu write pointer
  lda asm_ptr
  clc
  adc #32
  sta asm_ptr
  bcc :+

  inc asm_ptr + 1

: rts
.endproc

.proc prev_choice
  lda focus
  cmp #FOCUS_OP
  beq prev_opcode

  cmp #FOCUS_IMM
  beq next_immediate

  rts

prev_opcode:
  dec selection

  ; wrap selection if necessary
  lda selection
  bpl done

  lda #NUM_OPS - 1
  sta selection

  jmp done

next_immediate:
  inc immediate

done:
  rts
.endproc

.proc next_choice
  lda focus
  cmp #FOCUS_OP
  beq next_opcode

  cmp #FOCUS_IMM
  beq prev_immediate

  rts

next_opcode:
  inc selection

  ; wrap selection if necessary
  lda selection
  cmp #NUM_OPS
  bcc done

  lda #0
  sta selection

  jmp done

prev_immediate:
  dec immediate

done:
  rts
.endproc

.proc execute
  jsr EXE_RAM

  sta result_a
  stx result_x
  sty result_y

  lda #FOCUS_RES
  sta focus

  rts
.endproc

palettes:
; https://www.nesdev.org/wiki/PPU_palettes
  ; background palettes
  .byte $0f, $2a, $0f, $0f
  .byte $0f, $0f, $0f, $0f
  .byte $0f, $0f, $0f, $0f
  .byte $0f, $0f, $0f, $0f

  ; sprite palettes
  .byte $0f, $0f, $0f, $0f
  .byte $0f, $0f, $0f, $0f
  .byte $0f, $0f, $0f, $0f
  .byte $0f, $0f, $0f, $0f

instructions:
  NUM_OPS = 11

  .byte 'L' - 'A' + TILE_A
  .byte 'D' - 'A' + TILE_A
  .byte 'A' - 'A' + TILE_A
  .byte $a9, 2
  .byte 0, 0, 0

  .byte 'L' - 'A' + TILE_A
  .byte 'D' - 'A' + TILE_A
  .byte 'X' - 'A' + TILE_A
  .byte $a2, 2
  .byte 0, 0, 0

  .byte 'L' - 'A' + TILE_A
  .byte 'D' - 'A' + TILE_A
  .byte 'Y' - 'A' + TILE_A
  .byte $a0, 2
  .byte 0, 0, 0

  .byte 'T' - 'A' + TILE_A
  .byte 'A' - 'A' + TILE_A
  .byte 'X' - 'A' + TILE_A
  .byte $aa, 1
  .byte 0, 0, 0

  .byte 'T' - 'A' + TILE_A
  .byte 'X' - 'A' + TILE_A
  .byte 'A' - 'A' + TILE_A
  .byte $8a, 1
  .byte 0, 0, 0

  .byte 'D' - 'A' + TILE_A
  .byte 'E' - 'A' + TILE_A
  .byte 'X' - 'A' + TILE_A
  .byte $ca, 1
  .byte 0, 0, 0

  .byte 'I' - 'A' + TILE_A
  .byte 'N' - 'A' + TILE_A
  .byte 'X' - 'A' + TILE_A
  .byte $e8, 1
  .byte 0, 0, 0

  .byte 'T' - 'A' + TILE_A
  .byte 'A' - 'A' + TILE_A
  .byte 'Y' - 'A' + TILE_A
  .byte $a8, 1
  .byte 0, 0, 0

  .byte 'T' - 'A' + TILE_A
  .byte 'Y' - 'A' + TILE_A
  .byte 'A' - 'A' + TILE_A
  .byte $98, 1
  .byte 0, 0, 0

  .byte 'D' - 'A' + TILE_A
  .byte 'E' - 'A' + TILE_A
  .byte 'Y' - 'A' + TILE_A
  .byte $88, 1
  .byte 0, 0, 0

  .byte 'I' - 'A' + TILE_A
  .byte 'N' - 'A' + TILE_A
  .byte 'Y' - 'A' + TILE_A
  .byte $c8, 1
  .byte 0, 0, 0