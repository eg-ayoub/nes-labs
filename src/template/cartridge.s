; NES Cartridge template with basic functionality
; simple nrom cartridge with 32k PRG ROM and 8k CHR ROM
; NES 2.0 format
; compiled with ca65

; generic macros
.MACPACK generic

; imports from linker config
.IMPORTZP __MAPPERNUM__
.IMPORTZP __MAPPERPLN__
.IMPORTZP __SUBMAPPER__
.IMPORTZP __PRGSIZELO__
.IMPORTZP __PRGSIZEHI__
.IMPORTZP __PRGRAMSH__
.IMPORTZP __PRGNVRAMSH__
.IMPORTZP __CHRSIZELO__
.IMPORTZP __CHRSIZEHI__
.IMPORTZP __CHRRAMSH__
.IMPORTZP __CHRNVRAMSH__
.IMPORT __OAMRAM_START__

; important addresses
; VRAM
PATTERN_TABLE_0 = $0000; Pattern table 0 in VRAM
PATTERN_TABLE_1 = $1000; Pattern table 1 in VRAM
NAME_TABLE_0 = $2000; Name table 0 in VRAM
NAME_TABLE_1 = $2400; Name table 1 in VRAM
VRAM_PALETTE = $3F00; Palette in VRAM


; important registers
; PPU
PPUCTRL = $2000
PPUMASK = $2001
PPUSTATUS = $2002
OAMADDR = $2003
OAMDATA = $2004
PPUSCROLL = $2005
PPUADDR = $2006
PPUDATA = $2007
OAMDMA = $4014
; APU - DMC
DMCCTRL = $4010
; APU - General
APUCTRL = $4015
APUSTATUS = $4015
APUFRAMECNT = $4017

; some useful macro definitions
.MACRO phall
    pha
    txa
    pha
    tya
    pha
.ENDMACRO

.MACRO plall
    pla
    tay
    pla
    tax
    pla
.ENDMACRO

; some useful constants
NIBBLE_UPPER = $F0
NIBBLE_LOWER = $0F
PALETTE_SIZE = $20
NAME_TABLE_ROWS = $1E ; 30 rows
NAME_TABLE_COLS = $20 ; 32 columns
ATTRIBUTE_TABLE_SIZE = $40 ; 64 bytes


; NES 2.0 header
.SEGMENT "HEADER"

.byte "NES", $1A ; identification string

.byte __PRGSIZELO__ ;LSB of number of 16k PRG ROM banks

.byte __CHRSIZELO__ ;LSB of number of 8k CHR ROM banks

.byte (__MAPPERNUM__ & NIBBLE_UPPER) | %00000000 ; flags 6
;       NNNN                                FTBM
;       ||||                                |||+-- NT mirroring type : 0 for horizontal mirroring
;       ||||                                ||+--- Battery-backed RAM: 0 for not present
;       ||||                                |+---- 512-byte trainer: 0 for not present
;       ||||                                +----- 4-screen mode: 0 is no
;       ++++------ Mapper number D0..D3: inluded from ld conf

.byte (__MAPPERNUM__ & NIBBLE_LOWER) << 4 | %00001000 ; flags 7
;       NNNN                                     10TT
;       ||||                                     ||++-- console type: 0 for NES
;       ||||                                     ||
;       ||||                                     ++---- NES 2.0 identifier
;       ++++------ Mapper number D4..D7: inluded from ld conf

.byte (__SUBMAPPER__ & NIBBLE_LOWER) << 4 | (__MAPPERPLN__ & NIBBLE_LOWER) ; flags 8
;       SSSS                                 NNNN
;       ||||                                 ++++-- Mapper number D8..D11: inluded from ld conf
;       ++++------ Submapper numer: inluded from ld conf

.byte (__CHRSIZEHI__ & NIBBLE_LOWER) << 4 | (__PRGSIZEHI__ & NIBBLE_LOWER) ; flags 9
;       CCCC                                 PPPP
;       ||||                                 ++++-- MSB of number of 16k PRG ROM banks
;       ++++------ MSB of number of 8k CHR ROM banks

.byte (__PRGNVRAMSH__ & NIBBLE_LOWER) << 4 | (__PRGRAMSH__ & NIBBLE_LOWER) ; flags 10
;       NNNN                                  PPPP
;       ||||                                  ++++-- Shift size of PRG RAM
;       ++++------ Shift size of PRG NVRAM

.byte (__CHRNVRAMSH__ & NIBBLE_LOWER) << 4 | (__CHRRAMSH__ & NIBBLE_LOWER) ; flags 11
;       NNNN                                  PPPP
;       ||||                                  ++++-- Shift size of CHR RAM
;       ++++------ Shift size of CHR NVRAM

.byte %00000000 ; flags 12
;      ------VV
;            ++-- CPU/PPU timing: 0 for NTSC NES

.byte %00000000 ; flags 13
; only for vs system and extended console typing

.byte %00000000 ; flags 14
;      ------RR
;            ++-- number of miscellaneous ROMs present

.byte %00000000 ; flags 15
;      --DDDDDD
;        ++++++-- default expansion device : 0 for unspecified

; important: we use as many segment definitions as we can
; to make sure that the linker will not place code/data in the wrong segment
; also for code clarity (declarations then code)

.SEGMENT "CODE"
.PROC _RESET
    sei ; mask interrupts
    cld ; disable unsupported decimal mode

    ; reset PPU state and disable NMIs
    lda #$00
    sta PPUCTRL
    sta PPUMASK

    ; reset APU state
    sta APUCTRL ; disable all channels
    sta DMCCTRL ; disable DMC IRQs
    lda #%01000000
    ;      I = interrput inhibit flag
    sta APUFRAMECNT ; disable apu frame IRQs

    ; reset stack pointer
    ldx #$FF
    txs
    bit PPUSTATUS ; N <= PPUSTATUS bit 7 - clear it

    ; we need to wait for two VBLANKs to make sure that the PPU is in a stable state
    ; wait for first VBLANK
    :
        bit PPUSTATUS
        bpl :-

    ; use the cycles to clear RAM
    ; clear ram
    lda #$00
    ldx #$00; LSB in RAM
    :
        sta $0000,x
        sta $0100,x
        sta $0200,x
        sta $0300,x
        sta $0400,x
        sta $0500,x
        sta $0600,x
        sta $0700,x
        inx
        bne :-
    
    ; having OAM DMA filled with 0 sill displays
    ; sprite id 0 in the top left corner
    ; we move them off the screen at X = 0xFF
    lda #$FF
    ldx #$00
    :
        sta __OAMRAM_START__,X
        inx
        inx
        inx
        inx
        bne :-

    bit PPUSTATUS ; clear PPUSTATUS bit 7 again
    ; wait for second VBLANK
    :
        bit PPUSTATUS
        bpl :-

    ; initialized, we can enable NMIs again
    lda #%10001000
    ;     |   +--- Sprite pattern table at $1000
    ;     +------- NMI enable
    sta PPUCTRL
    jmp main
.ENDPROC

.SEGMENT "ZEROPAGE"
; these variables are used by the NMI handler
nmi_lock: .res 1 ; set to 1 when NMI handler is running
NMI_LOCKED = $01
NMI_UNLOCKED = $00
nmi_control: .res 1 ; set to 0 to skip NMI handler, 1 to run NMI handler, 2 to disable rendering and skip NMI handler
NMI_CTRL_NO_RDR = $02

.SEGMENT "BSS"
palette: .res 32 ; current palette in RAM
                 ; can be updated anytime, so we write it to VRAM every frame

.SEGMENT "CODE"
.PROC _NMI
    ; important: even if vblank NMIs are "timed",
    ; it is possible an NMI is triggered while the NMI handler is running
    ; we want to skip the NMI handler in this case
    ; or we will have a stack overflow
    ; as for any interrupt, we push A, X and Y
    phall
    lda nmi_lock
    bne :+
        ; NMI flag is set, we skip the NMI handler
        jmp end
    :
    ; we lock NMI handling
    lda #NMI_LOCKED
    sta nmi_lock
    ; check if NMI was disabled
    lda nmi_control
    bne :+
        ; NMI skipped
        jmp end_ppu_update
    :
    cmp #NMI_CTRL_NO_RDR
    bne :+
        ; NMI skipped + rendering disabled
        lda #$00
        sta PPUMASK
        sta nmi_control ; we set nmi_control to 0 to keep skipping
                        ; NMIs without re-disabling rendering every frame
        jmp end_ppu_update
    :
ppu_update:
    ; inside NMI, now we can update the PPU

    ; update OAM DMA
    lda #$00
    sta OAMADDR ; first, we write $00 to OAMADDR
    lda #>__OAMRAM_START__ ; we get high byte of OAM DMA location, in this case $20
    sta OAMDMA ; then we write it to DMA register
    ; this will copy 256 bytes from $2000 to $2FFF to OAM

    ; maintain PPUCTRL
    lda #%10001000
    ;     |   +--- Sprite pattern table at $1000
    ;     +------- NMI enable
    sta PPUCTRL

    ; update palette
    lda PPUSTATUS ; clear BIT 7 of PPUSTATUS and 
                  ; empty the address latch so we can use PPUADDR
    lda #>VRAM_PALETTE ; we get high byte of VRAM_PALETTE
    ldx #<VRAM_PALETTE ; we get low byte of VRAM_PALETTE
    sta PPUADDR ; we write HI to PPUADDR
    stx PPUADDR ; we write LO to PPUADDR again to set the address latch
    ldx #$00
    :
        lda palette,x
        sta PPUDATA ; we write the palette data to VRAM
        inx
        cpx #PALETTE_SIZE
        bne :-

    ; mark frame ended
    ldx #$00
    stx nmi_control ; we set nmi_control to 0 to keep skipping 
                    ; NMIs util rendering is enabled again
end_ppu_update:
    ; we unlock NMI handling
    lda #NMI_UNLOCKED
    sta nmi_lock
end:
    ; we pull A, X and Y
    plall
    rti
.ENDPROC

.SEGMENT "CODE"
.PROC _IRQ
    rti
.ENDPROC

.SEGMENT "CODE"
; enable rendering and wait for NMI
.PROC ppu_render
    lda #$01
    sta nmi_control
    :
        lda nmi_control
        bne :-
    rts
.ENDPROC

; some read-only data needed 
.SEGMENT "RODATA"
nes_palette:
;     0  , 1,   2,   3
.byte $0F, $00, $10, $20 ; bg0 - greyscale
.byte $0F, $36, $26, $16 ; bg1 - red
.byte $0F, $39, $29, $19 ; bg2 - green
.byte $0F, $3C, $2C, $1C ; bg3 - blue
.byte $0F, $00, $10, $20 ; sp0 - greyscale
.byte $0F, $36, $26, $16 ; sp1 - red
.byte $0F, $39, $29, $19 ; sp2 - green
.byte $0F, $3C, $2C, $1C ; sp3 - blue

.SEGMENT "CODE"
.PROC main
    ; main process
    ; setup
    ; 1 - load palette
    ldx #$00
    :
        lda nes_palette,x
        sta palette,x ; we write the palette data to RAM
        inx
        cpx #PALETTE_SIZE
        bne :-
    jsr load_background
    loop:
    draw:
        jsr ppu_render
        jmp loop
.ENDPROC

.SEGMENT "ZEROPAGE"
nmt_pointer: .res 2 ; pointer to nametable in ROM

.SEGMENT "RODATA"
.align 256 ; align to 256 bytes so we can use the high byte of the address
background_0: .incbin "graphics/background_0.nmt" ; background nametable

.SEGMENT "CODE"
.PROC load_background
    ; clear PPUSTATUS latch so we can use PPUADDR
    lda PPUSTATUS
    ; nametable 0 to PPUADDR
    lda #>NAME_TABLE_0
    sta PPUADDR
    lda #<NAME_TABLE_0
    sta PPUADDR
    ; clear nametable
    lda #$00
    ldy #NAME_TABLE_ROWS
    :
        ldx #NAME_TABLE_COLS
        :
            sta PPUDATA
            dex
            bne :-
        dey
        bne :-
    ; clear attribute table
    ldx #ATTRIBUTE_TABLE_SIZE
    :
        sta PPUDATA
        dex
        bne :-
    ; fill nametable with tiles
    lda #<background_0
    sta nmt_pointer
    ldy #4
    :
        ldx #255
        :
            lda (nmt_pointer),x
            sta PPUDATA
            dex
            bne :-
        inc nmt_pointer
        dey
        bne :-
.ENDPROC

.SEGMENT "CHRROM"
.incbin "graphics/pattern_table.chr" ; CHR ROM data

.SEGMENT "VECTORS"
.addr _NMI ; NMI handler
.addr _RESET ; RESET handler
.addr _IRQ ; IRQ handler