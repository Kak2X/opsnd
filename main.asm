INCLUDE "hardware.asm"
INCLUDE "macro.asm"
INCLUDE "constants.asm"
INCLUDE "memory.asm"

; 
; BANK $00 - ???
; 
SECTION "bank00", ROM0
INCLUDE "src/bank00.asm"

; 
; BANK $01 - ???
; 
SECTION "bank01", ROMX, BANK[$01]
INCLUDE "src/bank01.asm"

; 
; BANK $02 - ???
; 
SECTION "bank02", ROMX, BANK[$02]
INCLUDE "src/bank02.asm"

; 
; BANK $03 - ???
; 
SECTION "bank03", ROMX, BANK[$03]
INCLUDE "src/bank03.asm"

; 
; BANK $04 - ???
; 
SECTION "bank04", ROMX, BANK[$04]
INCLUDE "src/bank04.asm"

; 
; BANK $05 - ???
; 
SECTION "bank05", ROMX, BANK[$05]
INCLUDE "src/bank05.asm"

; 
; BANK $06 - ???
; 
SECTION "bank06", ROMX, BANK[$06]
INCLUDE "src/bank06.asm"

; 
; BANK $07 - ???
; 
SECTION "bank07", ROMX, BANK[$07]
INCLUDE "src/bank07.asm"

; 
; BANK $08 - ???
; 
SECTION "bank08", ROMX, BANK[$08]
INCLUDE "src/bank08.asm"

; 
; BANK $09 - ???
; 
SECTION "bank09", ROMX, BANK[$09]
INCLUDE "src/bank09.asm"

; 
; BANK $0A - ???
; 
SECTION "bank0A", ROMX, BANK[$0A]
INCLUDE "src/bank0A.asm"

; 
; BANK $0B - ???
; 
SECTION "bank0B", ROMX, BANK[$0B]
INCLUDE "src/bank0B.asm"

; 
; BANK $0C - ???
; 
SECTION "bank0C", ROMX, BANK[$0C]
INCLUDE "src/bank0C.asm"

; 
; BANK $0D - ???
; 
SECTION "bank0D", ROMX, BANK[$0D]
INCLUDE "src/bank0D.asm"
INCLUDE "driver/bgm/bgm_4a.asm"
INCLUDE "driver/bgm/bgm_40.asm"
INCLUDE "driver/bgm/bgm_3e.asm"
INCLUDE "driver/bgm/bgm_0b.asm"
INCLUDE "driver/bgm/bgm_0a.asm"
INCLUDE "driver/bgm/bgm_09.asm"
INCLUDE "padding/L0D7FCC.asm"

; 
; BANK $0E - ???
; 
SECTION "bank0E", ROMX, BANK[$0E]
INCLUDE "src/bank0E.asm"
INCLUDE "driver/bgm/bgm_4b.asm"
INCLUDE "driver/bgm/bgm_04.asm"
INCLUDE "driver/bgm/bgm_08.asm"
INCLUDE "driver/bgm/bgm_01.asm"
INCLUDE "driver/bgm/bgm_02.asm"
INCLUDE "padding/L0E7F97.asm"

; 
; BANK $0F - ???
; 
SECTION "bank0F", ROMX, BANK[$0F]
INCLUDE "src/bank0F.asm"
INCLUDE "driver/bgm/bgm_06.asm"
INCLUDE "driver/bgm/bgm_03.asm"
INCLUDE "driver/bgm/bgm_05.asm"
INCLUDE "driver/bgm/bgm_49.asm"
INCLUDE "driver/bgm/bgm_07.asm"
INCLUDE "driver/bgm/bgm_3f.asm"
INCLUDE "driver/sfx/cmd_pause_unpause.asm"
INCLUDE "driver/sfx/sfx_0e.asm"
INCLUDE "driver/sfx/sfx_0f.asm"
INCLUDE "driver/sfx/sfx_10.asm"
INCLUDE "driver/sfx/sfx_11.asm"
INCLUDE "driver/sfx/sfx_12.asm"
INCLUDE "driver/sfx/sfx_13.asm"
INCLUDE "driver/sfx/sfx_14.asm"
INCLUDE "driver/sfx/sfx_15.asm"
INCLUDE "driver/sfx/sfx_16.asm"
INCLUDE "driver/sfx/sfx_17.asm"
INCLUDE "driver/sfx/sfx_18.asm"
INCLUDE "driver/sfx/sfx_19.asm"
INCLUDE "driver/sfx/sfx_1a.asm"
INCLUDE "driver/sfx/sfx_1b.asm"
INCLUDE "driver/sfx/sfx_1c.asm"
INCLUDE "driver/sfx/sfx_1d.asm"
INCLUDE "driver/sfx/sfx_1e.asm"
INCLUDE "driver/sfx/sfx_1f.asm"
INCLUDE "driver/sfx/sfx_20.asm"
INCLUDE "driver/sfx/sfx_21.asm"
INCLUDE "driver/sfx/sfx_22.asm"
INCLUDE "driver/sfx/sfx_23.asm"
INCLUDE "driver/sfx/sfx_24.asm"
INCLUDE "driver/sfx/sfx_25.asm"
INCLUDE "driver/sfx/sfx_26.asm"
INCLUDE "driver/sfx/sfx_27.asm"
INCLUDE "driver/sfx/sfx_28.asm"
INCLUDE "driver/sfx/sfx_29.asm"
INCLUDE "driver/sfx/sfx_2a.asm"
INCLUDE "driver/sfx/sfx_2b.asm"
INCLUDE "driver/sfx/sfx_2c.asm"
INCLUDE "driver/sfx/sfx_2d.asm"
INCLUDE "driver/sfx/sfx_2e.asm"
INCLUDE "driver/sfx/sfx_2f.asm"
INCLUDE "driver/sfx/sfx_30.asm"
INCLUDE "driver/sfx/sfx_31.asm"
INCLUDE "driver/sfx/sfx_32.asm"
INCLUDE "driver/sfx/sfx_33.asm"
INCLUDE "driver/sfx/sfx_34.asm"
INCLUDE "driver/sfx/sfx_35.asm"
INCLUDE "driver/sfx/sfx_36.asm"
INCLUDE "driver/sfx/sfx_37.asm"
INCLUDE "driver/sfx/sfx_38.asm"
INCLUDE "driver/sfx/sfx_39.asm"
INCLUDE "driver/sfx/sfx_3a.asm"
INCLUDE "driver/sfx/sfx_3b.asm"
INCLUDE "padding/L0F7FF6.asm"

; 
; BANK $10 - ???
; 
SECTION "bank10", ROMX, BANK[$10]
INCLUDE "src/bank10.asm"

; 
; BANK $11 - ???
; 
SECTION "bank11", ROMX, BANK[$11]
INCLUDE "src/bank11.asm"

; 
; BANK $12 - ???
; 
SECTION "bank12", ROMX, BANK[$12]
INCLUDE "src/bank12.asm"

; 
; BANK $13 - ???
; 
SECTION "bank13", ROMX, BANK[$13]
INCLUDE "src/bank13.asm"

; 
; BANK $14 - ???
; 
SECTION "bank14", ROMX, BANK[$14]
INCLUDE "src/bank14.asm"

; 
; BANK $15 - ???
; 
SECTION "bank15", ROMX, BANK[$15]
INCLUDE "src/bank15.asm"

; 
; BANK $16 - ???
; 
SECTION "bank16", ROMX, BANK[$16]
INCLUDE "src/bank16.asm"

; 
; BANK $17 - ???
; 
SECTION "bank17", ROMX, BANK[$17]
INCLUDE "src/bank17.asm"

; 
; BANK $18 - ???
; 
SECTION "bank18", ROMX, BANK[$18]
INCLUDE "src/bank18.asm"

; 
; BANK $19 - ???
; 
SECTION "bank19", ROMX, BANK[$19]
INCLUDE "src/bank19.asm"

; 
; BANK $1A - ???
; 
SECTION "bank1A", ROMX, BANK[$1A]
INCLUDE "src/bank1A.asm"

; 
; BANK $1B - ???
; 
SECTION "bank1B", ROMX, BANK[$1B]
INCLUDE "src/bank1B.asm"

; 
; BANK $1C - ???
; 
SECTION "bank1C", ROMX, BANK[$1C]
INCLUDE "src/bank1C.asm"

; 
; BANK $1D - ???
; 
SECTION "bank1D", ROMX, BANK[$1D]
INCLUDE "src/bank1D.asm"

; 
; BANK $1E - ???
; 
SECTION "bank1E", ROMX, BANK[$1E]
INCLUDE "src/bank1E.asm"

; 
; BANK $1F - ???
; 
SECTION "bank1F", ROMX, BANK[$1F]
INCLUDE "src/bank1F.asm"
