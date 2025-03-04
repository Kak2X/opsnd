; =============== Sound_SndListTable ===============
; Table of sound assignments, ordered by ID.
Sound_SndListTable_\1:
IF \2
Sound_SndListTable_Main:
ENDC
	;     SONG HEADER         INIT CODE                          ID
	dsong SndHeader_BGM_01  , Sound_StartNothing_\1            ; $80
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $81
	dsong SndHeader_BGM_02  , Sound_StartNewBGM_\1             ; $82
	dsong SndHeader_BGM_03  , Sound_StartNewBGM_\1             ; $83
	dsong SndHeader_BGM_04  , Sound_StartNewBGM_\1             ; $84
	dsong SndHeader_BGM_05  , Sound_StartNewBGM_\1             ; $85
	dsong SndHeader_BGM_06  , Sound_StartNewBGM_\1             ; $86
	dsong SndHeader_BGM_07  , Sound_StartNewBGM_\1             ; $87
	dsong SndHeader_BGM_08  , Sound_StartNewBGM_\1             ; $88
	dsong SndHeader_BGM_09  , Sound_StartNewBGM_\1             ; $89
	dsong SndHeader_BGM_0A  , Sound_StartNewBGM_\1             ; $8A
	dsong SndHeader_BGM_0B  , Sound_StartNewBGM_\1             ; $8B
	dsong SndHeader_Pause   , Sound_PauseAll_\1                ; $8C
	dsong SndHeader_Unpause , Sound_UnpauseAll_\1              ; $8D
	dsong SndHeader_SFX_0E  , Sound_StartNewSFX234_\1          ; $8E
	dsong SndHeader_SFX_0F  , Sound_StartNewSFX234_\1          ; $8F
	dsong SndHeader_SFX_10  , Sound_StartNewSFX234_\1          ; $90
	dsong SndHeader_SFX_11  , Sound_StartNewSFX234_\1          ; $91
	dsong SndHeader_SFX_12  , Sound_StartNewSFX234_\1          ; $92
	dsong SndHeader_SFX_13  , Sound_StartNewSFX234_\1          ; $93
	dsong SndHeader_SFX_14  , Sound_StartNewSFX234_\1          ; $94
	dsong SndHeader_SFX_15  , Sound_StartNewSFX4_\1            ; $95
	dsong SndHeader_SFX_16  , Sound_StartNewSFX4_\1            ; $96
	dsong SndHeader_SFX_17  , Sound_StartNewSFX1234_\1         ; $97
	dsong SndHeader_SFX_18  , Sound_StartNewSFX4_\1            ; $98
	dsong SndHeader_SFX_19  , Sound_StartNewSFX234_\1          ; $99
	dsong SndHeader_SFX_1A  , Sound_StartNewSFX234_\1          ; $9A
	dsong SndHeader_SFX_1B  , Sound_StartNewSFX234_\1          ; $9B
	dsong SndHeader_SFX_1C  , Sound_StartNewSFX1234_\1         ; $9C
	dsong SndHeader_SFX_1D  , Sound_StartNewSFX1234_\1         ; $9D
	dsong SndHeader_SFX_1E  , Sound_StartNewSFX1234_\1         ; $9E
	dsong SndHeader_SFX_1F  , Sound_StartNewSFX4_\1            ; $9F
	dsong SndHeader_SFX_20  , Sound_StartNewSFX234_\1          ; $A0
	dsong SndHeader_SFX_21  , Sound_StartNewSFX4_\1            ; $A1
	dsong SndHeader_SFX_22  , Sound_StartNewSFX4_\1            ; $A2
	dsong SndHeader_SFX_23  , Sound_StartNewSFX1234_\1         ; $A3
	dsong SndHeader_SFX_24  , Sound_StartNewSFX4_\1            ; $A4
	dsong SndHeader_SFX_25  , Sound_StartNewSFX4_\1            ; $A5
	dsong SndHeader_SFX_26  , Sound_StartNewSFX234_\1          ; $A6
	dsong SndHeader_SFX_27  , Sound_StartNewSFX1234_\1         ; $A7
	dsong SndHeader_SFX_28  , Sound_StartNewSFX234_\1          ; $A8
	dsong SndHeader_SFX_29  , Sound_StartNewSFX4_\1            ; $A9
	dsong SndHeader_SFX_2A  , Sound_StartNewSFX4_\1            ; $AA
	dsong SndHeader_SFX_2B  , Sound_StartNewSFX234_\1          ; $AB
	dsong SndHeader_SFX_2C  , Sound_StartNewSFX234_\1          ; $AC
	dsong SndHeader_SFX_2D  , Sound_StartNewSFX234_\1          ; $AD
	dsong SndHeader_SFX_2E  , Sound_StartNewSFX4_\1            ; $AE
	dsong SndHeader_SFX_2F  , Sound_StartNewSFX4_\1            ; $AF
	dsong SndHeader_SFX_30  , Sound_StartNewSFX4_\1            ; $B0
	dsong SndHeader_SFX_31  , Sound_StartNewSFX1234_\1         ; $B1
	dsong SndHeader_SFX_32  , Sound_StartNewSFX4_\1            ; $B2
	dsong SndHeader_SFX_33  , Sound_StartNewSFX4_\1            ; $B3
	dsong SndHeader_SFX_34  , Sound_StartNewSFX1234_\1         ; $B4
	dsong SndHeader_SFX_35  , Sound_StartNewSFX4_\1            ; $B5
	dsong SndHeader_SFX_36  , Sound_StartNewSFX234_\1          ; $B6
	dsong SndHeader_SFX_37  , Sound_StartNewSFX4_\1            ; $B7
	dsong SndHeader_SFX_38  , Sound_StartNewSFX4_\1            ; $B8
	dsong SndHeader_SFX_39  , Sound_StartNewSFX1234_\1         ; $B9
	dsong SndHeader_SFX_3A  , Sound_StartNewSFX1234_\1         ; $BA
	dsong SndHeader_SFX_3B  , Sound_StartNewSFX234_\1          ; $BB
	dsong SndHeader_BGM_01  , Sound_StartNewSFX4_\1            ; $BC
	dsong SndHeader_BGM_01  , Sound_StartNewSFX234_\1          ; $BD
	dsong SndHeader_BGM_3E  , Sound_StartNewBGM_\1             ; $BE
	dsong SndHeader_BGM_3F  , Sound_StartNewBGM_\1             ; $BF
	dsong SndHeader_BGM_40  , Sound_StartNewBGM_\1             ; $C0
	dsong SndHeader_BGM_01  , Sound_StartNewSFX234_\1          ; $C1
	dsong SndHeader_BGM_01  , Sound_StartNewSFX4_\1            ; $C2
	dsong SndHeader_BGM_01  , Sound_StartNewSFX4_\1            ; $C3
	dsong SndHeader_BGM_01  , Sound_StartNewSFX234_\1          ; $C4
	dsong SndHeader_BGM_01  , Sound_StartNewSFX4_\1            ; $C5
	dsong SndHeader_BGM_01  , Sound_StartNewSFX4_\1            ; $C6
	dsong SndHeader_BGM_01  , Sound_StartNewSFX4_\1            ; $C7
	dsong SndHeader_BGM_01  , Sound_StartNewSFX4_\1            ; $C8
	dsong SndHeader_BGM_49  , Sound_StartNewBGM_\1             ; $C9
	dsong SndHeader_BGM_4A  , Sound_StartNewBGM_\1             ; $CA
	dsong SndHeader_BGM_4B  , Sound_StartNewBGM_\1             ; $CB
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $CC
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $CD
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $CE
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $CF
	dsong SndHeader_BGM_01  , Sound_StartNewSFX1234_\1         ; $D0
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $D1
	dsong SndHeader_BGM_01  , Sound_StartNewSFX4_\1            ; $D2
	dsong SndHeader_BGM_01  , Sound_StartNewSFX4_\1            ; $D3
	dsong SndHeader_BGM_01  , Sound_StartNewSFX4_\1            ; $D4
	dsong SndHeader_BGM_01  , Sound_StartNewSFX4_\1            ; $D5
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $D6
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $D7
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $D8
	dsong SndHeader_BGM_01  , Sound_StartNewSFX4_\1            ; $D9
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $DA
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $DB
	dsong SndHeader_BGM_01  , Sound_StartNewSFX1234_\1         ; $DC
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $DD
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $DE
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $DF
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $E0
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $E1
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $E2
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $E3
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $E4
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $E5
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $E6
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $E7
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $E8
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $E9
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $EA
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $EB
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $EC
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $ED
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $EE
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $EF
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $F0
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $F1
	dsong SndHeader_BGM_01  , Sound_StartNewBGM_\1             ; $F2
	dsong SndHeader_BGM_01  , Sound_FastSlideSFXtoC_8_\1       ; $F3
	dsong SndHeader_BGM_01  , Sound_SlowSlideSFXtoF#4_\1       ; $F4
.end: