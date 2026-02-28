SECTION "Sound RAM - Channel Slots", WRAM0, ALIGN[8]
wBGMCh1Info:                 ds $20
wBGMCh2Info:                 ds $20
wBGMCh3Info:                 ds $20
wBGMCh4Info:                 ds $20
wSFXCh1Info:                 ds $20
wSFXCh2Info:                 ds $20
wSFXCh3Info:                 ds $20
wSFXCh4Info:                 ds $20

SECTION "Sound RAM - Other variables", WRAM0
wSndSfxPriority:             db ; Keeps track of high priority sound effects being played.
wSndEnaChBGM:                db ; Keeps track of the last rNR51 value used modified by a BGM SndInfo.
wSndCh3DelayCut:             db ; Keeps track of the last rNR31 wave cutoff value (wave_cutoff).
wSndChProcLeft:              db ; Number of remaining wBGMCh*Info/wSFXCh*Info structs to process
wSndFadeStatus:              db ; Fade control
wSndFadeTimer:               db ; Fade timer. When it elapses, the global volume is altered by 1.
wSndVolume:                  db ; Global volume. Copied directly to rNR50.
wSndIdReqTbl:                ds $08 ; Sound IDs to play are written here

SECTION "Sound RAM - HRAM", HRAM
hSndInfoCurPtr_Low:          db ; Ptr to Currently processed SNDInfo structure
hSndInfoCurPtr_High:         db ; Ptr to Currently processed SNDInfo structure

hSndPlayCnt:                 db ; Sound Played Counter (bits3-0)
hSndPlaySetCnt:              db ; Sound Req Counter (bits3-0) (if != hSndPlaySetCnt, start a new track)
hSndInfoCurDataPtr_Low:      db ; Ptr to current sound channel data (initially copied from iSndInfo_DataPtr)
hSndInfoCurDataPtr_High:     db ; Ptr to current sound channel data (initially copied from iSndInfo_DataPtr)
hSndChEnaMask:               db ; rNR51 bitmask. This is a global version of iSndInfo_Unused11 which operates in mono (only the lower nybble is used)

hTemp:                       db ; Temporary value
hROMBank:                    db ; Current ROM Bank

; Sound channel data header (ROM)
; =============== SONG FORMAT ===============
DEF iSndHeader_NumChannels             EQU $00 ; Number of channels (array of iSndChHeader structs comes next)
DEF iSndChHeader_Status                EQU $00 ; Matches iSndInfo_Status and so on
DEF iSndChHeader_RegPtr                EQU $01
DEF iSndChHeader_DataPtr_Low           EQU $02
DEF iSndChHeader_DataPtr_High          EQU $03
DEF iSndChHeader_FreqDataIdBase        EQU $04
DEF iSndChHeader_Unused5               EQU $05

; Sound channel info (RAM)
DEF iSndInfo_Status                    EQU $00 ; SndInfo status bitmask
DEF iSndInfo_RegPtr                    EQU $01 ; Determines sound channel. Always points to rNR*3, and is never changed after being set.
DEF iSndInfo_DataPtr_Low               EQU $02 ; Pointer to song data (low byte)
DEF iSndInfo_DataPtr_High              EQU $03 ; Pointer to song data (high byte)
DEF iSndInfo_DataPtr_Bank              EQU $04 ; Pointer to song data (bank number)
DEF iSndInfo_FreqDataIdBase            EQU $05 ; Base index/note id to Sound_FreqDataTbl for indexes > 0
DEF iSndInfo_VibratoId                 EQU $06 ; Id of the vibrato set loaded. 
DEF iSndInfo_DataPtrStackIdx           EQU $07 ; Stack index for data pointers saved and restored by Sound_Cmd_Call and Sound_Cmd_Ret. Initialized to $20 (end of SndInfo) and decremented on pushes.
DEF iSndInfo_LengthTarget              EQU $08 ; Handles delays -- the current sound register settings are kept until it matches iSndInfo_LengthTarget Set by song data.
DEF iSndInfo_LengthTimer               EQU $09 ; Increases every time a SndInfo isn't paused/disabled. Once it reaches iSndInfo_LengthTarget it resets.
DEF iSndInfo_VibratoDataOffset         EQU $0A ; Offset to the current vibrato table.
DEF iSndInfo_RegNRx1Data               EQU $0B ; Last value written to rNR*1 | $FF00+(iSndInfo_RegPtr-2). Only written by Command IDs -- this isn't updated by the standard Sound_UpdateCustomRegs.
DEF iSndInfo_RegNR10Data               EQU $0C ; Last value written to NR10 by the unused sound command Sound_Cmd_WriteToNR10.
DEF iSndInfo_VolPredict                EQU $0D ; "Volume timer" which predicts the effective volume level (due to sweeps) at any given frame, used when restoring BGM playback. Low nybble is the timer, upper nybble is the predicted volume.
DEF iSndInfo_RegNRx2Data               EQU $0E ; Last value written to rNR*2 | $FF00+(iSndInfo_RegPtr-1)
DEF iSndInfo_RegNRx3Data               EQU $0F ; Last value written to rNR*3 | $FF00+(iSndInfo_RegPtr)
DEF iSndInfo_RegNRx4Data               EQU $10 ; Last value written to rNR*4 | $FF00+(iSndInfo_RegPtr+1)
DEF iSndInfo_Unused11                  EQU $11 ; [Free space]
DEF iSndInfo_WaveSetId                 EQU $12 ; Id of last wave set loaded
DEF iSndInfo_LoopTimerTbl              EQU $13 ; Table with timers counting down, used to determine how many times to "jump" the data pointer elsewhere before continuing.
DEF iSndInfo_SlideFreqOffsetLow        EQU $16 ; Frequency offset (low byte) applied each active frame of a pitch slide.
DEF iSndInfo_SlideFreqOffsetHigh       EQU $17 ; "" (high byte) ""
DEF iSndInfo_SlideTimer                EQU $18 ; When this elapses, the pitch slide ends.
DEF iSndInfo_FreqOffsetLow             EQU $19 ; Offset applied to the current frequency value.
DEF iSndInfo_FreqOffsetHigh            EQU $1A ; "" (high byte) ""
DEF iSndInfo_End                       EQU $20 ; Pointer stack moving up

; ldi requirements
ASSERT iSndInfo_LengthTarget + 1 == iSndInfo_LengthTimer
ASSERT iSndInfo_LengthTimer + 1 == iSndInfo_VibratoDataOffset