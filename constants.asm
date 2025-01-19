DEF SNDIDREQ_SIZE      EQU $08
DEF SNDINFO_SIZE       EQU $20 ; Size of iSndInfo struct
DEF SND_CH1_PTR        EQU LOW(rNR13)
DEF SND_CH2_PTR        EQU LOW(rNR23)
DEF SND_CH3_PTR        EQU LOW(rNR33)
DEF SND_CH4_PTR        EQU LOW(rNR43)
DEF SNDLEN_INFINITE    EQU $FF

; iSndInfo_Status
DEF SISB_PAUSE            EQU 0 ; If set, iSndInfo processing is paused for that channel
DEF SISB_SKIPNRx2         EQU 1 ; If set, rNR*2 won't be updated
DEF SISB_USEDBYSFX        EQU 2 ; wBGMCh*Info only. If set, it marks that a sound effect is currently using the channel.
DEF SISB_SFX              EQU 3 ; If set, the SndInfo is handled as a sound effect. If clear, it's a BGM.
DEF SISB_SLIDE            EQU 5 ; If set, a pitch slide is in progress.
DEF SISB_VIBRATO          EQU 6 ; If set, vibrato is enabled for that channel.
DEF SISB_ENABLED          EQU 7 ; If set, iSndInfo processing is enabled for that channel

DEF SIS_PAUSE             EQU 1 << SISB_PAUSE
DEF SIS_SKIPNRx2          EQU 1 << SISB_SKIPNRx2    
DEF SIS_USEDBYSFX         EQU 1 << SISB_USEDBYSFX   
DEF SIS_SFX               EQU 1 << SISB_SFX         
DEF SIS_SLIDE             EQU 1 << SISB_SLIDE
DEF SIS_VIBRATO           EQU 1 << SISB_VIBRATO
DEF SIS_ENABLED           EQU 1 << SISB_ENABLED

; wSndFadeStatus
DEF SFDB_FADEIN           EQU 7 ; If set, the song fades in
DEF SFD_FADEIN            EQU 1 << SFDB_FADEIN ; If set, the song fades in

DEF SNDCMD_FADEIN         EQU $10
DEF SNDCMD_FADEOUT        EQU $20
DEF SNDCMD_CH1VOL         EQU $30
DEF SNDCMD_CH2VOL         EQU $40
DEF SNDCMD_CH3VOL         EQU $50
DEF SNDCMD_CH4VOL         EQU $60
DEF SNDCMD_BASE           EQU $E0
DEF SNDNOTE_BASE          EQU $80

; Sound Channel Info IDs, passed from the outside world
DEF SCI_BGMCH1            EQU $00
DEF SCI_BGMCH2            EQU $01
DEF SCI_BGMCH3            EQU $02
DEF SCI_BGMCH4            EQU $03
DEF SCI_SFXCH1            EQU $04
DEF SCI_SFXCH2            EQU $05
DEF SCI_SFXCH3            EQU $06
DEF SCI_SFXCH4            EQU $07

; Vibrato data commands
DEF VIBCMD_LOOP           EQU $80

; Notes (note)
DEF C_                    EQU 0
DEF C#                    EQU 1
DEF D_                    EQU 2
DEF D#                    EQU 3
DEF E_                    EQU 4
DEF F_                    EQU 5
DEF F#                    EQU 6
DEF G_                    EQU 7
DEF G#                    EQU 8
DEF A_                    EQU 9
DEF A#                    EQU 10
DEF B_                    EQU 11

;--------------

; DMG Sound List
DEF SND_MUTE              EQU $00
DEF SND_BASE              EQU $80
DEF SND_NONE              EQU SND_BASE+$00
DEF BGM_CHARSELECT        EQU SND_BASE+$01
DEF BGM_STAGECLEAR        EQU SND_BASE+$02
DEF BGM_BIGSHOT           EQU SND_BASE+$03
DEF BGM_ESAKA             EQU SND_BASE+$04
DEF BGM_CREDITS           EQU SND_BASE+$05
DEF BGM_GEESE             EQU SND_BASE+$06
DEF BGM_ARASHI            EQU SND_BASE+$07
DEF BGM_KAGURA            EQU SND_BASE+$08
DEF BGM_GOENITZ           EQU SND_BASE+$09
DEF BGM_GOENITZCUTSCENE   EQU SND_BASE+$0A
DEF BGM_ENDING            EQU SND_BASE+$0B
DEF SNC_PAUSE             EQU SND_BASE+$0C
DEF SNC_UNPAUSE           EQU SND_BASE+$0D
DEF SFX_CURSORMOVE        EQU SND_BASE+$0E
DEF SFX_CHARSELECTED      EQU SND_BASE+$0F
DEF SFX_CHARGEMETER       EQU SND_BASE+$10
DEF SFX_SUPERMOVE         EQU SND_BASE+$11
DEF SFX_LIGHT             EQU SND_BASE+$12
DEF SFX_HEAVY             EQU SND_BASE+$13
DEF SFX_BLOCK             EQU SND_BASE+$14
DEF SFX_TAUNT             EQU SND_BASE+$15
DEF SFX_HIT               EQU SND_BASE+$16
DEF SFX_MULTIHIT          EQU SND_BASE+$17
DEF BGM_KAGURACUTSCENE    EQU SND_BASE+$18
DEF BGM_MRKARATE          EQU SND_BASE+$19
DEF SFX_GROUNDHIT         EQU SND_BASE+$1A
DEF SFX_DROP              EQU SND_BASE+$1B
DEF SFX_SUPERJUMP         EQU SND_BASE+$1C
DEF SFX_STEP              EQU SND_BASE+$1D
DEF BGM_INTRO             EQU SND_BASE+$1E
DEF BGM_MRKARATECUTSCENE  EQU SND_BASE+$1F
;DEF SND_ID_20            EQU SND_BASE+$20
;DEF SND_ID_21            EQU SND_BASE+$21
;DEF SND_ID_22            EQU SND_BASE+$22
;DEF SND_ID_23            EQU SND_BASE+$23
;DEF SND_ID_24            EQU SND_BASE+$24
;DEF SND_ID_25            EQU SND_BASE+$25
DEF SFX_STEP_HEAVY        EQU SND_BASE+$26
DEF SFX_GRAB              EQU SND_BASE+$27
DEF SFX_FIREHIT_A         EQU SND_BASE+$28
DEF SFX_FIREHIT_B         EQU SND_BASE+$29
DEF SFX_MOVEJUMP_A        EQU SND_BASE+$2A
DEF SFX_PROJ_SM           EQU SND_BASE+$2B
DEF SFX_MOVEJUMP_B        EQU SND_BASE+$2C
DEF SFX_REFLECT           EQU SND_BASE+$2D
DEF SFX_UNUSED_SIREN      EQU SND_BASE+$2E ; [TCRF] Not used by anything, only playable in the sound test.
DEF SFX_UNUSED_NULL       EQU SND_BASE+$2F ; [TCRF] Not used by anything, only playable in the sound test.
DEF SFX_PSYCTEL           EQU SND_BASE+$30
DEF SFX_GAMEOVER          EQU SND_BASE+$31
;DEF SND_ID_32            EQU SND_BASE+$32
;DEF SND_ID_33            EQU SND_BASE+$33
DEF SND_LAST_VALID        EQU SND_BASE+$74