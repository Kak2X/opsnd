; =============== Sound_Do ===============
; Entry point of the main sound code.
; This is an improved version of the sound driver used in KOF96.
Sound_Do_\1:
IF \2
Sound_Do_Main:
ENDC
	; Check if there's anything new that we want to play
	call Sound_ChkNewSnd_\1

	; Update the "SFX playing, mute BGM" flag for each channel
	ld   de, wSFXCh1Info
	ld   hl, wBGMCh1Info
	call Sound_MarkSFXChUse_\1
	ld   de, wSFXCh2Info
	ld   hl, wBGMCh2Info
	call Sound_MarkSFXChUse_\1
	ld   de, wSFXCh3Info
	ld   hl, wBGMCh3Info
	call Sound_MarkSFXChUse_\1
	ld   de, wSFXCh4Info
	ld   hl, wBGMCh4Info
	call Sound_MarkSFXChUse_\1

	;
	; Handle all of the sound channels.
	;
	ld   hl, wBGMCh1Info	; HL = Ptr to first SndInfo structure
	ld   a, $08				; Remaining SndInfo (4 channels, 2 sets (BGM + SFX))
	ld   [wSndChProcLeft], a
.loop:
	push hl						; Save ptr to SndInfo
		ld   a, [hl]			; Read iSndInfo_Status
		bit  SISB_ENABLED, a	; Processing for the channel enabled?
		call nz, Sound_DoChSndInfo_\1	; If so, call
		ld   hl, wSndChProcLeft ; SndInfoLeft--
		dec  [hl]				;
	pop  hl						; Restore SndInfo ptr

	; Seek to the next channel
	ld   de, SNDINFO_SIZE	; Ptr += SNDINFO_SIZE
	add  hl, de

	jr   nz, .loop			; If there are channels left to process, jump
	
	; Handle (global) fade in/out if requested
	ld   a, [wSndFadeStatus]
	or   a
	call nz, Sound_DoFade_\1
	
	; Update the volume level timer
	ld   hl, wBGMCh1Info + iSndInfo_VolPredict
	ld   de, wBGMCh1Info + iSndInfo_RegNRx2Data
	call Sound_UpdateVolPredict_\1
	ld   hl, wBGMCh2Info + iSndInfo_VolPredict
	ld   de, wBGMCh2Info + iSndInfo_RegNRx2Data
	call Sound_UpdateVolPredict_\1
	ld   hl, wBGMCh4Info + iSndInfo_VolPredict
	ld   de, wBGMCh4Info + iSndInfo_RegNRx2Data
	jp   Sound_UpdateVolPredict_\1
	
; =============== Sound_ReqPlayId ===============
; Requests playback for a new sound ID.
; IN
; - C: Sound ID to play
Sound_ReqPlayId_\1:
IF \2
Sound_ReqPlayId_Main:
ENDC
	; Increment request counter (circular buffer index)
	ld   hl, hSndPlaySetCnt
	ld   a, [hl]			; A = hSndPlaySetCnt+1 % 8
	inc  a
	and  $07
	
	; Use request counter as index to wSndIdReqTbl
	; and write the Sound ID there	
	ld   hl, wSndIdReqTbl	; HL = wSndIdReqTbl + A
	ld   e, a
	ld   d, $00
	add  hl, de
	ld   [hl], c			; Write ID there
	
	; Save request counter
	ld   hl, hSndPlaySetCnt
	ld   [hl], e
	ret

; =============== Sound_CmdS_StartSlide ===============
; Version of Sound_StartSlide used by sound data commands.
; IN
; - A: Slide length, in frames
; - BC: Target frequency
; - DE: Ptr to SndChInfo
Sound_CmdS_StartSlide_\1:
	push bc
		ld   c, a	; Matches C = E from below
		jr   Sound_StartSlide_Main.fromCmd

; =============== Sound_StartSlide ===============
; Starts a smooth pitch slide to the specified frequency in a fixed amount of frames.
;
; This subroutine will do the following:
; - Enable the feature
; - Save the slide length
; - Figure out by how much the frequency should be altered each frame
;   the slide is active, then save the result to the channel.
;
; With the feature flag enabled, the sound channel's frequency will be
; updated by Sound_DoChSndInfo_ChkSlide.
;
; This cannot be used when the Vibrato is enabled as that too directly modifies
; the sound registers and gets priority (Sound_DoChSndInfo_ChkSlide gets skipped over).
;
; IN
; - D: SndChInfo ID
;      Index to Sound_SndChInfoPtrTable.
; - E: Slide length, in frames
; - BC: Target frequency
Sound_StartSlide_\1:
IF \2
Sound_StartSlide_Main:
ENDC
	;
	; First, get the SndChInfo pointer.
	; We're given an ID for it since this is callable from outside the driver.
	;
	; Since this is also used as a sound data command (see Sound_CmdS_StartSlide),
	; we have to respect the convention and put the result into DE:
	; DE = Sound_SndChInfoPtrTable[A]
	;
	; As E is already used by the length, it will be moved to C until it gets saved.
	;
	push bc ; Save dest frequency
		push de		; Save timer from E...
			ld   c, d
			ld   b, $00
			ld   hl, Sound_SndChInfoPtrTable_\1
			add  hl, bc
			add  hl, bc
			ldi  a, [hl]
			ld   d, [hl]
			ld   e, a
		pop  bc		; ...to C
		
	.fromCmd:
	
		; Enable the feature
		ld   a, [de]
		set  SISB_SLIDE, a
		ld   [de], a
		
		; Write the amount of frames the slide should take.
		ld   hl, iSndInfo_SlideTimer
		add  hl, de
		ld   [hl], c
	pop  bc ; Restore dest frequency
	
	
	;
	; Calculate the frequency offset.
	; This is calculated once, and will be used for the remainder of the slide.
	;
	; Essentially:
	; Offset = (DestFreq - CurFreq) / Timer
	;
	; There's a bit more to it since there's no native division opcode and
	; the routine handling it in software requires unsigned numbers.
	;
	
	; BC = CurFreq - DestFreq
	; This is the other way around, causing the result to be inverted.
	; It doesn't matter here for the most part, because...
	ld   hl, iSndInfo_RegNRx3Data
	add  hl, de
	ldi  a, [hl]	; A = iSndInfo_RegNRx3Data
	sub  c			; - Target
	ld   c, a
	ld   a, [hl]
	sbc  b
	ld   b, a
	
	; ...the division function wants a positive number.
	push bc	; Save signed diff
		bit  7, b			; BC >= 0?
		jr   z, .findOffs	; If so, jump
		ld   a, c			; Otherwise, BC = -BC
		xor  $FF
		ld   c, a
		ld   a, b
		xor  $FF
		ld   b, a
	.findOffs:
		; BC /= iSndInfo_SlideTimer
		ld   hl, iSndInfo_SlideTimer
		add  hl, de
		ld   a, [hl]
		push de
			call Sound_Div_\1
		pop  de
		
	; Put back the minus sign we lost along the way, if needed.
	; Because we did Cur - Dest rather than Dest - Cur, the check below is inverted.
	pop  hl					; Restore unsigned diff
	bit  7, h				; Diff < 0?
	jr   nz, .setOffset		; If so, jump
	ld   a, c				; Otherwise, BC = -BC
	xor  $FF
	ld   c, a
	ld   a, b
	xor  $FF
	ld   b, a
	
.setOffset:
	; Save it to the sound channel info
	ld   hl, iSndInfo_SlideFreqOffsetLow
	add  hl, de
	ld   [hl], c
	inc  hl
	ld   [hl], b
	ret
	
; =============== Sound_Div ===============
; Binary division.
;
; Uses a standard long division algorithm.
; See also: https://en.wikipedia.org/wiki/Division_algorithm#Long_division
;
; IN
; - BC: Dividend
; - A: Divisor
; OUT
; - BC: Quotient
; - HL: Remainder
Sound_Div_\1:
	; Dividing by zero will return an invalid result,
	; so might as well return the dividend back early.
	or   a
	ret  z
	
	ld   e, a		; E = Divisor
	xor  a			; HL = Remainder
	ld   l, a		
	ld   h, a
	
	; For each bit/digit in the dividend, from left to right...
	ld   d, $10		; D = 16 (bit count)
	sla  c			; Initial << 1
	rl   b
.loop:
	rl   l			; Shift the digit left, to the reminder
	rl   h
	
	;
	; Subtract the divisor from the reminder only if it can fit.
	; If it does, shift a 1 to the reminder, otherwise a 0.
	;
	
	; HL < E? If so, don't subtract & shift 0.
	; The result of this check sets the carry flag, so ccf'ing it gets the 0.
	ld   a, l		; A = L - E
	sub  e			; (Carry)
	ld   a, h		
	sbc  a, $00		; H -= (carry)
	jr   c, .shOut	; Did we underflow? If so, jump
	
	; HL -= E otherwise, & shift 1.
	; Since HL will always be >= E when we get here, the carry will always be clear,
	; so ccf'ing it gets the 1.
	ld   a, l
	sub  e
	ld   l, a
	ld   a, h
	sbc  a, $00
	ld   h, a
	
.shOut:
	ccf  			; See above
	rl   c			; Shift the digit back into the Quotient
	rl   b
	
	dec  d			; Done with all digits?
	jr   nz, .loop	; If not, loop
	ret
	
; =============== Sound_SndChInfoPtrTable ===============
; Maps the IDs passed to Sound_StartSlide to sound channel info pointers.
Sound_SndChInfoPtrTable_\1:
	dw wBGMCh1Info ; $00
	dw wBGMCh2Info ; $01
	dw wBGMCh3Info ; $02
	dw wBGMCh4Info ; $03
	dw wSFXCh1Info ; $04
	dw wSFXCh2Info ; $05
	dw wSFXCh3Info ; $06
	dw wSFXCh4Info ; $07
	
; =============== Sound_SetVolume ===============
; Updates the global volume.
; IN
; - C: Volume, in the NR50 format:
;      -LLL-RRR
;      L -> Left speaker volume
;      R -> Right speaker volume
Sound_SetVolume_\1:
IF \2
Sound_SetVolume_Main:
ENDC
	ld   a, c
	ldh  [rNR50], a
	ld   [wSndVolume], a
	ret

; =============== Sound_DoFade ===============
Sound_DoFade_\1:

	; Wait until the timer elapses before continuing.
	ld   hl, wSndFadeTimer
	dec  [hl]
	ret  nz
	
	; Reset the timer.
	; The initial fade timer is in the lower nybble of the options.
	dec  hl
	ldi  a, [hl]
	ld   e, a				; E = Fade options (wSndFadeStatus)
	and  $0F				; Filter out other bits
	ld   [hl], a			; Save wSndFadeTimer
	
	;--
	
	;
	; Fading in or out?
	;
	
	bit  SFDB_FADEIN, e		
	jr   z, .fadeOut		; If not, jump
	
.fadeIn:
	;
	; Increment the speakers' volume by 1, capped at 7.
	; The two nybble represent the volume of the L and R speakers.
	;
	
	; D = Base NR50 bitmask
	;     In practice, this will always be 0 since VIN isn't used.
	ld   hl, wSndVolume
	ld   a, [hl]
	and  %10001000		; Filter out volume bits
	ld   d, a
	
	; Update R Speaker volume to E (low nybble)
	ld   a, [hl]			; A = wSndVolume
	and  $07				; Filter out unrelated bits
	cp   $07				; Already at max volume?
	jr   z, .fadeInSetR		; If so, skip
	inc  a					; Otherwise, volume++
.fadeInSetR:
	ld   e, a
	
	; Update L Speaker volume to A (high nybble)
	ld   a, [hl]			; A = wSndVolume
	swap a					; >> 4 for ease
	and  $07				; Filter out unrelated bits
	cp   $07				; Already at max volume?
	jr   z, .fadeInSetL		; If so, skip
	inc  a					; Otherwise, volume++
.fadeInSetL:
	swap a					; << 4 back out
	
.fadeInSet:
	; Merge the two halves and write them back
	or   e					; Merge with R speaker
	ld   e, a				; Copy to E for the next check
	or   d					; Merge with base bitmask
	ld   [hl], a			; Save to wSndVolume
	ldh  [rNR50], a			; Save to registers
	
.fadeInChkEnd:
	; End the fade if both speakers are at max volume.
	ld   a, e
	cp   $77				; E == $77?
	ret  nz					; If not, return
	xor  a					; Otherwise, we're done
	ld   [wSndFadeStatus], a
	ret
	
.fadeOut:
	;
	; Decrement the speakers' volume by 1, capped at 0.
	;
	
	; D = Base NR50 bitmask
	ld   hl, wSndVolume
	ld   a, [hl]
	and  %10001000
	ld   d, a
	
	; End the fade (and *stop all sounds*) if the volume of both speakers is now 0.
	ld   a, [hl]			; A = wSndVolume
	and  $77				; Filter out unrelated bits
	jr   z, .fadeOutEnd		; A == 0? If so, jump
	
	; Update R Speaker volume to E (low nybble)
	and  $07				; A == 0?
	jr   z, .fadeOutSetR	; If so, skip
	dec  a					; Otherwise, volume--
.fadeOutSetR:
	ld   e, a
	
	; Update L Speaker volume to A (high nybble)
	ld   a, [hl]
	swap a					; >> 4 in
	and  $07				; A == 0?
	jr   z, .fadeOutSetL	; If so, skip
	dec  a					; Otherwise, volume--
.fadeOutSetL:
	swap a					; << 4 out
	
.fadeOutSet:
	; Merge the two halves and write them back
	or   e					; Merge with R speaker
	ld   e, a				; (Not necessary here)
	or   d					; Merge with base bitmask
	ld   [hl], a			; Save to wSndVolume
	ldh  [rNR50], a			; Save to registers
	ret
	
.fadeOutEnd:
	xor  a
	ld   [wSndFadeStatus], a; End the fade
	ldh  [rNR51], a			; (Already done by Sound_StopAll)
	call Sound_StopAll_\1	; Reset everything
	
	;--
	; [POI] Also already done by Sound_StopAll
	xor  a
	ld   [wBGMCh1Info+iSndChHeader_Status], a
	ld   [wBGMCh2Info+iSndChHeader_Status], a
	ld   [wBGMCh3Info+iSndChHeader_Status], a
	ld   [wBGMCh4Info+iSndChHeader_Status], a
	ld   [wSFXCh1Info+iSndChHeader_Status], a
	ld   [wSFXCh2Info+iSndChHeader_Status], a
	ld   [wSFXCh3Info+iSndChHeader_Status], a
	ld   [wSFXCh4Info+iSndChHeader_Status], a
	;--
	ret
	
; =============== Sound_Cmd_FadeIn ===============
; Starts a fade-in.
; IN
; - E: Fade speed (in lower nybble)
Sound_Cmd_FadeIn_\1:
	ld   a, e
	and  $0F
	or   SFD_FADEIN
	jr   Sound_Cmd_FadeOut_\1.setFade
	
; =============== Sound_Cmd_FadeOut ===============
; Starts a fade-out.
; IN
; - E: Fade speed (in lower nybble)
Sound_Cmd_FadeOut_\1:
	ld   a, e
	and  $0F
.setFade:
	ld   [wSndFadeStatus], a	; Enable fade
	and  $0F					; Set initial timer (lower nybble only)
	ld   [wSndFadeTimer], a
	ret
	
; =============== Sound_Cmd_ChkSetChVol ===============
; Checks which sound channel we're setting the volume of.
; IN
; - A: Command ID
; - E: Command ID + volume (raw value)
Sound_Cmd_ChkSetChVol_\1:
	
	; Which channel?
	ld   b, e		; (Save raw for Sound_Cmd_SetChVol)
	cp   $30		; $3x -> Pulse 1
	jr   z, .ch1
	cp   $40		; $4x -> Pulse 2
	jr   z, .ch2
	cp   $50		; $5x -> Wave
	jr   z, .ch3
.ch4:				; $6x -> Noise
	ld   de, wBGMCh4Info
	call Sound_Cmd_SetChVol_\1
	ld   de, wSFXCh4Info
	jr   Sound_Cmd_SetChVol_\1
.ch1:
	ld   de, wBGMCh1Info
	call Sound_Cmd_SetChVol_\1
	ld   de, wSFXCh1Info
	jr   Sound_Cmd_SetChVol_\1
.ch2:
	ld   de, wBGMCh2Info
	call Sound_Cmd_SetChVol_\1
	ld   de, wSFXCh2Info
	jr   Sound_Cmd_SetChVol_\1
.ch3:
	ld   de, wBGMCh3Info
	call Sound_Cmd_SetChVol_\1
	ld   de, wSFXCh3Info
	
	; Fall-through
	
; =============== Sound_Cmd_SetChVol ===============
; Sets the volume for a specific sound channel, for both BGM and SFX.
; IN
; - DE: Ptr to SndInfo
; - B: Volume (lower nybble only)
Sound_Cmd_SetChVol_\1:

	; Not applicable if the channel is disabled
	ld   a, [de]
	bit  SISB_ENABLED, a
	ret  z
	
	;
	; Convert the raw volume in the lower nybble in the sound channel's appropriate format.
	; For most channels this just involves << 4'ing the volume, but Wave has coarser volume control.
	;
	ld   hl, iSndInfo_RegPtr
	add  hl, de					; Seek to register ptr
	ld   a, [hl]				; Read it
	cp   SND_CH3_PTR			; Does it point to ch3?
	jr   nz, .ch124				; If not, jump
.ch3:
	ld   a, b					; A = B << 5
	and  $03					
	swap a						; (<< 4)
	ld   c, a
	add  c						; (<< 1) (could have been "add a")
	jr   .setVol
.ch124:
	ld   a, b					; A = B << 4
	and  $0F
	swap a
.setVol:

	;
	; Apply the volume to the sound channel
	;
	
	; Save as predicted volume
	ld   hl, iSndInfo_VolPredict
	add  hl, de
	ldi  [hl], a				; Seek to iSndInfo_RegNRx2Data
	
	
	; Merge volume into NRx2
	ld   b, a					; B = New volume
	ld   a, [hl]				; A = iSndInfo_RegNRx2Data
	and  $0F					; Filter out old volume
	or   b						; Merge with new one
	ld   [hl], a				; Save back
	
	; Setting a volume requires updating NRx2, so the lock has to go.
	ld   a, [de]
	res  SISB_LOCKNRx2, a
	ld   [de], a
	
	;
	; Try to set the volume to the registers.
	;

	; Not applicable if the BGM slot is in use by a SFX.
	bit  SISB_USEDBYSFX, a
	ret  nz
	
	;--
	; [POI] We already just cleared this! This can never return.
	bit  SISB_LOCKNRx2, a
	ret  nz
	;--
	
	; Update NRx2
	ld   hl, iSndInfo_RegPtr
	add  hl, de
	ld   c, [hl]				; C = Ptr to NRx3
	dec  c						; C--, to NRx2
	
	ld   hl, iSndInfo_RegNRx2Data
	add  hl, de
	ldi  a, [hl]				; A = iSndInfo_RegNRx2Data
	ld   [c], a					; Write to NRx2
	
	; Update NRx3
	inc  c						; C++, to NRx3
	ld   hl, iSndInfo_RegNRx3Data
	add  hl, de
	ldi  a, [hl]				; A = iSndInfo_RegNRx3Data
	ld   [c], a					; Write to NRx3
	
	; Update NRx4
	inc  c						; C++, to NRx4
	ld   a, [hl]				; A = iSndInfo_RegNRx4Data
	or   SNDCHF_RESTART			; Restart the tone to apply the changes
	ld   [c], a
	
	;--
	; If the channel length isn't being used (SNDCHFB_LENSTOP cleared),
	; clear the length timer in NRx1.
	bit  SNDCHFB_LENSTOP, a
	ret  nz
	dec  c						; C -= 3, to NRx1
	dec  c
	dec  c
	ld   a, [c]
	and  $C0					; Only keep wave duty
	ld   [c], a
	;--
	ret

; =============== Sound_ChkNewSnd ===============
; Checks if we're trying to start a new BGM or SFX.
Sound_ChkNewSnd_\1:

	; The first counter is updated every time a new music track is started,
	; while the second one is increased when the new music track is requested.
	; If these values don't match, we know that we should play a new music track.
	ld   hl, hSndPlayCnt
	ldi  a, [hl]		; Read request counter
	cp   a, [hl]		; Does it match the playback counter?
	ret  z				; If so, there's nothing new to play
	
	; Increase the sound playback index/counter, looping it back to $00 if it would index past the end of the table
	; hSndPlayCnt = (hSndPlayCnt + 1) & $07
	inc  a						; TblId++
	and  (SNDIDREQ_SIZE-1)		; Keep range
	dec  hl						; Seek back to hSndPlayCnt
	ld   [hl], a				; Write there
	
	; To determine the ID of the music track to play, use the counter as index to the table at wSndIdReqTbl.
	; The value written there is treated as the sound ID.
	; A = wSndIdReqTbl[hSndPlayCnt]
	ld   hl, wSndIdReqTbl
	ld   e, a
	ld   d, $00
	add  hl, de
	ld   a, [hl]
	
	;--
	; Sounds with ID < $70 are special commands that did not exist in 96's version.
	;
	; These commands affect multiple channels at once and can be requested by the game
	; in a saner way that doesn't involve having to create a pair of a dummy sound &
	; sound data command, which is how these would be done in previous versions
	; of the driver (ie: see how pausing/unpausing works).
	;
	; These use an unique format that splits the ID in two nybbles:
	; - Upper nybble -> Command ID
	; - Lower nybble -> Arguments
	;
	ld   e, a
		and  $F0					; Check the upper nybble
		
		cp   SNDCMD_FADEIN			; $1x -> Fade in
		jp   z, Sound_Cmd_FadeIn_\1
		cp   SNDCMD_FADEOUT			; $2x -> Fade out
		jp   z, Sound_Cmd_FadeOut_\1
		
		cp   SNDCMD_CH4VOL + $10	; $3x to $6x -> Set single channel volume
		jp   c, Sound_Cmd_ChkSetChVol_\1
	ld   a, e
	;--
	
	; In the master sound list, the valid sounds have IDs >= $00 && < $74.
	; The entries written into the sound id request table have the MSB set, so the actual range check
	; is ID >= $80 && ID < $F4. Everything outside the range is rejected and stops all currently playing BGM/SFX.
	;
	; Only after the range check, these values are subtracted by $80 (SND_BASE).
	
	; Range validation
	bit  7, a						; SndId < $80?
	jp   z, Sound_StopAll_\1		; If so, jump
	cp   SND_BASE+(Sound_SndListTable_Main.end-Sound_SndListTable_Main)/5	; SndId >= EOL?
	jp   nc, Sound_StopAll_\1		; If so, jump
	
	; Index the sound list, where each entry is 5 bytes long.
	; HL = Sound_SndHeaderTable[SndId - $80]
	sub  a, SND_BASE				; Remove SND_BASE from the id
	ret  z							; Is it $00? (SND_NONE) If so, return
	ld   e, a	; DE = A
	ld   l, a	; HL = A
	xor  a
	ld   d, a
	ld   h, a
	add  hl, hl ; HL *= 2 (2)
	add  hl, hl ;    *= 2 (4)
	add  hl, de ;     + 1 (5)
	ld   de, Sound_SndListTable_Main
	add  hl, de

	;--
	; Disable existing fades
	xor  a
	ld   [wSndFadeStatus], a
	
	; Set max volume for both left/right speakers, resetting wSndVolume too
	ld   a, [wSndVolume]
	and  %10001000
	or   %01110111
	ld   [wSndVolume], a
	ldh  [rNR50], a
	;--
	
	;--
	;
	; Read data off the sound list entry.
	;
	
	; 0: Bank number to hROMBank
	ldi  a, [hl]
	call Bankswitch
	; 1-2: Song header ptr to BC
	ldi  a, [hl]
	ld   c, a
	ldi  a, [hl]
	ld   b, a
	; 3-4: Init code to HL
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a
	; Jump there
	jp   hl
                                          
; =============== Sound_StartNewBGM ===============
; Starts playback of a new BGM.           
; IN                                      
; - BC: Ptr to song data                  
Sound_StartNewBGM_\1:                        
	xor  a                                
	ld   [wSnd_Unused_SfxPriority], a          
	push bc                               
		call Sound_StopAll_\1                
	pop  bc                               
	ld   de, wBGMCh1Info                  
	jp   Sound_InitSongFromHeader_\1         
                                          
; =============== Sound_PauseAll ===============
; Handles the sound pause command during gameplay.
Sound_PauseAll_\1:                           
	; Pause everything except SFXCh1 and SFXCh2
	call Sound_PauseChPlayback_\1
	; Kill SFXCh1 and SFXCh2 with a silent SFX (SndHeader_Pause) that overrides whatever's still playing.
	; This SFX doesn't play notes, all it does is set the sound length to 0, then end itself.
	; It also pretends to be a BGM, so that once it ends, the game will mute the channel instead of attempting to resume the BGM.
	jp   Sound_StartNewSFX1234_\1

; =============== Sound_UnpauseAll ===============
; Handles the sound unpause command during gameplay.
Sound_UnpauseAll_\1:
	call Sound_UnpauseChPlayback_\1
	; No purpose here.
	jp   Sound_StartNewSFX1234_\1
	
; =============== Sound_StartNewBGM_Unused_Copy ===============
; [TCRF] Duplicate of Sound_StartNewBGM, filling in as action for unused sound IDs.
; IN
; - BC: Ptr to song data
Sound_StartNewBGM_Unused_Copy_\1:
	xor  a
	ld   [wSnd_Unused_SfxPriority], a
	push bc
		call Sound_StopAll_\1
	pop  bc
	ld   de, wBGMCh1Info
	jp   Sound_InitSongFromHeader_\1
	
; =============== Sound_FastSlideSFXtoC_8 ===============
; Slides the two SFX pulse channels to note C-8 over 1 second.
Sound_FastSlideSFXtoC_8_\1:
	ld   d, SCI_SFXCH1	; SFX - Pulse 1
	ld   e, 60 			; 1 sec
	ld   bc, $07E1		; C-8
	call Sound_StartSlide_\1
	ld   d, SCI_SFXCH2	; SFX - Pulse 2
	ld   e, 60 			; 1 sec
	ld   bc, $07E1		; C-8
	call Sound_StartSlide_\1
	jp   Sound_StartNothing_\1
	
; =============== Sound_SlowSlideSFXtoF#4 ===============
; Slides the two SFX pulse channels to note F#4 over 4 seconds.	
Sound_SlowSlideSFXtoF#4_\1: 
	ld   d, SCI_SFXCH1	; SFX - Pulse 1
	ld   e, 4*60		; 4 secs
	ld   bc, $069E		; F#4
	call Sound_StartSlide_\1
	ld   d, SCI_SFXCH2	; SFX - Pulse 2
	ld   e, 4*60		; 4 secs
	ld   bc, $069E		; F#4
	call Sound_StartSlide_\1
	jp   Sound_StartNothing_\1

; =============== Sound_PauseChPlayback ===============
; Pauses sound playback.
Sound_PauseChPlayback_\1:
	; Disable all sound channels
	xor  a
	ldh  [rNR51], a
	; Pause music and sound effect channels (but not SFXCh1 and SFXCh2, for some reason)
	ld   hl, wBGMCh1Info
	set  SISB_PAUSE, [hl]
	ld   hl, wBGMCh2Info
	set  SISB_PAUSE, [hl]
	ld   hl, wBGMCh3Info
	set  SISB_PAUSE, [hl]
	ld   hl, wBGMCh4Info
	set  SISB_PAUSE, [hl]
	ld   hl, wSFXCh3Info
	set  SISB_PAUSE, [hl]
	ld   hl, wSFXCh4Info
	set  SISB_PAUSE, [hl]
	ret

; =============== Sound_UnpauseChPlayback ===============
Sound_UnpauseChPlayback_\1:
	; Restore enabled channels
	ld   a, [wSndEnaChBGM]
	ldh  [rNR51], a
	; Resume music and sound effect channels
	ld   hl, wBGMCh1Info
	res  SISB_PAUSE, [hl]
	ld   hl, wBGMCh2Info
	res  SISB_PAUSE, [hl]
	ld   hl, wBGMCh3Info
	res  SISB_PAUSE, [hl]
	ld   hl, wBGMCh4Info
	res  SISB_PAUSE, [hl]
	ld   hl, wSFXCh3Info
	res  SISB_PAUSE, [hl]
	ld   hl, wSFXCh4Info
	res  SISB_PAUSE, [hl]
	ret 
	
; =============== Sound_Unused_StartNewSFX1234Hi ===============
; [TCRF] Unreferenced code. The priority system isn't used.
;
; Starts playback for an high-priority multi-channel SFX (uses ch1-2-3-4)
; Priority is defined by the start action defined in Sound_SndStartActionPtrTable.
;
; Three priority levels are defined:
; - High
; - (Default)
; - Low
;
; The priority rules are different depending on how many channels the new sound effect uses:
; - Uses all channels: Low priority SFX can't play over high priority ones
; - Does not use all : Low and default priority SFX can't play over high priority ones
;
; Specifically, when an high priority sound plays, its type is flagged in wSnd_Unused_SfxPriority.
; Low (and default) priority sounds won't play when the bit they check for is set.
;
; The bit isn't cleared when a SFX ends, so such sound effects need to use a different version
; of chan_stop which also clears the bit (chan_stop SNP_SFXMULTI & chan_stop SNP_SFX4)
;
; Note that, since all-channel sound effects with default priority can play over high priority ones,
; they can also wipe out the priority flags.
Sound_Unused_StartNewSFX1234Hi_\1:
	ld   a, SNP_SFXMULTI				; Clears SNP_SFX4, if set
	ld   [wSnd_Unused_SfxPriority], a
	jr   Sound_StartNewSFX1234_\1

; =============== Sound_Unused_StartNewSFX1234Lo ===============
; [TCRF] Unreferenced code.
; Starts playback for a low priority multi-channel SFX (uses ch1-2-3-4)
Sound_Unused_StartNewSFX1234Lo_\1:
	ld   a, [wSnd_Unused_SfxPriority]
	bit  SNPB_SFXMULTI, a				; High priority multi-channel SFX playing?
	jp   nz, Sound_StartNothing_\1		; If so, don't replace it
	; Fall-through

; =============== Sound_StartNewSFX1234 ===============
; Starts playback for a multi-channel SFX (uses ch1-2-3-4)
Sound_StartNewSFX1234_\1:
	xor  a
	ldh  [rNR10], a
	ld   de, wSFXCh1Info
	jr   Sound_InitSongFromHeader_\1
	
; =============== Sound_Unused_StopSFXCh1 ===============
; [TCRF] Unreferenced code.
Sound_Unused_StopSFXCh1_\1:
	xor  a
	ldh  [rNR10], a
	ld   [wSFXCh1Info], a
	call Sound_StopAll_Main.initNR
	jr   Sound_StartNothing_\1

; =============== Sound_Unused_StartNewSFX234Hi ===============
; [TCRF] Unreferenced code.
; Starts playback for an high-priority multi-channel SFX (uses ch2-3-4)
Sound_Unused_StartNewSFX234Hi_\1:
	ld   a, [wSnd_Unused_SfxPriority]	; High priority on
	or   a, SNP_SFXMULTI
	ld   [wSnd_Unused_SfxPriority], a
	jr   Sound_StartNewSFX234_\1.initSong

; =============== Sound_StartNewSFX234 ===============
; Starts playback for a multi-channel SFX (uses ch2-3-4)
Sound_StartNewSFX234_\1:
	ld   a, [wSnd_Unused_SfxPriority]
	bit  SNPB_SFXMULTI, a				; High priority multi-channel SFX playing?
	jp   nz, Sound_StartNothing_\1		; If so, don't replace it
.initSong:
	ld   de, wSFXCh2Info
	jr   Sound_InitSongFromHeader_\1
	
; =============== Sound_Unused_InitSongFromHeaderToCh3 ===============
; [TCRF] Unreferenced code.
Sound_Unused_InitSongFromHeaderToCh3_\1:
	ld   de, wSFXCh3Info
	jr   Sound_InitSongFromHeader_\1

; =============== Sound_Unused_StartNewSFX4Hi ===============
; [TCRF] Unreferenced code.
; Starts playback for an high priority channel-4 only SFX (SFX4).
Sound_Unused_StartNewSFX4Hi_\1:
	ld   a, [wSnd_Unused_SfxPriority]	; High priority on
	or   a, SNP_SFX4
	ld   [wSnd_Unused_SfxPriority], a
	jr   Sound_StartNewSFX4_\1.initSong

; =============== Sound_StartNewSFX4 ===============
; Starts playback for a channel-4 only SFX (SFX4).
Sound_StartNewSFX4_\1:
	; [TCRF] Bit never set
	ld   a, [wSnd_Unused_SfxPriority]
	bit  SNPB_SFX4, a					; High priority SFX4 playing?
	jp   nz, Sound_StartNothing_\1		; If so, don't replace it
.initSong:
	ld   de, wSFXCh4Info
	jr   Sound_InitSongFromHeader_\1
	
; =============== Sound_InitSongFromHeader ===============
; Copies song data from its header to multiple SndInfo.
; IN
; - BC: Ptr to sound header data
; - DE: Ptr to the initial SndInfo (destination)
Sound_InitSongFromHeader_\1:

	; HL = BC
	ld   l, c
	ld   h, b

	; A sound can take up multiple channels -- and channel info is stored into a $20 byte struct.
	; The first byte from the sound header marks how many channels ($20 byte blocks) need to be initialized.

	; B = Channels used
	ldi  a, [hl]
	ld   b, a
.chLoop:
	;
	; Copy over next 4 bytes
	; These map directly to the first four bytes of the SndInfo structure.
	;
REPT 4
	ldi  a, [hl]	; Read byte
	ld   [de], a	; Copy it over
	inc  de
ENDR
	
	; The bank number we previously bankswitched to goes to iSndInfo_DataPtr_Bank
	ldh  a, [hROMBank]
	ld   [de], a
	inc  de

	; Then the remaining 2 original bytes
REPT 2
	ldi  a, [hl]
	ld   [de], a
	inc  de
ENDR

	;
	; Then initialize other fields
	;

	; Point data "stack index" to the very end of the SndInfo structure
	ld   a, iSndInfo_End
	ld   [de], a			; Write to iSndInfo_DataPtrStackIdx
	inc  de

	; Set the lowest possible length target to handle new sound commands as soon as possible.
	ld   a, $01
	ld   [de], a			; Write to iSndInfo_LengthTarget
	inc  de

	;
	; Clear rest of the SndInfo ($18 bytes)
	;
	xor  a
	ld   c, SNDINFO_SIZE-iSndInfo_LengthTimer	; C = Bytes left
.clrLoop:
	ld   [de], a		; Clear
	inc  de
	dec  c				; Cleared all bytes?
	jr   nz, .clrLoop	; If not, loop

	dec  b				; Finished all loops?
	jr   nz, .chLoop	; If not, jump

	; Fall-through
	
; =============== Sound_StartNothing ===============
; Does absolutely nothing.
Sound_StartNothing_\1:
	; [TCRF] Leftover from 95 to clear the requested sound ID, does nothing here.
	ld   a, SND_NONE
	ld   [wSnd_Unused_Set], a
	ret
	
; =============== Sound_DoChSndInfo ===============
; IN
; - HL: Ptr to start of the current sound channel info (iSndInfo_Status)
Sound_DoChSndInfo_\1:
	; If the sound channel is paused, return immediately
	bit  SISB_PAUSE, a
	ret  nz
		
	;
	; VIBRATO
	;
	; If the feature is enabled, the driver goes down the specified list of frequency offsets
	; and applies it directly to the sound registers, without updating the channel info.
	;
	; This feature is notable because half-deleted leftovers remain in the drivers for 95/96,
	; and 95 even calls the command for it, but none of this code exists in those games.
	;
	
	; Not applicable if the feature is off
	bit  SISB_VIBRATO, a
	jr   z, Sound_DoChSndInfo_ChkSlide_\1
	
	; Fall-through
; =============== Sound_DoChSndInfo_Vibrato ===============	
Sound_DoChSndInfo_Vibrato_\1:

	; As the feature directly modifies the registers.
	; Skip this if the BGM is muted by another SFX.
	bit  SISB_USEDBYSFX, a
	jr   nz, .skipped
	
	push hl
	
		;--
		;
		; Read the data for the current Vibrato Set.
		; BC = Ptr to Vibrato table.
		;      This is the list of frequency offsets.
		; A  = Loop point
		;      When the data loops, the index will reset to this value.
		;      Note that the initial index is always 0, even if the loop isn't.
		;
		push hl
		
			; Index the table of settings.
			; Each table entry is 3 bytes long, but the index already accounts for it.
			ld   de, iSndInfo_VibratoId			; BC = Table offset
			add  hl, de
			ld   c, [hl]
			ld   b, $00
			
			; Index this table with it
			ld   hl, Sound_VibratoSetTable_\1	; HL = Table base
			add  hl, bc
			
			; BC = Vibrato Table ptr [byte0-1]
			ldi  a, [hl]
			ld   c, a
			ldi  a, [hl]
			ld   b, a
			; A = Initial table index [byte2]
			ld   a, [hl]
		pop  hl
		
		;--
		;
		; Seek to the current vibrato data, handling loops if needed.
		;
		ld   de, iSndInfo_VibratoDataOffset
		add  hl, de
	.getData:
		ld   e, a			; Save for later, in case we're looping
		
		; A = VibratoTable[iSndInfo_VibratoDataOffset++]
		push bc				; Save Vibrato Table ptr
			ld   c, [hl]	; HL = iSndInfo_VibratoDataOffset
			inc  [hl]		; iSndInfo_VibratoDataOffset++
			ld   h, $00
			ld   l, c
		pop  bc				; Restore Vibrato Table ptr
		add  hl, bc			; Index it with iSndInfo_VibratoDataOffset
		ld   a, [hl]
		
		; If the value we got out is $80, we reached a loop command.
		cp   VIBCMD_LOOP	; A == $80?
		jr   nz, .calcFreq	; If not, jump
	.resetIdx:
	pop  hl
	push hl
		; Reset iSndInfo_VibratoDataOffset to the loop point.
		ld   a, e
		ld   de, iSndInfo_VibratoDataOffset
		add  hl, de
		ld   [hl], a
		
		; Then try again.
		; The loop point should never point to a loop command, otherwise we're stuck in an infinite loop.
		jr   .getData
		;--
		
	.calcFreq:
		;
		; Calculate the sound channel's frequency.
		; This involves adding a signed number to a capped word value, which is not ideal.
		; The caps are a bit half-hearted, since they only affect the high byte.
		;
		
		ld   c, a		; C = Frequency offset (signed)
	pop  hl				; Restore base channel ptr
	push hl
	
		; Low byte.
		; E = iSndInfo_RegNRx3Data + C
		ld   de, iSndInfo_RegNRx3Data
		add  hl, de
		ldi  a, [hl]		; A = NRx3, seek to NRx4
		add  c				; += C
		ld   e, a		
		
		; High byte.
		bit  7, c			; C >= 0?
		jr   z, .offPos		; If so, jump
	.offNeg:
		; Adding a negative number as positive almost certainly caused an underflow (carry set).
		; The carry and $FF balance themselves out, unless the former isn't set, ie:
		; $0180 + $FF -> $017F -> $017F (c)
		; $0100 + $FF -> $01FF -> $00FF (nc)
		ld   a, [hl]		; A = NRx4
		adc  a, -1			; + carry - 1
		; If the high byte underflowed, force it back to 0.
		bit  7, a			; NRx4's MSB set?
		jr   z, .writeReg	; If not, skip
		xor  a
		jr   .writeReg
	.offPos:
		ld   a, [hl]		; A = NRx4
		adc  a, $00			; + carry
		; Enforce the cap, as the max valid frequency is $07FF before it overflows into unrelated bits.
		cp   $07			; NRx4 >= $07?
		jr   c, .writeReg	; If not, skip
		ld   a, $07
		
	.writeReg:
	
		;
		; Apply the updated frequency to the registers.
		;
		ld   b, a			; B = Edited iSndInfo_RegNRx4Data
		ld   a, e			; A = Edited iSndInfo_RegNRx3Data
	pop  hl
	push hl
		; NRx3 = A
		ld   de, iSndInfo_RegPtr
		add  hl, de
		ld   c, [hl]
		ld   [c], a
		inc  c
		
		; NRx4 = B
		ld   a, b
		ld   [c], a
		
		;--
		; If the channel length isn't being used (SNDCHFB_LENSTOP cleared),
		; clear the length timer in NRx1.
		bit  SNDCHFB_LENSTOP, a
		jr   nz, .end
		dec  c						; C -= 3, to NRx1
		dec  c
		dec  c
		ld   a, [c]
		and  $C0					; Only keep wave duty
		ld   [c], a
		;--
	.end:
	
	; DE = Ptr to sound channel info
	pop  hl
.skipped:
	ld   e, l
	ld   d, h
	
	; Vibrato has priority over pitch slides, skip them.
	jr   Sound_DoChSndInfo_Main_\1
	
Sound_DoChSndInfo_ChkSlide_\1:

	;
	; PITCH SLIDE loop
	;
	; If enabled, then every frame the channel's frequency will be modulated by
	; iSndInfo_SlideFreqOffset* over a period of time.
	;
	; This updates both the frequency inside the sound channel info *and* the sound registers.
	; To avoid conflicts, these get disabled when a vibrato is active.
	;
	
	; Not applicable if the feature is off
	bit  SISB_SLIDE, a
	jr   z, .skipped
	
.slideOk:
	; DE = Ptr to sound channel info
	ld   e, l
	ld   d, h
	
	; Disable the feature when its timer elapses
	ld   hl, iSndInfo_SlideTimer
	add  hl, de
	dec  [hl]					; iSndInfo_SlideTimer != 0?
	jr   nz, .setFreq			; If so, jump
	ld   a, [de]				; Otherwise, clear the feature flag
	and  a, $FF^SIS_SLIDE
	ld   [de], a
	
.setFreq:

	;
	; CurFreq += iSndInfo_SlideFreqOffset*
	;
	
	; BC = Frequency offset
	ld   hl, iSndInfo_SlideFreqOffsetLow
	add  hl, de
	ldi  a, [hl]	; iSndInfo_SlideFreqOffsetLow
	ld   b, [hl]	; iSndInfo_SlideFreqOffsetHigh
	ld   c, a
	
	; HL = Current frequency
	ld   hl, iSndInfo_RegNRx3Data
	add  hl, de
	ldi  a, [hl]	; iSndInfo_RegNRx3Data
	ld   h, [hl]	; iSndInfo_RegNRx4Data
	ld   l, a
	
	add  hl, bc		; Add them together
	
	; Save them back
	ld   a, l		; A = FreqLow
	ld   b, h		; B = FreqHi
	ld   hl, iSndInfo_RegNRx3Data
	add  hl, de
	ldi  [hl], a	; iSndInfo_RegNRx3Data = FreqLow
	ld   [hl], b	; iSndInfo_RegNRx4Data = FreqHi
	
	; Reread the status
	ld   a, [de]	; A = iSndInfo_Status
	jr   .tryUpdateRegs
	
.skipped:
	; DE = Ptr to sound channel info
	ld   e, l
	ld   d, h
	
	; With neither Vibrato or Slide active, jump directly to Sound_DoChSndInfo_Main.
	jp   Sound_DoChSndInfo_Main_\1
	
.tryUpdateRegs:
	
	; Don't update the registers if the channel is in use
	bit  SISB_USEDBYSFX, a
	jr   nz, Sound_DoChSndInfo_Main_\1
	
	; Seek to the frequency register
	ld   hl, iSndInfo_RegPtr
	add  hl, de
	ld   c, [hl]		; C = NRx3
	ld   a, c			
	
	; Don't update the wave channel if a fixed length is set.
	cp   SND_CH4_PTR	; >= CH4?
	jr   nc, .setRegs	; If so, jump
	cp   SND_CH3_PTR	; < CH3?
	jr   c, .setRegs	; If so, jump
.ch3:
	ld   a, [wSndCh3DelayCut]
	or   a								; wSndCh3DelayCut != 0?
	jr   nz, Sound_DoChSndInfo_Main_\1	; If so, skip ahead
	
.setRegs:
	; NRx3 = iSndInfo_RegNRx3Data
	ld   hl, iSndInfo_RegNRx3Data
	add  hl, de
	ldi  a, [hl]
	ld   [c], a
	
	; NRx4 = iSndInfo_RegNRx4Data
	inc  c
	ld   a, [hl]
	ld   [c], a
	
	;--
	; If the channel length isn't being used (SNDCHFB_LENSTOP cleared),
	; clear the length timer in NRx1.
	bit  SNDCHFB_LENSTOP, a
	jr   nz, .end
	dec  c						; C -= 3, to NRx1
	dec  c
	dec  c
	ld   a, [c]
	and  $C0					; Only keep wave duty
	ld   [c], a
	;--	
.end:
	
Sound_DoChSndInfo_Main_\1:
	;------------
	;
	; HANDLE LENGTH.
	; Until the length timer reaches the target, return immediately to avoid updating the sound register settings.
	; When the values match, later on the timer is reset and a new length will be set.
	;

	; Seek to the timer
	ld   hl, iSndInfo_LengthTimer
	add  hl, de

	; Timer++
	ld   a, [hl]
	inc  a
	ldd  [hl], a	; iSndInfo_LengthTimer++, seek back to iSndInfo_LengthTarget

	; If it doesn't match the target, return
	cp   a, [hl]	; iSndInfo_LengthTarget == iSndInfo_LengthTimer?
	ret  nz			; If not, return
	;------------	
	
	; Each new command resets the vibrato
	ld   hl, iSndInfo_VibratoDataOffset
	add  hl, de
	ld   [hl], $00
	
	; Save a copy of the SndInfo ptr to RAM.
	; This used to be done in Sound_Do before, which was a waste of time.
	ld   a, e
	ldh  [hSndInfoCurPtr_Low], a
	ld   a, d
	ldh  [hSndInfoCurPtr_High], a
	
	; Copy the data ptr field from the SndInfo to HRAM
	; Seek to the channel data ptr
	ld   hl, iSndInfo_DataPtr_Low
	add  hl, de
	; Save the ptr here
	ldi  a, [hl]
	ldh  [hSndInfoCurDataPtr_Low], a
	ldi  a, [hl]
	ldh  [hSndInfoCurDataPtr_High], a
	; And switch ankswitch as well
	ld   a, [hl]
	call Bankswitch
	
; =============== Sound_DoChSndInfo_Loop ===============
Sound_DoChSndInfo_Loop_\1:

	;
	; HL = Song data ptr
	;
	ld   hl, hSndInfoCurDataPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a
	
	;
	; Read out a "command byte" from the data.
	; This can be either register data, a sound length, an index to frequency data or a command ID.
	;
	; If this is a command ID, most of the time, after the command is processed, a new "command byte" will
	; be immediately processed.
	;
	; If it's not a command ID or a sound length, the next data byte can optionally be a new sound length value
	; (which, again, will get applied immediately on the same frame).
	;
	ld   a, [hl]						; A = Data value

	;
	; Point to the next data byte.
	; Later on we may be checking the new data byte.
	; hSndInfoCurDataPtr++
	;
	ld   hl, hSndInfoCurDataPtr_Low
	inc  [hl]							; Increase low byte
	jr   nz, .chkIsCmd					; Overflowed to 0? If not, skip
	inc  l								; Seek to high byte
	inc  [hl]							; Increase high byte
.chkIsCmd:

	;
	; If the value is higher than $E0, treat the "command byte" as a command ID.
	;
	cp   SNDCMD_BASE					; A >= $E0?
	jp   nc, Sound_DoCommandId_\1		; If so, jump

.notCmd:
	;--
	;
	; Check which channel iSndInfo_RegPtr is pointing to.
	;
	; Sound channel 4 is handled in a different way compared to the others since it does not have frequency
	; options at NR43 and NR44, meaning it should never use Sound_FreqDataTbl.
	;
	; Checking if the 5th bit is set is enough (range >= $FF20 && < $FF40).
	;

	; Seek to the sound register ptr
	ld   hl, iSndInfo_RegPtr
	add  hl, de
	bit  5, [hl]			; ptr >= $FF20?
	jr   z, .isCh123		; If not, jump

.isCh4:
	; If we're here, we're pointing to channel 4.
;--
	; Write A to the data for iSndInfo_RegPtr (NR43)
	ld   hl, iSndInfo_RegNRx3Data
	add  hl, de
	ldi  [hl], a
	; Write $00 to the data for iSndInfo_RegPtr+1 (NR44)
	xor  a
	ld   [hl], a

	; Perform the register update
	jr   .updateRegs
;--
.isCh123:

	;
	; If the value is < $80, directly treat it as the new target length
	; and don't update the registers.
	;
	cp   SNDNOTE_BASE	; A < $80? (MSB clear?)
	jp   c, .setLength	; If so, skip ahead a lot

	;------

	;
	; Otherwise, clear the MSB and treat it as a secondary index to a table of NRx3/NRx4 register data.
	; If the index is != 0, add the contents of iSndInfo_FreqDataIdBase to the index.
	;

	sub  a, SNDNOTE_BASE	; Clear MSB
	jr   z, .readRegData	; If the index is $00, don't add anything else
	ld   hl, iSndInfo_FreqDataIdBase
	add  hl, de				; Seek to iSndInfo_FreqDataIdBase
	add  [hl]				; A += *iSndInfo_FreqDataIdBase

.readRegData:

	;
	; Index the table of register data.
	; HL = Sound_FreqDataTbl[A * 2]
	;

	; offset table with 2 byte entries
	ld   hl, Sound_FreqDataTbl_\1	; HL = Tbl
	ld   c, a					; BC = A
	ld   b, $00
	add  hl, bc					; HL += BC * 2
	add  hl, bc

	; Read the entries from the table in ROM
	ld   c, [hl]		; Read out iSndInfo_RegNRx3Data
	inc  hl
	ld   b, [hl]		; Read out iSndInfo_RegNRx4Data

	; and write them over to the Frequency SndInfo in RAM
	ld   hl, iSndInfo_RegNRx3Data
	add  hl, de
	ld   [hl], c		; Save iSndInfo_RegNRx3Data
	inc  hl
	ld   [hl], b		; Save iSndInfo_RegNRx4Data
	;------

.updateRegs:

	;--
	;
	; This part does the direct updates to the sound register sets, now that everything
	; has been set (through command IDs and/or note ids) in the SndInfo fields.
	;
	; Since the registers "are in HRAM" all of the pointers pass through C and the data is written through "ld [c], a" instructions.
	; Depending on the status flags, more or less data will be written in sequence.
	;

	ld   a, [de]			; Read iSndInfo_Status

	;
	; If a SFX is marked as currently playing for the channel, skip updating the sound registers.
	; This can only jump if the current handled SndInfo set is for a BGM.
	;
	bit  SISB_USEDBYSFX, a		; Is the bit set?
	jr   nz, .chkNewLength		; If so, skip ahead
;##
	;
	; Set the 'z' flag for a later check.
	; If set, we won't be updating rNRx2 (C-1).
	;

	bit  SISB_LOCKNRx2, a			; ### Is the bit set?...

	;
	; Read out the current 1-byte register pointer from iSndInfo_RegPtr to C.
	;

	ld   hl, iSndInfo_RegPtr		; Seek to iSndInfo_RegPtr
	add  hl, de
	ld   a, [hl]					; Read out the ptr
	ld   c, a						; Store it to C for $FF00+C access

	; Check if we're skipping NRx2
	jr   nz, .updateNRx3			; ### ...if so, skip

.updateNRx2:

	;
	; Update NRx2 with the contents of iSndInfo_RegNRx2Data
	;

	dec  c				; C--, to NRx2

	ld   hl, iSndInfo_RegNRx2Data	; Seek to iSndInfo_RegNRx2Data
	add  hl, de

	ld   a, [hl]		; Read out iSndInfo_RegNRx2Data
	ld   [c], a			; Write to sound register

	inc  c				; C++, to NRx3
	
.updateNRx3:

	;
	; Update NRx3 with the contents of iSndInfo_RegNRx3Data
	;

	ld   hl, iSndInfo_RegNRx3Data	; Seek to iSndInfo_RegNRx3Data
	add  hl, de
	ldi  a, [hl]	; Read iSndInfo_RegNRx3Data, seek to iSndInfo_RegNRx4Data
	ld   [c], a		; Write to sound register
	inc  c			; Next register

.updateNRx4:
	;
	; Update NRx4 with the contents of iSndInfo_RegNRx4Data
	;
	ld   a, [hl]	; Read out iSndInfo_RegNRx4Data
	ld   [c], a		; Write to sound register
;##

	;--
	; If the channel length isn't being used (SNDCHFB_LENSTOP cleared),
	; clear the length timer in NRx1.
	bit  SNDCHFB_LENSTOP, a
	jr   nz, .chkNewLength
	dec  c						; C -= 3, to NRx1
	dec  c
	dec  c
	ld   a, [c]
	and  $C0					; Only keep wave duty
	ld   [c], a
	;--	
	
.chkNewLength:

	;
	; HL = Song data ptr
	;
	ld   hl, hSndInfoCurDataPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a


	;
	; NOTE: At the start of the loop, we incremented the data byte.
	;
	; After passing through the custom register update, an additional byte
	; may be used to specify a new length target.
	;

	;
	; If the (new) data byte is < $80, treat it as a new length target.
	; Otherwise, ignore it completely.
	;

	ld   a, [hl]			; Read data byte
	cp   $80				; A >= $80?
	jr   nc, .saveDataPtr	; If so, skip

	;
	; ++
	;
	ld   hl, hSndInfoCurDataPtr_Low
	inc  [hl]				; LowByte++
	jr   nz, .setLength		; If no overflow, skip
	inc  l
	inc  [hl]				; HighByte++

	;------
.setLength:

	; Write the updated length value to iSndInfo_LengthTarget
	ld   hl, iSndInfo_LengthTarget
	add  hl, de
	ld   [hl], a
	;------

.saveDataPtr:

	;
	; Save back the updated data pointer from HRAM back to the SndInfo
	;
	ld   hl, iSndInfo_DataPtr_Low
	add  hl, de							; HL = Data ptr in SndInfo
	ldh  a, [hSndInfoCurDataPtr_Low]	; Save low byte to SndInfo
	ldi  [hl], a
	ldh  a, [hSndInfoCurDataPtr_High]	; Save high byte to SndInfo
	ld   [hl], a

	;
	; Reset the length timer
	;
	ld   hl, iSndInfo_LengthTimer
	add  hl, de
	xor  a
	ld   [hl], a
	
	; The rest of these actions wouldn't be executed when using the explicit Sound_Cmd_ExtendNote command.

	;
	; Reset the volume prediction timer
	;
	ld   hl, iSndInfo_VolPredict
	add  hl, de			; Seek to iSndInfo_VolPredict
	ld   a, [hl]		; Read the byte
	and  $F0			; Remove low nybble
	ld   [hl], a		; Write it back

	;
	; The remainder of the subroutine involves checks for muting the sound channel.
	;
	; If we're a BGM SndInfo and the channel is in use by a sound effect, 
	; return immediately to avoid interfering.
	;
	ld   a, [de]						; Read iSndInfo_Status
	bit  SISB_USEDBYSFX, a				; Is a sound effect playing on the channel?
	jp   nz, Sound_DoChSndInfo_End_\1	; If so, return (jumps to ret)

	;
	; If both frequency bytes are zero, mute the sound channel.
	;
.chkMuteMode:
	ld   hl, iSndInfo_RegNRx3Data
	add  hl, de					;
	ldi  a, [hl]				; iSndInfo_RegNRx3Data != 0
	or   [hl]					; || iSndInfo_RegNRx4Data != 0?
	jr   nz, .chkReinit			; If so, skip ahead

.chkMuteCh:

	; Depending on the currently modified channel, decide which channel to mute.
	;
	; This is done by checking at the register ptr, which doubles as channel marker.
	; It will only ever point to the 4th register (rNRx3) of any given sound channel, thankfully.

	ld   hl, iSndInfo_RegPtr
	add  hl, de

	; Depending on the source address...
	ld   a, [hl]
	cp   SND_CH1_PTR
	jr   z, .muteCh1
	cp   SND_CH2_PTR
	jr   z, .muteCh2
	cp   SND_CH3_PTR
	jr   z, .muteCh3
.muteCh4:
	ldh  a, [rNR51]
	and  %01110111
	jr   .muteEnd
.muteCh3:
	xor  a				; ch3 has also its own mute register
	ldh  [rNR30], a
	ldh  a, [rNR51]
	and  %10111011
	jr   .muteEnd
.muteCh2:
	ldh  a, [rNR51]
	and  %11011101
	jr   .muteEnd
.muteCh1:
	ldh  a, [rNR51]
	and  %11101110
.muteEnd:
	;##
	; Before writing to the register, mask the finalized NR51 value against the global mask (hSndChEnaMask).
	
	; C = Updated NR51 value
	ld   c, a
	
	; B = Global mask
	; Unlike other NR51 masks, this one operates in mono.
	; Only the lower nybble is used, and it gets duplicated into the upper one.
	ldh  a, [hSndChEnaMask]
	and  $0F
	ld   b, a					; H = hSndChEnaMask & $0F
	swap a						; A = H << 4
	or   b						
	ld   b, a			
	
	; Mask the new NR51 against C
	ld   a, c					; Restore NR51 value
	and  b						; Mask it against
	;##
	ldh  [rNR51], a
	
	; If we did this for a BGM, save a copy of this elsewhere.
	ld   hl, iSndInfo_Status
	add  hl, de
	bit  SISB_SFX, [hl]			; Are we a BGM?
	jr   nz, .muteNoBgm			; If not, we're done
	ld   [wSndEnaChBGM], a
.muteNoBgm:
	jp   Sound_DoChSndInfo_End_\1
	;---------------------------

.chkReinit:
	; If we skipped the NRx2 update (volume + env), return immediately
	ld   a, [de]
	bit  SISB_LOCKNRx2, a
	jp   nz, Sound_DoChSndInfo_End_\1

	;
	; Determine which sound channel has to be re-initialized (rNR51 status + extra registers).
	;
	; The default status when enabling a sound channel is stored at iSndInfo_ChEnaMask,
	; so seek to that for later.
	;
	ld   hl, iSndInfo_ChEnaMask
	add  hl, de

	; Seek BC to iSndInfo_RegPtr
	ld   bc, iSndInfo_RegPtr
	ldh  a, [hSndInfoCurPtr_Low]	; LowByte += 1
	add  c
	ld   c, a
	ldh  a, [hSndInfoCurPtr_High]	; HighByte += (Carry)
	adc  b
	ld   b, a

	; Read iSndInfo_RegPtr to A for the upcoming check
	ld   a, [bc]
	; C = A+1 for later
	; Increased by 1 since the high byte of the frequency is what contains extra flags
	ld   c, a
	inc  c

	; Depending on the source address enable the correct sound registers
	cp   SND_CH1_PTR	; A == SND_CH1_PTR? Handling channel 1?
	jr   z, .setCh1Ena	; If so, jump
	cp   SND_CH2_PTR	; Handling channel 2?
	jr   z, .setCh2Ena	; If so, jump
	cp   SND_CH4_PTR	; Handling channel 4?
	jr   z, .setCh4Ena	; If so, jump
.setCh3Ena:
	; Clear and re-enable channel 3
	xor  a ; SNDCH3_OFF
	ldh  [rNR30], a
	ld   a, SNDCH3_ON
	ldh  [rNR30], a

	; Get the bits to OR over rNR51 from the initial sound output status
	; A = iSndInfo_ChEnaMask & $44
	ld   a, [hl]		; Read iSndInfo_ChEnaMask
	and  %01000100	; Filter away bits for the other channel numbers
	jr   .setChEna
.setCh4Ena:
	ld   a, [hl]
	and  %10001000
	jr   .setChEna
.setCh2Ena:
	ld   a, [hl]
	and  %00100010
	jr   .setChEna
.setCh1Ena:
	ld   a, [hl]
	and  %00010001

.setChEna:
	; OR the bits from before with rNR51 to re-enable (if needed) the sound channel playback
	ld   b, a
	ldh  a, [rNR51]
	or   b
	;##
	; Before writing to the register, mask the finalized NR51 value against the global mask (hSndChEnaMask).
	
	; L = Updated NR51 value
	ld   l, a
	
	; H = Global mask
	; Unlike other NR51 masks, this one operates in mono.
	; Only the lower nybble is used, and it gets duplicated into the upper one.
	ldh  a, [hSndChEnaMask]
	and  $0F
	ld   h, a					; H = hSndChEnaMask & $0F
	swap a						; A = H << 4
	or   h						
	ld   h, a			
	
	; Mask the new NR51 against H
	ld   a, l					; Restore NR51 value
	and  h						; Mask it against
	;##
	
	ldh  [rNR51], a

	; If we did this for a BGM, save a copy of this elsewhere.
	ld   hl, iSndInfo_Status
	add  hl, de
	bit  SISB_SFX, [hl]			; Are we a BGM?
	jr   nz, .chkCh3EndType		; If not, we're done
	ld   [wSndEnaChBGM], a

.chkCh3EndType:
	;
	; Determine how we want to handle the end of channel playback when the length in ch3 expires.
	; If the checks all pass, wSndCh3DelayCut is used as channel length (after which, the channel mutes itself).
	;

	; Not applicable if we aren't editing ch3
	ld   hl, iSndInfo_RegPtr
	add  hl, de					; Seek to register ptr
	ld   a, [hl]				; Read it
	cp   SND_CH3_PTR			; Does it point to ch3?
	jr   nz, .noStop			; If not, jump

	; If we're processing a sound effect, jump
	ld   a, [de]				; Read iOBJInfo_Status
	bit  SISB_SFX, a			; Is the bit set?
	jr   nz, .noStop			; If so, jump

	; If the target length is marked as "none" ($FF), jump
	ld   a, [wSndCh3DelayCut]
	cp   SNDLEN_INFINITE
	jr   z, .noStop

.ch3StopOnEnd:
	; Set the new ch3 length
	ldh  [rNR31], a

	; Restart the channel playback
	ld   hl, iSndInfo_RegNRx4Data
	add  hl, de
	ld   a, [hl]
	set  SNDCHFB_RESTART, a			; Restart channel
	or   a, SNDCHF_LENSTOP			; Stop channel playback when length expires
	ld   [c], a

	jr   Sound_DoChSndInfo_End_\1
.noStop:
	; Restart the sound channel playback
	ld   hl, iSndInfo_RegNRx4Data
	add  hl, de
	ld   a, [hl]
	set  SNDCHFB_RESTART, a			; Restart channel
	ld   [c], a
	
; =============== Sound_DoChSndInfo_End ===============
; Just returns... in this game.
Sound_DoChSndInfo_End_\1:
	ret
	
; =============== Sound_DoCommandId ===============
; Handles the specified command ID, which mostly involves different ways of writing out data to the SndInfo.
; IN
; - A: Command ID (+ $E0)
; - DE: SndInfo base ptr
Sound_DoCommandId_\1:

	; After the function in the jump table executes, increment the data ptr
	; *AND* return to the normal custom data update loop.
	; Make the next 'ret' instruction jump to Sound_IncDataPtr
	ld   hl, Sound_IncDataPtr_\1
	push hl

	;
	; Index the command fetch ptr table.
	;

	; Get rid of the upper three bits of the command id (essentially subtracting $E0).
	; The resulting value is perfectly in range of the command table at Sound_CmdPtrTbl.
	and  $1F				; A -= SNDCMD_BASE

	ld   hl, Sound_CmdPtrTbl_\1; HL = Ptr table
	ld   c, a				; BC = A * 2
	ld   b, $00
	add  hl, bc
	add  hl, bc
	ldi  a, [hl]			; Read out the ptr to HL
	ld   h, [hl]
	ld   l, a
	jp   hl					; Jump there

; =============== Sound_IncDataPtr ===============
; Increases the word value at hSndInfoCurDataPtr, then returns to the loop.
Sound_IncDataPtr_\1:
	ld   hl, hSndInfoCurDataPtr_Low
	inc  [hl]							; hSndInfoCurDataPtr_Low++
	jp   nz, Sound_DoChSndInfo_Loop_\1	; If low byte == 0, jump
	inc  hl								; Seek to hSndInfoCurDataPtr_High
	inc  [hl]							; Increase high byte
	jp   Sound_DoChSndInfo_Loop_\1

Sound_CmdPtrTbl_\1:
	dw Sound_DecDataPtr_\1;X					; $00
	dw Sound_DecDataPtr_\1;X
	dw Sound_DecDataPtr_\1;X
	dw Sound_Cmd_ChanStop_\1
	dw Sound_Cmd_WriteToNRx2_\1
	dw Sound_Cmd_JpFromLoop_\1
	dw Sound_Cmd_AddToBaseFreqId_\1
	dw Sound_Cmd_JpFromLoopByTimer_\1
	dw Sound_Cmd_WriteToNR10_\1;X				; $08
	dw Sound_Cmd_SetPanning_\1
	dw Sound_DecDataPtr_\1;X
	dw Sound_DecDataPtr_\1;X
	dw Sound_Cmd_Call_\1
	dw Sound_Cmd_Ret_\1
	dw Sound_Cmd_WriteToNRx1_\1
	dw Sound_Cmd_LockNRx2_\1
	dw Sound_Cmd_UnlockNRx2_\1					; $10
	dw Sound_Cmd_SetVibrato_\1
	dw Sound_Cmd_ClrVibrato_\1;X
	dw Sound_Cmd_SetWaveData_\1
	dw Sound_Cmd_ChanStopHiSFXMulti_\1;X
	dw Sound_Cmd_WriteToNR31_\1
	dw Sound_Cmd_ChanStopHiSFX4_\1;X
	dw Sound_DecDataPtr_\1;X
	dw Sound_DecDataPtr_\1;X					; $18
	dw Sound_DecDataPtr_\1;X
	dw Sound_Cmd_ExtendNote_\1
	dw Sound_DecDataPtr_\1;X
	dw Sound_Cmd_Unused_StartSlide_\1;X
	dw Sound_DecDataPtr_\1;X
	dw Sound_DecDataPtr_\1;X
	dw Sound_DecDataPtr_\1;X					; $1F

; =============== Sound_Cmd_Unused_StartSlide ===============
; [TCRF] Not used.
; Starts a pitch slide with custom parameters.
; See also: Sound_StartSlide
;
; Command data format:
; - 1: Slide length, in frames
; - 2: Target Note ID
Sound_Cmd_Unused_StartSlide_\1:
	push de
		; Read a value off the data ptr.
		ld   hl, hSndInfoCurDataPtr_Low
		ldi  a, [hl]
		ld   h, [hl]
		ld   l, a
		
		; Prepare call to Sound_CmdS_StartSlide from the args
		ldi  a, [hl]	; A = Slide length
		push af			; Save length
			push af
				ld   c, [hl]	; C = Note ID
				ld   hl, Sound_FreqDataTbl_\1
				ld   b, $00
				add  hl, bc		; Get the respective frequency..
				add  hl, bc
				ldi  a, [hl]	; ...to BC
				ld   b, [hl]
				ld   c, a
			pop  af
			call Sound_CmdS_StartSlide_\1
			
		; When the pitch slide ends, the note should end too
		pop  af			; A = Slide length
	pop  de
	ld   hl, iSndInfo_LengthTarget
	add  hl, de
	ld   [hl], a
	
	;
	; Increment the data ptr past the args (3 bytes)
	;
REPT 3
	ld   hl, hSndInfoCurDataPtr_Low
	inc  [hl]							; LowByte++
	jr   nz, .noHi_\@					; Overflowed? If not, jump
	inc  l								; Seek to hSndInfoCurDataPtr_High
	inc  [hl]							; HighByte++
.noHi_\@:
ENDR

	;
	; Save back the updated value to the SndInfo
	;
	ld   hl, iSndInfo_DataPtr_Low
	add  hl, de							; Seek to iSndInfo_DataPtr_Low
	ldh  a, [hSndInfoCurDataPtr_Low]	; Write hSndInfoCurDataPtr there
	ldi  [hl], a
	ldh  a, [hSndInfoCurDataPtr_High]
	ld   [hl], a
	
	;
	; Reset the length timer.
	;
	ld   hl, iSndInfo_LengthTimer
	add  hl, de
	ld   [hl], $00
	
	; ret
	jp   Sound_DoChSndInfo_End_\1

; =============== Sound_DecDataPtr ===============
; Decrements the data ptr by 1.
; If called once, it balances out the Sound_IncDataPtr that's always called after Sound_DoCommandId is executed.
Sound_DecDataPtr_\1:
	; hSndInfoCurDataPtr--
	ld   hl, hSndInfoCurDataPtr_Low
	ld   a, [hl]			; Subtract low byte
	sub  a, $01
	ldi  [hl], a			; Save val
	ret  nc					; Underflowed? If not, return
	dec  [hl]				; Subtract high byte (we never get here)
	ret
	
; =============== Sound_Cmd_WriteToNR31 ===============
; Sets a new length value for channel 3 (wSndCh3DelayCut), and applies it immediately.
; Command data format:
; - 0: New length value
Sound_Cmd_WriteToNR31_\1:
	; Read a value off the data ptr.
	; wSndCh3DelayCut = ^(*hSndInfoCurDataPtr)
	ld   hl, hSndInfoCurDataPtr_Low		; Read out to HL
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a
	ld   a, [hl]						; Read value off current data ptr
	cpl									; Invert the bits
	ld   [wSndCh3DelayCut], a			; Write it

	; If the length isn't "none" ($FF), write the value to the register immediately.
	; This also means other attempts to write wSndCh3DelayCut need to be guarded by a $FF check.
	cp   SNDLEN_INFINITE
	ret  z
	ldh  [rNR31], a
	ret
	
; =============== Sound_Cmd_AddToBaseFreqId ===============
; Increases the base frequency index by the read amount.
; Command data format:
; - 0: Frequency id offset
Sound_Cmd_AddToBaseFreqId_\1:
	; Read a value off the data ptr.
	ld   hl, hSndInfoCurDataPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a
	ld   a, [hl]

	; Add the value to iSndInfo_FreqDataIdBase
	ld   hl, iSndInfo_FreqDataIdBase
	add  hl, de				; Seek to the value
	add  [hl]				; A += iSndInfo_FreqDataIdBase
	ld   [hl], a			; Save it back
	ret
	
; =============== Sound_Cmd_SetVibrato ===============
; Enables the vibrato, using the specified set ID.
; Once enabled, this will remain active indefinitely until it's either explicitly
; disabled a new song plays.
; Command data format:
; - Vibrato ID (0-9)
Sound_Cmd_SetVibrato_\1:
	; Enable the feature
	ld   a, [de]
	set  SISB_VIBRATO, a
	ld   [de], a

	; Read vibrato id off the data ptr
	ld   hl, hSndInfoCurDataPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a
	ld   a, [hl]

	; hSndInfoCurDataPtr++
	ld   hl, hSndInfoCurDataPtr_Low
	inc  [hl]
	jr   nz, .noIncHi
	inc  l
	inc  [hl]
.noIncHi:

	; Save it to iSndInfo_VibratoId
	ld   hl, iSndInfo_VibratoId
	add  hl, de
	ld   [hl], a
	
	; Rewind the data offset
	ld   hl, iSndInfo_VibratoDataOffset
	add  hl, de
	ld   [hl], $00
	
	; Don't increase data ptr
	jp   Sound_DecDataPtr_\1
	
; =============== Sound_Cmd_ClrVibrato ===============
; [TCRF] Unused subroutine.
;        Disables vibrato.
Sound_Cmd_ClrVibrato_\1:
	ld   a, [de]
	res  SISB_VIBRATO, a
	ld   [de], a

	; Don't increase data ptr
	jp   Sound_DecDataPtr_\1

; =============== Sound_Cmd_UnlockNRx2 ===============
; Clears disable flag for NRx2 writes.
Sound_Cmd_UnlockNRx2_\1:
	ld   a, [de]
	res  SISB_LOCKNRx2, a
	ld   [de], a

	jp   Sound_DecDataPtr_\1

; =============== Sound_Cmd_LockNRx2 ===============
; Sets disable flag for NRx2 writes.
Sound_Cmd_LockNRx2_\1:
	ld   a, [de]
	set  SISB_LOCKNRx2, a
	ld   [de], a
	jp   Sound_DecDataPtr_\1

; =============== Sound_Cmd_ExtendNote ===============
; Extends the current note without restarting it.
; Command data format:
; - 0: Length amount
Sound_Cmd_ExtendNote_\1:

	; Do not return to Sound_IncDataPtr
	pop  hl

	;
	; The current data byte is a length target.
	; Write it over.
	;
	ld   hl, hSndInfoCurDataPtr_Low		; HL = Data ptr
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a

	ld   a, [hl]						; Read length target
	ld   hl, iSndInfo_LengthTarget
	add  hl, de							; Seek to iSndInfo_LengthTarget
	ld   [hl], a						; Write the value there

	;
	; Increment the data ptr
	;
	ld   hl, hSndInfoCurDataPtr_Low
	inc  [hl]							; LowByte++
	jr   nz, .noHi						; Overflowed? If not, jump
	inc  l								; Seek to hSndInfoCurDataPtr_High
	inc  [hl]							; HighByte++
.noHi:

	;
	; Save back the updated value to the SndInfo
	;
	ld   hl, iSndInfo_DataPtr_Low
	add  hl, de							; Seek to iSndInfo_DataPtr_Low
	ldh  a, [hSndInfoCurDataPtr_Low]	; Write hSndInfoCurDataPtr there
	ldi  [hl], a
	ldh  a, [hSndInfoCurDataPtr_High]
	ld   [hl], a

	;
	; Reset the length timer.
	;
	ld   hl, iSndInfo_LengthTimer
	add  hl, de
	ld   [hl], $00

	; ret
	jp   Sound_DoChSndInfo_End_\1
	
; =============== Sound_Cmd_SetPanning ===============
; Sets the channel's stereo panning.
; Updates NR51, but the bits affected should only affect the current channel.
;
; Command data format:
; - 0: Channels to enable
Sound_Cmd_SetPanning_\1:

	; C = Enabled channels
	ldh  a, [rNR51]
	ld   c, a
	; HL = Data ptr
	ld   hl, hSndInfoCurDataPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a

	;--
	; Merge the enabled channels with the existing settings
	;
	; If the enabled channels are the same on both the left and right side,
	; which appears to ALWAYS be the case, the operation is essentially rNR51 |= (data byte).
	;
	; When they aren't, the operation makes no sense.
	ld   a, [hl]	; B = Enabled channels
	ld   b, a
	swap a			; Switch left/right sides
	cpl				; Mark disabled channels with 1
	and  c		; A = (A & C) | B
	or   b
	;--

	;
	; Set the updated NR51 value
	;
	
	; Save the slot-specific NR51 mask settings
	ld   hl, iSndInfo_ChEnaMask
	add  hl, de
	ld   [hl], a
	
	;##
	; Before writing to the register, mask the finalized NR51 value against the global mask (hSndChEnaMask).
	
	; L = Updated NR51 value
	ld   l, a
	
	; H = Global mask
	; Unlike other NR51 masks, this one operates in mono.
	; Only the lower nybble is used, and it gets duplicated into the upper one.
	ldh  a, [hSndChEnaMask]
	and  $0F
	ld   h, a					; H = hSndChEnaMask & $0F
	swap a						; A = H << 4
	or   h						
	ld   h, a			
	
	; Mask the new NR51 against H
	ld   a, l					; Restore NR51 value
	and  h						; Mask it against
	;##
	
	ldh  [rNR51], a

	; If we did this for a BGM, save a copy of this elsewhere.
	ld   hl, iSndInfo_Status
	add  hl, de
	bit  SISB_SFX, [hl]			; Are we a BGM?
	ret  nz						; If not, we're done
	ld   [wSndEnaChBGM], a
	ret

; =============== Sound_Cmd_WriteToNRx2 ===============
; Writes the current sound channel data to NRx2, and updates the additional SndInfo fields.
;
; Command data format:
; - 0: Sound register data
Sound_Cmd_WriteToNRx2_\1:

	;--
	; SOUND REG PTR
	;
	; First read the location we have to write to
	;

	; Seek to iSndInfo_RegPtr
	ld   hl, iSndInfo_RegPtr
	add  hl, de

	; Read the ptr to NRx2
	ld   a, [hl]		; A = NRx3
	dec  a
	ld   c, a

	;--
	; SOUND REG VALUE
	;
	; Then read the value we will be writing.
	; This will be written to multiple locations.
	;

	; Dereference value at hSndInfoCurDataPtr and...
	ld   hl, hSndInfoCurDataPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a
	ld   a, [hl]

	; -> write it to iSndInfo_RegNRx2Data
	;    (since we decremented C before, that's the correct place)
	ld   hl, iSndInfo_RegNRx2Data
	add  hl, de
	ld   [hl], a

	; -> write it to the aforemented sound register if possible
	call Sound_WriteToReg_\1

	; -> write it to iSndInfo_VolPredict, with the low nybble cleared
	;    (since we have set a new volume value, the prediction should restart)
	and  $F0									; Erase timer nybble
	ld   hl, iSndInfo_VolPredict
	add  hl, de
	ld   [hl], a
	ret

; =============== Sound_Cmd_WriteToNRx1 ===============
; Writes the current sound channel data to NRx1, and updates the additional SndInfo fields.
; Strictly used by the two Pulse channels, to set the duty cycle.
;
; Command data format:
; - 0: Sound register data
Sound_Cmd_WriteToNRx1_\1:

	; Seek to iSndInfo_RegPtr
	ld   hl, iSndInfo_RegPtr
	add  hl, de

	; Read the sound register ptr - 2 to C
	ld   a, [hl]
	sub  a, $02
	ld   c, a

	; Dereference value at hSndInfoCurDataPtr and...
	ld   hl, hSndInfoCurDataPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a
	ld   a, [hl]

	; -> write it to iSndInfo_RegNRx1Data
	ld   hl, iSndInfo_RegNRx1Data
	add  hl, de
	ld   [hl], a

	; -> write it to the aforemented sound register if possible
	jp   Sound_WriteToReg_\1
	
; =============== Sound_Cmd_WriteToNR10 ===============
; [TCRF] Unused command.
; Writes the Pulse 1 sweep data to rNR10 and updates the bookkeeping value.
;
; Command data format:
; - 0: Sound channel data for NR10
Sound_Cmd_WriteToNR10_\1:

	; Read sound channel data value to A
	ld   hl, hSndInfoCurDataPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a
	ld   a, [hl]

	; Update the bookkeeping value
	ld   hl, iSndInfo_RegNR10Data
	add  hl, de
	ld   [hl], a

	; Write to the sound register if possible
	ld   c, LOW(rNR10)
	jp   Sound_WriteToReg_\1
	
; =============== Sound_Cmd_JpFromLoopByTimer ===============
; Loops the sound channel a certain amount of times.
; Depending on the loop timer table index in data byte 0, bytes 1-2 may be set as the new hSndInfoCurDataPtr.
;
; Command data format:
; - 0: Loop timer ID
; - 1: Initial timer value (used when the existing timer is 0)
; - 2: Dest. Sound data ptr (low byte)
; - 3: Dest. Sound data ptr (high byte)
;
; This command is a superset of what's used by Sound_Cmd_JpFromLoop, so the game can seek to byte2
; and then jump directly to Sound_Cmd_JpFromLoop.
Sound_Cmd_JpFromLoopByTimer_\1:

	; The first byte is the index to the table at iSndInfo_LoopTimerTbl
	; After indexing the value, that gets decremented. If it's already 0 the next data byte is treated as new table value.

	; byte0 - Read the timer ID to C
	ld   hl, hSndInfoCurDataPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a
	ld   c, [hl]

	; Increment hSndInfoCurDataPtr for later
	inc  hl
	ld   a, l
	ldh  [hSndInfoCurDataPtr_Low], a
	ld   a, h
	ldh  [hSndInfoCurDataPtr_High], a

	;--
	; Seek to iSndInfo_LoopTimerTbl[C]

	; BC = iSndInfo_LoopTimerTbl + C
	ld   a, c
	add  iSndInfo_LoopTimerTbl
	ld   b, $00
	ld   c, a

	; HL = SndInfo + BC
	ld   hl, $0000
	add  hl, bc
	add  hl, de
	;--

	; Determine if an existing looping point was already set.
	ld   a, [hl]			; Read loop timer
	or   a					; Is it 0?
	jr   nz, .contLoop		; If not, jump
.firstLoop:
	; If the loop timer is 0, this is the first time we reached this looping point.
	; Set the initial loop timer and loop the song (set the data ptr to what's specified).

	; BC = Ptr to sound channel data (second value)
	ldh  a, [hSndInfoCurDataPtr_Low]
	ld   c, a
	ldh  a, [hSndInfoCurDataPtr_High]
	ld   b, a

	; Write the second data value to the table entry
	ld   a, [bc]
	ld   [hl], a

	; hSndInfoCurDataPtr++
	inc  bc
	ld   a, c
	ldh  [hSndInfoCurDataPtr_Low], a
	ld   a, b
	ldh  [hSndInfoCurDataPtr_High], a

	dec  [hl]							; Decrement loop timer
	jp   nz, Sound_Cmd_JpFromLoop_\1	; Is it 0 now? If not, jump
	;--
	; [TCRF] Seemingly unreachable failsafe code, in case the loop timer was 1.
	; hSndInfoCurDataPtr++
	inc  bc
	ld   a, c
	ldh  [hSndInfoCurDataPtr_Low], a
	ld   a, b
	ldh  [hSndInfoCurDataPtr_High], a
	ret
	;--
.contLoop:
	; If the loop timer isn't 0, it's been already initialized.

	; Skip initial timer value
	; hSndInfoCurDataPtr++
	push hl
		ld   hl, hSndInfoCurDataPtr_Low
		inc  [hl]
		jr   nz, .incDone0
		inc  l
		inc  [hl]
	.incDone0:
	pop  hl

	dec  [hl]							; Decrement loop timer
	jr   nz, Sound_Cmd_JpFromLoop_\1	; Is it 0 now? If not, jump

	; Otherwise, the looping is over. Seek past the end of the data for this command.
	; While there are two bytes to to seek past, we're only incrementing by 1 due to Sound_IncDataPtr being called automatically at the end.
	push hl
		ld   hl, hSndInfoCurDataPtr_Low
		inc  [hl]
		jr   nz, .incDone1
		inc  l
		inc  [hl]
	.incDone1:
	pop  hl
	ret
	
; =============== Sound_Cmd_JpFromLoop ===============
; If called directly as a sound command, this will always loop the sound channel without loop limit.
;
; The next two data bytes will be treated as new hSndInfoCurDataPtr.
; Command data format:
; - 0: Sound data ptr (low byte)
; - 1: Sound data ptr (high byte)
Sound_Cmd_JpFromLoop_\1:
	; HL = hSndInfoCurDataPtr_Low
	ld   hl, hSndInfoCurDataPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a

	; BC = *hSndInfoCurDataPtr_Low - 1
	; -1 to balance out the automatic call to Sound_IncDataPtr when the subroutine returns.
	ldi  a, [hl]
	ld   c, a
	ld   a, [hl]
	ld   b, a
	dec  bc

	; Write it back
	ld   a, c
	ldh  [hSndInfoCurDataPtr_Low], a
	ld   a, b
	ldh  [hSndInfoCurDataPtr_High], a
	ret
	
; =============== Sound_Cmd_Call ===============
; Saves the current data ptr, then sets a new one.
; This is handled like code calling a subroutine.
; Command data format:
; - 0: Sound data ptr (low byte)
; - 1: Sound data ptr (high byte)
Sound_Cmd_Call_\1:
	;
	; Read 2 bytes of sound data to BC, and increment the data ptr
	;

	; HL = hSndInfoCurDataPtr
	ld   hl, hSndInfoCurDataPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a

	; Read out the word value (jump target) to BC
	ldi  a, [hl]						; hSndInfoCurDataPtr++
	ld   c, a
	ld   a, [hl] 						; Not ldi because Sound_IncDataPtr, so it won't be needed to do it on Sound_Cmd_Ret
	ld   b, a

	; For now write back the original incremented hSndInfoCurDataPtr, which is what will be written to the "stack".
	ld   a, l
	ldh  [hSndInfoCurDataPtr_Low], a
	ld   a, h
	ldh  [hSndInfoCurDataPtr_High], a

	;
	; Save the current sound data pointer in a stack-like way.
	;
	push bc
		; Seek to the stack index value
		ld   hl, iSndInfo_DataPtrStackIdx
		add  hl, de

		; Get the stack index decremented by one.
		; This is where the second byte of the old code ptr will get written to.
		dec  [hl]
		ld   a, [hl]
		; The stack index itself has to be decremented twice, since we're writing a pointer (2 bytes).
		dec  [hl]

		; Index the stack location (at the aforemented second byte of the word entry)
		ld   l, a
		ld   h, $00
		add  hl, de

		; Write the second byte first
		ldh  a, [hSndInfoCurDataPtr_High]
		ldd  [hl], a						; HL--
		; Then the first byte
		ldh  a, [hSndInfoCurDataPtr_Low]
		ld   [hl], a
		
		; Pop out the data ptr for the "subroutine". 
		; This points to the proper place already, so decrement it once to balance out Sound_IncDataPtr.
	pop  bc
	dec  bc
	ld   a, c
	ldh  [hSndInfoCurDataPtr_Low], a
	ld   a, b
	ldh  [hSndInfoCurDataPtr_High], a
	ret
	
; =============== Sound_Cmd_Ret ===============
; Restores the data ptr previously saved in Sound_Cmd_Call.
; This acts like code returning from a subroutine.
Sound_Cmd_Ret_\1:
	; Read the stack index value to A
	ld   hl, iSndInfo_DataPtrStackIdx
	add  hl, de
	ld   a, [hl]

	; Use it to index the stack location with the data ptr
	ld   l, a
	ld   h, $00
	add  hl, de

	; Restore the data ptr
	; NOTE: What is stored at HL already accounts for Sound_IncDataPtr.
	ldi  a, [hl]
	ldh  [hSndInfoCurDataPtr_Low], a
	ld   a, [hl]
	ldh  [hSndInfoCurDataPtr_High], a

	; Increment the stack index twice
	ld   hl, iSndInfo_DataPtrStackIdx
	add  hl, de
	inc  [hl]
	inc  [hl]
	ret
	
; =============== Sound_Cmd_SetWaveData ===============
; Writes a complete set of wave data. This will disable ch3 playback.
;
; Command data format:
; - 0: Wave set id
Sound_Cmd_SetWaveData_\1:

	; Ignore if the sound channel is used by a SFX
	ld   a, [de]
	bit  SISB_USEDBYSFX, a
	ret  nz

	; Disable wave ch
	xor  a
	ldh  [rNR30], a

	; Read wave set id from data
	ld   hl, hSndInfoCurDataPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a
	ld   a, [hl]

	; Write it to the SndInfo
	ld   hl, iSndInfo_WaveSetId
	add  hl, de
	ld   [hl], a

	; Index the ptr table with wave sets
	ld   hl, Sound_WaveSetPtrTable_\1
	call Sound_IndexPtrTable_\1				; HL = Wave table entry ptr

	; Replace the current wave data
	ld   c, LOW(rWave)						; C = Ptr to start of wave ram
	ld   b, rWave_End-rWave					; B = Bytes to copy
.loop:
	ldi  a, [hl]							; Read from wave set
	ld   [c], a								; Write it to the wave reg
	inc  c									; Ptr++
	dec  b									; Copied all bytes?
	jr   nz, .loop							; If not, loop
	ret
	
; =============== Sound_SetWaveDataCustom ===============
; Writes a complete set of wave data. This will disable ch3 playback.
; [TCRF] Unused in this game.
;
; IN
; - HL: Ptr to a wave set id
Sound_SetWaveDataCustom_\1:
	; Disable wave ch
	ld   a, SNDCH3_OFF
	ldh  [rNR30], a

	; Index the ptr table with wave sets
	ld   a, [hl]
	ld   hl, Sound_WaveSetPtrTable_\1
	call Sound_IndexPtrTable_\1				; HL = Wave table entry ptr

	; Replace the current wave data
	ld   c, LOW(rWave)						; C = Ptr to start of wave ram
	ld   b, rWave_End-rWave					; B = Bytes to copy
.loop:
	ldi  a, [hl]							; Read from wave set
	ld   [c], a								; Write it to the wave reg
	inc  c									; Ptr++
	dec  b									; Copied all bytes?
	jr   nz, .loop							; If not, loop
	ret
	
; =============== Sound_Cmd_ChanStopHiSFX4 ===============
; Stops playback of an high-priority SFX4.
Sound_Cmd_ChanStopHiSFX4_\1:
	; Like Sound_Cmd_ChanStop, except it also unmarks the priority flag.
	; This allows SFX4 with less priority to play again.
	ld   a, [wSnd_Unused_SfxPriority]
	and  $FF^SNP_SFX4
	ld   [wSnd_Unused_SfxPriority], a
	jr   Sound_Cmd_ChanStop_\1
	
; =============== Sound_Cmd_ChanStopHiSFXMulti ===============
; [TCRF] Unused command.
; See above, but for multi-channel SFX.
Sound_Cmd_ChanStopHiSFXMulti_\1:
	ld   a, [wSnd_Unused_SfxPriority]
	and  $FF^SNP_SFXMULTI
	ld   [wSnd_Unused_SfxPriority], a
	; Fall-through
	
; =============== Sound_Cmd_ChanStop ===============
; Called to permanently stop channel playback (ie: the song/sfx ended and didn't loop).
; This either stops the sound channel or resumes playback of the BGM.
Sound_Cmd_ChanStop_\1:

	; Mute the sound channel if there isn't a SFX playing on here, for good measure.
	; This isn't really needed.
	call Sound_SilenceCh_\1

	; HL = SndInfo base
	ld   hl, hSndInfoCurPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a

	;
	; Check if a BGM is currently playing.
	; Checking if wBGMCh1Info is enabled should be enough, given it's the main channel and every song defines/enables it.
	; If nothing is playing (BGM just ended, or SFX ended with no music), only mute the channel and disable this SndInfo.
	;
	ld   a, [wBGMCh1Info]
	bit  SISB_ENABLED, a		; Is playback enabled for ch1?
	jr   z, .stopCh				; If not, skip to the end

.bgmPlaying:

	ld   a, [hl]
	bit  SISB_SFX, a		; Is this a SFX?
	jr   nz, .isSFX			; If so, jump

.isBGM:
	; If this is a BGM SndInfo, completely disable this SndInfo (but don't mute the channel)
	xor  a				; Erase the status flags
	ld   [hl], a

	; Prevent Sound_IncDataPtr from being executed
	pop  hl
	ret

.isSFX:
	; If this is a SFX SndInfo, reapply the BGM SndInfo to the sound registers.

	; Disable this SndInfo
	xor  a
	ld   [hl], a

	; HL -> Seek to the NRx1 info of the BGM SndInfo of the current channel.
	; The SFX SndInfo are right after the ones for the BGM, which is why we move back.
	ld   bc, -(SNDINFO_SIZE * 4) + iSndInfo_RegNRx1Data
	add  hl, bc
	push hl
		; C -> Seek to NRx2
		ld   hl, iSndInfo_RegPtr
		add  hl, de
		ld   a, [hl]
	pop  hl
	ld   c, a		; NRx3
	dec  c			; -1 to NRx2


.ch1ExtraClr:
	;
	; If we're processing ch1, clear NR10.
	; We must clear it manually since that register can't be reached otherwise.
	;
	cp   SND_CH1_PTR		; Processing ch1?
	jr   nz, .ch3SkipChk	; If not, skip
	ld   a, $08				; Otherwise, clear ch1 reg
	ldh  [rNR10], a
.ch3SkipChk:

	;
	; If we're processing ch3, skip updating rNR31 and *don't* handle iSndInfo_VolPredict.
	; This is because ch3 doesn't go through the volume prediction system in Sound_UpdateVolPredict,
	; so it'd be useless.
	;

	cp   SND_CH3_PTR		; Processing ch3?
	jr   nz, .cpAll			; If not, jump

.ch3:
	; [POI] We never get here in this game.
	; A = iSndInfo_RegNRx2Data
	inc  hl			; Seek to iSndInfo_RegNR10Data
	inc  hl			; Seek to iSndInfo_VolPredict
	inc  hl			; Seek to iSndInfo_RegNRx2Data
	ldi  a, [hl] 	; Read it

	jr   .cpNRx2

.cpAll:
	; Now copy over all of the BGM SndInfo to the registers.

	;
	; NRx1
	;
	dec  c				; Seek back to NRx1, since HL is pointing to iSndInfo_RegNRx1Data

	ldi  a, [hl]		; Read iSndInfo_RegNRx1Data, seek to SndInfo_Unknown_Unused_NR10Data
	ld   [c], a			; Update NRx1

	;
	; NRx2
	;

	;
	; Merge the volume settings from iSndInfo_VolPredict with the existing
	; low byte of iSndInfo_RegNRx2Data.
	;

	inc  hl				; Seek to BGM iSndInfo_VolPredict
	inc  c				; seek to NRx2
	; B = BGM Volume info
	ldi  a, [hl]
	and  $F0				; Only in the upper nybble
	ld   b, a
	; A = BGM iSndInfo_RegNRx2Data
	ldi  a, [hl]
	and  $0F				; Get rid of its volume info
	add  b					; Merge it with the one from iSndInfo_VolPredict
.cpNRx2:
	ld   [c], a				; Write it to NRx2


	;
	; NRx3
	;
	inc  c
	ldi  a, [hl]			; Seek to NRx4 too
	ld   [c], a

	;
	; NRx4
	;
	inc  c
	push hl
		; A = RegPtr of SFX SndInfo
		ld   hl, iSndInfo_RegPtr
		add  hl, de
		ld   a, [hl]
	pop  hl

	; Ch3 is stopped in a different way
	cp   SND_CH3_PTR	; Processing ch3?
	jr   z, .stopCh3	; If so, jump

	; Write BGM iSndInfo_RegNRx4Data to NRx4, and restart the tone
	ldi  a, [hl]
	or   a, SNDCHF_RESTART
	ld   [c], a

	; Restore the "enabled channels" register from the BGM-only copy
	ld   a, [wSndEnaChBGM]
	ldh  [rNR51], a

.stopCh:

	;
	; Mutes and stops the sound channel.
	;

	;--
	; Write $00 to the sound register NRx2 to silence it.
	; This is also done in Sound_SilenceCh, but it's repeated here just in case Sound_SilenceCh doesn't mute playback?)
	ld   hl, iSndInfo_RegPtr
	add  hl, de

	ld   a, [hl]			; C = iSndInfo_RegPtr-1
	dec  a
	ld   c, a
	xor  a					; A = $00
	ld   [c], a				; Write it to C
	;--

	;
	; Write $00 to the status byte of the current SndInfo.
	; This outright stops its playback.
	;

	; HL = SndInfo base
	ld   hl, hSndInfoCurPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a

	xor  a
	ld   [hl], a

	; Prevent Sound_IncDataPtr from being executed, since we disabled the channel playback
	pop  hl
	ret


.stopCh3:
	; [POI] We never get here in this game.
	
	;--
	;
	; Make ch3 stop when its length expires.
	; [POI] Weirdly organized code. The first two lines are pointless,
	;       as is the useless $FF check.
	;

	ld   a, [wSndCh3DelayCut]
	or   a
	;--
	ldi  a, [hl]			; Read from iSndInfo_RegNRx4Data, seek to iSndInfo_ChEnaMask
	cp   $FF
	jr   z, .isFF
	or   a, SNDCHF_LENSTOP	; Set kill flag
.isFF:
	ld   [c], a				; Write to rNR34

	;
	; Restore the BGM wave set
	;
	inc  hl					; Seek to iSndInfo_WaveSetId
	call Sound_SetWaveDataCustom_\1

	; Prevent Sound_IncDataPtr from being executed
	pop  hl
	ret
	

; =============== Sound_UpdateVolPredict ===============
; Updates the volume prediction value.
; This is used to guess the volume to set when restoring BGM after a SFX ends.
;
; Every frame, the timer in the low nybble of iSndInfo_VolPredict ticks up from $00 until
; it matches the low nybble of iSndInfo_RegNRx2Data (essentially the amount of envelope sweeps + dir flag).
;
; Once the timer/sweep count matches, the predicted volume level is decreased with a decreasing envelope sweep,
; or increased with an increasing one.
;
; IN
; - HL: Ptr to iSndInfo_VolPredict field
; - DE: Ptr to iSndInfo_RegNRx2Data field
Sound_UpdateVolPredict_\1:
	inc  [hl]			; iSndInfo_VolPredict++

	; If the timers don't match yet, return
	ld   a, [hl]		; C = iSndInfo_VolPredict & $0F
	and  $0F
	ld   c, a
	ld   a, [de]		; A = iSndInfo_RegNRx2Data & $0F
	and  $0F
	cp   a, c			; A != C?
	ret  nz				; If so, return

	; Either increase or decrease the volume depending on the envelope direction
	bit  SNDENVB_INC, a	; Is bit 3 set?
	jr   z, .dec		; If not, decrease the volume
.inc:
	; Reset the timer and increase the volume by 1
	ld   a, [hl]		; A = (iSndInfo_VolPredict & $F0) + $10
	and  $F0
	add  $10
	ret  c				; If we overflowed, return
	ld   [hl], a		; Save it to iSndInfo_VolPredict
	ret
.dec:
	; Reset the timer and decrease the volume by 1
	ld   a, [hl]		; A = (iSndInfo_VolPredict & $F0)
	and  $F0
	ret  z				; If it's already 0, return
	sub  a, $10			; A -= $10
	ld   [hl], a		; Save it to iSndInfo_VolPredict
	ret
	

; =============== Sound_SilenceCh ===============
; Writes $00 to the sound register NRx2, which silences the volume the sound channel (but doesn't disable it).
; This checks if the sound channel is being used by a sound effect, and if so, doesn't perform the write.
; IN
; - DE: SndInfo base ptr. Should be a wBGMCh*Info structure.
Sound_SilenceCh_\1:
	; Seek to iSndInfo_RegPtr and read out its value
	; C = Destination register (RegPtr - 1)
	ld   hl, iSndInfo_RegPtr
	add  hl, de
	ld   a, [hl]
	dec  a
	ld   c, a

	; A = Value to write
	xor  a

	; Seek to SndInfo status
	ld   hl, iSndInfo_Status
	add  hl, de

	bit  SISB_USEDBYSFX, [hl]	; Is the sound channel being used by a sound effect?
	ret  nz						; If so, return
	ld   [c], a					; Otherwise, write the $00 value to the register
	ret

; =============== Sound_WriteToReg ===============
; Writes a value to the specified sound register.
; This checks if the sound channel is being used by a sound effect, and if so, doesn't perform the write.
; IN
; - A: Data to write to the sound register
; - C: Ptr to sound register
; - DE: SndInfo base ptr. Should be a wBGMCh*Info structure.
Sound_WriteToReg_\1:
	; Seek to SndInfo status
	ld   hl, iSndInfo_Status
	add  hl, de

	bit  SISB_USEDBYSFX, [hl]	; Is the sound channel being used a sound effect?
	ret  nz						; If so, return
	ld   [c], a					; Otherwise, write the value to the register
	ret
; =============== Sound_MarkSFXChUse ===============
; Registers to the BGM Channel Info if a sound effect is currently using that channel.
; This sets/unsets the flag to mute BGM playback for the channel, in order to not cut off the sound effect.
; IN
; - DE: Ptr to SFX Channel Info (iSndInfo)
; - HL: Ptr to BGM Channel Info (iSndInfo)
Sound_MarkSFXChUse_\1:
	ld   a, [de]		; Read iSndInfo_Status
	or   a				; Is a sound effect playing here?
	jr   z, .clrSFXPlay	; If not, jump
.setSFXPlay:
	set  SISB_USEDBYSFX, [hl]		; Mark as used
	ret
.clrSFXPlay:
	bit  SISB_USEDBYSFX, [hl]		; Is it cleared already?
	ret  z							; If so, return
	res  SISB_USEDBYSFX, [hl]		; Mark as free
	ret
	
; =============== Sound_IndexPtrTable ===============
; Indexes a pointer table.
;
; IN
; - HL: Ptr to ptr table
; -  A: Index (starting at $01)
; OUT
; - HL: Indexed value
Sound_IndexPtrTable_\1:
	; Offset the table
	; BC = A - 1
	dec  a
	ld   c, a
	ld   b, $00
	; HL += BC * 2
	add  hl, bc
	add  hl, bc
	; Read out ptr to HL
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a
	ret
	
; =============== Sound_StopAll ===============
; Reloads the sound driver, which stops any currently playing song.
; Also used to initialize the sound driver.
Sound_StopAll_\1:
IF \2
Sound_StopAll_Main:
ENDC
	; Enable all sound channels
	ld   a, $FF
	ldh  [hSndChEnaMask], a
	
	; Enable sound hardware
	ld   a, SNDCTRL_ON
	ldh  [rNR52], a
	
	; Silence all channels
	xor  a
	ldh  [rNR51], a
	
	;
	; Clear channel registers.
	; This zeroes out addresses in the list at Sound_ChRegAddrTable.
	;
	ld   hl, Sound_ChRegAddrTable_\1		; HL = Start of table
	ld   b, (Sound_ChRegAddrTable_\1.end-Sound_ChRegAddrTable_\1)	; B = Bytes to overwrite (table size)
	xor  a								; A = Value copied
.loop:
	ld   c, [hl]		; Read the ptr to C
	ld   [c], a			; Write $00 to $FF00+C
	inc  hl				; Ptr++
	dec  b				; Copied all bytes?
	jr   nz, .loop		; If not, loop
	
	;
	; Clear the entire memory range used by the sound driver (wBGMCh1Info-$D5C5)
	;
	ld   hl, wBGMCh1Info; HL = Initial addr
	xor  a				; A = $00
	ld   c, $08			; BC = ($09*$20)
.loopH:
	ld   b, $20
.loopL:
	ldi  [hl], a		; Clear byte
	dec  b				; B == 0?
	jr   nz, .loopL		; If not, loop
	dec  c				; C == 0?
	jr   nz, .loopH		; If not, loop
	
	;
	; Initialize other regs
	;
	ld   a, %1110111	; Set max volume for both left/right speakers
	ld   [wSndVolume], a
	ldh  [rNR50], a
	
	xor  a
	ld   [wSnd_Unused_SfxPriority], a
.initNR:
	ld   a, $08			; Use downwards sweep for ch1 (standard)
	ldh  [rNR10], a
	xor  a
	ldh  [rNR30], a		; Stop Ch3
	ldh  [rNR51], a		; Silence all channels
	
	; These weren't initialized in the older version of the driver.
	; It matters more here due to conditional updates.
	ld   [wSndEnaChBGM], a
	ld   [wSndCh3DelayCut], a
	
	; Clear circular buffer of sound requests.
	; This used to be done during the previous loop in 96, but memory rearrangements
	; necessitated this change.
	ld   hl, wSndIdReqTbl
REPT 8
	ldi  [hl], a
ENDR
	ld   hl, hSndPlayCnt
	ldi  [hl], a
	ldi  [hl], a
	
	; [TCRF] Leftover from 95's sound driver.
	ld   a, SND_NONE
	ld   [wSnd_Unused_Set], a
	ldh  [rNR52], a			; SNDCTRL_ON a second time, for good measure
	ret

; =============== Sound_Unused_InitCh1Regs ===============
; [TCRF] Unreferenced code.
Sound_Unused_InitCh1Regs_\1:
	ld   a, $FF
	ldh  [rNR51], a
	ld   a, $80
	ldh  [rNR11], a
	ld   a, $F7
	ldh  [rNR12], a
	ld   a, $D6
	ldh  [rNR13], a
	ld   a, $86
	ldh  [rNR14], a
	ret

; =============== Sound_Unused_InitCh2Regs ===============
; [TCRF] Unreferenced code.
Sound_Unused_InitCh2Regs_\1:
	ld   a, $F7
	ldh  [rNR22], a
	ld   a, $14
	ldh  [rNR23], a
	ld   a, $87
	ldh  [rNR24], a
	ret
	
; =============== Sound_ChRegAddrTable ===============
; List of memory adddresses cleared by Sound_Init.
Sound_ChRegAddrTable_\1:
	db LOW(rNR10)
	db LOW(rNR14)
	db LOW(rNR12)
	db LOW(rNR13)
	db LOW(rNR11)
	db LOW(rNR24)
	db LOW(rNR22)
	db LOW(rNR23)
	db LOW(rNR21)
	db LOW(rNR30)
	db LOW(rNR34)
	db LOW(rNR32)
	db LOW(rNR33)
	db LOW(rNR31)
	db LOW(rNR44)
	db LOW(rNR42)
	db LOW(rNR43)
	db LOW(rNR41)
.end: