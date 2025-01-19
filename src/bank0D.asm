; =============== INTERFACE ===============
SoundInt_Init:
	jp   Sound_StopAll
SoundInt_Do:
	jp   Sound_Do
SoundInt_ReqPlayId:
	jp   Sound_ReqPlayId
SoundInt_SetVolume: 
	jp   Sound_SetVolume
SoundInt_StartSlide: 
	jp   Sound_StartSlide
	
; =============== Sound_Do ===============
; Entry point of the main sound code.
; This is an improved version of the sound driver used in KOF96.
Sound_Do:
	; Check if there's anything new that we want to play
	call Sound_ChkNewSnd

	; Update the "SFX playing, mute BGM" flag for each channel
	ld   de, wSFXCh1Info
	ld   hl, wBGMCh1Info
	call Sound_MarkSFXChUse
	ld   de, wSFXCh2Info
	ld   hl, wBGMCh2Info
	call Sound_MarkSFXChUse
	ld   de, wSFXCh3Info
	ld   hl, wBGMCh3Info
	call Sound_MarkSFXChUse
	ld   de, wSFXCh4Info
	ld   hl, wBGMCh4Info
	call Sound_MarkSFXChUse

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
		call nz, Sound_DoChSndInfo	; If so, call
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
	call nz, Sound_DoFade
	
	; Update the volume level timer
	ld   hl, wBGMCh1Info + iSndInfo_VolPredict
	ld   de, wBGMCh1Info + iSndInfo_RegNRx2Data
	call Sound_UpdateVolPredict
	ld   hl, wBGMCh2Info + iSndInfo_VolPredict
	ld   de, wBGMCh2Info + iSndInfo_RegNRx2Data
	call Sound_UpdateVolPredict
	ld   hl, wBGMCh4Info + iSndInfo_VolPredict
	ld   de, wBGMCh4Info + iSndInfo_RegNRx2Data
	jp   Sound_UpdateVolPredict
	
; =============== Sound_ReqPlayId ===============
; Requests playback for a new sound ID.
; IN
; - A: Sound ID to play
Sound_ReqPlayId:
	ld   c, a				; C = Sound ID
	
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
Sound_CmdS_StartSlide:
	push bc
		ld   c, a	; Matches C = E from below
		jr   Sound_StartSlide.fromCmd

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
; - A: SndChInfo ID
;      Index to Sound_SndChInfoPtrTable.
; - E: Slide length, in frames
; - BC: Target frequency
Sound_StartSlide:

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
			ld   c, a
			ld   b, $00
			ld   hl, Sound_SndChInfoPtrTable
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
			call Sound_Div
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
Sound_Div:
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
Sound_SndChInfoPtrTable:
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
; - A: Volume, in the NR50 format:
;      -LLL-RRR
;      L -> Left speaker volume
;      R -> Right speaker volume
Sound_SetVolume:
	ldh  [rNR50], a
	ld   [wSndVolume], a
	ret

; =============== Sound_DoFade ===============
Sound_DoFade:

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
	call Sound_StopAll		; Reset everything
	
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
Sound_Cmd_FadeIn:
	ld   a, e
	and  $0F
	or   SFD_FADEIN
	jr   Sound_Cmd_FadeOut.setFade
	
; =============== Sound_Cmd_FadeOut ===============
; Starts a fade-out.
; IN
; - E: Fade speed (in lower nybble)
Sound_Cmd_FadeOut:
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
Sound_Cmd_ChkSetChVol:
	
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
	call Sound_Cmd_SetChVol
	ld   de, wSFXCh4Info
	jr   Sound_Cmd_SetChVol
.ch1:
	ld   de, wBGMCh1Info
	call Sound_Cmd_SetChVol
	ld   de, wSFXCh1Info
	jr   Sound_Cmd_SetChVol
.ch2:
	ld   de, wBGMCh2Info
	call Sound_Cmd_SetChVol
	ld   de, wSFXCh2Info
	jr   Sound_Cmd_SetChVol
.ch3:
	ld   de, wBGMCh3Info
	call Sound_Cmd_SetChVol
	ld   de, wSFXCh3Info
	
	; Fall-through
	
; =============== Sound_Cmd_SetChVol ===============
; Sets the volume for a specific sound channel, for both BGM and SFX.
; IN
; - DE: Ptr to SndInfo
; - B: Volume (lower nybble only)
Sound_Cmd_SetChVol:

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
	res  SISB_SKIPNRx2, a
	ld   [de], a
	
	;
	; Try to set the volume to the registers.
	;

	; Not applicable if the BGM slot is in use by a SFX.
	bit  SISB_USEDBYSFX, a
	ret  nz
	
	;--
	; [POI] We already just cleared this! This can never return.
	bit  SISB_SKIPNRx2, a
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
Sound_ChkNewSnd:

	; The first counter is updated every time a new music track is started,
	; while the second one is increased when the new music track is requested.
	; If these values don't match, we know that we should play a new music track.
	ld   hl, hSndPlayCnt
	ldi  a, [hl]		; Read request counter
	cp   a, [hl]		; Does it match the playback counter?
	jr   z, .noReq		; If so, there's nothing new to play (could have been ret z)
	
	; Increase the sound playback index/counter, looping it back to $00 if it would index past the end of the table
	; hSndPlayCnt = (hSndPlayCnt + 1) & $07
	inc  a						; TblId++
	and  (SNDIDREQ_SIZE-1)	; Keep range
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
	
.chkId:
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
		jp   z, Sound_Cmd_FadeIn
		cp   SNDCMD_FADEOUT			; $2x -> Fade out
		jp   z, Sound_Cmd_FadeOut
		
		cp   SNDCMD_CH4VOL + $10	; $3x to $6x -> Set single channel volume
		jp   c, Sound_Cmd_ChkSetChVol
	ld   a, e
	;--
	
	; In the master sound list, the valid sounds have IDs >= $00 && < $74.
	; The entries written into the sound id request table have the MSB set, so the actual range check
	; is ID >= $80 && ID < $F4. Everything outside the range is rejected and stops all currently playing BGM/SFX.
	;
	; Only after the range check, these values are subtracted by $80 (SND_BASE).
	
	; Range validation
	bit  7, a						; SndId < $80?
	jp   z, Sound_StopAll			; If so, jump
	cp   SND_LAST_VALID+1			; SndId >= $C6?
	jp   nc, Sound_StopAll			; If so, jump
	; Calculate the index to the next tables
	; DE = SndId - $80
	sub  a, SND_BASE				; Remove SND_BASE from the id
	ret  z							; Is it $00? (SND_NONE) If so, return
	ld   e, a
	ld   d, $00

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
	; 0: Perform the bankswitch to Sound_SndBankPtrTable[DE]
	;	
	ld   hl, Sound_SndBankPtrTable
	add  hl, de
	ld   a, [hl]
	call Int_Bankswitch

	;--
	;
	; 1: BC = Sound_SndHeaderPtrTable[DE*2]
	;

	; HL = Ptr table to song header
	ld   hl, Sound_SndHeaderPtrTable
	; Add index twice (each entry is 2 bytes)
	add  hl, de
	add  hl, de
	; Read out the ptr to BC
	ldi  a, [hl]
	ld   b, [hl]
	ld   c, a

	;--
	;
	; Get the code ptr for the init code for the sound.
	; 2: HL = Sound_SndStartActionPtrTable[DE*2]
	;

	; HL = Ptr table to code
	ld   hl, Sound_SndStartActionPtrTable
	; Add index twice (each entry is 2 bytes)
	add  hl, de
	add  hl, de
	; Read out the ptr to HL
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a

	; Jump there
	jp   hl
.noReq:
	; This forces .chkId to return early without doing anything.
	ld   a, SND_NONE
	jr   .chkId
	
Sound_SndStartActionPtrTable:
	dw Sound_StartNothing               ;
	dw Sound_StartNewBGM                ; 130
	dw Sound_StartNewBGM                ; 131
	dw Sound_StartNewBGM                ; 132
	dw Sound_StartNewBGM                ; 133
	dw Sound_StartNewBGM                ; 134
	dw Sound_StartNewBGM                ; 135
	dw Sound_StartNewBGM                ; 136
	dw Sound_StartNewBGM                ; 137
	dw Sound_StartNewBGM                ; 138
	dw Sound_StartNewBGM                ; 139
	dw Sound_StartNewBGM                ; 140
	dw Sound_PauseAll                   ; 141
	dw Sound_UnpauseAll                 ; 142
	dw Sound_StartNewSFX234             ; 143
	dw Sound_StartNewSFX234             ; 144
	dw Sound_StartNewSFX234             ; 145
	dw Sound_StartNewSFX234             ; 146
	dw Sound_StartNewSFX234             ; 147
	dw Sound_StartNewSFX234             ; 148
	dw Sound_StartNewSFX234             ; 149
	dw Sound_StartNewSFX4               ; 150
	dw Sound_StartNewSFX4               ; 151
	dw Sound_StartNewSFX1234            ; 152
	dw Sound_StartNewSFX4               ; 153
	dw Sound_StartNewSFX234             ; 154
	dw Sound_StartNewSFX234             ; 155
	dw Sound_StartNewSFX234             ; 156
	dw Sound_StartNewSFX1234            ; 157
	dw Sound_StartNewSFX1234            ; 158
	dw Sound_StartNewSFX1234            ; 159
	dw Sound_StartNewSFX4               ; 160
	dw Sound_StartNewSFX234             ; 161
	dw Sound_StartNewSFX4               ; 162
	dw Sound_StartNewSFX4               ; 163
	dw Sound_StartNewSFX1234            ; 164
	dw Sound_StartNewSFX4               ; 165
	dw Sound_StartNewSFX4               ; 166
	dw Sound_StartNewSFX234             ; 167
	dw Sound_StartNewSFX1234            ; 168
	dw Sound_StartNewSFX234             ; 169
	dw Sound_StartNewSFX4               ; 170
	dw Sound_StartNewSFX4               ; 171
	dw Sound_StartNewSFX234             ; 172
	dw Sound_StartNewSFX234             ; 173
	dw Sound_StartNewSFX234             ; 174
	dw Sound_StartNewSFX4               ; 175
	dw Sound_StartNewSFX4               ; 176
	dw Sound_StartNewSFX4               ; 177
	dw Sound_StartNewSFX1234            ; 178
	dw Sound_StartNewSFX4               ; 179
	dw Sound_StartNewSFX4               ; 180
	dw Sound_StartNewSFX1234            ; 181
	dw Sound_StartNewSFX4               ; 182
	dw Sound_StartNewSFX234             ; 183
	dw Sound_StartNewSFX4               ; 184
	dw Sound_StartNewSFX4               ; 185
	dw Sound_StartNewSFX1234            ; 186
	dw Sound_StartNewSFX1234            ; 187
	dw Sound_StartNewSFX234             ; 188
	dw Sound_StartNewSFX4               ; 189
	dw Sound_StartNewSFX234             ; 190
	dw Sound_StartNewBGM                ; 191
	dw Sound_StartNewBGM                ; 192
	dw Sound_StartNewBGM                ; 193
	dw Sound_StartNewSFX234             ; 194
	dw Sound_StartNewSFX4               ; 195
	dw Sound_StartNewSFX4               ; 196
	dw Sound_StartNewSFX234             ; 197
	dw Sound_StartNewSFX4               ; 198
	dw Sound_StartNewSFX4               ; 199
	dw Sound_StartNewSFX4               ; 200
	dw Sound_StartNewSFX4               ; 201
	dw Sound_StartNewBGM                ; 202
	dw Sound_StartNewBGM                ; 203
	dw Sound_StartNewBGM                ; 204
	dw Sound_StartNewBGM                ; 205
	dw Sound_StartNewBGM                ; 206
	dw Sound_StartNewBGM                ; 207
	dw Sound_StartNewBGM                ; 208
	dw Sound_StartNewSFX1234            ; 209
	dw Sound_StartNewBGM                ; 210
	dw Sound_StartNewSFX4               ; 211
	dw Sound_StartNewSFX4               ; 212
	dw Sound_StartNewSFX4               ; 213
	dw Sound_StartNewSFX4               ; 214
	dw Sound_StartNewBGM                ; 215
	dw Sound_StartNewBGM                ; 216
	dw Sound_StartNewBGM                ; 217
	dw Sound_StartNewSFX4               ; 218
	dw Sound_StartNewBGM                ; 219
	dw Sound_StartNewBGM                ; 220
	dw Sound_StartNewSFX1234            ; 221
	dw Sound_StartNewBGM_Unused_Copy    ; 222
	dw Sound_StartNewBGM_Unused_Copy    ; 223
	dw Sound_StartNewBGM_Unused_Copy    ; 224
	dw Sound_StartNewBGM_Unused_Copy    ; 225
	dw Sound_StartNewBGM_Unused_Copy    ; 226
	dw Sound_StartNewBGM_Unused_Copy    ; 227
	dw Sound_StartNewBGM_Unused_Copy    ; 228
	dw Sound_StartNewBGM_Unused_Copy    ; 229
	dw Sound_StartNewBGM_Unused_Copy    ; 230
	dw Sound_StartNewBGM_Unused_Copy    ; 231
	dw Sound_StartNewBGM_Unused_Copy    ; 232
	dw Sound_StartNewBGM_Unused_Copy    ; 233
	dw Sound_StartNewBGM_Unused_Copy    ; 234
	dw Sound_StartNewBGM_Unused_Copy    ; 235
	dw Sound_StartNewBGM_Unused_Copy    ; 236
	dw Sound_StartNewBGM_Unused_Copy    ; 237
	dw Sound_StartNewBGM_Unused_Copy    ; 238
	dw Sound_StartNewBGM_Unused_Copy    ; 239
	dw Sound_StartNewBGM_Unused_Copy    ; 240
	dw Sound_StartNewBGM_Unused_Copy    ; 241
	dw Sound_StartNewBGM_Unused_Copy    ; 242
	dw Sound_StartNewBGM_Unused_Copy    ; 243
	dw Sound_FastSlideSFXtoC8           ; 244
	dw Sound_SlowSlideSFXtoFx4          ; 245
                                          
; =============== Sound_StartNewBGM ===============
; Starts playback of a new BGM.           
; IN                                      
; - BC: Ptr to song data                  
Sound_StartNewBGM:                        
	xor  a                                
	ld   [wSnd_Unused_ChUsed], a          
	push bc                               
		call Sound_StopAll                
	pop  bc                               
	ld   de, wBGMCh1Info                  
	jp   Sound_InitSongFromHeader         
                                          
; =============== Sound_PauseAll ===============
; Handles the sound pause command during gameplay.
Sound_PauseAll:                           
	; Pause everything except SFXCh1 and SFXCh2
	call Sound_PauseChPlayback
	; Kill SFXCh1 and SFXCh2 with a silent SFX (SndHeader_Pause) that overrides whatever's still playing.
	; This SFX doesn't play notes, all it does is set the sound length to 0, then end itself.
	; It also pretends to be a BGM, so that once it ends, the game will mute the channel instead of attempting to resume the BGM.
	jp   Sound_StartNewSFX1234

; =============== Sound_UnpauseAll ===============
; Handles the sound unpause command during gameplay.
Sound_UnpauseAll:
	call Sound_UnpauseChPlayback
	; No purpose here.
	jp   Sound_StartNewSFX1234
	
; =============== Sound_StartNewBGM_Unused_Copy ===============
; [TCRF] Duplicate of Sound_StartNewBGM, filling in as action for unused sound IDs.
; IN
; - BC: Ptr to song data
Sound_StartNewBGM_Unused_Copy:
	xor  a
	ld   [wSnd_Unused_ChUsed], a
	push bc
		call Sound_StopAll
	pop  bc
	ld   de, wBGMCh1Info
	jp   Sound_InitSongFromHeader
	
; =============== Sound_FastSlideSFXtoC8 ===============
; Slides the two SFX pulse channels to note C-8 over 1 second.
Sound_FastSlideSFXtoC8:
	ld   a, SCI_SFXCH1	; SFX - Pulse 1
	ld   e, 60 			; 1 sec
	ld   bc, $07E1		; C-8
	call Sound_StartSlide
	ld   a, SCI_SFXCH2	; SFX - Pulse 2
	ld   e, 60 			; 1 sec
	ld   bc, $07E1		; C-8
	call Sound_StartSlide
	jp   Sound_StartNothing
	
; =============== Sound_SlowSlideSFXtoFx4 ===============
; Slides the two SFX pulse channels to note F#4 over 4 seconds.	
Sound_SlowSlideSFXtoFx4: 
	ld   a, SCI_SFXCH1	; SFX - Pulse 1
	ld   e, 4*60		; 4 secs
	ld   bc, $069E		; F#4
	call Sound_StartSlide
	ld   a, SCI_SFXCH2	; SFX - Pulse 2
	ld   e, 4*60		; 4 secs
	ld   bc, $069E		; F#4
	call Sound_StartSlide
	jp   Sound_StartNothing

; =============== Sound_PauseChPlayback ===============
; Pauses sound playback.
Sound_PauseChPlayback:
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
Sound_UnpauseChPlayback:
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
	
; =============== Sound_Unused_StartNewSFX1234WithStat ===============
; [TCRF] Unreferenced code.
Sound_Unused_StartNewSFX1234WithStat:
	ld   a, $80
	ld   [wSnd_Unused_ChUsed], a
	jr   Sound_StartNewSFX1234

; =============== Sound_Unused_StartNewSFX1234IfChNotUsed ===============
; [TCRF] Unreferenced code.
Sound_Unused_StartNewSFX1234IfChNotUsed:
	ld   a, [wSnd_Unused_ChUsed]
	bit  7, a
	jp   nz, Sound_StartNothing

; =============== Sound_StartNewSFX1234 ===============
; Starts playback for a multi-channel SFX (uses ch1-2-3-4)
Sound_StartNewSFX1234:
	xor  a
	ldh  [rNR10], a
	ld   de, wSFXCh1Info
	jr   Sound_InitSongFromHeader
	
; =============== Sound_Unused_StopSFXCh1 ===============
; [TCRF] Unreferenced code.
Sound_Unused_StopSFXCh1:
	xor  a
	ldh  [rNR10], a
	ld   [wSFXCh1Info], a
	call Sound_StopAll.initNR
	jr   Sound_StartNothing

; =============== Sound_Unused_StartNewSFX234WithStat ===============
; [TCRF] Unreferenced code.
Sound_Unused_StartNewSFX234WithStat:
	ld   a, [wSnd_Unused_ChUsed]
	or   a, $80
	ld   [wSnd_Unused_ChUsed], a
	jr   Sound_StartNewSFX234.initSong

; =============== Sound_StartNewSFX234 ===============
; Starts playback for a multi-channel SFX (uses ch2-3-4)
Sound_StartNewSFX234:
	; [TCRF] Bit never set
	ld   a, [wSnd_Unused_ChUsed]
	bit  7, a							; Is the channel used?
	jp   nz, Sound_StartNothing			; If so, jump (don't start SFX)
.initSong:
	ld   de, wSFXCh2Info
	jr   Sound_InitSongFromHeader
	
; =============== Sound_Unused_InitSongFromHeaderToCh3 ===============
; [TCRF] Unreferenced code.
Sound_Unused_InitSongFromHeaderToCh3:
	ld   de, wSFXCh3Info
	jr   Sound_InitSongFromHeader

; =============== Sound_Unused_StartNewSFX4WithStat ===============
; [TCRF] Unreferenced code.
Sound_Unused_StartNewSFX4WithStat:
	ld   a, [wSnd_Unused_ChUsed]
	or   a, $40
	ld   [wSnd_Unused_ChUsed], a
	jr   Sound_StartNewSFX4.initSong

; =============== Sound_StartNewSFX4 ===============
; Starts playback for a channel-4 only SFX (SFX4).
Sound_StartNewSFX4:
	; [TCRF] Bit never set
	ld   a, [wSnd_Unused_ChUsed]
	bit  6, a							; Is the channel used?
	jp   nz, Sound_StartNothing			; If so, jump (don't start SFX)
.initSong:
	ld   de, wSFXCh4Info
	jr   Sound_InitSongFromHeader
	
; =============== Sound_InitSongFromHeader ===============
; Copies song data from its header to multiple SndInfo.
; IN
; - BC: Ptr to sound header data
; - DE: Ptr to the initial SndInfo (destination)
Sound_InitSongFromHeader:

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
Sound_StartNothing:
	; [TCRF] Leftover from 95 to clear the requested sound ID, does nothing here.
	ld   a, SND_NONE
	ld   [wSnd_Unused_Set], a
	ret
	
; =============== Sound_DoChSndInfo ===============
; IN
; - HL: Ptr to start of the current sound channel info (iSndInfo_Status)
Sound_DoChSndInfo:
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
	jr   z, Sound_DoChSndInfo_ChkSlide
	
	; Fall-through
; =============== Sound_DoChSndInfo_Vibrato ===============	
Sound_DoChSndInfo_Vibrato:

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
			ld   de, iSndInfo_VibratoId		; BC = Table offset
			add  hl, de
			ld   c, [hl]
			ld   b, $00
			
			; Index this table with it
			ld   hl, Sound_VibratoSetTable	; HL = Table base
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
	jr   Sound_DoChSndInfo_Main
	
Sound_DoChSndInfo_ChkSlide:

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
	
	; [POI] With neither Vibrato or Slide active, it could have jumped directly to Sound_DoChSndInfo_Main.
	;       The code below is only useful to apply the frequency changes from the pitch slide to the registers,
	;       it's worthless doing it all the time (while in 96 none of this code existed).
	; jp   Sound_DoChSndInfo_Main
	
.tryUpdateRegs:
	
	; Don't update the registers if the channel is in use
	bit  SISB_USEDBYSFX, a
	jr   nz, Sound_DoChSndInfo_Main
	
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
	ld   a, [wSndCh3StopLength]
	or   a							; wSndCh3StopLength != 0?
	jr   nz, Sound_DoChSndInfo_Main	; If so, skip ahead
	
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
	
Sound_DoChSndInfo_Main:
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
	call Int_Bankswitch
	
; =============== Sound_DoChSndInfo_Loop ===============
Sound_DoChSndInfo_Loop:

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
	jp   nc, Sound_DoCommandId			; If so, jump

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
	ld   hl, Sound_FreqDataTbl	; HL = Tbl
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

	bit  SISB_SKIPNRx2, a			; ### Is the bit set?...

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
	
	; The rest of these actions wouldn't be executed when using the explicit Sound_Cmd_SetLength command.

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
	ld   a, [de]					; Read iSndInfo_Status
	bit  SISB_USEDBYSFX, a			; Is a sound effect playing on the channel?
	jp   nz, Sound_DoChSndInfo_End	; If so, return (jumps to ret)

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
	jp   Sound_DoChSndInfo_End
	;---------------------------

.chkReinit:
	; If we skipped the NRx2 update (volume + ...), return immediately
	ld   a, [de]
	bit  SISB_SKIPNRx2, a
	jp   nz, Sound_DoChSndInfo_End

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
	; If the checks all pass, wSndCh3StopLength is used as channel length (after which, the channel mutes itself).
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
	ld   a, [wSndCh3StopLength]
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

	jr   Sound_DoChSndInfo_End
.noStop:
	; Restart the sound channel playback
	ld   hl, iSndInfo_RegNRx4Data
	add  hl, de
	ld   a, [hl]
	set  SNDCHFB_RESTART, a			; Restart channel
	ld   [c], a
	
; =============== Sound_DoChSndInfo_End ===============
; Just returns... in this game.
Sound_DoChSndInfo_End:
	ret
	
; =============== Sound_DoCommandId ===============
; Handles the specified command ID, which mostly involves different ways of writing out data to the SndInfo.
; IN
; - A: Command ID (+ $E0)
; - DE: SndInfo base ptr
Sound_DoCommandId:

	; After the function in the jump table executes, increment the data ptr
	; *AND* return to the normal custom data update loop.
	; Make the next 'ret' instruction jump to Sound_IncDataPtr
	ld   hl, Sound_IncDataPtr
	push hl

	;
	; Index the command fetch ptr table.
	;

	; Get rid of the upper three bits of the command id (essentially subtracting $E0).
	; The resulting value is perfectly in range of the command table at Sound_CmdPtrTbl.
	and  $1F				; A -= SNDCMD_BASE

	ld   hl, Sound_CmdPtrTbl; HL = Ptr table
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
Sound_IncDataPtr:
	ld   hl, hSndInfoCurDataPtr_Low
	inc  [hl]						; hSndInfoCurDataPtr_Low++
	jp   nz, Sound_DoChSndInfo_Loop	; If low byte == 0, jump
	inc  hl							; Seek to hSndInfoCurDataPtr_High
	inc  [hl]						; Increase high byte
	jp   Sound_DoChSndInfo_Loop

Sound_CmdPtrTbl:
	dw Sound_DecDataPtr;X					; $00
	dw Sound_DecDataPtr;X
	dw Sound_DecDataPtr;X
	dw Sound_Cmd_EndCh;X
	dw Sound_Cmd_WriteToNRx2
	dw Sound_Cmd_JpFromLoop
	dw Sound_Cmd_AddToBaseFreqId
	dw Sound_Cmd_JpFromLoopByTimer
	dw Sound_Cmd_Unused_WriteToNR10;X		; $08
	dw Sound_Cmd_SetChEna
	dw Sound_DecDataPtr;X
	dw Sound_DecDataPtr;X
	dw Sound_Cmd_Call
	dw Sound_Cmd_Ret
	dw Sound_Cmd_WriteToNRx1
	dw Sound_Cmd_SetSkipNRx2;X
	dw Sound_Cmd_ClrSkipNRx2;X				; $10
	dw Sound_Cmd_SetVibrato
	dw Sound_Cmd_ClrVibrato;X
	dw Sound_Cmd_SetWaveData
	dw Sound_Cmd_Unused_EndChFlag7F;X
	dw Sound_Cmd_SetCh3StopLength
	dw Sound_Cmd_EndChFlagBF;X
	dw Sound_DecDataPtr;X
	dw Sound_DecDataPtr;X					; $18
	dw Sound_DecDataPtr;X
	dw Sound_Cmd_SetLength
	dw Sound_DecDataPtr;X
	dw Sound_Cmd_Unused_StartSlide;X
	dw Sound_DecDataPtr;X
	dw Sound_DecDataPtr;X
	dw Sound_DecDataPtr;X					; $1F

; =============== Sound_Cmd_Unused_StartSlide ===============
; [TCRF] Not used.
; Starts a pitch slide with custom parameters.
; See also: Sound_StartSlide
;
; Command data format:
; - 1: Slide length, in frames
; - 2: Target Note ID
Sound_Cmd_Unused_StartSlide:
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
				ld   hl, Sound_FreqDataTbl
				ld   b, $00
				add  hl, bc		; Get the respective frequency..
				add  hl, bc
				ldi  a, [hl]	; ...to BC
				ld   b, [hl]
				ld   c, a
			pop  af
			call Sound_CmdS_StartSlide
			
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
	jp   Sound_DoChSndInfo_End

; =============== Sound_DecDataPtr ===============
; Decrements the data ptr by 1.
; If called once, it balances out the Sound_IncDataPtr that's always called after Sound_DoCommandId is executed.
Sound_DecDataPtr:
	; hSndInfoCurDataPtr--
	ld   hl, hSndInfoCurDataPtr_Low
	ld   a, [hl]			; Subtract low byte
	sub  a, $01
	ldi  [hl], a			; Save val
	ret  nc					; Underflowed? If not, return
	dec  [hl]				; Subtract high byte (we never get here)
	ret
	
; =============== Sound_Cmd_SetCh3StopLength ===============
; Sets a new length value for channel 3 (wSndCh3StopLength), and applies it immediately.
; Command data format:
; - 0: New length value
Sound_Cmd_SetCh3StopLength:
	; Read a value off the data ptr.
	; wSndCh3StopLength = ^(*hSndInfoCurDataPtr)
	ld   hl, hSndInfoCurDataPtr_Low		; Read out to HL
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a
	ld   a, [hl]						; Read value off current data ptr
	cpl									; Invert the bits
	ld   [wSndCh3StopLength], a			; Write it

	; If the length isn't "none" ($FF), write the value to the register immediately.
	; This also means other attempts to write wSndCh3StopLength need to be guarded by a $FF check.
	cp   SNDLEN_INFINITE
	ret  z
	ldh  [rNR31], a
	ret
	
; =============== Sound_Cmd_AddToBaseFreqId ===============
; Increases the base frequency index by the read amount.
; Command data format:
; - 0: Frequency id offset
Sound_Cmd_AddToBaseFreqId:
	; Read a value off the data ptr.
	ld   hl, hSndInfoCurDataPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a
	ld   a, [hl]

	; Add the value to iSndInfo_FreqDataIdBase
	ld   hl, iSndInfo_FreqDataIdBase
	add  hl, de				; Seek to the value
	add  [hl]			; A += iSndInfo_FreqDataIdBase
	ld   [hl], a			; Save it back
	ret
	
; =============== Sound_Cmd_SetVibrato ===============
; Enables the vibrato, using the specified set ID.
; Once enabled, this will remain active indefinitely until it's either explicitly
; disabled a new song plays.
; Command data format:
; - Vibrato ID (0-9)
Sound_Cmd_SetVibrato:
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
	jp   Sound_DecDataPtr
	
; =============== Sound_Cmd_ClrVibrato ===============
; [TCRF] Unused subroutine.
;        Disables vibrato.
Sound_Cmd_ClrVibrato:
	ld   a, [de]
	res  SISB_VIBRATO, a
	ld   [de], a

	; Don't increase data ptr
	jp   Sound_DecDataPtr

; =============== Sound_Cmd_ClrSkipNRx2 ===============
; Clears disable flag for NRx2 writes
Sound_Cmd_ClrSkipNRx2:
	ld   a, [de]
	res  SISB_SKIPNRx2, a
	ld   [de], a

	jp   Sound_DecDataPtr

; =============== Sound_Cmd_SetSkipNRx2 ===============
; Sets disable flag for NRx2 writes
Sound_Cmd_SetSkipNRx2:
	ld   a, [de]
	set  SISB_SKIPNRx2, a
	ld   [de], a
	jp   Sound_DecDataPtr

; =============== Sound_Cmd_SetLength ===============
; Sets a new channel length target, which also resets the timer.
; This doesn't return to the sync loop, unlike the other way to set the length.
; Command data format:
; - 0: Length target
Sound_Cmd_SetLength:

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
	jp   Sound_DoChSndInfo_End
	
; =============== Sound_Cmd_SetChEna ===============
; Sets a new "enabled channels" bitmask. The read value should only affect a single channel.
;
; Command data format:
; - 0: Sound channels to enable
Sound_Cmd_SetChEna:

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
; Should only be used by BGM.
;
; Command data format:
; - 0: Sound register data
Sound_Cmd_WriteToNRx2:

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
	call Sound_WriteToReg

	; -> write it to iSndInfo_VolPredict, with the low nybble cleared
	;    (since we have set a new volume value, the prediction should restart)
	and  $F0									; Erase timer nybble
	ld   hl, iSndInfo_VolPredict
	add  hl, de
	ld   [hl], a
	ret

; =============== Sound_Cmd_WriteToNRx1 ===============
; Writes the current sound channel data to NRx1, and updates the additional SndInfo fields.
; Should only be used by BGM.
;
; Command data format:
; - 0: Sound register data
Sound_Cmd_WriteToNRx1:

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
	jp   Sound_WriteToReg
	
; =============== Sound_Cmd_Unused_WriteToNR10 ===============
; [TCRF] Unused command.
; Writes the current sound channel data to rNR10 and updates the bookkeeping value.
;
; Command data format:
; - 0: Sound channel data for NR10
Sound_Cmd_Unused_WriteToNR10:

	; Read sound channel data value to A
	ld   hl, hSndInfoCurDataPtr_Low
	ldi  a, [hl]
	ld   h, [hl]
	ld   l, a
	ld   a, [hl]

	; Update the bookkeeping value
	ld   hl, iSndInfo_Unknown_Unused_NR10Data
	add  hl, de
	ld   [hl], a

	; Write to the sound register if possible
	ld   c, LOW(rNR10)
	jp   Sound_WriteToReg
	
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
Sound_Cmd_JpFromLoopByTimer:

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

	dec  [hl]						; Decrement loop timer
	jp   nz, Sound_Cmd_JpFromLoop	; Is it 0 now? If not, jump
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
	jr   nz, Sound_Cmd_JpFromLoop	; Is it 0 now? If not, jump

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
Sound_Cmd_JpFromLoop:
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
Sound_Cmd_Call:
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
Sound_Cmd_Ret:
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
Sound_Cmd_SetWaveData:

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
	ld   hl, Sound_WaveSetPtrTable
	call Sound_IndexPtrTable				; HL = Wave table entry ptr

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
	
; =============== Sound_Unused_SetWaveDataCustom ===============
; Writes a complete set of wave data. This will disable ch3 playback.
; [TCRF] Unused in this game.
;
; IN
; - HL: Ptr to a wave set id
Sound_Unused_SetWaveDataCustom:
	; Disable wave ch
	ld   a, SNDCH3_OFF
	ldh  [rNR30], a

	; Index the ptr table with wave sets
	ld   a, [hl]
	ld   hl, Sound_WaveSetPtrTable
	call Sound_IndexPtrTable				; HL = Wave table entry ptr

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
	
; =============== Sound_Cmd_EndChFlagBF ===============
; Used in this game for some reason.
Sound_Cmd_EndChFlagBF:
	ld   a, [wSnd_Unused_ChUsed]
	and  $BF
	ld   [wSnd_Unused_ChUsed], a
	jr   Sound_Cmd_EndCh
	
; =============== Sound_Cmd_Unused_EndChFlag7F ===============
; [TCRF] Unused command.
Sound_Cmd_Unused_EndChFlag7F:
	ld   a, [wSnd_Unused_ChUsed]
	and  $7F
	ld   [wSnd_Unused_ChUsed], a
	
; =============== Sound_Cmd_EndCh ===============
; Called to permanently stop channel playback (ie: the song/sfx ended and didn't loop).
; This either stops the sound channel or resumes playback of the BGM.
Sound_Cmd_EndCh:

	; Mute the sound channel if there isn't a SFX playing on here, for good measure.
	; This isn't really needed.
	call Sound_SilenceCh

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
	inc  hl			; Seek to iSndInfo_Unknown_Unused_NR10Data
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

	ld   a, [wSndCh3StopLength]
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
	call Sound_Unused_SetWaveDataCustom

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
Sound_UpdateVolPredict:
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
Sound_SilenceCh:
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
Sound_WriteToReg:
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
Sound_MarkSFXChUse:
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
Sound_IndexPtrTable:
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
Sound_StopAll:
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
	ld   hl, Sound_ChRegAddrTable		; HL = Start of table
	ld   b, (Sound_ChRegAddrTable.end-Sound_ChRegAddrTable)	; B = Bytes to overwrite (table size)
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
	ld   [wSnd_Unused_ChUsed], a
.initNR:
	ld   a, $08			; Use downwards sweep for ch1 (standard)
	ldh  [rNR10], a
	xor  a
	ldh  [rNR30], a		; Stop Ch3
	ldh  [rNR51], a		; Silence all channels
	
	; These weren't initialized in the older version of the driver.
	; It matters more here due to conditional updates.
	ld   [wSndEnaChBGM], a
	ld   [wSndCh3StopLength], a
	
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
Sound_Unused_InitCh1Regs:
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
Sound_Unused_InitCh2Regs:
	ld   a, $F7
	ldh  [rNR22], a
	ld   a, $14
	ldh  [rNR23], a
	ld   a, $87
	ldh  [rNR24], a
	ret
	
; =============== Sound_ChRegAddrTable ===============
; List of memory adddresses cleared by Sound_Init.
Sound_ChRegAddrTable:
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

; =============== Sound_SndHeaderPtrTable ===============
; Table of sound headers, ordered by ID.
; Each of the valid ones follows the format as specified in iSndHeader_*
Sound_SndHeaderPtrTable:
	dw SndHeader_BGM_01  ;
	dw SndHeader_BGM_01  ; 130
	dw SndHeader_BGM_02  ; 131
	dw SndHeader_BGM_03  ; 132
	dw SndHeader_BGM_04  ; 133
	dw SndHeader_BGM_05  ; 134
	dw SndHeader_BGM_06  ; 135
	dw SndHeader_BGM_07  ; 136
	dw SndHeader_BGM_08  ; 137
	dw SndHeader_BGM_09  ; 138
	dw SndHeader_BGM_0A  ; 139
	dw SndHeader_BGM_0B  ; 140
	dw SndHeader_Pause   ; 141
	dw SndHeader_Unpause ; 142
	dw SndHeader_SFX_0E  ; 143
	dw SndHeader_SFX_0F  ; 144
	dw SndHeader_SFX_10  ; 145
	dw SndHeader_SFX_11  ; 146
	dw SndHeader_SFX_12  ; 147
	dw SndHeader_SFX_13  ; 148
	dw SndHeader_SFX_14  ; 149
	dw SndHeader_SFX_15  ; 150
	dw SndHeader_SFX_16  ; 151
	dw SndHeader_SFX_17  ; 152
	dw SndHeader_SFX_18  ; 153
	dw SndHeader_SFX_19  ; 154
	dw SndHeader_SFX_1A  ; 155
	dw SndHeader_SFX_1B  ; 156
	dw SndHeader_SFX_1C  ; 157
	dw SndHeader_SFX_1D  ; 158
	dw SndHeader_SFX_1E  ; 159
	dw SndHeader_SFX_1F  ; 160
	dw SndHeader_SFX_20  ; 161
	dw SndHeader_SFX_21  ; 162
	dw SndHeader_SFX_22  ; 163
	dw SndHeader_SFX_23  ; 164
	dw SndHeader_SFX_24  ; 165
	dw SndHeader_SFX_25  ; 166
	dw SndHeader_SFX_26  ; 167
	dw SndHeader_SFX_27  ; 168
	dw SndHeader_SFX_28  ; 169
	dw SndHeader_SFX_29  ; 170
	dw SndHeader_SFX_2A  ; 171
	dw SndHeader_SFX_2B  ; 172
	dw SndHeader_SFX_2C  ; 173
	dw SndHeader_SFX_2D  ; 174
	dw SndHeader_SFX_2E  ; 175
	dw SndHeader_SFX_2F  ; 176
	dw SndHeader_SFX_30  ; 177
	dw SndHeader_SFX_31  ; 178
	dw SndHeader_SFX_32  ; 179
	dw SndHeader_SFX_33  ; 180
	dw SndHeader_SFX_34  ; 181
	dw SndHeader_SFX_35  ; 182
	dw SndHeader_SFX_36  ; 183
	dw SndHeader_SFX_37  ; 184
	dw SndHeader_SFX_38  ; 185
	dw SndHeader_SFX_39  ; 186
	dw SndHeader_SFX_3A  ; 187
	dw SndHeader_SFX_3B  ; 188
	dw SndHeader_BGM_01  ; 189
	dw SndHeader_BGM_01  ; 190
	dw SndHeader_BGM_3E  ; 191
	dw SndHeader_BGM_3F  ; 192
	dw SndHeader_BGM_40  ; 193
	dw SndHeader_BGM_01  ; 194
	dw SndHeader_BGM_01  ; 195
	dw SndHeader_BGM_01  ; 196
	dw SndHeader_BGM_01  ; 197
	dw SndHeader_BGM_01  ; 198
	dw SndHeader_BGM_01  ; 199
	dw SndHeader_BGM_01  ; 200
	dw SndHeader_BGM_01  ; 201
	dw SndHeader_BGM_49  ; 202
	dw SndHeader_BGM_4A  ; 203
	dw SndHeader_BGM_4B  ; 204
	dw SndHeader_BGM_01  ; 205
	dw SndHeader_BGM_01  ; 206
	dw SndHeader_BGM_01  ; 207
	dw SndHeader_BGM_01  ; 208
	dw SndHeader_BGM_01  ; 209
	dw SndHeader_BGM_01  ; 210
	dw SndHeader_BGM_01  ; 211
	dw SndHeader_BGM_01  ; 212
	dw SndHeader_BGM_01  ; 213
	dw SndHeader_BGM_01  ; 214
	dw SndHeader_BGM_01  ; 215
	dw SndHeader_BGM_01  ; 216
	dw SndHeader_BGM_01  ; 217
	dw SndHeader_BGM_01  ; 218
	dw SndHeader_BGM_01  ; 219
	dw SndHeader_BGM_01  ; 220
	dw SndHeader_BGM_01  ; 221
	dw SndHeader_BGM_01  ; 222
	dw SndHeader_BGM_01  ; 223
	dw SndHeader_BGM_01  ; 224
	dw SndHeader_BGM_01  ; 225
	dw SndHeader_BGM_01  ; 226
	dw SndHeader_BGM_01  ; 227
	dw SndHeader_BGM_01  ; 228
	dw SndHeader_BGM_01  ; 229
	dw SndHeader_BGM_01  ; 230
	dw SndHeader_BGM_01  ; 231
	dw SndHeader_BGM_01  ; 232
	dw SndHeader_BGM_01  ; 233
	dw SndHeader_BGM_01  ; 234
	dw SndHeader_BGM_01  ; 235
	dw SndHeader_BGM_01  ; 236
	dw SndHeader_BGM_01  ; 237
	dw SndHeader_BGM_01  ; 238
	dw SndHeader_BGM_01  ; 239
	dw SndHeader_BGM_01  ; 240
	dw SndHeader_BGM_01  ; 241
	dw SndHeader_BGM_01  ; 242
	dw SndHeader_BGM_01  ; 243
	dw SndHeader_BGM_01  ; 244
	dw SndHeader_BGM_01  ; 245
	dw SndHeader_BGM_01  ; 246
	dw SndHeader_BGM_01  ; 247
	dw SndHeader_BGM_01  ; 248
	dw SndHeader_BGM_01  ; 249
	dw SndHeader_BGM_01  ; 250
	dw SndHeader_BGM_01  ; 251
	dw SndHeader_BGM_01  ; 252
	dw SndHeader_BGM_01  ; 253
	dw SndHeader_BGM_01  ; 254
	dw SndHeader_BGM_01  ; 255
	dw SndHeader_BGM_01  ; 256
	dw SndHeader_BGM_01  ; 257
	dw SndHeader_BGM_01  ; 258
	dw SndHeader_BGM_01  ; 259
	dw SndHeader_BGM_01  ; 260
	dw SndHeader_BGM_01  ; 261
	dw SndHeader_BGM_01  ; 262
Sound_SndBankPtrTable: 
	db BANK(SndHeader_BGM_01)  ;
	db BANK(SndHeader_BGM_01)  ; 130
	db BANK(SndHeader_BGM_02)  ; 131
	db BANK(SndHeader_BGM_03)  ; 132
	db BANK(SndHeader_BGM_04)  ; 133
	db BANK(SndHeader_BGM_05)  ; 134
	db BANK(SndHeader_BGM_06)  ; 135
	db BANK(SndHeader_BGM_07)  ; 136
	db BANK(SndHeader_BGM_08)  ; 137
	db BANK(SndHeader_BGM_09)  ; 138
	db BANK(SndHeader_BGM_0A)  ; 139
	db BANK(SndHeader_BGM_0B)  ; 140
	db BANK(SndHeader_Pause)   ; 141
	db BANK(SndHeader_Unpause) ; 142
	db BANK(SndHeader_SFX_0E)  ; 143
	db BANK(SndHeader_SFX_0F)  ; 144
	db BANK(SndHeader_SFX_10)  ; 145
	db BANK(SndHeader_SFX_11)  ; 146
	db BANK(SndHeader_SFX_12)  ; 147
	db BANK(SndHeader_SFX_13)  ; 148
	db BANK(SndHeader_SFX_14)  ; 149
	db BANK(SndHeader_SFX_15)  ; 150
	db BANK(SndHeader_SFX_16)  ; 151
	db BANK(SndHeader_SFX_17)  ; 152
	db BANK(SndHeader_SFX_18)  ; 153
	db BANK(SndHeader_SFX_19)  ; 154
	db BANK(SndHeader_SFX_1A)  ; 155
	db BANK(SndHeader_SFX_1B)  ; 156
	db BANK(SndHeader_SFX_1C)  ; 157
	db BANK(SndHeader_SFX_1D)  ; 158
	db BANK(SndHeader_SFX_1E)  ; 159
	db BANK(SndHeader_SFX_1F)  ; 160
	db BANK(SndHeader_SFX_20)  ; 161
	db BANK(SndHeader_SFX_21)  ; 162
	db BANK(SndHeader_SFX_22)  ; 163
	db BANK(SndHeader_SFX_23)  ; 164
	db BANK(SndHeader_SFX_24)  ; 165
	db BANK(SndHeader_SFX_25)  ; 166
	db BANK(SndHeader_SFX_26)  ; 167
	db BANK(SndHeader_SFX_27)  ; 168
	db BANK(SndHeader_SFX_28)  ; 169
	db BANK(SndHeader_SFX_29)  ; 170
	db BANK(SndHeader_SFX_2A)  ; 171
	db BANK(SndHeader_SFX_2B)  ; 172
	db BANK(SndHeader_SFX_2C)  ; 173
	db BANK(SndHeader_SFX_2D)  ; 174
	db BANK(SndHeader_SFX_2E)  ; 175
	db BANK(SndHeader_SFX_2F)  ; 176
	db BANK(SndHeader_SFX_30)  ; 177
	db BANK(SndHeader_SFX_31)  ; 178
	db BANK(SndHeader_SFX_32)  ; 179
	db BANK(SndHeader_SFX_33)  ; 180
	db BANK(SndHeader_SFX_34)  ; 181
	db BANK(SndHeader_SFX_35)  ; 182
	db BANK(SndHeader_SFX_36)  ; 183
	db BANK(SndHeader_SFX_37)  ; 184
	db BANK(SndHeader_SFX_38)  ; 185
	db BANK(SndHeader_SFX_39)  ; 186
	db BANK(SndHeader_SFX_3A)  ; 187
	db BANK(SndHeader_SFX_3B)  ; 188
	db BANK(SndHeader_BGM_01)  ; 189
	db BANK(SndHeader_BGM_01)  ; 190
	db BANK(SndHeader_BGM_3E)  ; 191
	db BANK(SndHeader_BGM_3F)  ; 192
	db BANK(SndHeader_BGM_40)  ; 193
	db BANK(SndHeader_BGM_01)  ; 194
	db BANK(SndHeader_BGM_01)  ; 195
	db BANK(SndHeader_BGM_01)  ; 196
	db BANK(SndHeader_BGM_01)  ; 197
	db BANK(SndHeader_BGM_01)  ; 198
	db BANK(SndHeader_BGM_01)  ; 199
	db BANK(SndHeader_BGM_01)  ; 200
	db BANK(SndHeader_BGM_01)  ; 201
	db BANK(SndHeader_BGM_49)  ; 202
	db BANK(SndHeader_BGM_4A)  ; 203
	db BANK(SndHeader_BGM_4B)  ; 204
	db BANK(SndHeader_BGM_01)  ; 205
	db BANK(SndHeader_BGM_01)  ; 206
	db BANK(SndHeader_BGM_01)  ; 207
	db BANK(SndHeader_BGM_01)  ; 208
	db BANK(SndHeader_BGM_01)  ; 209
	db BANK(SndHeader_BGM_01)  ; 210
	db BANK(SndHeader_BGM_01)  ; 211
	db BANK(SndHeader_BGM_01)  ; 212
	db BANK(SndHeader_BGM_01)  ; 213
	db BANK(SndHeader_BGM_01)  ; 214
	db BANK(SndHeader_BGM_01)  ; 215
	db BANK(SndHeader_BGM_01)  ; 216
	db BANK(SndHeader_BGM_01)  ; 217
	db BANK(SndHeader_BGM_01)  ; 218
	db BANK(SndHeader_BGM_01)  ; 219
	db BANK(SndHeader_BGM_01)  ; 220
	db BANK(SndHeader_BGM_01)  ; 221
	db BANK(SndHeader_BGM_01)  ; 222
	db BANK(SndHeader_BGM_01)  ; 223
	db BANK(SndHeader_BGM_01)  ; 224
	db BANK(SndHeader_BGM_01)  ; 225
	db BANK(SndHeader_BGM_01)  ; 226
	db BANK(SndHeader_BGM_01)  ; 227
	db BANK(SndHeader_BGM_01)  ; 228
	db BANK(SndHeader_BGM_01)  ; 229
	db BANK(SndHeader_BGM_01)  ; 230
	db BANK(SndHeader_BGM_01)  ; 231
	db BANK(SndHeader_BGM_01)  ; 232
	db BANK(SndHeader_BGM_01)  ; 233
	db BANK(SndHeader_BGM_01)  ; 234
	db BANK(SndHeader_BGM_01)  ; 235
	db BANK(SndHeader_BGM_01)  ; 236
	db BANK(SndHeader_BGM_01)  ; 237
	db BANK(SndHeader_BGM_01)  ; 238
	db BANK(SndHeader_BGM_01)  ; 239
	db BANK(SndHeader_BGM_01)  ; 240
	db BANK(SndHeader_BGM_01)  ; 241
	db BANK(SndHeader_BGM_01)  ; 242
	db BANK(SndHeader_BGM_01)  ; 243
	db BANK(SndHeader_BGM_01)  ; 244
	db BANK(SndHeader_BGM_01)  ; 245
	db BANK(SndHeader_BGM_01)  ; 246
	db BANK(SndHeader_BGM_01)  ; 247
	db BANK(SndHeader_BGM_01)  ; 248
	db BANK(SndHeader_BGM_01)  ; 249
	db BANK(SndHeader_BGM_01)  ; 250
	db BANK(SndHeader_BGM_01)  ; 251
	db BANK(SndHeader_BGM_01)  ; 252
	db BANK(SndHeader_BGM_01)  ; 253
	db BANK(SndHeader_BGM_01)  ; 254
	db BANK(SndHeader_BGM_01)  ; 255
	db BANK(SndHeader_BGM_01)  ; 256
	db BANK(SndHeader_BGM_01)  ; 257
	db BANK(SndHeader_BGM_01)  ; 258
	db BANK(SndHeader_BGM_01)  ; 259
	db BANK(SndHeader_BGM_01)  ; 260
	db BANK(SndHeader_BGM_01)  ; 261
	db BANK(SndHeader_BGM_01)  ; 262
; =============== Sound_FreqDataTbl ===============
; Table with pairs of frequency values for the frequency registers (sound channels 1-2-3).
; Essentially these are "musical notes" ordered from lowest to highest.
Sound_FreqDataTbl:
	dw $0000 ; N/A | $00 | $80
	dw $002C ; C-2 | $01 | $81
	dw $009C ; C#2 | $02 | $82
	dw $0106 ; D-2 | $03 | $83
	dw $016B ; D#2 | $04 | $84
	dw $01C9 ; E-2 | $05 | $85
	dw $0223 ; F-2 | $06 | $86
	dw $0277 ; F#2 | $07 | $87
	dw $02C7 ; G-2 | $08 | $88
	dw $0312 ; G#2 | $09 | $89
	dw $0358 ; A-2 | $0A | $8A
	dw $039B ; A#2 | $0B | $8B
	dw $03DA ; B-2 | $0C | $8C
	dw $0416 ; C-3 | $0D | $8D
	dw $044E ; C#3 | $0E | $8E
	dw $0483 ; D-3 | $0F | $8F
	dw $04B5 ; D#3 | $10 | $90
	dw $04E5 ; E-3 | $11 | $91
	dw $0511 ; F-3 | $12 | $92
	dw $053C ; F#3 | $13 | $93
	dw $0563 ; G-3 | $14 | $94
	dw $0589 ; G#3 | $15 | $95
	dw $05AC ; A-3 | $16 | $96
	dw $05CE ; A#3 | $17 | $97
	dw $05ED ; B-3 | $18 | $98
	dw $060B ; C-4 | $19 | $99
	dw $0628 ; C#4 | $1A | $9A
	dw $0642 ; D-4 | $1B | $9B
	dw $065B ; D#4 | $1C | $9C
	dw $0672 ; E-4 | $1D | $9D
	dw $0689 ; F-4 | $1E | $9E
	dw $069E ; F#4 | $1F | $9F
	dw $06B2 ; G-4 | $20 | $A0
	dw $06C4 ; G#4 | $21 | $A1
	dw $06D6 ; A-4 | $22 | $A2
	dw $06E7 ; A#4 | $23 | $A3
	dw $06F7 ; B-4 | $24 | $A4
	dw $0705 ; C-5 | $25 | $A5
	dw $0714 ; C#5 | $26 | $A6
	dw $0721 ; D-5 | $27 | $A7
	dw $072D ; D#5 | $28 | $A8
	dw $0739 ; E-5 | $29 | $A9
	dw $0744 ; F-5 | $2A | $AA
	dw $074F ; F#5 | $2B | $AB
	dw $0759 ; G-5 | $2C | $AC
	dw $0762 ; G#5 | $2D | $AD
	dw $076B ; A-5 | $2E | $AE
	dw $0773 ; A#5 | $2F | $AF
	dw $077B ; B-5 | $30 | $B0
	dw $0783 ; C-6 | $31 | $B1
	dw $078A ; C#6 | $32 | $B2
	dw $0790 ; D-6 | $33 | $B3
	dw $0797 ; D#6 | $34 | $B4
	dw $079D ; E-6 | $35 | $B5
	dw $07A2 ; F-6 | $36 | $B6
	dw $07A7 ; F#6 | $37 | $B7
	dw $07AC ; G-6 | $38 | $B8
	dw $07B1 ; G#6 | $39 | $B9
	dw $07B6 ; A-6 | $3A | $BA
	dw $07BA ; A#6 | $3B | $BB
	dw $07BE ; B-6 | $3C | $BC
	dw $07C1 ; C-7 | $3D | $BD
	dw $07C5 ; C#7 | $3E | $BE
	dw $07C8 ; D-7 | $3F | $BF
	dw $07CB ; D#7 | $40 | $C0
	dw $07CE ; E-7 | $41 | $C1
	dw $07D1 ; F-7 | $42 | $C2
	dw $07D4 ; F#7 | $43 | $C3
	dw $07D6 ; G-7 | $44 | $C4
	dw $07D9 ; G#7 | $45 | $C5
	dw $07DB ; A-7 | $46 | $C6
	dw $07DD ; A#7 | $47 | $C7
	dw $07DF ; B-7 | $48 | $C8
	dw $07E1 ; C-8 | $49 | $C9

; =============== Sound_WaveSetPtrTable ===============
; Sets of Wave data for channel 3, copied directly to the rWave registers.
; [TCRF] More than half of the sets are unused!
Sound_WaveSetPtrTable:
	dw Sound_WaveSet_Unused_0
	dw Sound_WaveSet1
	dw Sound_WaveSet2
	dw Sound_WaveSet_Unused_3
	dw Sound_WaveSet_Unused_4
	dw Sound_WaveSet5
	dw Sound_WaveSet_Unused_6
	dw Sound_WaveSet_Unused_7
	dw Sound_WaveSet_Unused_8
	dw Sound_WaveSet_Unused_9

Sound_WaveSet_Unused_0: db $01,$23,$45,$67,$89,$AB,$CD,$EF,$ED,$CB,$A9,$87,$65,$43,$21,$00
Sound_WaveSet1: db $FF,$EE,$DD,$CC,$BB,$AA,$99,$88,$77,$66,$55,$44,$33,$22,$11,$00
Sound_WaveSet2: db $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
Sound_WaveSet_Unused_3: db $FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
Sound_WaveSet_Unused_4: db $02,$46,$89,$AB,$CC,$DD,$EE,$FF,$FE,$ED,$DD,$CC,$BA,$98,$64,$31
Sound_WaveSet5: db $CF,$AF,$30,$12,$21,$01,$7F,$C2,$EA,$07,$FC,$62,$12,$5B,$FB,$12
Sound_WaveSet_Unused_6: db $86,$33,$22,$11,$00,$0B,$EF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$EE
Sound_WaveSet_Unused_7: db $87,$76,$65,$54,$43,$32,$21,$10,$0F,$FE,$ED,$DC,$CB,$BA,$A9,$98
Sound_WaveSet_Unused_8: db $18,$F2,$68,$4E,$18,$76,$1A,$4C,$85,$43,$2E,$DC,$FF,$12,$84,$48
Sound_WaveSet_Unused_9: db $CC,$6B,$93,$77,$28,$BA,$6E,$35,$51,$C3,$9C,$ED,$B8,$2B,$29,$E2

; =============== Sound_VibratoSetTable ===============
; Sets of vibrato data, usable by all channels.

; IN
; - 1: Ptr to a vibrato table.
;      This is a list of frequency offsets applied every frame, one after the other.
; - 2: Loop point
MACRO mVbDef
	dw \1
	db \2
ENDM
Sound_VibratoSetTable: 
	mVbDef Sound_VibratoSet_Unused_0, $00 ; $00 ;X
	mVbDef Sound_VibratoSet1, $10 ; $01 
	mVbDef Sound_VibratoSet_Unused_2, $08 ; $02 ;X
	mVbDef Sound_VibratoSet_Unused_3, $20 ; $03 ;X
	mVbDef Sound_VibratoSet_Unused_4, $07 ; $04 ;X
	mVbDef Sound_VibratoSet_Unused_5, $10 ; $05 ;X
	mVbDef Sound_VibratoSet_Unused_6, $18 ; $06 ;X
	mVbDef Sound_VibratoSet_Unused_7, $17 ; $07 ;X
	mVbDef Sound_VibratoSet8, $00 ; $08 
	mVbDef Sound_VibratoSet9, $00 ; $09 
	
Sound_VibratoSet_Unused_0: 
;.loop: ; $00
	db 0
	db +1
	db +2
	db +3
	db +2
	db +1
	db 0
	db -1
	db -2
	db -3
	db -2
	db -1
	db VIBCMD_LOOP
Sound_VibratoSet1: 
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
.loop: ; $10
	db 0
	db +1
	db +2
	db +3
	db +2
	db +1
	db 0
	db -1
	db -2
	db -1
	db VIBCMD_LOOP
Sound_VibratoSet_Unused_2: 
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
.loop: ; $08
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db +1
	db +1
	db +2
	db +1
	db +1
	db 0
	db 0
	db -1
	db -1
	db -2
	db -1
	db -1
	db VIBCMD_LOOP
Sound_VibratoSet_Unused_3:
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
.loop: ; $20
	db 0
	db 0
	db +1
	db +1
	db +1
	db +2
	db +2
	db +2
	db +1
	db +1
	db +1
	db 0
	db 0
	db VIBCMD_LOOP
Sound_VibratoSet_Unused_4:
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
.loop: ; $07
	db 0
	db +1
	db +2
	db +1
	db 0
	db -1
	db -2
	db -1
	db VIBCMD_LOOP
Sound_VibratoSet_Unused_5:
	db -7
	db -6
	db -5
	db -5
	db -4
	db -4
	db -3
	db -3
	db -2
	db -2
	db -2
	db -1
	db -1
	db -1
	db 0
	db 0
.loop: ; $10
	db 0
	db 0
	db +1
	db +1
	db +2
	db +1
	db +1
	db 0
	db 0
	db -1
	db -1
	db -2
	db -1
	db -1
	db VIBCMD_LOOP
Sound_VibratoSet_Unused_6:
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
.loop: ; $18
	db 0
	db +1
	db +2
	db +3
	db +2
	db +1
	db 0
	db -1
	db -2
	db -3
	db -2
	db -1
	db VIBCMD_LOOP
Sound_VibratoSet_Unused_7:
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
.loop: ; $17
	db +11
	db +22
	db +34
	db +45
	db +56
	db +69
	db +69
	db +69
	db +56
	db +45
	db +34
	db +22
	db +11
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db 0
	db VIBCMD_LOOP
Sound_VibratoSet8:
;.loop: ; $00
	db 0
	db +6
	db +12
	db +18
	db +24
	db +30
	db +36
	db +42
	db +48
	db +42
	db +36
	db +30
	db +24
	db +18
	db +12
	db +6
	db VIBCMD_LOOP
Sound_VibratoSet9:
;.loop: ; $00
	db 0
	db +3
	db +6
	db +9
	db +12
	db +15
	db +18
	db +21
	db +24
	db +27
	db +30
	db +33
	db +36
	db +39
	db +42
	db +45
	db +48
	db +45
	db +42
	db +39
	db +36
	db +33
	db +30
	db +27
	db +26
	db +25
	db +24
	db +23
	db +22
	db +21
	db +20
	db +19
	db +18
	db +17
	db +15
	db +14
	db +13
	db +12
	db +11
	db +10
	db +9
	db +8
	db +7
	db +6
	db +5
	db +4
	db +3
	db +2
	db +1
	db 0
	db 0
	db -1
	db -2
	db -3
	db -4
	db -5
	db -6
	db -7
	db -8
	db -9
	db -10
	db -11
	db -12
	db -13
	db -14
	db -15
	db -16
	db -17
	db -18
	db -19
	db -20
	db -21
	db -22
	db -23
	db -24
	db -25
	db -26
	db -27
	db -28
	db -29
	db -30
	db -31
	db -32
	db -33
	db -34
	db -35
	db -36
	db -37
	db -38
	db -39
	db -40
	db -41
	db -42
	db -43
	db -44
	db -45
	db -46
	db -47
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48
	db -48;X
	db -48;X
	db -48;X
	db -48;X
	db -48;X
	db -48;X
	db -48;X
	db -48;X
	db -48;X
	db -48;X
	db -48;X
	db -48;X
	db -48;X
	db -48;X
	db -48;X
	db -48;X
	db -48;X
	db -48;X
	db VIBCMD_LOOP;X
;;;;;;;;;;
SndHeader_BGM_4A: db $04;X
L0D4F9B: db $80;X
L0D4F9C: db $13;X
L0D4F9D: db $B3;X
L0D4F9E: db $4F;X
L0D4F9F: db $00;X
L0D4FA0: db $81;X
L0D4FA1: db $80;X
L0D4FA2: db $18;X
L0D4FA3: db $5D;X
L0D4FA4: db $50;X
L0D4FA5: db $00;X
L0D4FA6: db $81;X
L0D4FA7: db $80;X
L0D4FA8: db $1D;X
L0D4FA9: db $4D;X
L0D4FAA: db $51;X
L0D4FAB: db $00;X
L0D4FAC: db $81;X
L0D4FAD: db $80;X
L0D4FAE: db $22;X
L0D4FAF: db $8A;X
L0D4FB0: db $51;X
L0D4FB1: db $00;X
L0D4FB2: db $81;X
L0D4FB3: db $E4;X
L0D4FB4: db $A8;X
L0D4FB5: db $E9;X
L0D4FB6: db $11;X
L0D4FB7: db $EE;X
L0D4FB8: db $80;X
L0D4FB9: db $F1;X
L0D4FBA: db $03;X
L0D4FBB: db $E4;X
L0D4FBC: db $88;X
L0D4FBD: db $A2;X
L0D4FBE: db $02;X
L0D4FBF: db $E4;X
L0D4FC0: db $A8;X
L0D4FC1: db $A3;X
L0D4FC2: db $08;X
L0D4FC3: db $E4;X
L0D4FC4: db $A8;X
L0D4FC5: db $A5;X
L0D4FC6: db $05;X
L0D4FC7: db $80;X
L0D4FC8: db $02;X
L0D4FC9: db $E4;X
L0D4FCA: db $88;X
L0D4FCB: db $A5;X
L0D4FCC: db $03;X
L0D4FCD: db $E4;X
L0D4FCE: db $A8;X
L0D4FCF: db $A8;X
L0D4FD0: db $0A;X
L0D4FD1: db $A8;X
L0D4FD2: db $05;X
L0D4FD3: db $80;X
L0D4FD4: db $02;X
L0D4FD5: db $E4;X
L0D4FD6: db $88;X
L0D4FD7: db $A8;X
L0D4FD8: db $03;X
L0D4FD9: db $EE;X
L0D4FDA: db $C0;X
L0D4FDB: db $E4;X
L0D4FDC: db $A8;X
L0D4FDD: db $A3;X
L0D4FDE: db $0A;X
L0D4FDF: db $A2;X
L0D4FE0: db $02;X
L0D4FE1: db $A1;X
L0D4FE2: db $03;X
L0D4FE3: db $A0;X
L0D4FE4: db $02;X
L0D4FE5: db $9F;X
L0D4FE6: db $03;X
L0D4FE7: db $EE;X
L0D4FE8: db $80;X
L0D4FE9: db $E4;X
L0D4FEA: db $88;X
L0D4FEB: db $A2;X
L0D4FEC: db $02;X
L0D4FED: db $E4;X
L0D4FEE: db $A8;X
L0D4FEF: db $A3;X
L0D4FF0: db $12;X
L0D4FF1: db $A8;X
L0D4FF2: db $0A;X
L0D4FF3: db $AA;X
L0D4FF4: db $28;X
L0D4FF5: db $FA;X
L0D4FF6: db $0A;X
L0D4FF7: db $E4;X
L0D4FF8: db $88;X
L0D4FF9: db $AA;X
L0D4FFA: db $05;X
L0D4FFB: db $80;X
L0D4FFC: db $02;X
L0D4FFD: db $E4;X
L0D4FFE: db $68;X
L0D4FFF: db $AA;X
L0D5000: db $03;X
L0D5001: db $E4;X
L0D5002: db $48;X
L0D5003: db $AA;X
L0D5004: db $05;X
L0D5005: db $80;X
L0D5006: db $02;X
L0D5007: db $E4;X
L0D5008: db $38;X
L0D5009: db $AA;X
L0D500A: db $03;X
L0D500B: db $E4;X
L0D500C: db $A8;X
L0D500D: db $A8;X
L0D500E: db $0A;X
L0D500F: db $AA;X
L0D5010: db $AD;X
L0D5011: db $AC;X
L0D5012: db $80;X
L0D5013: db $AA;X
L0D5014: db $14;X
L0D5015: db $A8;X
L0D5016: db $0A;X
L0D5017: db $80;X
L0D5018: db $A6;X
L0D5019: db $28;X
L0D501A: db $E4;X
L0D501B: db $88;X
L0D501C: db $A6;X
L0D501D: db $05;X
L0D501E: db $80;X
L0D501F: db $02;X
L0D5020: db $E4;X
L0D5021: db $68;X
L0D5022: db $A6;X
L0D5023: db $03;X
L0D5024: db $E4;X
L0D5025: db $48;X
L0D5026: db $A6;X
L0D5027: db $05;X
L0D5028: db $80;X
L0D5029: db $02;X
L0D502A: db $E4;X
L0D502B: db $38;X
L0D502C: db $A6;X
L0D502D: db $03;X
L0D502E: db $80;X
L0D502F: db $08;X
L0D5030: db $E4;X
L0D5031: db $A8;X
L0D5032: db $A9;X
L0D5033: db $02;X
L0D5034: db $AA;X
L0D5035: db $14;X
L0D5036: db $EC;X
L0D5037: db $3A;X
L0D5038: db $50;X
L0D5039: db $E3;X
L0D503A: db $A8;X
L0D503B: db $14;X
L0D503C: db $E4;X
L0D503D: db $88;X
L0D503E: db $A8;X
L0D503F: db $05;X
L0D5040: db $E4;X
L0D5041: db $68;X
L0D5042: db $A8;X
L0D5043: db $02;X
L0D5044: db $E4;X
L0D5045: db $78;X
L0D5046: db $A8;X
L0D5047: db $03;X
L0D5048: db $E4;X
L0D5049: db $68;X
L0D504A: db $A8;X
L0D504B: db $05;X
L0D504C: db $A8;X
L0D504D: db $02;X
L0D504E: db $E4;X
L0D504F: db $58;X
L0D5050: db $A8;X
L0D5051: db $03;X
L0D5052: db $E4;X
L0D5053: db $48;X
L0D5054: db $A8;X
L0D5055: db $05;X
L0D5056: db $E4;X
L0D5057: db $38;X
L0D5058: db $A8;X
L0D5059: db $02;X
L0D505A: db $A8;X
L0D505B: db $03;X
L0D505C: db $ED;X
L0D505D: db $E4;X
L0D505E: db $A8;X
L0D505F: db $E9;X
L0D5060: db $22;X
L0D5061: db $EE;X
L0D5062: db $40;X
L0D5063: db $F1;X
L0D5064: db $03;X
L0D5065: db $E4;X
L0D5066: db $A8;X
L0D5067: db $9E;X
L0D5068: db $14;X
L0D5069: db $E4;X
L0D506A: db $A8;X
L0D506B: db $94;X
L0D506C: db $05;X
L0D506D: db $80;X
L0D506E: db $02;X
L0D506F: db $E4;X
L0D5070: db $68;X
L0D5071: db $94;X
L0D5072: db $03;X
L0D5073: db $E4;X
L0D5074: db $A8;X
L0D5075: db $97;X
L0D5076: db $05;X
L0D5077: db $80;X
L0D5078: db $02;X
L0D5079: db $E4;X
L0D507A: db $68;X
L0D507B: db $97;X
L0D507C: db $03;X
L0D507D: db $E4;X
L0D507E: db $A8;X
L0D507F: db $A8;X
L0D5080: db $0A;X
L0D5081: db $A7;X
L0D5082: db $02;X
L0D5083: db $A6;X
L0D5084: db $03;X
L0D5085: db $A5;X
L0D5086: db $02;X
L0D5087: db $A3;X
L0D5088: db $03;X
L0D5089: db $9C;X
L0D508A: db $05;X
L0D508B: db $80;X
L0D508C: db $02;X
L0D508D: db $E4;X
L0D508E: db $68;X
L0D508F: db $9C;X
L0D5090: db $03;X
L0D5091: db $E4;X
L0D5092: db $A8;X
L0D5093: db $97;X
L0D5094: db $05;X
L0D5095: db $80;X
L0D5096: db $02;X
L0D5097: db $E4;X
L0D5098: db $68;X
L0D5099: db $97;X
L0D509A: db $03;X
L0D509B: db $E4;X
L0D509C: db $A8;X
L0D509D: db $94;X
L0D509E: db $05;X
L0D509F: db $80;X
L0D50A0: db $02;X
L0D50A1: db $E4;X
L0D50A2: db $68;X
L0D50A3: db $94;X
L0D50A4: db $03;X
L0D50A5: db $E4;X
L0D50A6: db $A8;X
L0D50A7: db $9C;X
L0D50A8: db $05;X
L0D50A9: db $80;X
L0D50AA: db $02;X
L0D50AB: db $E4;X
L0D50AC: db $68;X
L0D50AD: db $9C;X
L0D50AE: db $03;X
L0D50AF: db $E4;X
L0D50B0: db $A8;X
L0D50B1: db $9B;X
L0D50B2: db $05;X
L0D50B3: db $80;X
L0D50B4: db $02;X
L0D50B5: db $E4;X
L0D50B6: db $68;X
L0D50B7: db $9B;X
L0D50B8: db $03;X
L0D50B9: db $E4;X
L0D50BA: db $A8;X
L0D50BB: db $97;X
L0D50BC: db $05;X
L0D50BD: db $80;X
L0D50BE: db $02;X
L0D50BF: db $E4;X
L0D50C0: db $68;X
L0D50C1: db $97;X
L0D50C2: db $03;X
L0D50C3: db $E4;X
L0D50C4: db $A8;X
L0D50C5: db $92;X
L0D50C6: db $05;X
L0D50C7: db $80;X
L0D50C8: db $02;X
L0D50C9: db $E4;X
L0D50CA: db $68;X
L0D50CB: db $92;X
L0D50CC: db $03;X
L0D50CD: db $E4;X
L0D50CE: db $A8;X
L0D50CF: db $9B;X
L0D50D0: db $05;X
L0D50D1: db $80;X
L0D50D2: db $02;X
L0D50D3: db $E4;X
L0D50D4: db $68;X
L0D50D5: db $9B;X
L0D50D6: db $03;X
L0D50D7: db $E4;X
L0D50D8: db $A8;X
L0D50D9: db $97;X
L0D50DA: db $05;X
L0D50DB: db $80;X
L0D50DC: db $02;X
L0D50DD: db $E4;X
L0D50DE: db $68;X
L0D50DF: db $97;X
L0D50E0: db $03;X
L0D50E1: db $E4;X
L0D50E2: db $A8;X
L0D50E3: db $96;X
L0D50E4: db $05;X
L0D50E5: db $80;X
L0D50E6: db $02;X
L0D50E7: db $E4;X
L0D50E8: db $68;X
L0D50E9: db $96;X
L0D50EA: db $03;X
L0D50EB: db $E4;X
L0D50EC: db $A8;X
L0D50ED: db $97;X
L0D50EE: db $05;X
L0D50EF: db $80;X
L0D50F0: db $02;X
L0D50F1: db $E4;X
L0D50F2: db $68;X
L0D50F3: db $97;X
L0D50F4: db $03;X
L0D50F5: db $E4;X
L0D50F6: db $A8;X
L0D50F7: db $9B;X
L0D50F8: db $05;X
L0D50F9: db $80;X
L0D50FA: db $02;X
L0D50FB: db $E4;X
L0D50FC: db $68;X
L0D50FD: db $9B;X
L0D50FE: db $03;X
L0D50FF: db $EC;X
L0D5100: db $29;X
L0D5101: db $51;X
L0D5102: db $E4;X
L0D5103: db $A8;X
L0D5104: db $9E;X
L0D5105: db $05;X
L0D5106: db $A1;X
L0D5107: db $A3;X
L0D5108: db $A5;X
L0D5109: db $B2;X
L0D510A: db $14;X
L0D510B: db $B1;X
L0D510C: db $05;X
L0D510D: db $B0;X
L0D510E: db $AF;X
L0D510F: db $02;X
L0D5110: db $AE;X
L0D5111: db $03;X
L0D5112: db $AD;X
L0D5113: db $AC;X
L0D5114: db $02;X
L0D5115: db $AB;X
L0D5116: db $AA;X
L0D5117: db $03;X
L0D5118: db $A9;X
L0D5119: db $A8;X
L0D511A: db $02;X
L0D511B: db $80;X
L0D511C: db $08;X
L0D511D: db $AC;X
L0D511E: db $02;X
L0D511F: db $AD;X
L0D5120: db $14;X
L0D5121: db $E6;X
L0D5122: db $04;X
L0D5123: db $EC;X
L0D5124: db $3A;X
L0D5125: db $50;X
L0D5126: db $E6;X
L0D5127: db $FC;X
L0D5128: db $E3;X
L0D5129: db $E4;X
L0D512A: db $A8;X
L0D512B: db $9E;X
L0D512C: db $05;X
L0D512D: db $80;X
L0D512E: db $02;X
L0D512F: db $E4;X
L0D5130: db $68;X
L0D5131: db $9E;X
L0D5132: db $03;X
L0D5133: db $E4;X
L0D5134: db $A8;X
L0D5135: db $9A;X
L0D5136: db $05;X
L0D5137: db $80;X
L0D5138: db $02;X
L0D5139: db $E4;X
L0D513A: db $68;X
L0D513B: db $9A;X
L0D513C: db $03;X
L0D513D: db $E4;X
L0D513E: db $A8;X
L0D513F: db $95;X
L0D5140: db $05;X
L0D5141: db $80;X
L0D5142: db $02;X
L0D5143: db $E4;X
L0D5144: db $68;X
L0D5145: db $95;X
L0D5146: db $03;X
L0D5147: db $E7;X
L0D5148: db $00;X
L0D5149: db $02;X
L0D514A: db $29;X
L0D514B: db $51;X
L0D514C: db $ED;X
L0D514D: db $E4;X
L0D514E: db $20;X
L0D514F: db $E9;X
L0D5150: db $44;X
L0D5151: db $F3;X
L0D5152: db $03;X
L0D5153: db $F5;X
L0D5154: db $00;X
L0D5155: db $F1;X
L0D5156: db $03;X
L0D5157: db $9E;X
L0D5158: db $0A;X
L0D5159: db $97;X
L0D515A: db $90;X
L0D515B: db $08;X
L0D515C: db $80;X
L0D515D: db $02;X
L0D515E: db $90;X
L0D515F: db $05;X
L0D5160: db $97;X
L0D5161: db $0A;X
L0D5162: db $90;X
L0D5163: db $14;X
L0D5164: db $99;X
L0D5165: db $0A;X
L0D5166: db $97;X
L0D5167: db $90;X
L0D5168: db $8F;X
L0D5169: db $14;X
L0D516A: db $80;X
L0D516B: db $0A;X
L0D516C: db $8F;X
L0D516D: db $14;X
L0D516E: db $92;X
L0D516F: db $0A;X
L0D5170: db $97;X
L0D5171: db $14;X
L0D5172: db $8E;X
L0D5173: db $0A;X
L0D5174: db $95;X
L0D5175: db $05;X
L0D5176: db $80;X
L0D5177: db $95;X
L0D5178: db $0A;X
L0D5179: db $8E;X
L0D517A: db $95;X
L0D517B: db $05;X
L0D517C: db $80;X
L0D517D: db $95;X
L0D517E: db $0A;X
L0D517F: db $8E;X
L0D5180: db $95;X
L0D5181: db $8E;X
L0D5182: db $14;X
L0D5183: db $80;X
L0D5184: db $28;X
L0D5185: db $8E;X
L0D5186: db $14;X
L0D5187: db $90;X
L0D5188: db $14;X
L0D5189: db $E3;X
L0D518A: db $E9;X
L0D518B: db $88;X
L0D518C: db $E4;X
L0D518D: db $A1;X
L0D518E: db $26;X
L0D518F: db $05;X
L0D5190: db $26;X
L0D5191: db $05;X
L0D5192: db $E4;X
L0D5193: db $91;X
L0D5194: db $35;X
L0D5195: db $05;X
L0D5196: db $E4;X
L0D5197: db $91;X
L0D5198: db $37;X
L0D5199: db $05;X
L0D519A: db $E4;X
L0D519B: db $C1;X
L0D519C: db $57;X
L0D519D: db $0A;X
L0D519E: db $E4;X
L0D519F: db $51;X
L0D51A0: db $17;X
L0D51A1: db $0A;X
L0D51A2: db $E4;X
L0D51A3: db $A1;X
L0D51A4: db $26;X
L0D51A5: db $0A;X
L0D51A6: db $E4;X
L0D51A7: db $53;X
L0D51A8: db $13;X
L0D51A9: db $0A;X
L0D51AA: db $E4;X
L0D51AB: db $51;X
L0D51AC: db $17;X
L0D51AD: db $0A;X
L0D51AE: db $E4;X
L0D51AF: db $C1;X
L0D51B0: db $57;X
L0D51B1: db $0A;X
L0D51B2: db $E4;X
L0D51B3: db $A1;X
L0D51B4: db $26;X
L0D51B5: db $0A;X
L0D51B6: db $E4;X
L0D51B7: db $51;X
L0D51B8: db $17;X
L0D51B9: db $0A;X
L0D51BA: db $E4;X
L0D51BB: db $C1;X
L0D51BC: db $57;X
L0D51BD: db $0A;X
L0D51BE: db $E4;X
L0D51BF: db $51;X
L0D51C0: db $17;X
L0D51C1: db $0A;X
L0D51C2: db $E4;X
L0D51C3: db $A1;X
L0D51C4: db $26;X
L0D51C5: db $0A;X
L0D51C6: db $E4;X
L0D51C7: db $C1;X
L0D51C8: db $57;X
L0D51C9: db $05;X
L0D51CA: db $E4;X
L0D51CB: db $53;X
L0D51CC: db $13;X
L0D51CD: db $0F;X
L0D51CE: db $E4;X
L0D51CF: db $C1;X
L0D51D0: db $57;X
L0D51D1: db $0A;X
L0D51D2: db $E4;X
L0D51D3: db $A1;X
L0D51D4: db $26;X
L0D51D5: db $05;X
L0D51D6: db $26;X
L0D51D7: db $05;X
L0D51D8: db $26;X
L0D51D9: db $05;X
L0D51DA: db $26;X
L0D51DB: db $05;X
L0D51DC: db $E4;X
L0D51DD: db $A1;X
L0D51DE: db $26;X
L0D51DF: db $0A;X
L0D51E0: db $E4;X
L0D51E1: db $C1;X
L0D51E2: db $57;X
L0D51E3: db $0A;X
L0D51E4: db $57;X
L0D51E5: db $0A;X
L0D51E6: db $E4;X
L0D51E7: db $A1;X
L0D51E8: db $26;X
L0D51E9: db $0A;X
L0D51EA: db $E4;X
L0D51EB: db $C1;X
L0D51EC: db $57;X
L0D51ED: db $0A;X
L0D51EE: db $57;X
L0D51EF: db $0A;X
L0D51F0: db $E4;X
L0D51F1: db $A1;X
L0D51F2: db $26;X
L0D51F3: db $0A;X
L0D51F4: db $E4;X
L0D51F5: db $C1;X
L0D51F6: db $57;X
L0D51F7: db $0A;X
L0D51F8: db $E4;X
L0D51F9: db $A1;X
L0D51FA: db $26;X
L0D51FB: db $32;X
L0D51FC: db $26;X
L0D51FD: db $0A;X
L0D51FE: db $E4;X
L0D51FF: db $91;X
L0D5200: db $37;X
L0D5201: db $14;X
L0D5202: db $E4;X
L0D5203: db $A1;X
L0D5204: db $26;X
L0D5205: db $0A;X
L0D5206: db $E3;X
SndHeader_BGM_40: db $03;X
L0D5208: db $80;X
L0D5209: db $13;X
L0D520A: db $20;X
L0D520B: db $52;X
L0D520C: db $00;X
L0D520D: db $81;X
L0D520E: db $80;X
L0D520F: db $18;X
L0D5210: db $75;X
L0D5211: db $52;X
L0D5212: db $00;X
L0D5213: db $81;X
L0D5214: db $80;X
L0D5215: db $1D;X
L0D5216: db $10;X
L0D5217: db $53;X
L0D5218: db $00;X
L0D5219: db $81;X
L0D521A: db $80;X
L0D521B: db $22;X
L0D521C: db $30;X
L0D521D: db $53;X
L0D521E: db $00;X
L0D521F: db $81;X
L0D5220: db $E4;X
L0D5221: db $A8;X
L0D5222: db $E9;X
L0D5223: db $11;X
L0D5224: db $EE;X
L0D5225: db $80;X
L0D5226: db $F1;X
L0D5227: db $03;X
L0D5228: db $9B;X
L0D5229: db $0E;X
L0D522A: db $9D;X
L0D522B: db $9E;X
L0D522C: db $9B;X
L0D522D: db $2A;X
L0D522E: db $E4;X
L0D522F: db $88;X
L0D5230: db $9B;X
L0D5231: db $07;X
L0D5232: db $80;X
L0D5233: db $03;X
L0D5234: db $E4;X
L0D5235: db $68;X
L0D5236: db $9B;X
L0D5237: db $04;X
L0D5238: db $E4;X
L0D5239: db $48;X
L0D523A: db $9B;X
L0D523B: db $07;X
L0D523C: db $80;X
L0D523D: db $03;X
L0D523E: db $E4;X
L0D523F: db $38;X
L0D5240: db $9B;X
L0D5241: db $04;X
L0D5242: db $80;X
L0D5243: db $0E;X
L0D5244: db $E4;X
L0D5245: db $A8;X
L0D5246: db $99;X
L0D5247: db $2A;X
L0D5248: db $97;X
L0D5249: db $96;X
L0D524A: db $0E;X
L0D524B: db $92;X
L0D524C: db $96;X
L0D524D: db $97;X
L0D524E: db $38;X
L0D524F: db $FA;X
L0D5250: db $0E;X
L0D5251: db $E4;X
L0D5252: db $88;X
L0D5253: db $97;X
L0D5254: db $07;X
L0D5255: db $80;X
L0D5256: db $03;X
L0D5257: db $E4;X
L0D5258: db $68;X
L0D5259: db $97;X
L0D525A: db $04;X
L0D525B: db $80;X
L0D525C: db $07;X
L0D525D: db $E4;X
L0D525E: db $48;X
L0D525F: db $97;X
L0D5260: db $07;X
L0D5261: db $80;X
L0D5262: db $04;X
L0D5263: db $E4;X
L0D5264: db $38;X
L0D5265: db $97;X
L0D5266: db $07;X
L0D5267: db $80;X
L0D5268: db $11;X
L0D5269: db $E4;X
L0D526A: db $A8;X
L0D526B: db $A0;X
L0D526C: db $07;X
L0D526D: db $9E;X
L0D526E: db $9D;X
L0D526F: db $99;X
L0D5270: db $96;X
L0D5271: db $92;X
L0D5272: db $8F;X
L0D5273: db $1C;X
L0D5274: db $E3;X
L0D5275: db $E4;X
L0D5276: db $88;X
L0D5277: db $E9;X
L0D5278: db $22;X
L0D5279: db $EE;X
L0D527A: db $40;X
L0D527B: db $F1;X
L0D527C: db $03;X
L0D527D: db $E4;X
L0D527E: db $88;X
L0D527F: db $9E;X
L0D5280: db $07;X
L0D5281: db $80;X
L0D5282: db $03;X
L0D5283: db $E4;X
L0D5284: db $68;X
L0D5285: db $9E;X
L0D5286: db $04;X
L0D5287: db $E4;X
L0D5288: db $88;X
L0D5289: db $A0;X
L0D528A: db $07;X
L0D528B: db $80;X
L0D528C: db $03;X
L0D528D: db $E4;X
L0D528E: db $68;X
L0D528F: db $A0;X
L0D5290: db $04;X
L0D5291: db $E4;X
L0D5292: db $88;X
L0D5293: db $A2;X
L0D5294: db $07;X
L0D5295: db $80;X
L0D5296: db $03;X
L0D5297: db $E4;X
L0D5298: db $68;X
L0D5299: db $A2;X
L0D529A: db $04;X
L0D529B: db $E4;X
L0D529C: db $88;X
L0D529D: db $9E;X
L0D529E: db $1C;X
L0D529F: db $E4;X
L0D52A0: db $68;X
L0D52A1: db $9E;X
L0D52A2: db $07;X
L0D52A3: db $80;X
L0D52A4: db $03;X
L0D52A5: db $E4;X
L0D52A6: db $48;X
L0D52A7: db $9E;X
L0D52A8: db $04;X
L0D52A9: db $E4;X
L0D52AA: db $88;X
L0D52AB: db $AC;X
L0D52AC: db $07;X
L0D52AD: db $AA;X
L0D52AE: db $A9;X
L0D52AF: db $A7;X
L0D52B0: db $A5;X
L0D52B1: db $A3;X
L0D52B2: db $A2;X
L0D52B3: db $1C;X
L0D52B4: db $E4;X
L0D52B5: db $68;X
L0D52B6: db $A2;X
L0D52B7: db $07;X
L0D52B8: db $80;X
L0D52B9: db $03;X
L0D52BA: db $E4;X
L0D52BB: db $48;X
L0D52BC: db $A2;X
L0D52BD: db $04;X
L0D52BE: db $E4;X
L0D52BF: db $88;X
L0D52C0: db $A0;X
L0D52C1: db $1C;X
L0D52C2: db $E4;X
L0D52C3: db $68;X
L0D52C4: db $A0;X
L0D52C5: db $07;X
L0D52C6: db $80;X
L0D52C7: db $03;X
L0D52C8: db $E4;X
L0D52C9: db $48;X
L0D52CA: db $A0;X
L0D52CB: db $04;X
L0D52CC: db $E4;X
L0D52CD: db $88;X
L0D52CE: db $9E;X
L0D52CF: db $07;X
L0D52D0: db $80;X
L0D52D1: db $03;X
L0D52D2: db $E4;X
L0D52D3: db $68;X
L0D52D4: db $9E;X
L0D52D5: db $04;X
L0D52D6: db $E4;X
L0D52D7: db $88;X
L0D52D8: db $9B;X
L0D52D9: db $07;X
L0D52DA: db $80;X
L0D52DB: db $03;X
L0D52DC: db $E4;X
L0D52DD: db $68;X
L0D52DE: db $9B;X
L0D52DF: db $04;X
L0D52E0: db $E4;X
L0D52E1: db $88;X
L0D52E2: db $9E;X
L0D52E3: db $07;X
L0D52E4: db $80;X
L0D52E5: db $03;X
L0D52E6: db $E4;X
L0D52E7: db $68;X
L0D52E8: db $9E;X
L0D52E9: db $04;X
L0D52EA: db $E4;X
L0D52EB: db $88;X
L0D52EC: db $A0;X
L0D52ED: db $38;X
L0D52EE: db $FA;X
L0D52EF: db $0E;X
L0D52F0: db $80;X
L0D52F1: db $03;X
L0D52F2: db $E4;X
L0D52F3: db $68;X
L0D52F4: db $A0;X
L0D52F5: db $07;X
L0D52F6: db $80;X
L0D52F7: db $04;X
L0D52F8: db $E4;X
L0D52F9: db $48;X
L0D52FA: db $A0;X
L0D52FB: db $07;X
L0D52FC: db $80;X
L0D52FD: db $03;X
L0D52FE: db $E4;X
L0D52FF: db $38;X
L0D5300: db $A0;X
L0D5301: db $07;X
L0D5302: db $80;X
L0D5303: db $19;X
L0D5304: db $E4;X
L0D5305: db $88;X
L0D5306: db $A3;X
L0D5307: db $07;X
L0D5308: db $A2;X
L0D5309: db $A0;X
L0D530A: db $A2;X
L0D530B: db $9D;X
L0D530C: db $99;X
L0D530D: db $96;X
L0D530E: db $1C;X
L0D530F: db $E3;X
L0D5310: db $E4;X
L0D5311: db $20;X
L0D5312: db $E9;X
L0D5313: db $44;X
L0D5314: db $F3;X
L0D5315: db $03;X
L0D5316: db $F5;X
L0D5317: db $00;X
L0D5318: db $F1;X
L0D5319: db $03;X
L0D531A: db $8F;X
L0D531B: db $70;X
L0D531C: db $FA;X
L0D531D: db $0E;X
L0D531E: db $94;X
L0D531F: db $2A;X
L0D5320: db $92;X
L0D5321: db $8F;X
L0D5322: db $90;X
L0D5323: db $54;X
L0D5324: db $80;X
L0D5325: db $2A;X
L0D5326: db $A5;X
L0D5327: db $07;X
L0D5328: db $A2;X
L0D5329: db $A0;X
L0D532A: db $9D;X
L0D532B: db $99;X
L0D532C: db $96;X
L0D532D: db $8F;X
L0D532E: db $1C;X
L0D532F: db $E3;X
L0D5330: db $E9;X
L0D5331: db $88;X
L0D5332: db $E3;X
SndHeader_BGM_3E: db $04;X
L0D5334: db $80;X
L0D5335: db $13;X
L0D5336: db $4C;X
L0D5337: db $53;X
L0D5338: db $00;X
L0D5339: db $81;X
L0D533A: db $80;X
L0D533B: db $18;X
L0D533C: db $9F;X
L0D533D: db $55;X
L0D533E: db $00;X
L0D533F: db $81;X
L0D5340: db $80;X
L0D5341: db $1D;X
L0D5342: db $10;X
L0D5343: db $5C;X
L0D5344: db $00;X
L0D5345: db $81;X
L0D5346: db $80;X
L0D5347: db $22;X
L0D5348: db $EC;X
L0D5349: db $5D;X
L0D534A: db $00;X
L0D534B: db $81;X
L0D534C: db $E4;X
L0D534D: db $A8;X
L0D534E: db $E9;X
L0D534F: db $11;X
L0D5350: db $EE;X
L0D5351: db $80;X
L0D5352: db $F1;X
L0D5353: db $03;X
L0D5354: db $9B;X
L0D5355: db $07;X
L0D5356: db $9D;X
L0D5357: db $9E;X
L0D5358: db $9F;X
L0D5359: db $A0;X
L0D535A: db $0E;X
L0D535B: db $9B;X
L0D535C: db $07;X
L0D535D: db $9D;X
L0D535E: db $9E;X
L0D535F: db $9F;X
L0D5360: db $A0;X
L0D5361: db $0E;X
L0D5362: db $9A;X
L0D5363: db $07;X
L0D5364: db $99;X
L0D5365: db $97;X
L0D5366: db $0E;X
L0D5367: db $9B;X
L0D5368: db $07;X
L0D5369: db $9D;X
L0D536A: db $9E;X
L0D536B: db $9F;X
L0D536C: db $A0;X
L0D536D: db $0E;X
L0D536E: db $9B;X
L0D536F: db $07;X
L0D5370: db $9D;X
L0D5371: db $9E;X
L0D5372: db $9F;X
L0D5373: db $A0;X
L0D5374: db $0E;X
L0D5375: db $98;X
L0D5376: db $07;X
L0D5377: db $99;X
L0D5378: db $9B;X
L0D5379: db $0E;X
L0D537A: db $9B;X
L0D537B: db $07;X
L0D537C: db $9D;X
L0D537D: db $9E;X
L0D537E: db $9F;X
L0D537F: db $A0;X
L0D5380: db $0E;X
L0D5381: db $9B;X
L0D5382: db $07;X
L0D5383: db $9D;X
L0D5384: db $9E;X
L0D5385: db $9F;X
L0D5386: db $A0;X
L0D5387: db $0E;X
L0D5388: db $9A;X
L0D5389: db $07;X
L0D538A: db $99;X
L0D538B: db $97;X
L0D538C: db $0E;X
L0D538D: db $9B;X
L0D538E: db $07;X
L0D538F: db $9D;X
L0D5390: db $9E;X
L0D5391: db $9F;X
L0D5392: db $A0;X
L0D5393: db $0E;X
L0D5394: db $9B;X
L0D5395: db $07;X
L0D5396: db $9D;X
L0D5397: db $9E;X
L0D5398: db $9F;X
L0D5399: db $A0;X
L0D539A: db $0E;X
L0D539B: db $A4;X
L0D539C: db $07;X
L0D539D: db $A5;X
L0D539E: db $A7;X
L0D539F: db $0E;X
L0D53A0: db $EC;X
L0D53A1: db $81;X
L0D53A2: db $54;X
L0D53A3: db $E4;X
L0D53A4: db $A8;X
L0D53A5: db $AE;X
L0D53A6: db $07;X
L0D53A7: db $AB;X
L0D53A8: db $A7;X
L0D53A9: db $AE;X
L0D53AA: db $AB;X
L0D53AB: db $A7;X
L0D53AC: db $AE;X
L0D53AD: db $AB;X
L0D53AE: db $AE;X
L0D53AF: db $AA;X
L0D53B0: db $A7;X
L0D53B1: db $AE;X
L0D53B2: db $AA;X
L0D53B3: db $A7;X
L0D53B4: db $AE;X
L0D53B5: db $AA;X
L0D53B6: db $AC;X
L0D53B7: db $A9;X
L0D53B8: db $A5;X
L0D53B9: db $AC;X
L0D53BA: db $A9;X
L0D53BB: db $A5;X
L0D53BC: db $AC;X
L0D53BD: db $A9;X
L0D53BE: db $AC;X
L0D53BF: db $A8;X
L0D53C0: db $A5;X
L0D53C1: db $AC;X
L0D53C2: db $A8;X
L0D53C3: db $A5;X
L0D53C4: db $AC;X
L0D53C5: db $A8;X
L0D53C6: db $AB;X
L0D53C7: db $A7;X
L0D53C8: db $A4;X
L0D53C9: db $A9;X
L0D53CA: db $A6;X
L0D53CB: db $A2;X
L0D53CC: db $AA;X
L0D53CD: db $A7;X
L0D53CE: db $A3;X
L0D53CF: db $AC;X
L0D53D0: db $A9;X
L0D53D1: db $A5;X
L0D53D2: db $AE;X
L0D53D3: db $AB;X
L0D53D4: db $A7;X
L0D53D5: db $0E;X
L0D53D6: db $EC;X
L0D53D7: db $F0;X
L0D53D8: db $54;X
L0D53D9: db $EC;X
L0D53DA: db $1C;X
L0D53DB: db $55;X
L0D53DC: db $EC;X
L0D53DD: db $54;X
L0D53DE: db $55;X
L0D53DF: db $EC;X
L0D53E0: db $1C;X
L0D53E1: db $55;X
L0D53E2: db $EC;X
L0D53E3: db $79;X
L0D53E4: db $55;X
L0D53E5: db $EC;X
L0D53E6: db $1C;X
L0D53E7: db $55;X
L0D53E8: db $EC;X
L0D53E9: db $54;X
L0D53EA: db $55;X
L0D53EB: db $EC;X
L0D53EC: db $1C;X
L0D53ED: db $55;X
L0D53EE: db $EC;X
L0D53EF: db $79;X
L0D53F0: db $55;X
L0D53F1: db $E4;X
L0D53F2: db $A8;X
L0D53F3: db $A4;X
L0D53F4: db $07;X
L0D53F5: db $A2;X
L0D53F6: db $A0;X
L0D53F7: db $0E;X
L0D53F8: db $A5;X
L0D53F9: db $07;X
L0D53FA: db $A4;X
L0D53FB: db $A2;X
L0D53FC: db $0E;X
L0D53FD: db $A7;X
L0D53FE: db $07;X
L0D53FF: db $A5;X
L0D5400: db $A4;X
L0D5401: db $0E;X
L0D5402: db $A9;X
L0D5403: db $07;X
L0D5404: db $A7;X
L0D5405: db $A5;X
L0D5406: db $0E;X
L0D5407: db $AB;X
L0D5408: db $07;X
L0D5409: db $A9;X
L0D540A: db $A7;X
L0D540B: db $0E;X
L0D540C: db $AC;X
L0D540D: db $07;X
L0D540E: db $AB;X
L0D540F: db $A9;X
L0D5410: db $0E;X
L0D5411: db $AE;X
L0D5412: db $07;X
L0D5413: db $AC;X
L0D5414: db $AB;X
L0D5415: db $0E;X
L0D5416: db $B0;X
L0D5417: db $07;X
L0D5418: db $AE;X
L0D5419: db $AC;X
L0D541A: db $0E;X
L0D541B: db $B1;X
L0D541C: db $07;X
L0D541D: db $B0;X
L0D541E: db $AC;X
L0D541F: db $AA;X
L0D5420: db $AC;X
L0D5421: db $AA;X
L0D5422: db $A7;X
L0D5423: db $A5;X
L0D5424: db $AF;X
L0D5425: db $AC;X
L0D5426: db $AA;X
L0D5427: db $A7;X
L0D5428: db $AC;X
L0D5429: db $AA;X
L0D542A: db $A7;X
L0D542B: db $A5;X
L0D542C: db $A4;X
L0D542D: db $07;X
L0D542E: db $A7;X
L0D542F: db $80;X
L0D5430: db $03;X
L0D5431: db $E4;X
L0D5432: db $88;X
L0D5433: db $A7;X
L0D5434: db $04;X
L0D5435: db $E4;X
L0D5436: db $A8;X
L0D5437: db $A4;X
L0D5438: db $07;X
L0D5439: db $A6;X
L0D543A: db $80;X
L0D543B: db $03;X
L0D543C: db $E4;X
L0D543D: db $88;X
L0D543E: db $A6;X
L0D543F: db $04;X
L0D5440: db $E4;X
L0D5441: db $A8;X
L0D5442: db $A9;X
L0D5443: db $0E;X
L0D5444: db $A7;X
L0D5445: db $07;X
L0D5446: db $AB;X
L0D5447: db $80;X
L0D5448: db $03;X
L0D5449: db $E4;X
L0D544A: db $88;X
L0D544B: db $AB;X
L0D544C: db $04;X
L0D544D: db $E4;X
L0D544E: db $A8;X
L0D544F: db $A7;X
L0D5450: db $07;X
L0D5451: db $A9;X
L0D5452: db $80;X
L0D5453: db $03;X
L0D5454: db $E4;X
L0D5455: db $88;X
L0D5456: db $A9;X
L0D5457: db $04;X
L0D5458: db $E4;X
L0D5459: db $A8;X
L0D545A: db $AC;X
L0D545B: db $0E;X
L0D545C: db $E4;X
L0D545D: db $A8;X
L0D545E: db $A5;X
L0D545F: db $07;X
L0D5460: db $A9;X
L0D5461: db $AC;X
L0D5462: db $A9;X
L0D5463: db $AE;X
L0D5464: db $A7;X
L0D5465: db $AB;X
L0D5466: db $AC;X
L0D5467: db $1C;X
L0D5468: db $FA;X
L0D5469: db $07;X
L0D546A: db $E4;X
L0D546B: db $88;X
L0D546C: db $AC;X
L0D546D: db $07;X
L0D546E: db $80;X
L0D546F: db $03;X
L0D5470: db $E4;X
L0D5471: db $68;X
L0D5472: db $AC;X
L0D5473: db $04;X
L0D5474: db $E4;X
L0D5475: db $48;X
L0D5476: db $AC;X
L0D5477: db $07;X
L0D5478: db $80;X
L0D5479: db $03;X
L0D547A: db $E4;X
L0D547B: db $38;X
L0D547C: db $AC;X
L0D547D: db $04;X
L0D547E: db $E5;X
L0D547F: db $4C;X
L0D5480: db $53;X
L0D5481: db $E4;X
L0D5482: db $A8;X
L0D5483: db $A7;X
L0D5484: db $07;X
L0D5485: db $AA;X
L0D5486: db $0E;X
L0D5487: db $A7;X
L0D5488: db $07;X
L0D5489: db $AA;X
L0D548A: db $0E;X
L0D548B: db $A7;X
L0D548C: db $A5;X
L0D548D: db $A7;X
L0D548E: db $07;X
L0D548F: db $A4;X
L0D5490: db $15;X
L0D5491: db $E4;X
L0D5492: db $88;X
L0D5493: db $A4;X
L0D5494: db $07;X
L0D5495: db $80;X
L0D5496: db $03;X
L0D5497: db $E4;X
L0D5498: db $68;X
L0D5499: db $A4;X
L0D549A: db $04;X
L0D549B: db $E4;X
L0D549C: db $A8;X
L0D549D: db $A7;X
L0D549E: db $07;X
L0D549F: db $AA;X
L0D54A0: db $0E;X
L0D54A1: db $A7;X
L0D54A2: db $07;X
L0D54A3: db $AA;X
L0D54A4: db $0E;X
L0D54A5: db $A7;X
L0D54A6: db $A8;X
L0D54A7: db $AB;X
L0D54A8: db $07;X
L0D54A9: db $A8;X
L0D54AA: db $15;X
L0D54AB: db $E4;X
L0D54AC: db $88;X
L0D54AD: db $A8;X
L0D54AE: db $07;X
L0D54AF: db $80;X
L0D54B0: db $03;X
L0D54B1: db $E4;X
L0D54B2: db $68;X
L0D54B3: db $A8;X
L0D54B4: db $04;X
L0D54B5: db $E4;X
L0D54B6: db $A8;X
L0D54B7: db $A7;X
L0D54B8: db $07;X
L0D54B9: db $AA;X
L0D54BA: db $0E;X
L0D54BB: db $A7;X
L0D54BC: db $07;X
L0D54BD: db $AA;X
L0D54BE: db $0E;X
L0D54BF: db $A7;X
L0D54C0: db $A5;X
L0D54C1: db $A7;X
L0D54C2: db $07;X
L0D54C3: db $A4;X
L0D54C4: db $15;X
L0D54C5: db $E4;X
L0D54C6: db $88;X
L0D54C7: db $A4;X
L0D54C8: db $07;X
L0D54C9: db $80;X
L0D54CA: db $03;X
L0D54CB: db $E4;X
L0D54CC: db $68;X
L0D54CD: db $A4;X
L0D54CE: db $04;X
L0D54CF: db $E4;X
L0D54D0: db $A8;X
L0D54D1: db $A7;X
L0D54D2: db $07;X
L0D54D3: db $AA;X
L0D54D4: db $0E;X
L0D54D5: db $A7;X
L0D54D6: db $07;X
L0D54D7: db $AA;X
L0D54D8: db $0E;X
L0D54D9: db $A7;X
L0D54DA: db $AB;X
L0D54DB: db $07;X
L0D54DC: db $AC;X
L0D54DD: db $AB;X
L0D54DE: db $AC;X
L0D54DF: db $15;X
L0D54E0: db $E4;X
L0D54E1: db $88;X
L0D54E2: db $AC;X
L0D54E3: db $07;X
L0D54E4: db $80;X
L0D54E5: db $03;X
L0D54E6: db $E4;X
L0D54E7: db $68;X
L0D54E8: db $AC;X
L0D54E9: db $04;X
L0D54EA: db $E7;X
L0D54EB: db $00;X
L0D54EC: db $02;X
L0D54ED: db $81;X
L0D54EE: db $54;X
L0D54EF: db $ED;X
L0D54F0: db $E4;X
L0D54F1: db $A8;X
L0D54F2: db $B0;X
L0D54F3: db $07;X
L0D54F4: db $AC;X
L0D54F5: db $AF;X
L0D54F6: db $AB;X
L0D54F7: db $AE;X
L0D54F8: db $AA;X
L0D54F9: db $AD;X
L0D54FA: db $A9;X
L0D54FB: db $0E;X
L0D54FC: db $B1;X
L0D54FD: db $07;X
L0D54FE: db $B0;X
L0D54FF: db $AC;X
L0D5500: db $B1;X
L0D5501: db $B0;X
L0D5502: db $AC;X
L0D5503: db $A7;X
L0D5504: db $B0;X
L0D5505: db $07;X
L0D5506: db $AC;X
L0D5507: db $AF;X
L0D5508: db $AB;X
L0D5509: db $AE;X
L0D550A: db $AA;X
L0D550B: db $AD;X
L0D550C: db $A9;X
L0D550D: db $0E;X
L0D550E: db $B3;X
L0D550F: db $07;X
L0D5510: db $B1;X
L0D5511: db $AF;X
L0D5512: db $B1;X
L0D5513: db $B0;X
L0D5514: db $AC;X
L0D5515: db $AA;X
L0D5516: db $E7;X
L0D5517: db $00;X
L0D5518: db $02;X
L0D5519: db $F0;X
L0D551A: db $54;X
L0D551B: db $ED;X
L0D551C: db $E4;X
L0D551D: db $A8;X
L0D551E: db $A7;X
L0D551F: db $0E;X
L0D5520: db $AC;X
L0D5521: db $07;X
L0D5522: db $A9;X
L0D5523: db $80;X
L0D5524: db $03;X
L0D5525: db $E4;X
L0D5526: db $88;X
L0D5527: db $A9;X
L0D5528: db $04;X
L0D5529: db $E4;X
L0D552A: db $A8;X
L0D552B: db $A5;X
L0D552C: db $07;X
L0D552D: db $80;X
L0D552E: db $03;X
L0D552F: db $E4;X
L0D5530: db $88;X
L0D5531: db $A5;X
L0D5532: db $04;X
L0D5533: db $E4;X
L0D5534: db $A8;X
L0D5535: db $A7;X
L0D5536: db $07;X
L0D5537: db $80;X
L0D5538: db $03;X
L0D5539: db $E4;X
L0D553A: db $88;X
L0D553B: db $A7;X
L0D553C: db $04;X
L0D553D: db $E4;X
L0D553E: db $A8;X
L0D553F: db $A0;X
L0D5540: db $0C;X
L0D5541: db $80;X
L0D5542: db $02;X
L0D5543: db $A0;X
L0D5544: db $07;X
L0D5545: db $80;X
L0D5546: db $03;X
L0D5547: db $E4;X
L0D5548: db $88;X
L0D5549: db $A0;X
L0D554A: db $04;X
L0D554B: db $E4;X
L0D554C: db $A8;X
L0D554D: db $A0;X
L0D554E: db $0C;X
L0D554F: db $80;X
L0D5550: db $02;X
L0D5551: db $A0;X
L0D5552: db $07;X
L0D5553: db $ED;X
L0D5554: db $E4;X
L0D5555: db $A8;X
L0D5556: db $A5;X
L0D5557: db $0C;X
L0D5558: db $80;X
L0D5559: db $02;X
L0D555A: db $A5;X
L0D555B: db $07;X
L0D555C: db $A4;X
L0D555D: db $80;X
L0D555E: db $03;X
L0D555F: db $E4;X
L0D5560: db $88;X
L0D5561: db $A4;X
L0D5562: db $04;X
L0D5563: db $E4;X
L0D5564: db $A8;X
L0D5565: db $A4;X
L0D5566: db $07;X
L0D5567: db $A2;X
L0D5568: db $0E;X
L0D5569: db $A4;X
L0D556A: db $A9;X
L0D556B: db $07;X
L0D556C: db $A7;X
L0D556D: db $15;X
L0D556E: db $E4;X
L0D556F: db $88;X
L0D5570: db $A7;X
L0D5571: db $07;X
L0D5572: db $80;X
L0D5573: db $03;X
L0D5574: db $E4;X
L0D5575: db $68;X
L0D5576: db $A7;X
L0D5577: db $04;X
L0D5578: db $ED;X
L0D5579: db $E4;X
L0D557A: db $A8;X
L0D557B: db $A5;X
L0D557C: db $05;X
L0D557D: db $80;X
L0D557E: db $02;X
L0D557F: db $A5;X
L0D5580: db $07;X
L0D5581: db $A6;X
L0D5582: db $A7;X
L0D5583: db $A4;X
L0D5584: db $A0;X
L0D5585: db $A2;X
L0D5586: db $A0;X
L0D5587: db $1C;X
L0D5588: db $FA;X
L0D5589: db $07;X
L0D558A: db $E4;X
L0D558B: db $88;X
L0D558C: db $A0;X
L0D558D: db $07;X
L0D558E: db $80;X
L0D558F: db $03;X
L0D5590: db $E4;X
L0D5591: db $68;X
L0D5592: db $A0;X
L0D5593: db $04;X
L0D5594: db $E4;X
L0D5595: db $48;X
L0D5596: db $A0;X
L0D5597: db $07;X
L0D5598: db $80;X
L0D5599: db $03;X
L0D559A: db $E4;X
L0D559B: db $38;X
L0D559C: db $A0;X
L0D559D: db $04;X
L0D559E: db $ED;X
L0D559F: db $E4;X
L0D55A0: db $11;X
L0D55A1: db $E9;X
L0D55A2: db $22;X
L0D55A3: db $EE;X
L0D55A4: db $40;X
L0D55A5: db $F1;X
L0D55A6: db $03;X
L0D55A7: db $EC;X
L0D55A8: db $24;X
L0D55A9: db $59;X
L0D55AA: db $EC;X
L0D55AB: db $52;X
L0D55AC: db $59;X
L0D55AD: db $EC;X
L0D55AE: db $24;X
L0D55AF: db $59;X
L0D55B0: db $EC;X
L0D55B1: db $6B;X
L0D55B2: db $59;X
L0D55B3: db $E6;X
L0D55B4: db $0C;X
L0D55B5: db $EC;X
L0D55B6: db $24;X
L0D55B7: db $59;X
L0D55B8: db $EC;X
L0D55B9: db $52;X
L0D55BA: db $59;X
L0D55BB: db $EC;X
L0D55BC: db $24;X
L0D55BD: db $59;X
L0D55BE: db $EC;X
L0D55BF: db $6B;X
L0D55C0: db $59;X
L0D55C1: db $E6;X
L0D55C2: db $F4;X
L0D55C3: db $EC;X
L0D55C4: db $84;X
L0D55C5: db $59;X
L0D55C6: db $EC;X
L0D55C7: db $90;X
L0D55C8: db $59;X
L0D55C9: db $EC;X
L0D55CA: db $84;X
L0D55CB: db $59;X
L0D55CC: db $EC;X
L0D55CD: db $9B;X
L0D55CE: db $59;X
L0D55CF: db $EC;X
L0D55D0: db $84;X
L0D55D1: db $59;X
L0D55D2: db $EC;X
L0D55D3: db $90;X
L0D55D4: db $59;X
L0D55D5: db $EC;X
L0D55D6: db $84;X
L0D55D7: db $59;X
L0D55D8: db $EC;X
L0D55D9: db $A6;X
L0D55DA: db $59;X
L0D55DB: db $EC;X
L0D55DC: db $84;X
L0D55DD: db $59;X
L0D55DE: db $EC;X
L0D55DF: db $90;X
L0D55E0: db $59;X
L0D55E1: db $EC;X
L0D55E2: db $84;X
L0D55E3: db $59;X
L0D55E4: db $EC;X
L0D55E5: db $9B;X
L0D55E6: db $59;X
L0D55E7: db $EC;X
L0D55E8: db $84;X
L0D55E9: db $59;X
L0D55EA: db $EC;X
L0D55EB: db $90;X
L0D55EC: db $59;X
L0D55ED: db $EC;X
L0D55EE: db $84;X
L0D55EF: db $59;X
L0D55F0: db $EC;X
L0D55F1: db $A6;X
L0D55F2: db $59;X
L0D55F3: db $E4;X
L0D55F4: db $A8;X
L0D55F5: db $AB;X
L0D55F6: db $03;X
L0D55F7: db $E4;X
L0D55F8: db $88;X
L0D55F9: db $AE;X
L0D55FA: db $04;X
L0D55FB: db $E4;X
L0D55FC: db $A8;X
L0D55FD: db $A7;X
L0D55FE: db $03;X
L0D55FF: db $E4;X
L0D5600: db $88;X
L0D5601: db $AB;X
L0D5602: db $04;X
L0D5603: db $E4;X
L0D5604: db $A8;X
L0D5605: db $A2;X
L0D5606: db $03;X
L0D5607: db $E4;X
L0D5608: db $88;X
L0D5609: db $A7;X
L0D560A: db $04;X
L0D560B: db $E4;X
L0D560C: db $A8;X
L0D560D: db $AB;X
L0D560E: db $03;X
L0D560F: db $E4;X
L0D5610: db $88;X
L0D5611: db $A2;X
L0D5612: db $04;X
L0D5613: db $E4;X
L0D5614: db $A8;X
L0D5615: db $A7;X
L0D5616: db $03;X
L0D5617: db $E4;X
L0D5618: db $88;X
L0D5619: db $AB;X
L0D561A: db $04;X
L0D561B: db $E4;X
L0D561C: db $A8;X
L0D561D: db $A7;X
L0D561E: db $03;X
L0D561F: db $E4;X
L0D5620: db $88;X
L0D5621: db $A7;X
L0D5622: db $04;X
L0D5623: db $E4;X
L0D5624: db $A8;X
L0D5625: db $AB;X
L0D5626: db $03;X
L0D5627: db $E4;X
L0D5628: db $88;X
L0D5629: db $A7;X
L0D562A: db $04;X
L0D562B: db $E4;X
L0D562C: db $A8;X
L0D562D: db $A7;X
L0D562E: db $03;X
L0D562F: db $E4;X
L0D5630: db $88;X
L0D5631: db $AB;X
L0D5632: db $04;X
L0D5633: db $E4;X
L0D5634: db $A8;X
L0D5635: db $AA;X
L0D5636: db $03;X
L0D5637: db $E4;X
L0D5638: db $88;X
L0D5639: db $A7;X
L0D563A: db $04;X
L0D563B: db $E4;X
L0D563C: db $A8;X
L0D563D: db $A7;X
L0D563E: db $03;X
L0D563F: db $E4;X
L0D5640: db $88;X
L0D5641: db $AA;X
L0D5642: db $04;X
L0D5643: db $E4;X
L0D5644: db $A8;X
L0D5645: db $A4;X
L0D5646: db $03;X
L0D5647: db $E4;X
L0D5648: db $88;X
L0D5649: db $A7;X
L0D564A: db $04;X
L0D564B: db $E4;X
L0D564C: db $A8;X
L0D564D: db $AA;X
L0D564E: db $03;X
L0D564F: db $E4;X
L0D5650: db $88;X
L0D5651: db $A4;X
L0D5652: db $04;X
L0D5653: db $E4;X
L0D5654: db $A8;X
L0D5655: db $A7;X
L0D5656: db $03;X
L0D5657: db $E4;X
L0D5658: db $88;X
L0D5659: db $AA;X
L0D565A: db $04;X
L0D565B: db $E4;X
L0D565C: db $A8;X
L0D565D: db $A4;X
L0D565E: db $03;X
L0D565F: db $E4;X
L0D5660: db $88;X
L0D5661: db $A7;X
L0D5662: db $04;X
L0D5663: db $E4;X
L0D5664: db $A8;X
L0D5665: db $AA;X
L0D5666: db $03;X
L0D5667: db $E4;X
L0D5668: db $88;X
L0D5669: db $A4;X
L0D566A: db $04;X
L0D566B: db $E4;X
L0D566C: db $A8;X
L0D566D: db $A7;X
L0D566E: db $03;X
L0D566F: db $E4;X
L0D5670: db $88;X
L0D5671: db $AA;X
L0D5672: db $04;X
L0D5673: db $EC;X
L0D5674: db $C3;X
L0D5675: db $59;X
L0D5676: db $EC;X
L0D5677: db $F1;X
L0D5678: db $59;X
L0D5679: db $E4;X
L0D567A: db $A8;X
L0D567B: db $A7;X
L0D567C: db $03;X
L0D567D: db $E4;X
L0D567E: db $88;X
L0D567F: db $A5;X
L0D5680: db $04;X
L0D5681: db $E4;X
L0D5682: db $A8;X
L0D5683: db $A4;X
L0D5684: db $03;X
L0D5685: db $E4;X
L0D5686: db $88;X
L0D5687: db $A7;X
L0D5688: db $04;X
L0D5689: db $E4;X
L0D568A: db $A8;X
L0D568B: db $A0;X
L0D568C: db $03;X
L0D568D: db $E4;X
L0D568E: db $88;X
L0D568F: db $A4;X
L0D5690: db $04;X
L0D5691: db $E4;X
L0D5692: db $A8;X
L0D5693: db $A6;X
L0D5694: db $03;X
L0D5695: db $E4;X
L0D5696: db $88;X
L0D5697: db $A0;X
L0D5698: db $04;X
L0D5699: db $E4;X
L0D569A: db $A8;X
L0D569B: db $A2;X
L0D569C: db $03;X
L0D569D: db $E4;X
L0D569E: db $88;X
L0D569F: db $A6;X
L0D56A0: db $04;X
L0D56A1: db $E4;X
L0D56A2: db $A8;X
L0D56A3: db $9D;X
L0D56A4: db $03;X
L0D56A5: db $E4;X
L0D56A6: db $88;X
L0D56A7: db $A2;X
L0D56A8: db $04;X
L0D56A9: db $E4;X
L0D56AA: db $A8;X
L0D56AB: db $A7;X
L0D56AC: db $03;X
L0D56AD: db $E4;X
L0D56AE: db $88;X
L0D56AF: db $9D;X
L0D56B0: db $04;X
L0D56B1: db $E4;X
L0D56B2: db $A8;X
L0D56B3: db $A3;X
L0D56B4: db $03;X
L0D56B5: db $E4;X
L0D56B6: db $88;X
L0D56B7: db $A7;X
L0D56B8: db $04;X
L0D56B9: db $E4;X
L0D56BA: db $A8;X
L0D56BB: db $A0;X
L0D56BC: db $03;X
L0D56BD: db $E4;X
L0D56BE: db $88;X
L0D56BF: db $A3;X
L0D56C0: db $04;X
L0D56C1: db $E4;X
L0D56C2: db $A8;X
L0D56C3: db $A9;X
L0D56C4: db $03;X
L0D56C5: db $E4;X
L0D56C6: db $88;X
L0D56C7: db $A0;X
L0D56C8: db $04;X
L0D56C9: db $E4;X
L0D56CA: db $A8;X
L0D56CB: db $A5;X
L0D56CC: db $03;X
L0D56CD: db $E4;X
L0D56CE: db $88;X
L0D56CF: db $A9;X
L0D56D0: db $04;X
L0D56D1: db $E4;X
L0D56D2: db $A8;X
L0D56D3: db $A0;X
L0D56D4: db $03;X
L0D56D5: db $E4;X
L0D56D6: db $88;X
L0D56D7: db $A5;X
L0D56D8: db $04;X
L0D56D9: db $E4;X
L0D56DA: db $A8;X
L0D56DB: db $AB;X
L0D56DC: db $03;X
L0D56DD: db $E4;X
L0D56DE: db $88;X
L0D56DF: db $A0;X
L0D56E0: db $04;X
L0D56E1: db $E4;X
L0D56E2: db $A8;X
L0D56E3: db $A7;X
L0D56E4: db $03;X
L0D56E5: db $E4;X
L0D56E6: db $88;X
L0D56E7: db $AB;X
L0D56E8: db $04;X
L0D56E9: db $E4;X
L0D56EA: db $A8;X
L0D56EB: db $A2;X
L0D56EC: db $07;X
L0D56ED: db $80;X
L0D56EE: db $03;X
L0D56EF: db $E4;X
L0D56F0: db $88;X
L0D56F1: db $A2;X
L0D56F2: db $04;X
L0D56F3: db $EC;X
L0D56F4: db $1F;X
L0D56F5: db $5A;X
L0D56F6: db $EC;X
L0D56F7: db $5E;X
L0D56F8: db $5A;X
L0D56F9: db $EC;X
L0D56FA: db $1F;X
L0D56FB: db $5A;X
L0D56FC: db $EC;X
L0D56FD: db $85;X
L0D56FE: db $5A;X
L0D56FF: db $EC;X
L0D5700: db $1F;X
L0D5701: db $5A;X
L0D5702: db $EC;X
L0D5703: db $5E;X
L0D5704: db $5A;X
L0D5705: db $EC;X
L0D5706: db $1F;X
L0D5707: db $5A;X
L0D5708: db $EC;X
L0D5709: db $85;X
L0D570A: db $5A;X
L0D570B: db $EC;X
L0D570C: db $A5;X
L0D570D: db $5A;X
L0D570E: db $EC;X
L0D570F: db $22;X
L0D5710: db $5B;X
L0D5711: db $EC;X
L0D5712: db $A5;X
L0D5713: db $5A;X
L0D5714: db $EC;X
L0D5715: db $A3;X
L0D5716: db $5B;X
L0D5717: db $EC;X
L0D5718: db $A5;X
L0D5719: db $5A;X
L0D571A: db $EC;X
L0D571B: db $22;X
L0D571C: db $5B;X
L0D571D: db $EC;X
L0D571E: db $A5;X
L0D571F: db $5A;X
L0D5720: db $EC;X
L0D5721: db $A3;X
L0D5722: db $5B;X
L0D5723: db $E4;X
L0D5724: db $A8;X
L0D5725: db $A0;X
L0D5726: db $03;X
L0D5727: db $80;X
L0D5728: db $04;X
L0D5729: db $9F;X
L0D572A: db $03;X
L0D572B: db $E4;X
L0D572C: db $88;X
L0D572D: db $A0;X
L0D572E: db $04;X
L0D572F: db $E4;X
L0D5730: db $A8;X
L0D5731: db $9D;X
L0D5732: db $07;X
L0D5733: db $80;X
L0D5734: db $03;X
L0D5735: db $E4;X
L0D5736: db $88;X
L0D5737: db $9D;X
L0D5738: db $04;X
L0D5739: db $E4;X
L0D573A: db $A8;X
L0D573B: db $A2;X
L0D573C: db $03;X
L0D573D: db $80;X
L0D573E: db $04;X
L0D573F: db $A0;X
L0D5740: db $03;X
L0D5741: db $E4;X
L0D5742: db $88;X
L0D5743: db $A2;X
L0D5744: db $04;X
L0D5745: db $E4;X
L0D5746: db $A8;X
L0D5747: db $9F;X
L0D5748: db $07;X
L0D5749: db $80;X
L0D574A: db $03;X
L0D574B: db $E4;X
L0D574C: db $88;X
L0D574D: db $9F;X
L0D574E: db $04;X
L0D574F: db $E4;X
L0D5750: db $A8;X
L0D5751: db $A4;X
L0D5752: db $03;X
L0D5753: db $80;X
L0D5754: db $04;X
L0D5755: db $A2;X
L0D5756: db $03;X
L0D5757: db $E4;X
L0D5758: db $88;X
L0D5759: db $A4;X
L0D575A: db $04;X
L0D575B: db $E4;X
L0D575C: db $A8;X
L0D575D: db $A0;X
L0D575E: db $07;X
L0D575F: db $80;X
L0D5760: db $03;X
L0D5761: db $E4;X
L0D5762: db $88;X
L0D5763: db $A0;X
L0D5764: db $04;X
L0D5765: db $E4;X
L0D5766: db $A8;X
L0D5767: db $A5;X
L0D5768: db $03;X
L0D5769: db $80;X
L0D576A: db $04;X
L0D576B: db $A4;X
L0D576C: db $03;X
L0D576D: db $E4;X
L0D576E: db $88;X
L0D576F: db $A5;X
L0D5770: db $04;X
L0D5771: db $E4;X
L0D5772: db $A8;X
L0D5773: db $A2;X
L0D5774: db $07;X
L0D5775: db $80;X
L0D5776: db $03;X
L0D5777: db $E4;X
L0D5778: db $88;X
L0D5779: db $A2;X
L0D577A: db $04;X
L0D577B: db $E4;X
L0D577C: db $A8;X
L0D577D: db $A7;X
L0D577E: db $03;X
L0D577F: db $80;X
L0D5780: db $04;X
L0D5781: db $A5;X
L0D5782: db $03;X
L0D5783: db $E4;X
L0D5784: db $88;X
L0D5785: db $A7;X
L0D5786: db $04;X
L0D5787: db $E4;X
L0D5788: db $A8;X
L0D5789: db $A4;X
L0D578A: db $07;X
L0D578B: db $80;X
L0D578C: db $03;X
L0D578D: db $E4;X
L0D578E: db $88;X
L0D578F: db $A4;X
L0D5790: db $04;X
L0D5791: db $E4;X
L0D5792: db $A8;X
L0D5793: db $A9;X
L0D5794: db $03;X
L0D5795: db $80;X
L0D5796: db $04;X
L0D5797: db $A7;X
L0D5798: db $03;X
L0D5799: db $E4;X
L0D579A: db $88;X
L0D579B: db $A9;X
L0D579C: db $04;X
L0D579D: db $E4;X
L0D579E: db $A8;X
L0D579F: db $A5;X
L0D57A0: db $07;X
L0D57A1: db $80;X
L0D57A2: db $03;X
L0D57A3: db $E4;X
L0D57A4: db $88;X
L0D57A5: db $A5;X
L0D57A6: db $04;X
L0D57A7: db $E4;X
L0D57A8: db $A8;X
L0D57A9: db $AB;X
L0D57AA: db $03;X
L0D57AB: db $80;X
L0D57AC: db $04;X
L0D57AD: db $A9;X
L0D57AE: db $03;X
L0D57AF: db $E4;X
L0D57B0: db $88;X
L0D57B1: db $AB;X
L0D57B2: db $04;X
L0D57B3: db $E4;X
L0D57B4: db $A8;X
L0D57B5: db $A7;X
L0D57B6: db $07;X
L0D57B7: db $80;X
L0D57B8: db $03;X
L0D57B9: db $E4;X
L0D57BA: db $88;X
L0D57BB: db $A7;X
L0D57BC: db $04;X
L0D57BD: db $E4;X
L0D57BE: db $A8;X
L0D57BF: db $AC;X
L0D57C0: db $03;X
L0D57C1: db $80;X
L0D57C2: db $04;X
L0D57C3: db $AB;X
L0D57C4: db $03;X
L0D57C5: db $E4;X
L0D57C6: db $88;X
L0D57C7: db $AC;X
L0D57C8: db $04;X
L0D57C9: db $E4;X
L0D57CA: db $A8;X
L0D57CB: db $A9;X
L0D57CC: db $07;X
L0D57CD: db $80;X
L0D57CE: db $03;X
L0D57CF: db $E4;X
L0D57D0: db $88;X
L0D57D1: db $A9;X
L0D57D2: db $04;X
L0D57D3: db $E4;X
L0D57D4: db $A8;X
L0D57D5: db $AC;X
L0D57D6: db $07;X
L0D57D7: db $AA;X
L0D57D8: db $03;X
L0D57D9: db $E4;X
L0D57DA: db $88;X
L0D57DB: db $AC;X
L0D57DC: db $04;X
L0D57DD: db $E4;X
L0D57DE: db $A8;X
L0D57DF: db $A7;X
L0D57E0: db $03;X
L0D57E1: db $E4;X
L0D57E2: db $88;X
L0D57E3: db $AA;X
L0D57E4: db $04;X
L0D57E5: db $E4;X
L0D57E6: db $A8;X
L0D57E7: db $A5;X
L0D57E8: db $03;X
L0D57E9: db $E4;X
L0D57EA: db $88;X
L0D57EB: db $A7;X
L0D57EC: db $04;X
L0D57ED: db $E4;X
L0D57EE: db $A8;X
L0D57EF: db $A7;X
L0D57F0: db $03;X
L0D57F1: db $E4;X
L0D57F2: db $88;X
L0D57F3: db $A5;X
L0D57F4: db $04;X
L0D57F5: db $E4;X
L0D57F6: db $A8;X
L0D57F7: db $A6;X
L0D57F8: db $03;X
L0D57F9: db $E4;X
L0D57FA: db $88;X
L0D57FB: db $A7;X
L0D57FC: db $04;X
L0D57FD: db $E4;X
L0D57FE: db $A8;X
L0D57FF: db $A5;X
L0D5800: db $03;X
L0D5801: db $E4;X
L0D5802: db $88;X
L0D5803: db $A6;X
L0D5804: db $04;X
L0D5805: db $E4;X
L0D5806: db $A8;X
L0D5807: db $A0;X
L0D5808: db $03;X
L0D5809: db $E4;X
L0D580A: db $88;X
L0D580B: db $A5;X
L0D580C: db $04;X
L0D580D: db $E4;X
L0D580E: db $A8;X
L0D580F: db $A7;X
L0D5810: db $03;X
L0D5811: db $E4;X
L0D5812: db $88;X
L0D5813: db $A0;X
L0D5814: db $04;X
L0D5815: db $E4;X
L0D5816: db $A8;X
L0D5817: db $A7;X
L0D5818: db $03;X
L0D5819: db $E4;X
L0D581A: db $88;X
L0D581B: db $A7;X
L0D581C: db $04;X
L0D581D: db $E4;X
L0D581E: db $A8;X
L0D581F: db $A5;X
L0D5820: db $03;X
L0D5821: db $E4;X
L0D5822: db $88;X
L0D5823: db $A7;X
L0D5824: db $04;X
L0D5825: db $E4;X
L0D5826: db $A8;X
L0D5827: db $A3;X
L0D5828: db $03;X
L0D5829: db $E4;X
L0D582A: db $88;X
L0D582B: db $A5;X
L0D582C: db $04;X
L0D582D: db $E4;X
L0D582E: db $A8;X
L0D582F: db $A7;X
L0D5830: db $03;X
L0D5831: db $E4;X
L0D5832: db $88;X
L0D5833: db $A3;X
L0D5834: db $04;X
L0D5835: db $E4;X
L0D5836: db $A8;X
L0D5837: db $A7;X
L0D5838: db $03;X
L0D5839: db $E4;X
L0D583A: db $88;X
L0D583B: db $A7;X
L0D583C: db $04;X
L0D583D: db $E4;X
L0D583E: db $A8;X
L0D583F: db $A2;X
L0D5840: db $03;X
L0D5841: db $E4;X
L0D5842: db $88;X
L0D5843: db $A7;X
L0D5844: db $04;X
L0D5845: db $E4;X
L0D5846: db $A8;X
L0D5847: db $A0;X
L0D5848: db $03;X
L0D5849: db $E4;X
L0D584A: db $88;X
L0D584B: db $A2;X
L0D584C: db $04;X
L0D584D: db $E4;X
L0D584E: db $A8;X
L0D584F: db $A0;X
L0D5850: db $03;X
L0D5851: db $E4;X
L0D5852: db $88;X
L0D5853: db $A0;X
L0D5854: db $04;X
L0D5855: db $E4;X
L0D5856: db $A8;X
L0D5857: db $A4;X
L0D5858: db $03;X
L0D5859: db $E4;X
L0D585A: db $88;X
L0D585B: db $A0;X
L0D585C: db $04;X
L0D585D: db $E4;X
L0D585E: db $A8;X
L0D585F: db $A3;X
L0D5860: db $03;X
L0D5861: db $E4;X
L0D5862: db $88;X
L0D5863: db $A4;X
L0D5864: db $04;X
L0D5865: db $E4;X
L0D5866: db $A8;X
L0D5867: db $A0;X
L0D5868: db $03;X
L0D5869: db $E4;X
L0D586A: db $88;X
L0D586B: db $A3;X
L0D586C: db $04;X
L0D586D: db $E4;X
L0D586E: db $A8;X
L0D586F: db $A2;X
L0D5870: db $07;X
L0D5871: db $E4;X
L0D5872: db $68;X
L0D5873: db $A3;X
L0D5874: db $03;X
L0D5875: db $E4;X
L0D5876: db $88;X
L0D5877: db $A2;X
L0D5878: db $04;X
L0D5879: db $E4;X
L0D587A: db $A8;X
L0D587B: db $A6;X
L0D587C: db $07;X
L0D587D: db $80;X
L0D587E: db $03;X
L0D587F: db $E4;X
L0D5880: db $88;X
L0D5881: db $A6;X
L0D5882: db $04;X
L0D5883: db $E4;X
L0D5884: db $A8;X
L0D5885: db $A4;X
L0D5886: db $03;X
L0D5887: db $80;X
L0D5888: db $04;X
L0D5889: db $A7;X
L0D588A: db $03;X
L0D588B: db $E4;X
L0D588C: db $88;X
L0D588D: db $A4;X
L0D588E: db $04;X
L0D588F: db $E4;X
L0D5890: db $A8;X
L0D5891: db $A6;X
L0D5892: db $03;X
L0D5893: db $E4;X
L0D5894: db $88;X
L0D5895: db $A7;X
L0D5896: db $04;X
L0D5897: db $E4;X
L0D5898: db $A8;X
L0D5899: db $A4;X
L0D589A: db $03;X
L0D589B: db $E4;X
L0D589C: db $88;X
L0D589D: db $A6;X
L0D589E: db $04;X
L0D589F: db $E4;X
L0D58A0: db $A8;X
L0D58A1: db $A5;X
L0D58A2: db $07;X
L0D58A3: db $E4;X
L0D58A4: db $68;X
L0D58A5: db $A6;X
L0D58A6: db $03;X
L0D58A7: db $E4;X
L0D58A8: db $88;X
L0D58A9: db $A5;X
L0D58AA: db $04;X
L0D58AB: db $E4;X
L0D58AC: db $A8;X
L0D58AD: db $A9;X
L0D58AE: db $07;X
L0D58AF: db $80;X
L0D58B0: db $03;X
L0D58B1: db $E4;X
L0D58B2: db $88;X
L0D58B3: db $A9;X
L0D58B4: db $04;X
L0D58B5: db $E4;X
L0D58B6: db $A8;X
L0D58B7: db $A0;X
L0D58B8: db $07;X
L0D58B9: db $A5;X
L0D58BA: db $03;X
L0D58BB: db $E4;X
L0D58BC: db $88;X
L0D58BD: db $A0;X
L0D58BE: db $04;X
L0D58BF: db $E4;X
L0D58C0: db $A8;X
L0D58C1: db $A9;X
L0D58C2: db $03;X
L0D58C3: db $E4;X
L0D58C4: db $88;X
L0D58C5: db $A5;X
L0D58C6: db $04;X
L0D58C7: db $E4;X
L0D58C8: db $A8;X
L0D58C9: db $A5;X
L0D58CA: db $03;X
L0D58CB: db $E4;X
L0D58CC: db $88;X
L0D58CD: db $A9;X
L0D58CE: db $04;X
L0D58CF: db $E4;X
L0D58D0: db $A8;X
L0D58D1: db $AB;X
L0D58D2: db $03;X
L0D58D3: db $E4;X
L0D58D4: db $88;X
L0D58D5: db $A5;X
L0D58D6: db $04;X
L0D58D7: db $E4;X
L0D58D8: db $A8;X
L0D58D9: db $A4;X
L0D58DA: db $03;X
L0D58DB: db $E4;X
L0D58DC: db $88;X
L0D58DD: db $AB;X
L0D58DE: db $04;X
L0D58DF: db $E4;X
L0D58E0: db $A8;X
L0D58E1: db $A7;X
L0D58E2: db $03;X
L0D58E3: db $E4;X
L0D58E4: db $88;X
L0D58E5: db $A4;X
L0D58E6: db $04;X
L0D58E7: db $E4;X
L0D58E8: db $A8;X
L0D58E9: db $A4;X
L0D58EA: db $0B;X
L0D58EB: db $E4;X
L0D58EC: db $88;X
L0D58ED: db $A4;X
L0D58EE: db $03;X
L0D58EF: db $E4;X
L0D58F0: db $A8;X
L0D58F1: db $A2;X
L0D58F2: db $07;X
L0D58F3: db $A0;X
L0D58F4: db $03;X
L0D58F5: db $E4;X
L0D58F6: db $88;X
L0D58F7: db $A2;X
L0D58F8: db $04;X
L0D58F9: db $E4;X
L0D58FA: db $A8;X
L0D58FB: db $9F;X
L0D58FC: db $03;X
L0D58FD: db $E4;X
L0D58FE: db $88;X
L0D58FF: db $A0;X
L0D5900: db $04;X
L0D5901: db $E4;X
L0D5902: db $A8;X
L0D5903: db $9D;X
L0D5904: db $03;X
L0D5905: db $E4;X
L0D5906: db $88;X
L0D5907: db $9F;X
L0D5908: db $04;X
L0D5909: db $E4;X
L0D590A: db $A8;X
L0D590B: db $9B;X
L0D590C: db $03;X
L0D590D: db $E4;X
L0D590E: db $88;X
L0D590F: db $9D;X
L0D5910: db $04;X
L0D5911: db $E4;X
L0D5912: db $A8;X
L0D5913: db $99;X
L0D5914: db $03;X
L0D5915: db $E4;X
L0D5916: db $88;X
L0D5917: db $9B;X
L0D5918: db $04;X
L0D5919: db $E4;X
L0D591A: db $A8;X
L0D591B: db $96;X
L0D591C: db $03;X
L0D591D: db $E4;X
L0D591E: db $88;X
L0D591F: db $99;X
L0D5920: db $04;X
L0D5921: db $E5;X
L0D5922: db $9F;X
L0D5923: db $55;X
L0D5924: db $E4;X
L0D5925: db $A8;X
L0D5926: db $98;X
L0D5927: db $05;X
L0D5928: db $80;X
L0D5929: db $02;X
L0D592A: db $99;X
L0D592B: db $03;X
L0D592C: db $E4;X
L0D592D: db $88;X
L0D592E: db $98;X
L0D592F: db $04;X
L0D5930: db $E4;X
L0D5931: db $A8;X
L0D5932: db $9A;X
L0D5933: db $03;X
L0D5934: db $E4;X
L0D5935: db $88;X
L0D5936: db $99;X
L0D5937: db $04;X
L0D5938: db $E4;X
L0D5939: db $A8;X
L0D593A: db $9B;X
L0D593B: db $03;X
L0D593C: db $E4;X
L0D593D: db $88;X
L0D593E: db $9A;X
L0D593F: db $04;X
L0D5940: db $E4;X
L0D5941: db $A8;X
L0D5942: db $9B;X
L0D5943: db $05;X
L0D5944: db $80;X
L0D5945: db $02;X
L0D5946: db $80;X
L0D5947: db $03;X
L0D5948: db $E4;X
L0D5949: db $88;X
L0D594A: db $9B;X
L0D594B: db $04;X
L0D594C: db $E7;X
L0D594D: db $00;X
L0D594E: db $02;X
L0D594F: db $24;X
L0D5950: db $59;X
L0D5951: db $ED;X
L0D5952: db $E4;X
L0D5953: db $A8;X
L0D5954: db $97;X
L0D5955: db $05;X
L0D5956: db $80;X
L0D5957: db $02;X
L0D5958: db $96;X
L0D5959: db $03;X
L0D595A: db $E4;X
L0D595B: db $88;X
L0D595C: db $97;X
L0D595D: db $04;X
L0D595E: db $E4;X
L0D595F: db $A8;X
L0D5960: db $94;X
L0D5961: db $05;X
L0D5962: db $80;X
L0D5963: db $02;X
L0D5964: db $80;X
L0D5965: db $03;X
L0D5966: db $E4;X
L0D5967: db $88;X
L0D5968: db $94;X
L0D5969: db $04;X
L0D596A: db $ED;X
L0D596B: db $E4;X
L0D596C: db $A3;X
L0D596D: db $94;X
L0D596E: db $05;X
L0D596F: db $80;X
L0D5970: db $02;X
L0D5971: db $96;X
L0D5972: db $03;X
L0D5973: db $E4;X
L0D5974: db $88;X
L0D5975: db $94;X
L0D5976: db $04;X
L0D5977: db $E4;X
L0D5978: db $A3;X
L0D5979: db $96;X
L0D597A: db $05;X
L0D597B: db $80;X
L0D597C: db $02;X
L0D597D: db $80;X
L0D597E: db $03;X
L0D597F: db $E4;X
L0D5980: db $88;X
L0D5981: db $96;X
L0D5982: db $04;X
L0D5983: db $ED;X
L0D5984: db $E4;X
L0D5985: db $A2;X
L0D5986: db $A4;X
L0D5987: db $07;X
L0D5988: db $A7;X
L0D5989: db $A0;X
L0D598A: db $A4;X
L0D598B: db $A7;X
L0D598C: db $A0;X
L0D598D: db $A4;X
L0D598E: db $A0;X
L0D598F: db $ED;X
L0D5990: db $A2;X
L0D5991: db $07;X
L0D5992: db $9E;X
L0D5993: db $A4;X
L0D5994: db $A0;X
L0D5995: db $0E;X
L0D5996: db $9F;X
L0D5997: db $07;X
L0D5998: db $9D;X
L0D5999: db $9B;X
L0D599A: db $ED;X
L0D599B: db $A5;X
L0D599C: db $07;X
L0D599D: db $A0;X
L0D599E: db $A8;X
L0D599F: db $A5;X
L0D59A0: db $0E;X
L0D59A1: db $A2;X
L0D59A2: db $07;X
L0D59A3: db $9F;X
L0D59A4: db $9C;X
L0D59A5: db $ED;X
L0D59A6: db $A6;X
L0D59A7: db $07;X
L0D59A8: db $A7;X
L0D59A9: db $A6;X
L0D59AA: db $A7;X
L0D59AB: db $0E;X
L0D59AC: db $EE;X
L0D59AD: db $80;X
L0D59AE: db $E4;X
L0D59AF: db $A2;X
L0D59B0: db $B0;X
L0D59B1: db $07;X
L0D59B2: db $AF;X
L0D59B3: db $03;X
L0D59B4: db $E4;X
L0D59B5: db $82;X
L0D59B6: db $B0;X
L0D59B7: db $04;X
L0D59B8: db $E4;X
L0D59B9: db $A2;X
L0D59BA: db $AE;X
L0D59BB: db $03;X
L0D59BC: db $E4;X
L0D59BD: db $82;X
L0D59BE: db $AF;X
L0D59BF: db $04;X
L0D59C0: db $EE;X
L0D59C1: db $40;X
L0D59C2: db $ED;X
L0D59C3: db $E4;X
L0D59C4: db $A8;X
L0D59C5: db $A9;X
L0D59C6: db $03;X
L0D59C7: db $E4;X
L0D59C8: db $88;X
L0D59C9: db $A0;X
L0D59CA: db $04;X
L0D59CB: db $E4;X
L0D59CC: db $A8;X
L0D59CD: db $A5;X
L0D59CE: db $03;X
L0D59CF: db $E4;X
L0D59D0: db $88;X
L0D59D1: db $A9;X
L0D59D2: db $04;X
L0D59D3: db $E4;X
L0D59D4: db $A8;X
L0D59D5: db $A0;X
L0D59D6: db $03;X
L0D59D7: db $E4;X
L0D59D8: db $88;X
L0D59D9: db $A5;X
L0D59DA: db $04;X
L0D59DB: db $E7;X
L0D59DC: db $00;X
L0D59DD: db $02;X
L0D59DE: db $C3;X
L0D59DF: db $59;X
L0D59E0: db $E4;X
L0D59E1: db $A8;X
L0D59E2: db $A9;X
L0D59E3: db $03;X
L0D59E4: db $E4;X
L0D59E5: db $88;X
L0D59E6: db $A0;X
L0D59E7: db $04;X
L0D59E8: db $E4;X
L0D59E9: db $A8;X
L0D59EA: db $A5;X
L0D59EB: db $03;X
L0D59EC: db $E4;X
L0D59ED: db $88;X
L0D59EE: db $A9;X
L0D59EF: db $04;X
L0D59F0: db $ED;X
L0D59F1: db $E4;X
L0D59F2: db $A8;X
L0D59F3: db $A7;X
L0D59F4: db $03;X
L0D59F5: db $E4;X
L0D59F6: db $88;X
L0D59F7: db $A0;X
L0D59F8: db $04;X
L0D59F9: db $E4;X
L0D59FA: db $A8;X
L0D59FB: db $A5;X
L0D59FC: db $03;X
L0D59FD: db $E4;X
L0D59FE: db $88;X
L0D59FF: db $A8;X
L0D5A00: db $04;X
L0D5A01: db $E4;X
L0D5A02: db $A8;X
L0D5A03: db $A0;X
L0D5A04: db $03;X
L0D5A05: db $E4;X
L0D5A06: db $88;X
L0D5A07: db $A5;X
L0D5A08: db $04;X
L0D5A09: db $E7;X
L0D5A0A: db $00;X
L0D5A0B: db $02;X
L0D5A0C: db $F1;X
L0D5A0D: db $59;X
L0D5A0E: db $E4;X
L0D5A0F: db $A8;X
L0D5A10: db $A7;X
L0D5A11: db $03;X
L0D5A12: db $E4;X
L0D5A13: db $88;X
L0D5A14: db $A0;X
L0D5A15: db $04;X
L0D5A16: db $E4;X
L0D5A17: db $A8;X
L0D5A18: db $A5;X
L0D5A19: db $03;X
L0D5A1A: db $E4;X
L0D5A1B: db $88;X
L0D5A1C: db $A8;X
L0D5A1D: db $04;X
L0D5A1E: db $ED;X
L0D5A1F: db $E4;X
L0D5A20: db $A8;X
L0D5A21: db $AC;X
L0D5A22: db $03;X
L0D5A23: db $80;X
L0D5A24: db $04;X
L0D5A25: db $A9;X
L0D5A26: db $03;X
L0D5A27: db $E4;X
L0D5A28: db $88;X
L0D5A29: db $AC;X
L0D5A2A: db $04;X
L0D5A2B: db $E4;X
L0D5A2C: db $A8;X
L0D5A2D: db $AB;X
L0D5A2E: db $03;X
L0D5A2F: db $E4;X
L0D5A30: db $88;X
L0D5A31: db $A9;X
L0D5A32: db $04;X
L0D5A33: db $E4;X
L0D5A34: db $A8;X
L0D5A35: db $A8;X
L0D5A36: db $03;X
L0D5A37: db $E4;X
L0D5A38: db $88;X
L0D5A39: db $AB;X
L0D5A3A: db $04;X
L0D5A3B: db $E4;X
L0D5A3C: db $A8;X
L0D5A3D: db $AA;X
L0D5A3E: db $03;X
L0D5A3F: db $E4;X
L0D5A40: db $88;X
L0D5A41: db $A8;X
L0D5A42: db $04;X
L0D5A43: db $E4;X
L0D5A44: db $A8;X
L0D5A45: db $A7;X
L0D5A46: db $03;X
L0D5A47: db $E4;X
L0D5A48: db $88;X
L0D5A49: db $AA;X
L0D5A4A: db $04;X
L0D5A4B: db $E4;X
L0D5A4C: db $A8;X
L0D5A4D: db $A9;X
L0D5A4E: db $03;X
L0D5A4F: db $E4;X
L0D5A50: db $88;X
L0D5A51: db $A7;X
L0D5A52: db $04;X
L0D5A53: db $E4;X
L0D5A54: db $A8;X
L0D5A55: db $A6;X
L0D5A56: db $07;X
L0D5A57: db $80;X
L0D5A58: db $03;X
L0D5A59: db $E4;X
L0D5A5A: db $88;X
L0D5A5B: db $A6;X
L0D5A5C: db $04;X
L0D5A5D: db $ED;X
L0D5A5E: db $E4;X
L0D5A5F: db $A8;X
L0D5A60: db $AC;X
L0D5A61: db $05;X
L0D5A62: db $80;X
L0D5A63: db $02;X
L0D5A64: db $AC;X
L0D5A65: db $03;X
L0D5A66: db $E4;X
L0D5A67: db $88;X
L0D5A68: db $AC;X
L0D5A69: db $04;X
L0D5A6A: db $E4;X
L0D5A6B: db $A8;X
L0D5A6C: db $A7;X
L0D5A6D: db $03;X
L0D5A6E: db $E4;X
L0D5A6F: db $88;X
L0D5A70: db $AC;X
L0D5A71: db $04;X
L0D5A72: db $E4;X
L0D5A73: db $A8;X
L0D5A74: db $AC;X
L0D5A75: db $03;X
L0D5A76: db $E4;X
L0D5A77: db $88;X
L0D5A78: db $A7;X
L0D5A79: db $04;X
L0D5A7A: db $EE;X
L0D5A7B: db $80;X
L0D5A7C: db $E4;X
L0D5A7D: db $A2;X
L0D5A7E: db $AA;X
L0D5A7F: db $07;X
L0D5A80: db $AB;X
L0D5A81: db $AC;X
L0D5A82: db $EE;X
L0D5A83: db $40;X
L0D5A84: db $ED;X
L0D5A85: db $E4;X
L0D5A86: db $A8;X
L0D5A87: db $AF;X
L0D5A88: db $05;X
L0D5A89: db $80;X
L0D5A8A: db $02;X
L0D5A8B: db $AE;X
L0D5A8C: db $03;X
L0D5A8D: db $E4;X
L0D5A8E: db $88;X
L0D5A8F: db $AF;X
L0D5A90: db $04;X
L0D5A91: db $E4;X
L0D5A92: db $A8;X
L0D5A93: db $AC;X
L0D5A94: db $03;X
L0D5A95: db $E4;X
L0D5A96: db $88;X
L0D5A97: db $AE;X
L0D5A98: db $04;X
L0D5A99: db $EE;X
L0D5A9A: db $80;X
L0D5A9B: db $E4;X
L0D5A9C: db $A2;X
L0D5A9D: db $A2;X
L0D5A9E: db $07;X
L0D5A9F: db $A7;X
L0D5AA0: db $AA;X
L0D5AA1: db $AF;X
L0D5AA2: db $EE;X
L0D5AA3: db $40;X
L0D5AA4: db $ED;X
L0D5AA5: db $E4;X
L0D5AA6: db $A8;X
L0D5AA7: db $A4;X
L0D5AA8: db $03;X
L0D5AA9: db $80;X
L0D5AAA: db $04;X
L0D5AAB: db $A0;X
L0D5AAC: db $03;X
L0D5AAD: db $E4;X
L0D5AAE: db $88;X
L0D5AAF: db $A4;X
L0D5AB0: db $04;X
L0D5AB1: db $E4;X
L0D5AB2: db $A8;X
L0D5AB3: db $9B;X
L0D5AB4: db $03;X
L0D5AB5: db $E4;X
L0D5AB6: db $88;X
L0D5AB7: db $A0;X
L0D5AB8: db $04;X
L0D5AB9: db $E4;X
L0D5ABA: db $A8;X
L0D5ABB: db $A5;X
L0D5ABC: db $03;X
L0D5ABD: db $E4;X
L0D5ABE: db $88;X
L0D5ABF: db $9B;X
L0D5AC0: db $04;X
L0D5AC1: db $E4;X
L0D5AC2: db $A8;X
L0D5AC3: db $A0;X
L0D5AC4: db $03;X
L0D5AC5: db $E4;X
L0D5AC6: db $88;X
L0D5AC7: db $A5;X
L0D5AC8: db $04;X
L0D5AC9: db $E4;X
L0D5ACA: db $A8;X
L0D5ACB: db $9D;X
L0D5ACC: db $03;X
L0D5ACD: db $E4;X
L0D5ACE: db $88;X
L0D5ACF: db $A0;X
L0D5AD0: db $04;X
L0D5AD1: db $E4;X
L0D5AD2: db $A8;X
L0D5AD3: db $A2;X
L0D5AD4: db $03;X
L0D5AD5: db $E4;X
L0D5AD6: db $88;X
L0D5AD7: db $9D;X
L0D5AD8: db $04;X
L0D5AD9: db $E4;X
L0D5ADA: db $A8;X
L0D5ADB: db $A4;X
L0D5ADC: db $03;X
L0D5ADD: db $E4;X
L0D5ADE: db $88;X
L0D5ADF: db $A2;X
L0D5AE0: db $04;X
L0D5AE1: db $E4;X
L0D5AE2: db $A8;X
L0D5AE3: db $A0;X
L0D5AE4: db $03;X
L0D5AE5: db $E4;X
L0D5AE6: db $88;X
L0D5AE7: db $A4;X
L0D5AE8: db $04;X
L0D5AE9: db $E4;X
L0D5AEA: db $A8;X
L0D5AEB: db $9B;X
L0D5AEC: db $03;X
L0D5AED: db $E4;X
L0D5AEE: db $88;X
L0D5AEF: db $A0;X
L0D5AF0: db $04;X
L0D5AF1: db $E4;X
L0D5AF2: db $A8;X
L0D5AF3: db $98;X
L0D5AF4: db $03;X
L0D5AF5: db $E4;X
L0D5AF6: db $88;X
L0D5AF7: db $9B;X
L0D5AF8: db $04;X
L0D5AF9: db $E4;X
L0D5AFA: db $A8;X
L0D5AFB: db $94;X
L0D5AFC: db $03;X
L0D5AFD: db $E4;X
L0D5AFE: db $88;X
L0D5AFF: db $98;X
L0D5B00: db $04;X
L0D5B01: db $E4;X
L0D5B02: db $A8;X
L0D5B03: db $8F;X
L0D5B04: db $03;X
L0D5B05: db $E4;X
L0D5B06: db $88;X
L0D5B07: db $94;X
L0D5B08: db $04;X
L0D5B09: db $E4;X
L0D5B0A: db $A8;X
L0D5B0B: db $98;X
L0D5B0C: db $03;X
L0D5B0D: db $E4;X
L0D5B0E: db $88;X
L0D5B0F: db $8F;X
L0D5B10: db $04;X
L0D5B11: db $E4;X
L0D5B12: db $A8;X
L0D5B13: db $9B;X
L0D5B14: db $03;X
L0D5B15: db $E4;X
L0D5B16: db $88;X
L0D5B17: db $98;X
L0D5B18: db $04;X
L0D5B19: db $E4;X
L0D5B1A: db $A8;X
L0D5B1B: db $A0;X
L0D5B1C: db $03;X
L0D5B1D: db $E4;X
L0D5B1E: db $88;X
L0D5B1F: db $9B;X
L0D5B20: db $04;X
L0D5B21: db $ED;X
L0D5B22: db $E4;X
L0D5B23: db $A8;X
L0D5B24: db $A2;X
L0D5B25: db $03;X
L0D5B26: db $E4;X
L0D5B27: db $88;X
L0D5B28: db $A0;X
L0D5B29: db $04;X
L0D5B2A: db $E4;X
L0D5B2B: db $A8;X
L0D5B2C: db $A0;X
L0D5B2D: db $03;X
L0D5B2E: db $E4;X
L0D5B2F: db $88;X
L0D5B30: db $A2;X
L0D5B31: db $04;X
L0D5B32: db $E4;X
L0D5B33: db $A8;X
L0D5B34: db $9D;X
L0D5B35: db $03;X
L0D5B36: db $E4;X
L0D5B37: db $88;X
L0D5B38: db $A0;X
L0D5B39: db $04;X
L0D5B3A: db $E4;X
L0D5B3B: db $A8;X
L0D5B3C: db $A0;X
L0D5B3D: db $03;X
L0D5B3E: db $E4;X
L0D5B3F: db $88;X
L0D5B40: db $9D;X
L0D5B41: db $04;X
L0D5B42: db $E4;X
L0D5B43: db $A8;X
L0D5B44: db $9D;X
L0D5B45: db $03;X
L0D5B46: db $E4;X
L0D5B47: db $88;X
L0D5B48: db $A0;X
L0D5B49: db $04;X
L0D5B4A: db $E4;X
L0D5B4B: db $A8;X
L0D5B4C: db $99;X
L0D5B4D: db $03;X
L0D5B4E: db $E4;X
L0D5B4F: db $88;X
L0D5B50: db $9D;X
L0D5B51: db $04;X
L0D5B52: db $E4;X
L0D5B53: db $A8;X
L0D5B54: db $9F;X
L0D5B55: db $03;X
L0D5B56: db $E4;X
L0D5B57: db $88;X
L0D5B58: db $99;X
L0D5B59: db $04;X
L0D5B5A: db $E4;X
L0D5B5B: db $A8;X
L0D5B5C: db $9B;X
L0D5B5D: db $03;X
L0D5B5E: db $E4;X
L0D5B5F: db $88;X
L0D5B60: db $9F;X
L0D5B61: db $04;X
L0D5B62: db $E4;X
L0D5B63: db $A8;X
L0D5B64: db $A0;X
L0D5B65: db $03;X
L0D5B66: db $E4;X
L0D5B67: db $88;X
L0D5B68: db $9B;X
L0D5B69: db $04;X
L0D5B6A: db $E4;X
L0D5B6B: db $A8;X
L0D5B6C: db $9B;X
L0D5B6D: db $03;X
L0D5B6E: db $E4;X
L0D5B6F: db $88;X
L0D5B70: db $A0;X
L0D5B71: db $04;X
L0D5B72: db $E4;X
L0D5B73: db $A8;X
L0D5B74: db $A5;X
L0D5B75: db $03;X
L0D5B76: db $E4;X
L0D5B77: db $88;X
L0D5B78: db $9B;X
L0D5B79: db $04;X
L0D5B7A: db $E4;X
L0D5B7B: db $A8;X
L0D5B7C: db $A2;X
L0D5B7D: db $03;X
L0D5B7E: db $E4;X
L0D5B7F: db $88;X
L0D5B80: db $A5;X
L0D5B81: db $04;X
L0D5B82: db $E4;X
L0D5B83: db $A8;X
L0D5B84: db $9F;X
L0D5B85: db $03;X
L0D5B86: db $E4;X
L0D5B87: db $88;X
L0D5B88: db $A2;X
L0D5B89: db $04;X
L0D5B8A: db $E4;X
L0D5B8B: db $A8;X
L0D5B8C: db $9B;X
L0D5B8D: db $03;X
L0D5B8E: db $E4;X
L0D5B8F: db $88;X
L0D5B90: db $9F;X
L0D5B91: db $04;X
L0D5B92: db $E4;X
L0D5B93: db $A8;X
L0D5B94: db $99;X
L0D5B95: db $03;X
L0D5B96: db $E4;X
L0D5B97: db $88;X
L0D5B98: db $9B;X
L0D5B99: db $04;X
L0D5B9A: db $E4;X
L0D5B9B: db $A8;X
L0D5B9C: db $96;X
L0D5B9D: db $03;X
L0D5B9E: db $E4;X
L0D5B9F: db $88;X
L0D5BA0: db $99;X
L0D5BA1: db $04;X
L0D5BA2: db $ED;X
L0D5BA3: db $E4;X
L0D5BA4: db $A8;X
L0D5BA5: db $A0;X
L0D5BA6: db $03;X
L0D5BA7: db $E4;X
L0D5BA8: db $88;X
L0D5BA9: db $98;X
L0D5BAA: db $04;X
L0D5BAB: db $E4;X
L0D5BAC: db $A8;X
L0D5BAD: db $A2;X
L0D5BAE: db $03;X
L0D5BAF: db $E4;X
L0D5BB0: db $88;X
L0D5BB1: db $A0;X
L0D5BB2: db $04;X
L0D5BB3: db $E4;X
L0D5BB4: db $A8;X
L0D5BB5: db $A3;X
L0D5BB6: db $03;X
L0D5BB7: db $E4;X
L0D5BB8: db $88;X
L0D5BB9: db $A2;X
L0D5BBA: db $04;X
L0D5BBB: db $E4;X
L0D5BBC: db $A8;X
L0D5BBD: db $A4;X
L0D5BBE: db $03;X
L0D5BBF: db $E4;X
L0D5BC0: db $88;X
L0D5BC1: db $A3;X
L0D5BC2: db $04;X
L0D5BC3: db $E4;X
L0D5BC4: db $A8;X
L0D5BC5: db $9E;X
L0D5BC6: db $03;X
L0D5BC7: db $E4;X
L0D5BC8: db $88;X
L0D5BC9: db $A4;X
L0D5BCA: db $04;X
L0D5BCB: db $E4;X
L0D5BCC: db $A8;X
L0D5BCD: db $9B;X
L0D5BCE: db $03;X
L0D5BCF: db $E4;X
L0D5BD0: db $88;X
L0D5BD1: db $9E;X
L0D5BD2: db $04;X
L0D5BD3: db $E4;X
L0D5BD4: db $A8;X
L0D5BD5: db $9D;X
L0D5BD6: db $03;X
L0D5BD7: db $E4;X
L0D5BD8: db $88;X
L0D5BD9: db $9B;X
L0D5BDA: db $04;X
L0D5BDB: db $E4;X
L0D5BDC: db $A8;X
L0D5BDD: db $9B;X
L0D5BDE: db $0E;X
L0D5BDF: db $94;X
L0D5BE0: db $07;X
L0D5BE1: db $98;X
L0D5BE2: db $03;X
L0D5BE3: db $E4;X
L0D5BE4: db $88;X
L0D5BE5: db $94;X
L0D5BE6: db $04;X
L0D5BE7: db $E4;X
L0D5BE8: db $A8;X
L0D5BE9: db $99;X
L0D5BEA: db $03;X
L0D5BEB: db $E4;X
L0D5BEC: db $88;X
L0D5BED: db $98;X
L0D5BEE: db $04;X
L0D5BEF: db $E4;X
L0D5BF0: db $A8;X
L0D5BF1: db $9B;X
L0D5BF2: db $03;X
L0D5BF3: db $E4;X
L0D5BF4: db $88;X
L0D5BF5: db $99;X
L0D5BF6: db $04;X
L0D5BF7: db $E4;X
L0D5BF8: db $A8;X
L0D5BF9: db $9D;X
L0D5BFA: db $03;X
L0D5BFB: db $E4;X
L0D5BFC: db $88;X
L0D5BFD: db $9B;X
L0D5BFE: db $04;X
L0D5BFF: db $E4;X
L0D5C00: db $A8;X
L0D5C01: db $9F;X
L0D5C02: db $03;X
L0D5C03: db $E4;X
L0D5C04: db $88;X
L0D5C05: db $9D;X
L0D5C06: db $04;X
L0D5C07: db $E4;X
L0D5C08: db $A8;X
L0D5C09: db $A2;X
L0D5C0A: db $03;X
L0D5C0B: db $E4;X
L0D5C0C: db $88;X
L0D5C0D: db $9F;X
L0D5C0E: db $04;X
L0D5C0F: db $ED;X
L0D5C10: db $E4;X
L0D5C11: db $20;X
L0D5C12: db $E9;X
L0D5C13: db $44;X
L0D5C14: db $F3;X
L0D5C15: db $03;X
L0D5C16: db $F5;X
L0D5C17: db $00;X
L0D5C18: db $EC;X
L0D5C19: db $18;X
L0D5C1A: db $5D;X
L0D5C1B: db $98;X
L0D5C1C: db $0E;X
L0D5C1D: db $99;X
L0D5C1E: db $EC;X
L0D5C1F: db $18;X
L0D5C20: db $5D;X
L0D5C21: db $9E;X
L0D5C22: db $0E;X
L0D5C23: db $9D;X
L0D5C24: db $EC;X
L0D5C25: db $18;X
L0D5C26: db $5D;X
L0D5C27: db $98;X
L0D5C28: db $0E;X
L0D5C29: db $99;X
L0D5C2A: db $EC;X
L0D5C2B: db $18;X
L0D5C2C: db $5D;X
L0D5C2D: db $9E;X
L0D5C2E: db $0E;X
L0D5C2F: db $9D;X
L0D5C30: db $EC;X
L0D5C31: db $2D;X
L0D5C32: db $5D;X
L0D5C33: db $EC;X
L0D5C34: db $5C;X
L0D5C35: db $5D;X
L0D5C36: db $EC;X
L0D5C37: db $2D;X
L0D5C38: db $5D;X
L0D5C39: db $EC;X
L0D5C3A: db $6D;X
L0D5C3B: db $5D;X
L0D5C3C: db $EC;X
L0D5C3D: db $2D;X
L0D5C3E: db $5D;X
L0D5C3F: db $EC;X
L0D5C40: db $5C;X
L0D5C41: db $5D;X
L0D5C42: db $EC;X
L0D5C43: db $2D;X
L0D5C44: db $5D;X
L0D5C45: db $EC;X
L0D5C46: db $6D;X
L0D5C47: db $5D;X
L0D5C48: db $9B;X
L0D5C49: db $07;X
L0D5C4A: db $93;X
L0D5C4B: db $0C;X
L0D5C4C: db $80;X
L0D5C4D: db $02;X
L0D5C4E: db $93;X
L0D5C4F: db $07;X
L0D5C50: db $80;X
L0D5C51: db $93;X
L0D5C52: db $9B;X
L0D5C53: db $93;X
L0D5C54: db $9E;X
L0D5C55: db $92;X
L0D5C56: db $0C;X
L0D5C57: db $80;X
L0D5C58: db $02;X
L0D5C59: db $96;X
L0D5C5A: db $07;X
L0D5C5B: db $80;X
L0D5C5C: db $96;X
L0D5C5D: db $05;X
L0D5C5E: db $80;X
L0D5C5F: db $02;X
L0D5C60: db $9E;X
L0D5C61: db $05;X
L0D5C62: db $80;X
L0D5C63: db $02;X
L0D5C64: db $96;X
L0D5C65: db $07;X
L0D5C66: db $99;X
L0D5C67: db $07;X
L0D5C68: db $94;X
L0D5C69: db $0C;X
L0D5C6A: db $80;X
L0D5C6B: db $02;X
L0D5C6C: db $94;X
L0D5C6D: db $07;X
L0D5C6E: db $80;X
L0D5C6F: db $99;X
L0D5C70: db $9B;X
L0D5C71: db $99;X
L0D5C72: db $9C;X
L0D5C73: db $05;X
L0D5C74: db $80;X
L0D5C75: db $02;X
L0D5C76: db $94;X
L0D5C77: db $0E;X
L0D5C78: db $99;X
L0D5C79: db $07;X
L0D5C7A: db $80;X
L0D5C7B: db $9C;X
L0D5C7C: db $05;X
L0D5C7D: db $80;X
L0D5C7E: db $02;X
L0D5C7F: db $9C;X
L0D5C80: db $05;X
L0D5C81: db $80;X
L0D5C82: db $02;X
L0D5C83: db $9C;X
L0D5C84: db $05;X
L0D5C85: db $80;X
L0D5C86: db $02;X
L0D5C87: db $98;X
L0D5C88: db $0E;X
L0D5C89: db $93;X
L0D5C8A: db $07;X
L0D5C8B: db $96;X
L0D5C8C: db $0E;X
L0D5C8D: db $91;X
L0D5C8E: db $07;X
L0D5C8F: db $97;X
L0D5C90: db $0E;X
L0D5C91: db $92;X
L0D5C92: db $07;X
L0D5C93: db $99;X
L0D5C94: db $0E;X
L0D5C95: db $94;X
L0D5C96: db $07;X
L0D5C97: db $9B;X
L0D5C98: db $80;X
L0D5C99: db $9B;X
L0D5C9A: db $0E;X
L0D5C9B: db $EC;X
L0D5C9C: db $7A;X
L0D5C9D: db $5D;X
L0D5C9E: db $EC;X
L0D5C9F: db $B2;X
L0D5CA0: db $5D;X
L0D5CA1: db $EC;X
L0D5CA2: db $C1;X
L0D5CA3: db $5D;X
L0D5CA4: db $EC;X
L0D5CA5: db $B2;X
L0D5CA6: db $5D;X
L0D5CA7: db $EC;X
L0D5CA8: db $D4;X
L0D5CA9: db $5D;X
L0D5CAA: db $EC;X
L0D5CAB: db $B2;X
L0D5CAC: db $5D;X
L0D5CAD: db $EC;X
L0D5CAE: db $C1;X
L0D5CAF: db $5D;X
L0D5CB0: db $EC;X
L0D5CB1: db $B2;X
L0D5CB2: db $5D;X
L0D5CB3: db $EC;X
L0D5CB4: db $D4;X
L0D5CB5: db $5D;X
L0D5CB6: db $94;X
L0D5CB7: db $07;X
L0D5CB8: db $98;X
L0D5CB9: db $0E;X
L0D5CBA: db $94;X
L0D5CBB: db $07;X
L0D5CBC: db $96;X
L0D5CBD: db $93;X
L0D5CBE: db $0E;X
L0D5CBF: db $96;X
L0D5CC0: db $07;X
L0D5CC1: db $98;X
L0D5CC2: db $94;X
L0D5CC3: db $0E;X
L0D5CC4: db $98;X
L0D5CC5: db $07;X
L0D5CC6: db $99;X
L0D5CC7: db $91;X
L0D5CC8: db $94;X
L0D5CC9: db $99;X
L0D5CCA: db $9B;X
L0D5CCB: db $93;X
L0D5CCC: db $0E;X
L0D5CCD: db $9B;X
L0D5CCE: db $07;X
L0D5CCF: db $9D;X
L0D5CD0: db $96;X
L0D5CD1: db $0E;X
L0D5CD2: db $9D;X
L0D5CD3: db $07;X
L0D5CD4: db $9F;X
L0D5CD5: db $96;X
L0D5CD6: db $9B;X
L0D5CD7: db $93;X
L0D5CD8: db $94;X
L0D5CD9: db $96;X
L0D5CDA: db $0E;X
L0D5CDB: db $98;X
L0D5CDC: db $07;X
L0D5CDD: db $99;X
L0D5CDE: db $94;X
L0D5CDF: db $97;X
L0D5CE0: db $99;X
L0D5CE1: db $80;X
L0D5CE2: db $99;X
L0D5CE3: db $94;X
L0D5CE4: db $99;X
L0D5CE5: db $97;X
L0D5CE6: db $92;X
L0D5CE7: db $94;X
L0D5CE8: db $97;X
L0D5CE9: db $80;X
L0D5CEA: db $97;X
L0D5CEB: db $92;X
L0D5CEC: db $0E;X
L0D5CED: db $94;X
L0D5CEE: db $07;X
L0D5CEF: db $80;X
L0D5CF0: db $94;X
L0D5CF1: db $0E;X
L0D5CF2: db $96;X
L0D5CF3: db $07;X
L0D5CF4: db $80;X
L0D5CF5: db $96;X
L0D5CF6: db $0E;X
L0D5CF7: db $98;X
L0D5CF8: db $07;X
L0D5CF9: db $80;X
L0D5CFA: db $98;X
L0D5CFB: db $0E;X
L0D5CFC: db $99;X
L0D5CFD: db $07;X
L0D5CFE: db $80;X
L0D5CFF: db $91;X
L0D5D00: db $96;X
L0D5D01: db $99;X
L0D5D02: db $96;X
L0D5D03: db $98;X
L0D5D04: db $99;X
L0D5D05: db $9B;X
L0D5D06: db $9D;X
L0D5D07: db $9F;X
L0D5D08: db $A0;X
L0D5D09: db $80;X
L0D5D0A: db $A0;X
L0D5D0B: db $98;X
L0D5D0C: db $A0;X
L0D5D0D: db $05;X
L0D5D0E: db $80;X
L0D5D0F: db $02;X
L0D5D10: db $A0;X
L0D5D11: db $07;X
L0D5D12: db $9F;X
L0D5D13: db $9D;X
L0D5D14: db $9B;X
L0D5D15: db $E5;X
L0D5D16: db $10;X
L0D5D17: db $5C;X
L0D5D18: db $94;X
L0D5D19: db $0E;X
L0D5D1A: db $9B;X
L0D5D1B: db $03;X
L0D5D1C: db $80;X
L0D5D1D: db $04;X
L0D5D1E: db $94;X
L0D5D1F: db $03;X
L0D5D20: db $80;X
L0D5D21: db $04;X
L0D5D22: db $94;X
L0D5D23: db $0E;X
L0D5D24: db $9B;X
L0D5D25: db $07;X
L0D5D26: db $94;X
L0D5D27: db $0E;X
L0D5D28: db $93;X
L0D5D29: db $07;X
L0D5D2A: db $94;X
L0D5D2B: db $0E;X
L0D5D2C: db $ED;X
L0D5D2D: db $98;X
L0D5D2E: db $0E;X
L0D5D2F: db $80;X
L0D5D30: db $07;X
L0D5D31: db $94;X
L0D5D32: db $15;X
L0D5D33: db $98;X
L0D5D34: db $05;X
L0D5D35: db $80;X
L0D5D36: db $02;X
L0D5D37: db $98;X
L0D5D38: db $05;X
L0D5D39: db $80;X
L0D5D3A: db $02;X
L0D5D3B: db $99;X
L0D5D3C: db $0E;X
L0D5D3D: db $80;X
L0D5D3E: db $07;X
L0D5D3F: db $99;X
L0D5D40: db $13;X
L0D5D41: db $80;X
L0D5D42: db $02;X
L0D5D43: db $94;X
L0D5D44: db $05;X
L0D5D45: db $80;X
L0D5D46: db $02;X
L0D5D47: db $99;X
L0D5D48: db $05;X
L0D5D49: db $80;X
L0D5D4A: db $02;X
L0D5D4B: db $9B;X
L0D5D4C: db $0E;X
L0D5D4D: db $80;X
L0D5D4E: db $07;X
L0D5D4F: db $94;X
L0D5D50: db $13;X
L0D5D51: db $80;X
L0D5D52: db $02;X
L0D5D53: db $9B;X
L0D5D54: db $05;X
L0D5D55: db $80;X
L0D5D56: db $02;X
L0D5D57: db $9B;X
L0D5D58: db $05;X
L0D5D59: db $80;X
L0D5D5A: db $02;X
L0D5D5B: db $ED;X
L0D5D5C: db $9C;X
L0D5D5D: db $0E;X
L0D5D5E: db $80;X
L0D5D5F: db $07;X
L0D5D60: db $94;X
L0D5D61: db $13;X
L0D5D62: db $80;X
L0D5D63: db $02;X
L0D5D64: db $9C;X
L0D5D65: db $05;X
L0D5D66: db $80;X
L0D5D67: db $02;X
L0D5D68: db $9C;X
L0D5D69: db $05;X
L0D5D6A: db $80;X
L0D5D6B: db $02;X
L0D5D6C: db $ED;X
L0D5D6D: db $94;X
L0D5D6E: db $05;X
L0D5D6F: db $80;X
L0D5D70: db $02;X
L0D5D71: db $94;X
L0D5D72: db $07;X
L0D5D73: db $80;X
L0D5D74: db $94;X
L0D5D75: db $80;X
L0D5D76: db $94;X
L0D5D77: db $93;X
L0D5D78: db $94;X
L0D5D79: db $ED;X
L0D5D7A: db $9E;X
L0D5D7B: db $07;X
L0D5D7C: db $92;X
L0D5D7D: db $80;X
L0D5D7E: db $92;X
L0D5D7F: db $80;X
L0D5D80: db $92;X
L0D5D81: db $9E;X
L0D5D82: db $92;X
L0D5D83: db $94;X
L0D5D84: db $05;X
L0D5D85: db $80;X
L0D5D86: db $02;X
L0D5D87: db $94;X
L0D5D88: db $0C;X
L0D5D89: db $80;X
L0D5D8A: db $02;X
L0D5D8B: db $94;X
L0D5D8C: db $0C;X
L0D5D8D: db $80;X
L0D5D8E: db $02;X
L0D5D8F: db $94;X
L0D5D90: db $07;X
L0D5D91: db $93;X
L0D5D92: db $94;X
L0D5D93: db $9E;X
L0D5D94: db $07;X
L0D5D95: db $92;X
L0D5D96: db $80;X
L0D5D97: db $92;X
L0D5D98: db $80;X
L0D5D99: db $92;X
L0D5D9A: db $9E;X
L0D5D9B: db $92;X
L0D5D9C: db $99;X
L0D5D9D: db $05;X
L0D5D9E: db $80;X
L0D5D9F: db $02;X
L0D5DA0: db $99;X
L0D5DA1: db $0C;X
L0D5DA2: db $80;X
L0D5DA3: db $02;X
L0D5DA4: db $9B;X
L0D5DA5: db $0C;X
L0D5DA6: db $80;X
L0D5DA7: db $02;X
L0D5DA8: db $9B;X
L0D5DA9: db $07;X
L0D5DAA: db $99;X
L0D5DAB: db $9B;X
L0D5DAC: db $E7;X
L0D5DAD: db $00;X
L0D5DAE: db $02;X
L0D5DAF: db $7A;X
L0D5DB0: db $5D;X
L0D5DB1: db $ED;X
L0D5DB2: db $94;X
L0D5DB3: db $0E;X
L0D5DB4: db $8F;X
L0D5DB5: db $07;X
L0D5DB6: db $94;X
L0D5DB7: db $80;X
L0D5DB8: db $94;X
L0D5DB9: db $98;X
L0D5DBA: db $99;X
L0D5DBB: db $E7;X
L0D5DBC: db $00;X
L0D5DBD: db $02;X
L0D5DBE: db $B2;X
L0D5DBF: db $5D;X
L0D5DC0: db $ED;X
L0D5DC1: db $99;X
L0D5DC2: db $0E;X
L0D5DC3: db $94;X
L0D5DC4: db $07;X
L0D5DC5: db $99;X
L0D5DC6: db $80;X
L0D5DC7: db $99;X
L0D5DC8: db $98;X
L0D5DC9: db $99;X
L0D5DCA: db $9B;X
L0D5DCB: db $0E;X
L0D5DCC: db $96;X
L0D5DCD: db $07;X
L0D5DCE: db $9B;X
L0D5DCF: db $80;X
L0D5DD0: db $9B;X
L0D5DD1: db $99;X
L0D5DD2: db $96;X
L0D5DD3: db $ED;X
L0D5DD4: db $99;X
L0D5DD5: db $07;X
L0D5DD6: db $91;X
L0D5DD7: db $94;X
L0D5DD8: db $99;X
L0D5DD9: db $9B;X
L0D5DDA: db $96;X
L0D5DDB: db $9B;X
L0D5DDC: db $93;X
L0D5DDD: db $94;X
L0D5DDE: db $0C;X
L0D5DDF: db $80;X
L0D5DE0: db $02;X
L0D5DE1: db $94;X
L0D5DE2: db $05;X
L0D5DE3: db $80;X
L0D5DE4: db $02;X
L0D5DE5: db $94;X
L0D5DE6: db $07;X
L0D5DE7: db $80;X
L0D5DE8: db $94;X
L0D5DE9: db $98;X
L0D5DEA: db $9B;X
L0D5DEB: db $ED;X
L0D5DEC: db $E9;X
L0D5DED: db $88;X
L0D5DEE: db $EC;X
L0D5DEF: db $D2;X
L0D5DF0: db $5E;X
L0D5DF1: db $EC;X
L0D5DF2: db $46;X
L0D5DF3: db $5F;X
L0D5DF4: db $EC;X
L0D5DF5: db $5F;X
L0D5DF6: db $5F;X
L0D5DF7: db $EC;X
L0D5DF8: db $46;X
L0D5DF9: db $5F;X
L0D5DFA: db $EC;X
L0D5DFB: db $78;X
L0D5DFC: db $5F;X
L0D5DFD: db $EC;X
L0D5DFE: db $46;X
L0D5DFF: db $5F;X
L0D5E00: db $EC;X
L0D5E01: db $5F;X
L0D5E02: db $5F;X
L0D5E03: db $EC;X
L0D5E04: db $46;X
L0D5E05: db $5F;X
L0D5E06: db $EC;X
L0D5E07: db $8D;X
L0D5E08: db $5F;X
L0D5E09: db $EC;X
L0D5E0A: db $9E;X
L0D5E0B: db $5F;X
L0D5E0C: db $EC;X
L0D5E0D: db $BB;X
L0D5E0E: db $5F;X
L0D5E0F: db $EC;X
L0D5E10: db $9E;X
L0D5E11: db $5F;X
L0D5E12: db $EC;X
L0D5E13: db $D6;X
L0D5E14: db $5F;X
L0D5E15: db $EC;X
L0D5E16: db $9E;X
L0D5E17: db $5F;X
L0D5E18: db $EC;X
L0D5E19: db $BB;X
L0D5E1A: db $5F;X
L0D5E1B: db $EC;X
L0D5E1C: db $9E;X
L0D5E1D: db $5F;X
L0D5E1E: db $EC;X
L0D5E1F: db $8D;X
L0D5E20: db $5F;X
L0D5E21: db $EC;X
L0D5E22: db $F3;X
L0D5E23: db $5F;X
L0D5E24: db $EC;X
L0D5E25: db $55;X
L0D5E26: db $60;X
L0D5E27: db $EC;X
L0D5E28: db $6C;X
L0D5E29: db $60;X
L0D5E2A: db $EC;X
L0D5E2B: db $55;X
L0D5E2C: db $60;X
L0D5E2D: db $EC;X
L0D5E2E: db $8B;X
L0D5E2F: db $60;X
L0D5E30: db $EC;X
L0D5E31: db $55;X
L0D5E32: db $60;X
L0D5E33: db $EC;X
L0D5E34: db $6C;X
L0D5E35: db $60;X
L0D5E36: db $EC;X
L0D5E37: db $55;X
L0D5E38: db $60;X
L0D5E39: db $EC;X
L0D5E3A: db $8B;X
L0D5E3B: db $60;X
L0D5E3C: db $EC;X
L0D5E3D: db $AA;X
L0D5E3E: db $60;X
L0D5E3F: db $EC;X
L0D5E40: db $C0;X
L0D5E41: db $60;X
L0D5E42: db $EC;X
L0D5E43: db $AA;X
L0D5E44: db $60;X
L0D5E45: db $EC;X
L0D5E46: db $DF;X
L0D5E47: db $60;X
L0D5E48: db $EC;X
L0D5E49: db $AA;X
L0D5E4A: db $60;X
L0D5E4B: db $EC;X
L0D5E4C: db $C0;X
L0D5E4D: db $60;X
L0D5E4E: db $EC;X
L0D5E4F: db $AA;X
L0D5E50: db $60;X
L0D5E51: db $EC;X
L0D5E52: db $00;X
L0D5E53: db $61;X
L0D5E54: db $EC;X
L0D5E55: db $AA;X
L0D5E56: db $60;X
L0D5E57: db $EC;X
L0D5E58: db $C0;X
L0D5E59: db $60;X
L0D5E5A: db $EC;X
L0D5E5B: db $AA;X
L0D5E5C: db $60;X
L0D5E5D: db $EC;X
L0D5E5E: db $DF;X
L0D5E5F: db $60;X
L0D5E60: db $EC;X
L0D5E61: db $AA;X
L0D5E62: db $60;X
L0D5E63: db $EC;X
L0D5E64: db $C0;X
L0D5E65: db $60;X
L0D5E66: db $EC;X
L0D5E67: db $AA;X
L0D5E68: db $60;X
L0D5E69: db $EC;X
L0D5E6A: db $00;X
L0D5E6B: db $61;X
L0D5E6C: db $EC;X
L0D5E6D: db $17;X
L0D5E6E: db $61;X
L0D5E6F: db $E4;X
L0D5E70: db $A1;X
L0D5E71: db $26;X
L0D5E72: db $07;X
L0D5E73: db $E7;X
L0D5E74: db $00;X
L0D5E75: db $06;X
L0D5E76: db $6F;X
L0D5E77: db $5E;X
L0D5E78: db $E4;X
L0D5E79: db $C1;X
L0D5E7A: db $57;X
L0D5E7B: db $07;X
L0D5E7C: db $57;X
L0D5E7D: db $07;X
L0D5E7E: db $E4;X
L0D5E7F: db $A1;X
L0D5E80: db $26;X
L0D5E81: db $07;X
L0D5E82: db $26;X
L0D5E83: db $07;X
L0D5E84: db $E4;X
L0D5E85: db $C1;X
L0D5E86: db $57;X
L0D5E87: db $07;X
L0D5E88: db $57;X
L0D5E89: db $07;X
L0D5E8A: db $57;X
L0D5E8B: db $07;X
L0D5E8C: db $57;X
L0D5E8D: db $07;X
L0D5E8E: db $E4;X
L0D5E8F: db $A1;X
L0D5E90: db $26;X
L0D5E91: db $07;X
L0D5E92: db $26;X
L0D5E93: db $07;X
L0D5E94: db $EC;X
L0D5E95: db $3B;X
L0D5E96: db $61;X
L0D5E97: db $E4;X
L0D5E98: db $C1;X
L0D5E99: db $57;X
L0D5E9A: db $07;X
L0D5E9B: db $E4;X
L0D5E9C: db $51;X
L0D5E9D: db $17;X
L0D5E9E: db $07;X
L0D5E9F: db $E4;X
L0D5EA0: db $A1;X
L0D5EA1: db $26;X
L0D5EA2: db $07;X
L0D5EA3: db $E4;X
L0D5EA4: db $53;X
L0D5EA5: db $13;X
L0D5EA6: db $07;X
L0D5EA7: db $E4;X
L0D5EA8: db $C1;X
L0D5EA9: db $57;X
L0D5EAA: db $07;X
L0D5EAB: db $E4;X
L0D5EAC: db $A1;X
L0D5EAD: db $26;X
L0D5EAE: db $07;X
L0D5EAF: db $26;X
L0D5EB0: db $07;X
L0D5EB1: db $26;X
L0D5EB2: db $07;X
L0D5EB3: db $E4;X
L0D5EB4: db $C1;X
L0D5EB5: db $57;X
L0D5EB6: db $07;X
L0D5EB7: db $E4;X
L0D5EB8: db $53;X
L0D5EB9: db $13;X
L0D5EBA: db $07;X
L0D5EBB: db $E4;X
L0D5EBC: db $A1;X
L0D5EBD: db $26;X
L0D5EBE: db $07;X
L0D5EBF: db $26;X
L0D5EC0: db $07;X
L0D5EC1: db $26;X
L0D5EC2: db $07;X
L0D5EC3: db $E4;X
L0D5EC4: db $91;X
L0D5EC5: db $34;X
L0D5EC6: db $07;X
L0D5EC7: db $E4;X
L0D5EC8: db $91;X
L0D5EC9: db $35;X
L0D5ECA: db $07;X
L0D5ECB: db $E4;X
L0D5ECC: db $91;X
L0D5ECD: db $37;X
L0D5ECE: db $07;X
L0D5ECF: db $E5;X
L0D5ED0: db $EC;X
L0D5ED1: db $5D;X
L0D5ED2: db $E4;X
L0D5ED3: db $C1;X
L0D5ED4: db $57;X
L0D5ED5: db $07;X
L0D5ED6: db $E4;X
L0D5ED7: db $51;X
L0D5ED8: db $17;X
L0D5ED9: db $07;X
L0D5EDA: db $E4;X
L0D5EDB: db $A1;X
L0D5EDC: db $26;X
L0D5EDD: db $07;X
L0D5EDE: db $E4;X
L0D5EDF: db $C1;X
L0D5EE0: db $57;X
L0D5EE1: db $07;X
L0D5EE2: db $57;X
L0D5EE3: db $07;X
L0D5EE4: db $E4;X
L0D5EE5: db $51;X
L0D5EE6: db $17;X
L0D5EE7: db $07;X
L0D5EE8: db $E4;X
L0D5EE9: db $A1;X
L0D5EEA: db $26;X
L0D5EEB: db $0E;X
L0D5EEC: db $E4;X
L0D5EED: db $C1;X
L0D5EEE: db $57;X
L0D5EEF: db $07;X
L0D5EF0: db $E4;X
L0D5EF1: db $51;X
L0D5EF2: db $17;X
L0D5EF3: db $07;X
L0D5EF4: db $E4;X
L0D5EF5: db $A1;X
L0D5EF6: db $26;X
L0D5EF7: db $07;X
L0D5EF8: db $E4;X
L0D5EF9: db $C1;X
L0D5EFA: db $57;X
L0D5EFB: db $07;X
L0D5EFC: db $E4;X
L0D5EFD: db $A1;X
L0D5EFE: db $26;X
L0D5EFF: db $07;X
L0D5F00: db $26;X
L0D5F01: db $07;X
L0D5F02: db $26;X
L0D5F03: db $07;X
L0D5F04: db $E4;X
L0D5F05: db $53;X
L0D5F06: db $13;X
L0D5F07: db $07;X
L0D5F08: db $E4;X
L0D5F09: db $C1;X
L0D5F0A: db $57;X
L0D5F0B: db $07;X
L0D5F0C: db $E4;X
L0D5F0D: db $51;X
L0D5F0E: db $17;X
L0D5F0F: db $07;X
L0D5F10: db $E4;X
L0D5F11: db $A1;X
L0D5F12: db $26;X
L0D5F13: db $07;X
L0D5F14: db $E4;X
L0D5F15: db $C1;X
L0D5F16: db $57;X
L0D5F17: db $07;X
L0D5F18: db $57;X
L0D5F19: db $07;X
L0D5F1A: db $E4;X
L0D5F1B: db $51;X
L0D5F1C: db $17;X
L0D5F1D: db $07;X
L0D5F1E: db $E4;X
L0D5F1F: db $A1;X
L0D5F20: db $26;X
L0D5F21: db $0E;X
L0D5F22: db $E4;X
L0D5F23: db $C1;X
L0D5F24: db $57;X
L0D5F25: db $07;X
L0D5F26: db $E4;X
L0D5F27: db $51;X
L0D5F28: db $17;X
L0D5F29: db $07;X
L0D5F2A: db $E4;X
L0D5F2B: db $A1;X
L0D5F2C: db $26;X
L0D5F2D: db $07;X
L0D5F2E: db $E4;X
L0D5F2F: db $C1;X
L0D5F30: db $57;X
L0D5F31: db $07;X
L0D5F32: db $E4;X
L0D5F33: db $A1;X
L0D5F34: db $26;X
L0D5F35: db $07;X
L0D5F36: db $26;X
L0D5F37: db $07;X
L0D5F38: db $E4;X
L0D5F39: db $91;X
L0D5F3A: db $34;X
L0D5F3B: db $07;X
L0D5F3C: db $E4;X
L0D5F3D: db $91;X
L0D5F3E: db $37;X
L0D5F3F: db $07;X
L0D5F40: db $E7;X
L0D5F41: db $00;X
L0D5F42: db $02;X
L0D5F43: db $D2;X
L0D5F44: db $5E;X
L0D5F45: db $ED;X
L0D5F46: db $E4;X
L0D5F47: db $C1;X
L0D5F48: db $57;X
L0D5F49: db $07;X
L0D5F4A: db $E4;X
L0D5F4B: db $51;X
L0D5F4C: db $17;X
L0D5F4D: db $07;X
L0D5F4E: db $E4;X
L0D5F4F: db $53;X
L0D5F50: db $13;X
L0D5F51: db $0E;X
L0D5F52: db $E4;X
L0D5F53: db $A1;X
L0D5F54: db $26;X
L0D5F55: db $0E;X
L0D5F56: db $E4;X
L0D5F57: db $51;X
L0D5F58: db $17;X
L0D5F59: db $07;X
L0D5F5A: db $E4;X
L0D5F5B: db $C1;X
L0D5F5C: db $57;X
L0D5F5D: db $07;X
L0D5F5E: db $ED;X
L0D5F5F: db $E4;X
L0D5F60: db $C1;X
L0D5F61: db $57;X
L0D5F62: db $07;X
L0D5F63: db $E4;X
L0D5F64: db $51;X
L0D5F65: db $17;X
L0D5F66: db $07;X
L0D5F67: db $E4;X
L0D5F68: db $53;X
L0D5F69: db $13;X
L0D5F6A: db $0E;X
L0D5F6B: db $E4;X
L0D5F6C: db $A1;X
L0D5F6D: db $26;X
L0D5F6E: db $0E;X
L0D5F6F: db $E4;X
L0D5F70: db $51;X
L0D5F71: db $17;X
L0D5F72: db $07;X
L0D5F73: db $E4;X
L0D5F74: db $A1;X
L0D5F75: db $26;X
L0D5F76: db $07;X
L0D5F77: db $ED;X
L0D5F78: db $E4;X
L0D5F79: db $C1;X
L0D5F7A: db $57;X
L0D5F7B: db $07;X
L0D5F7C: db $E4;X
L0D5F7D: db $51;X
L0D5F7E: db $17;X
L0D5F7F: db $07;X
L0D5F80: db $E4;X
L0D5F81: db $53;X
L0D5F82: db $13;X
L0D5F83: db $0E;X
L0D5F84: db $E4;X
L0D5F85: db $A1;X
L0D5F86: db $26;X
L0D5F87: db $0E;X
L0D5F88: db $26;X
L0D5F89: db $07;X
L0D5F8A: db $26;X
L0D5F8B: db $07;X
L0D5F8C: db $ED;X
L0D5F8D: db $E4;X
L0D5F8E: db $A1;X
L0D5F8F: db $26;X
L0D5F90: db $07;X
L0D5F91: db $26;X
L0D5F92: db $07;X
L0D5F93: db $26;X
L0D5F94: db $07;X
L0D5F95: db $26;X
L0D5F96: db $0E;X
L0D5F97: db $26;X
L0D5F98: db $07;X
L0D5F99: db $26;X
L0D5F9A: db $07;X
L0D5F9B: db $26;X
L0D5F9C: db $07;X
L0D5F9D: db $ED;X
L0D5F9E: db $E4;X
L0D5F9F: db $C1;X
L0D5FA0: db $57;X
L0D5FA1: db $07;X
L0D5FA2: db $E4;X
L0D5FA3: db $51;X
L0D5FA4: db $17;X
L0D5FA5: db $07;X
L0D5FA6: db $E4;X
L0D5FA7: db $A1;X
L0D5FA8: db $26;X
L0D5FA9: db $0E;X
L0D5FAA: db $E4;X
L0D5FAB: db $C1;X
L0D5FAC: db $57;X
L0D5FAD: db $07;X
L0D5FAE: db $E4;X
L0D5FAF: db $51;X
L0D5FB0: db $17;X
L0D5FB1: db $07;X
L0D5FB2: db $E4;X
L0D5FB3: db $A1;X
L0D5FB4: db $26;X
L0D5FB5: db $07;X
L0D5FB6: db $E4;X
L0D5FB7: db $C1;X
L0D5FB8: db $57;X
L0D5FB9: db $07;X
L0D5FBA: db $ED;X
L0D5FBB: db $E4;X
L0D5FBC: db $53;X
L0D5FBD: db $13;X
L0D5FBE: db $07;X
L0D5FBF: db $E4;X
L0D5FC0: db $C1;X
L0D5FC1: db $57;X
L0D5FC2: db $07;X
L0D5FC3: db $E4;X
L0D5FC4: db $A1;X
L0D5FC5: db $26;X
L0D5FC6: db $0E;X
L0D5FC7: db $E4;X
L0D5FC8: db $51;X
L0D5FC9: db $17;X
L0D5FCA: db $07;X
L0D5FCB: db $E4;X
L0D5FCC: db $C1;X
L0D5FCD: db $57;X
L0D5FCE: db $07;X
L0D5FCF: db $E4;X
L0D5FD0: db $A1;X
L0D5FD1: db $26;X
L0D5FD2: db $07;X
L0D5FD3: db $26;X
L0D5FD4: db $07;X
L0D5FD5: db $ED;X
L0D5FD6: db $E4;X
L0D5FD7: db $53;X
L0D5FD8: db $13;X
L0D5FD9: db $07;X
L0D5FDA: db $E4;X
L0D5FDB: db $C1;X
L0D5FDC: db $57;X
L0D5FDD: db $07;X
L0D5FDE: db $E4;X
L0D5FDF: db $A1;X
L0D5FE0: db $26;X
L0D5FE1: db $0E;X
L0D5FE2: db $E4;X
L0D5FE3: db $51;X
L0D5FE4: db $17;X
L0D5FE5: db $07;X
L0D5FE6: db $E4;X
L0D5FE7: db $C1;X
L0D5FE8: db $57;X
L0D5FE9: db $07;X
L0D5FEA: db $E4;X
L0D5FEB: db $A1;X
L0D5FEC: db $26;X
L0D5FED: db $07;X
L0D5FEE: db $E4;X
L0D5FEF: db $C1;X
L0D5FF0: db $57;X
L0D5FF1: db $07;X
L0D5FF2: db $ED;X
L0D5FF3: db $E4;X
L0D5FF4: db $91;X
L0D5FF5: db $34;X
L0D5FF6: db $07;X
L0D5FF7: db $E4;X
L0D5FF8: db $A1;X
L0D5FF9: db $26;X
L0D5FFA: db $07;X
L0D5FFB: db $26;X
L0D5FFC: db $07;X
L0D5FFD: db $E4;X
L0D5FFE: db $91;X
L0D5FFF: db $35;X
L0D6000: db $07;X
L0D6001: db $E4;X
L0D6002: db $A1;X
L0D6003: db $26;X
L0D6004: db $07;X
L0D6005: db $26;X
L0D6006: db $07;X
L0D6007: db $E4;X
L0D6008: db $91;X
L0D6009: db $37;X
L0D600A: db $07;X
L0D600B: db $E4;X
L0D600C: db $C1;X
L0D600D: db $57;X
L0D600E: db $07;X
L0D600F: db $E7;X
L0D6010: db $00;X
L0D6011: db $04;X
L0D6012: db $F3;X
L0D6013: db $5F;X
L0D6014: db $E4;X
L0D6015: db $C1;X
L0D6016: db $57;X
L0D6017: db $07;X
L0D6018: db $E4;X
L0D6019: db $51;X
L0D601A: db $17;X
L0D601B: db $07;X
L0D601C: db $E4;X
L0D601D: db $53;X
L0D601E: db $13;X
L0D601F: db $07;X
L0D6020: db $E4;X
L0D6021: db $A1;X
L0D6022: db $26;X
L0D6023: db $07;X
L0D6024: db $E4;X
L0D6025: db $51;X
L0D6026: db $17;X
L0D6027: db $07;X
L0D6028: db $E4;X
L0D6029: db $53;X
L0D602A: db $13;X
L0D602B: db $07;X
L0D602C: db $E4;X
L0D602D: db $C1;X
L0D602E: db $57;X
L0D602F: db $07;X
L0D6030: db $E4;X
L0D6031: db $51;X
L0D6032: db $17;X
L0D6033: db $07;X
L0D6034: db $E4;X
L0D6035: db $53;X
L0D6036: db $13;X
L0D6037: db $07;X
L0D6038: db $E4;X
L0D6039: db $A1;X
L0D603A: db $26;X
L0D603B: db $07;X
L0D603C: db $E4;X
L0D603D: db $51;X
L0D603E: db $17;X
L0D603F: db $07;X
L0D6040: db $E4;X
L0D6041: db $53;X
L0D6042: db $13;X
L0D6043: db $07;X
L0D6044: db $E4;X
L0D6045: db $A1;X
L0D6046: db $26;X
L0D6047: db $07;X
L0D6048: db $E4;X
L0D6049: db $53;X
L0D604A: db $13;X
L0D604B: db $07;X
L0D604C: db $E4;X
L0D604D: db $A1;X
L0D604E: db $26;X
L0D604F: db $07;X
L0D6050: db $E4;X
L0D6051: db $53;X
L0D6052: db $13;X
L0D6053: db $07;X
L0D6054: db $ED;X
L0D6055: db $E4;X
L0D6056: db $C1;X
L0D6057: db $57;X
L0D6058: db $07;X
L0D6059: db $57;X
L0D605A: db $07;X
L0D605B: db $E4;X
L0D605C: db $A1;X
L0D605D: db $26;X
L0D605E: db $07;X
L0D605F: db $26;X
L0D6060: db $07;X
L0D6061: db $26;X
L0D6062: db $07;X
L0D6063: db $26;X
L0D6064: db $07;X
L0D6065: db $E4;X
L0D6066: db $C1;X
L0D6067: db $57;X
L0D6068: db $07;X
L0D6069: db $57;X
L0D606A: db $07;X
L0D606B: db $ED;X
L0D606C: db $E4;X
L0D606D: db $51;X
L0D606E: db $17;X
L0D606F: db $07;X
L0D6070: db $E4;X
L0D6071: db $53;X
L0D6072: db $13;X
L0D6073: db $07;X
L0D6074: db $E4;X
L0D6075: db $C1;X
L0D6076: db $57;X
L0D6077: db $07;X
L0D6078: db $E4;X
L0D6079: db $A1;X
L0D607A: db $26;X
L0D607B: db $07;X
L0D607C: db $26;X
L0D607D: db $07;X
L0D607E: db $E4;X
L0D607F: db $C1;X
L0D6080: db $57;X
L0D6081: db $07;X
L0D6082: db $E4;X
L0D6083: db $A1;X
L0D6084: db $26;X
L0D6085: db $07;X
L0D6086: db $E4;X
L0D6087: db $C1;X
L0D6088: db $57;X
L0D6089: db $07;X
L0D608A: db $ED;X
L0D608B: db $E4;X
L0D608C: db $51;X
L0D608D: db $17;X
L0D608E: db $07;X
L0D608F: db $E4;X
L0D6090: db $53;X
L0D6091: db $13;X
L0D6092: db $07;X
L0D6093: db $E4;X
L0D6094: db $C1;X
L0D6095: db $57;X
L0D6096: db $07;X
L0D6097: db $E4;X
L0D6098: db $A1;X
L0D6099: db $26;X
L0D609A: db $07;X
L0D609B: db $26;X
L0D609C: db $07;X
L0D609D: db $E4;X
L0D609E: db $91;X
L0D609F: db $34;X
L0D60A0: db $07;X
L0D60A1: db $E4;X
L0D60A2: db $91;X
L0D60A3: db $35;X
L0D60A4: db $07;X
L0D60A5: db $E4;X
L0D60A6: db $91;X
L0D60A7: db $37;X
L0D60A8: db $07;X
L0D60A9: db $ED;X
L0D60AA: db $E4;X
L0D60AB: db $C1;X
L0D60AC: db $57;X
L0D60AD: db $07;X
L0D60AE: db $E4;X
L0D60AF: db $51;X
L0D60B0: db $17;X
L0D60B1: db $07;X
L0D60B2: db $E4;X
L0D60B3: db $A1;X
L0D60B4: db $26;X
L0D60B5: db $07;X
L0D60B6: db $E4;X
L0D60B7: db $53;X
L0D60B8: db $13;X
L0D60B9: db $07;X
L0D60BA: db $E7;X
L0D60BB: db $00;X
L0D60BC: db $02;X
L0D60BD: db $AA;X
L0D60BE: db $60;X
L0D60BF: db $ED;X
L0D60C0: db $E4;X
L0D60C1: db $C1;X
L0D60C2: db $57;X
L0D60C3: db $07;X
L0D60C4: db $E4;X
L0D60C5: db $51;X
L0D60C6: db $17;X
L0D60C7: db $07;X
L0D60C8: db $E4;X
L0D60C9: db $A1;X
L0D60CA: db $26;X
L0D60CB: db $07;X
L0D60CC: db $E4;X
L0D60CD: db $53;X
L0D60CE: db $13;X
L0D60CF: db $07;X
L0D60D0: db $E4;X
L0D60D1: db $C1;X
L0D60D2: db $57;X
L0D60D3: db $07;X
L0D60D4: db $E4;X
L0D60D5: db $51;X
L0D60D6: db $17;X
L0D60D7: db $07;X
L0D60D8: db $E4;X
L0D60D9: db $A1;X
L0D60DA: db $26;X
L0D60DB: db $07;X
L0D60DC: db $26;X
L0D60DD: db $07;X
L0D60DE: db $ED;X
L0D60DF: db $E4;X
L0D60E0: db $C1;X
L0D60E1: db $57;X
L0D60E2: db $07;X
L0D60E3: db $E4;X
L0D60E4: db $51;X
L0D60E5: db $17;X
L0D60E6: db $07;X
L0D60E7: db $E4;X
L0D60E8: db $A1;X
L0D60E9: db $26;X
L0D60EA: db $07;X
L0D60EB: db $E4;X
L0D60EC: db $91;X
L0D60ED: db $34;X
L0D60EE: db $07;X
L0D60EF: db $E4;X
L0D60F0: db $C1;X
L0D60F1: db $57;X
L0D60F2: db $07;X
L0D60F3: db $E4;X
L0D60F4: db $91;X
L0D60F5: db $35;X
L0D60F6: db $07;X
L0D60F7: db $E4;X
L0D60F8: db $A1;X
L0D60F9: db $26;X
L0D60FA: db $07;X
L0D60FB: db $E4;X
L0D60FC: db $91;X
L0D60FD: db $37;X
L0D60FE: db $07;X
L0D60FF: db $ED;X
L0D6100: db $E4;X
L0D6101: db $C1;X
L0D6102: db $57;X
L0D6103: db $07;X
L0D6104: db $E4;X
L0D6105: db $51;X
L0D6106: db $17;X
L0D6107: db $07;X
L0D6108: db $E4;X
L0D6109: db $A1;X
L0D610A: db $26;X
L0D610B: db $07;X
L0D610C: db $26;X
L0D610D: db $07;X
L0D610E: db $26;X
L0D610F: db $07;X
L0D6110: db $26;X
L0D6111: db $07;X
L0D6112: db $26;X
L0D6113: db $07;X
L0D6114: db $26;X
L0D6115: db $07;X
L0D6116: db $ED;X
L0D6117: db $E4;X
L0D6118: db $A1;X
L0D6119: db $26;X
L0D611A: db $07;X
L0D611B: db $E4;X
L0D611C: db $51;X
L0D611D: db $17;X
L0D611E: db $07;X
L0D611F: db $E4;X
L0D6120: db $53;X
L0D6121: db $13;X
L0D6122: db $07;X
L0D6123: db $E4;X
L0D6124: db $C1;X
L0D6125: db $57;X
L0D6126: db $07;X
L0D6127: db $57;X
L0D6128: db $07;X
L0D6129: db $E4;X
L0D612A: db $51;X
L0D612B: db $17;X
L0D612C: db $07;X
L0D612D: db $E4;X
L0D612E: db $53;X
L0D612F: db $13;X
L0D6130: db $07;X
L0D6131: db $E4;X
L0D6132: db $A1;X
L0D6133: db $26;X
L0D6134: db $07;X
L0D6135: db $E7;X
L0D6136: db $00;X
L0D6137: db $04;X
L0D6138: db $17;X
L0D6139: db $61;X
L0D613A: db $ED;X
L0D613B: db $E4;X
L0D613C: db $C1;X
L0D613D: db $57;X
L0D613E: db $07;X
L0D613F: db $E4;X
L0D6140: db $53;X
L0D6141: db $13;X
L0D6142: db $07;X
L0D6143: db $E4;X
L0D6144: db $A1;X
L0D6145: db $26;X
L0D6146: db $07;X
L0D6147: db $E4;X
L0D6148: db $C1;X
L0D6149: db $57;X
L0D614A: db $07;X
L0D614B: db $57;X
L0D614C: db $07;X
L0D614D: db $E4;X
L0D614E: db $91;X
L0D614F: db $34;X
L0D6150: db $07;X
L0D6151: db $E4;X
L0D6152: db $A1;X
L0D6153: db $26;X
L0D6154: db $07;X
L0D6155: db $E4;X
L0D6156: db $91;X
L0D6157: db $37;X
L0D6158: db $07;X
L0D6159: db $E7;X
L0D615A: db $00;X
L0D615B: db $02;X
L0D615C: db $3B;X
L0D615D: db $61;X
L0D615E: db $ED;X
SndHeader_BGM_0B: db $04
L0D6160: db $80
L0D6161: db $13
L0D6162: db $78
L0D6163: db $61
L0D6164: db $00
L0D6165: db $81
L0D6166: db $80
L0D6167: db $18
L0D6168: db $75
L0D6169: db $63
L0D616A: db $00
L0D616B: db $81
L0D616C: db $80
L0D616D: db $1D
L0D616E: db $24
L0D616F: db $69
L0D6170: db $00
L0D6171: db $81
L0D6172: db $80
L0D6173: db $22
L0D6174: db $56
L0D6175: db $6A
L0D6176: db $00
L0D6177: db $81
L0D6178: db $E4
L0D6179: db $A8
L0D617A: db $E9
L0D617B: db $11
L0D617C: db $EE
L0D617D: db $C0
L0D617E: db $F1
L0D617F: db $03
L0D6180: db $EC
L0D6181: db $73
L0D6182: db $62
L0D6183: db $EC
L0D6184: db $A9
L0D6185: db $62
L0D6186: db $EC
L0D6187: db $73
L0D6188: db $62
L0D6189: db $EC
L0D618A: db $E0
L0D618B: db $62
L0D618C: db $EC
L0D618D: db $73
L0D618E: db $62
L0D618F: db $EC
L0D6190: db $A9
L0D6191: db $62
L0D6192: db $EC
L0D6193: db $73
L0D6194: db $62
L0D6195: db $EC
L0D6196: db $11
L0D6197: db $63
L0D6198: db $EC
L0D6199: db $42
L0D619A: db $63
L0D619B: db $E4
L0D619C: db $A8
L0D619D: db $B7
L0D619E: db $0E
L0D619F: db $80
L0D61A0: db $07
L0D61A1: db $B5
L0D61A2: db $B3
L0D61A3: db $B2
L0D61A4: db $80
L0D61A5: db $B3
L0D61A6: db $15
L0D61A7: db $B2
L0D61A8: db $07
L0D61A9: db $80
L0D61AA: db $B3
L0D61AB: db $B4
L0D61AC: db $80
L0D61AD: db $B5
L0D61AE: db $38
L0D61AF: db $FA
L0D61B0: db $07
L0D61B1: db $E4
L0D61B2: db $88
L0D61B3: db $B5
L0D61B4: db $07
L0D61B5: db $80
L0D61B6: db $03
L0D61B7: db $E4
L0D61B8: db $68
L0D61B9: db $B5
L0D61BA: db $04
L0D61BB: db $E4
L0D61BC: db $48
L0D61BD: db $B5
L0D61BE: db $07
L0D61BF: db $80
L0D61C0: db $03
L0D61C1: db $E4
L0D61C2: db $38
L0D61C3: db $B5
L0D61C4: db $04
L0D61C5: db $80
L0D61C6: db $1C
L0D61C7: db $EC
L0D61C8: db $42
L0D61C9: db $63
L0D61CA: db $E4
L0D61CB: db $A8
L0D61CC: db $B0
L0D61CD: db $05
L0D61CE: db $80
L0D61CF: db $02
L0D61D0: db $B0
L0D61D1: db $05
L0D61D2: db $80
L0D61D3: db $02
L0D61D4: db $B0
L0D61D5: db $05
L0D61D6: db $80
L0D61D7: db $02
L0D61D8: db $B0
L0D61D9: db $07
L0D61DA: db $80
L0D61DB: db $AE
L0D61DC: db $80
L0D61DD: db $AD
L0D61DE: db $13
L0D61DF: db $80
L0D61E0: db $02
L0D61E1: db $AE
L0D61E2: db $0E
L0D61E3: db $B0
L0D61E4: db $B2
L0D61E5: db $80
L0D61E6: db $03
L0D61E7: db $E4
L0D61E8: db $88
L0D61E9: db $B2
L0D61EA: db $04
L0D61EB: db $E4
L0D61EC: db $A8
L0D61ED: db $B3
L0D61EE: db $07
L0D61EF: db $80
L0D61F0: db $B3
L0D61F1: db $05
L0D61F2: db $80
L0D61F3: db $02
L0D61F4: db $B3
L0D61F5: db $05
L0D61F6: db $80
L0D61F7: db $02
L0D61F8: db $B4
L0D61F9: db $07
L0D61FA: db $80
L0D61FB: db $B4
L0D61FC: db $B5
L0D61FD: db $1C
L0D61FE: db $E4
L0D61FF: db $88
L0D6200: db $B5
L0D6201: db $07
L0D6202: db $80
L0D6203: db $03
L0D6204: db $E4
L0D6205: db $68
L0D6206: db $B5
L0D6207: db $04
L0D6208: db $E4
L0D6209: db $48
L0D620A: db $B5
L0D620B: db $07
L0D620C: db $80
L0D620D: db $03
L0D620E: db $E4
L0D620F: db $38
L0D6210: db $B5
L0D6211: db $04
L0D6212: db $E4
L0D6213: db $A8
L0D6214: db $B3
L0D6215: db $07
L0D6216: db $AE
L0D6217: db $AB
L0D6218: db $0E
L0D6219: db $B3
L0D621A: db $07
L0D621B: db $AE
L0D621C: db $AB
L0D621D: db $0E
L0D621E: db $B5
L0D621F: db $07
L0D6220: db $B2
L0D6221: db $AE
L0D6222: db $0E
L0D6223: db $B5
L0D6224: db $07
L0D6225: db $B2
L0D6226: db $AE
L0D6227: db $0E
L0D6228: db $B3
L0D6229: db $07
L0D622A: db $B0
L0D622B: db $AD
L0D622C: db $0E
L0D622D: db $B3
L0D622E: db $07
L0D622F: db $B0
L0D6230: db $AD
L0D6231: db $0E
L0D6232: db $B2
L0D6233: db $07
L0D6234: db $AE
L0D6235: db $A9
L0D6236: db $0E
L0D6237: db $B2
L0D6238: db $07
L0D6239: db $AE
L0D623A: db $A9
L0D623B: db $0E
L0D623C: db $B3
L0D623D: db $07
L0D623E: db $AE
L0D623F: db $AB
L0D6240: db $0E
L0D6241: db $B3
L0D6242: db $07
L0D6243: db $AE
L0D6244: db $AB
L0D6245: db $0E
L0D6246: db $B5
L0D6247: db $07
L0D6248: db $B2
L0D6249: db $AE
L0D624A: db $0E
L0D624B: db $B5
L0D624C: db $07
L0D624D: db $B2
L0D624E: db $AE
L0D624F: db $0E
L0D6250: db $B3
L0D6251: db $07
L0D6252: db $B0
L0D6253: db $A9
L0D6254: db $0E
L0D6255: db $B3
L0D6256: db $07
L0D6257: db $B0
L0D6258: db $A9
L0D6259: db $0E
L0D625A: db $AE
L0D625B: db $1C
L0D625C: db $E4
L0D625D: db $88
L0D625E: db $AE
L0D625F: db $07
L0D6260: db $80
L0D6261: db $03
L0D6262: db $E4
L0D6263: db $68
L0D6264: db $AE
L0D6265: db $04
L0D6266: db $E4
L0D6267: db $48
L0D6268: db $AE
L0D6269: db $07
L0D626A: db $80
L0D626B: db $03
L0D626C: db $E4
L0D626D: db $38
L0D626E: db $AE
L0D626F: db $04
L0D6270: db $E5
L0D6271: db $78
L0D6272: db $61
L0D6273: db $E4
L0D6274: db $A8
L0D6275: db $EE
L0D6276: db $C0
L0D6277: db $A2
L0D6278: db $0E
L0D6279: db $80
L0D627A: db $07
L0D627B: db $A2
L0D627C: db $A4
L0D627D: db $A6
L0D627E: db $80
L0D627F: db $A7
L0D6280: db $80
L0D6281: db $AE
L0D6282: db $0C
L0D6283: db $80
L0D6284: db $02
L0D6285: db $AE
L0D6286: db $07
L0D6287: db $AD
L0D6288: db $AB
L0D6289: db $AD
L0D628A: db $0E
L0D628B: db $AB
L0D628C: db $A9
L0D628D: db $05
L0D628E: db $80
L0D628F: db $02
L0D6290: db $A9
L0D6291: db $1C
L0D6292: db $E4
L0D6293: db $88
L0D6294: db $A9
L0D6295: db $07
L0D6296: db $80
L0D6297: db $03
L0D6298: db $E4
L0D6299: db $68
L0D629A: db $A9
L0D629B: db $04
L0D629C: db $E4
L0D629D: db $82
L0D629E: db $EE
L0D629F: db $80
L0D62A0: db $B5
L0D62A1: db $07
L0D62A2: db $80
L0D62A3: db $B5
L0D62A4: db $B3
L0D62A5: db $B2
L0D62A6: db $B0
L0D62A7: db $0E
L0D62A8: db $ED
L0D62A9: db $E4
L0D62AA: db $A8
L0D62AB: db $EE
L0D62AC: db $C0
L0D62AD: db $A2
L0D62AE: db $0E
L0D62AF: db $80
L0D62B0: db $07
L0D62B1: db $A2
L0D62B2: db $A4
L0D62B3: db $A6
L0D62B4: db $80
L0D62B5: db $A7
L0D62B6: db $80
L0D62B7: db $AE
L0D62B8: db $0C
L0D62B9: db $80
L0D62BA: db $02
L0D62BB: db $AE
L0D62BC: db $07
L0D62BD: db $AD
L0D62BE: db $AB
L0D62BF: db $AD
L0D62C0: db $AE
L0D62C1: db $B0
L0D62C2: db $2A
L0D62C3: db $E4
L0D62C4: db $88
L0D62C5: db $B0
L0D62C6: db $07
L0D62C7: db $80
L0D62C8: db $03
L0D62C9: db $E4
L0D62CA: db $68
L0D62CB: db $B0
L0D62CC: db $04
L0D62CD: db $E4
L0D62CE: db $48
L0D62CF: db $B0
L0D62D0: db $07
L0D62D1: db $80
L0D62D2: db $03
L0D62D3: db $E4
L0D62D4: db $38
L0D62D5: db $B0
L0D62D6: db $04
L0D62D7: db $E4
L0D62D8: db $82
L0D62D9: db $EE
L0D62DA: db $80
L0D62DB: db $B3
L0D62DC: db $0E
L0D62DD: db $B2
L0D62DE: db $B0
L0D62DF: db $ED
L0D62E0: db $E4
L0D62E1: db $A8
L0D62E2: db $EE
L0D62E3: db $C0
L0D62E4: db $A2
L0D62E5: db $0E
L0D62E6: db $80
L0D62E7: db $07
L0D62E8: db $A2
L0D62E9: db $A4
L0D62EA: db $A6
L0D62EB: db $80
L0D62EC: db $A7
L0D62ED: db $80
L0D62EE: db $B3
L0D62EF: db $0C
L0D62F0: db $80
L0D62F1: db $02
L0D62F2: db $B3
L0D62F3: db $07
L0D62F4: db $B2
L0D62F5: db $B0
L0D62F6: db $B2
L0D62F7: db $B3
L0D62F8: db $AE
L0D62F9: db $38
L0D62FA: db $E4
L0D62FB: db $88
L0D62FC: db $AE
L0D62FD: db $07
L0D62FE: db $80
L0D62FF: db $03
L0D6300: db $E4
L0D6301: db $68
L0D6302: db $AE
L0D6303: db $04
L0D6304: db $E4
L0D6305: db $48
L0D6306: db $AE
L0D6307: db $07
L0D6308: db $80
L0D6309: db $03
L0D630A: db $E4
L0D630B: db $38
L0D630C: db $AE
L0D630D: db $04
L0D630E: db $80
L0D630F: db $1C
L0D6310: db $ED
L0D6311: db $E4
L0D6312: db $A8
L0D6313: db $EE
L0D6314: db $C0
L0D6315: db $A2
L0D6316: db $0E
L0D6317: db $80
L0D6318: db $07
L0D6319: db $A2
L0D631A: db $A4
L0D631B: db $A6
L0D631C: db $80
L0D631D: db $A7
L0D631E: db $80
L0D631F: db $B3
L0D6320: db $0C
L0D6321: db $80
L0D6322: db $02
L0D6323: db $B3
L0D6324: db $07
L0D6325: db $B2
L0D6326: db $A9
L0D6327: db $AB
L0D6328: db $AD
L0D6329: db $AE
L0D632A: db $38
L0D632B: db $E4
L0D632C: db $88
L0D632D: db $AE
L0D632E: db $07
L0D632F: db $80
L0D6330: db $03
L0D6331: db $E4
L0D6332: db $68
L0D6333: db $AE
L0D6334: db $04
L0D6335: db $E4
L0D6336: db $48
L0D6337: db $AE
L0D6338: db $07
L0D6339: db $80
L0D633A: db $03
L0D633B: db $E4
L0D633C: db $38
L0D633D: db $AE
L0D633E: db $04
L0D633F: db $80
L0D6340: db $1C
L0D6341: db $ED
L0D6342: db $E4
L0D6343: db $A8
L0D6344: db $EE
L0D6345: db $C0
L0D6346: db $B0
L0D6347: db $0E
L0D6348: db $80
L0D6349: db $07
L0D634A: db $AE
L0D634B: db $B0
L0D634C: db $B2
L0D634D: db $80
L0D634E: db $B2
L0D634F: db $01
L0D6350: db $B3
L0D6351: db $14
L0D6352: db $B2
L0D6353: db $07
L0D6354: db $80
L0D6355: db $B3
L0D6356: db $B5
L0D6357: db $80
L0D6358: db $B1
L0D6359: db $01
L0D635A: db $B2
L0D635B: db $14
L0D635C: db $AE
L0D635D: db $2A
L0D635E: db $E4
L0D635F: db $88
L0D6360: db $AE
L0D6361: db $07
L0D6362: db $80
L0D6363: db $03
L0D6364: db $E4
L0D6365: db $68
L0D6366: db $AE
L0D6367: db $04
L0D6368: db $E4
L0D6369: db $48
L0D636A: db $AE
L0D636B: db $07
L0D636C: db $80
L0D636D: db $03
L0D636E: db $E4
L0D636F: db $38
L0D6370: db $AE
L0D6371: db $04
L0D6372: db $80
L0D6373: db $1C
L0D6374: db $ED
L0D6375: db $E4
L0D6376: db $A8
L0D6377: db $E9
L0D6378: db $22
L0D6379: db $EE
L0D637A: db $40
L0D637B: db $F1
L0D637C: db $03
L0D637D: db $EC
L0D637E: db $FA
L0D637F: db $66
L0D6380: db $EC
L0D6381: db $8F
L0D6382: db $67
L0D6383: db $EC
L0D6384: db $FA
L0D6385: db $66
L0D6386: db $EE
L0D6387: db $40
L0D6388: db $E4
L0D6389: db $88
L0D638A: db $9D
L0D638B: db $0B
L0D638C: db $E4
L0D638D: db $68
L0D638E: db $9D
L0D638F: db $07
L0D6390: db $80
L0D6391: db $03
L0D6392: db $E4
L0D6393: db $88
L0D6394: db $9D
L0D6395: db $07
L0D6396: db $A1
L0D6397: db $03
L0D6398: db $E4
L0D6399: db $68
L0D639A: db $9D
L0D639B: db $04
L0D639C: db $E4
L0D639D: db $88
L0D639E: db $A2
L0D639F: db $03
L0D63A0: db $E4
L0D63A1: db $68
L0D63A2: db $A1
L0D63A3: db $04
L0D63A4: db $80
L0D63A5: db $03
L0D63A6: db $A2
L0D63A7: db $04
L0D63A8: db $E4
L0D63A9: db $88
L0D63AA: db $A4
L0D63AB: db $07
L0D63AC: db $80
L0D63AD: db $03
L0D63AE: db $E4
L0D63AF: db $68
L0D63B0: db $A4
L0D63B1: db $04
L0D63B2: db $E4
L0D63B3: db $88
L0D63B4: db $A9
L0D63B5: db $0B
L0D63B6: db $E4
L0D63B7: db $68
L0D63B8: db $A9
L0D63B9: db $03
L0D63BA: db $E4
L0D63BB: db $88
L0D63BC: db $A9
L0D63BD: db $07
L0D63BE: db $AB
L0D63BF: db $03
L0D63C0: db $E4
L0D63C1: db $68
L0D63C2: db $A9
L0D63C3: db $04
L0D63C4: db $E4
L0D63C5: db $88
L0D63C6: db $AB
L0D63C7: db $03
L0D63C8: db $E4
L0D63C9: db $68
L0D63CA: db $AB
L0D63CB: db $04
L0D63CC: db $E4
L0D63CD: db $88
L0D63CE: db $AC
L0D63CF: db $03
L0D63D0: db $E4
L0D63D1: db $68
L0D63D2: db $AB
L0D63D3: db $04
L0D63D4: db $E4
L0D63D5: db $88
L0D63D6: db $AD
L0D63D7: db $03
L0D63D8: db $E4
L0D63D9: db $68
L0D63DA: db $AC
L0D63DB: db $04
L0D63DC: db $E4
L0D63DD: db $A8
L0D63DE: db $A9
L0D63DF: db $03
L0D63E0: db $E4
L0D63E1: db $68
L0D63E2: db $AD
L0D63E3: db $04
L0D63E4: db $E4
L0D63E5: db $A8
L0D63E6: db $A6
L0D63E7: db $03
L0D63E8: db $E4
L0D63E9: db $88
L0D63EA: db $A9
L0D63EB: db $04
L0D63EC: db $E4
L0D63ED: db $A8
L0D63EE: db $A2
L0D63EF: db $03
L0D63F0: db $E4
L0D63F1: db $88
L0D63F2: db $A6
L0D63F3: db $04
L0D63F4: db $E4
L0D63F5: db $A8
L0D63F6: db $9D
L0D63F7: db $03
L0D63F8: db $E4
L0D63F9: db $88
L0D63FA: db $A2
L0D63FB: db $04
L0D63FC: db $E4
L0D63FD: db $A8
L0D63FE: db $9B
L0D63FF: db $03
L0D6400: db $E4
L0D6401: db $88
L0D6402: db $9D
L0D6403: db $04
L0D6404: db $E4
L0D6405: db $A8
L0D6406: db $9A
L0D6407: db $03
L0D6408: db $E4
L0D6409: db $88
L0D640A: db $9B
L0D640B: db $04
L0D640C: db $E4
L0D640D: db $A8
L0D640E: db $9B
L0D640F: db $03
L0D6410: db $E4
L0D6411: db $88
L0D6412: db $9A
L0D6413: db $04
L0D6414: db $E4
L0D6415: db $A8
L0D6416: db $9D
L0D6417: db $03
L0D6418: db $E4
L0D6419: db $88
L0D641A: db $9B
L0D641B: db $04
L0D641C: db $E4
L0D641D: db $A8
L0D641E: db $9B
L0D641F: db $03
L0D6420: db $E4
L0D6421: db $88
L0D6422: db $9D
L0D6423: db $04
L0D6424: db $E4
L0D6425: db $A8
L0D6426: db $9A
L0D6427: db $03
L0D6428: db $E4
L0D6429: db $88
L0D642A: db $9B
L0D642B: db $04
L0D642C: db $80
L0D642D: db $03
L0D642E: db $9A
L0D642F: db $04
L0D6430: db $E4
L0D6431: db $A8
L0D6432: db $96
L0D6433: db $15
L0D6434: db $E4
L0D6435: db $88
L0D6436: db $96
L0D6437: db $07
L0D6438: db $80
L0D6439: db $03
L0D643A: db $E4
L0D643B: db $68
L0D643C: db $96
L0D643D: db $04
L0D643E: db $EC
L0D643F: db $FA
L0D6440: db $66
L0D6441: db $EC
L0D6442: db $8F
L0D6443: db $67
L0D6444: db $EC
L0D6445: db $FA
L0D6446: db $66
L0D6447: db $EE
L0D6448: db $40
L0D6449: db $E4
L0D644A: db $88
L0D644B: db $9D
L0D644C: db $0B
L0D644D: db $E4
L0D644E: db $68
L0D644F: db $9D
L0D6450: db $07
L0D6451: db $80
L0D6452: db $03
L0D6453: db $E4
L0D6454: db $88
L0D6455: db $9D
L0D6456: db $07
L0D6457: db $A1
L0D6458: db $03
L0D6459: db $E4
L0D645A: db $68
L0D645B: db $9D
L0D645C: db $04
L0D645D: db $E4
L0D645E: db $88
L0D645F: db $A2
L0D6460: db $03
L0D6461: db $E4
L0D6462: db $68
L0D6463: db $A1
L0D6464: db $04
L0D6465: db $80
L0D6466: db $03
L0D6467: db $A2
L0D6468: db $04
L0D6469: db $E4
L0D646A: db $88
L0D646B: db $A4
L0D646C: db $07
L0D646D: db $80
L0D646E: db $03
L0D646F: db $E4
L0D6470: db $68
L0D6471: db $A4
L0D6472: db $04
L0D6473: db $E4
L0D6474: db $88
L0D6475: db $AE
L0D6476: db $0B
L0D6477: db $E4
L0D6478: db $68
L0D6479: db $AE
L0D647A: db $03
L0D647B: db $E4
L0D647C: db $88
L0D647D: db $AE
L0D647E: db $05
L0D647F: db $80
L0D6480: db $02
L0D6481: db $AE
L0D6482: db $03
L0D6483: db $E4
L0D6484: db $68
L0D6485: db $AE
L0D6486: db $04
L0D6487: db $E4
L0D6488: db $88
L0D6489: db $A6
L0D648A: db $03
L0D648B: db $E4
L0D648C: db $68
L0D648D: db $AE
L0D648E: db $04
L0D648F: db $E4
L0D6490: db $88
L0D6491: db $A7
L0D6492: db $03
L0D6493: db $E4
L0D6494: db $68
L0D6495: db $A6
L0D6496: db $04
L0D6497: db $E4
L0D6498: db $88
L0D6499: db $A9
L0D649A: db $03
L0D649B: db $E4
L0D649C: db $68
L0D649D: db $A7
L0D649E: db $04
L0D649F: db $E4
L0D64A0: db $A8
L0D64A1: db $A6
L0D64A2: db $03
L0D64A3: db $E4
L0D64A4: db $68
L0D64A5: db $A9
L0D64A6: db $04
L0D64A7: db $E4
L0D64A8: db $A8
L0D64A9: db $A9
L0D64AA: db $03
L0D64AB: db $E4
L0D64AC: db $88
L0D64AD: db $A6
L0D64AE: db $04
L0D64AF: db $E4
L0D64B0: db $A8
L0D64B1: db $A2
L0D64B2: db $03
L0D64B3: db $E4
L0D64B4: db $88
L0D64B5: db $A9
L0D64B6: db $04
L0D64B7: db $E4
L0D64B8: db $A8
L0D64B9: db $A6
L0D64BA: db $03
L0D64BB: db $E4
L0D64BC: db $88
L0D64BD: db $A2
L0D64BE: db $04
L0D64BF: db $E4
L0D64C0: db $A8
L0D64C1: db $9D
L0D64C2: db $03
L0D64C3: db $E4
L0D64C4: db $88
L0D64C5: db $A6
L0D64C6: db $04
L0D64C7: db $E4
L0D64C8: db $A8
L0D64C9: db $A2
L0D64CA: db $03
L0D64CB: db $E4
L0D64CC: db $88
L0D64CD: db $9D
L0D64CE: db $04
L0D64CF: db $E4
L0D64D0: db $A8
L0D64D1: db $9A
L0D64D2: db $03
L0D64D3: db $E4
L0D64D4: db $88
L0D64D5: db $A2
L0D64D6: db $04
L0D64D7: db $E4
L0D64D8: db $A8
L0D64D9: db $9D
L0D64DA: db $03
L0D64DB: db $E4
L0D64DC: db $88
L0D64DD: db $9A
L0D64DE: db $04
L0D64DF: db $E4
L0D64E0: db $A8
L0D64E1: db $96
L0D64E2: db $03
L0D64E3: db $E4
L0D64E4: db $88
L0D64E5: db $9D
L0D64E6: db $04
L0D64E7: db $E4
L0D64E8: db $A8
L0D64E9: db $9A
L0D64EA: db $03
L0D64EB: db $E4
L0D64EC: db $88
L0D64ED: db $96
L0D64EE: db $04
L0D64EF: db $E4
L0D64F0: db $A8
L0D64F1: db $9D
L0D64F2: db $03
L0D64F3: db $E4
L0D64F4: db $88
L0D64F5: db $9A
L0D64F6: db $04
L0D64F7: db $E4
L0D64F8: db $A8
L0D64F9: db $A2
L0D64FA: db $0E
L0D64FB: db $80
L0D64FC: db $03
L0D64FD: db $E4
L0D64FE: db $88
L0D64FF: db $A2
L0D6500: db $04
L0D6501: db $E4
L0D6502: db $A8
L0D6503: db $A6
L0D6504: db $07
L0D6505: db $80
L0D6506: db $03
L0D6507: db $E4
L0D6508: db $88
L0D6509: db $A6
L0D650A: db $04
L0D650B: db $EC
L0D650C: db $24
L0D650D: db $68
L0D650E: db $E4
L0D650F: db $82
L0D6510: db $AE
L0D6511: db $03
L0D6512: db $E4
L0D6513: db $62
L0D6514: db $A1
L0D6515: db $04
L0D6516: db $E4
L0D6517: db $82
L0D6518: db $A9
L0D6519: db $03
L0D651A: db $E4
L0D651B: db $62
L0D651C: db $AE
L0D651D: db $04
L0D651E: db $E4
L0D651F: db $82
L0D6520: db $A6
L0D6521: db $03
L0D6522: db $E4
L0D6523: db $62
L0D6524: db $A9
L0D6525: db $04
L0D6526: db $E4
L0D6527: db $82
L0D6528: db $A2
L0D6529: db $03
L0D652A: db $E4
L0D652B: db $62
L0D652C: db $A6
L0D652D: db $04
L0D652E: db $E4
L0D652F: db $82
L0D6530: db $A9
L0D6531: db $03
L0D6532: db $E4
L0D6533: db $62
L0D6534: db $A2
L0D6535: db $04
L0D6536: db $E4
L0D6537: db $82
L0D6538: db $A6
L0D6539: db $03
L0D653A: db $E4
L0D653B: db $62
L0D653C: db $A9
L0D653D: db $04
L0D653E: db $E4
L0D653F: db $82
L0D6540: db $A2
L0D6541: db $03
L0D6542: db $E4
L0D6543: db $62
L0D6544: db $A6
L0D6545: db $04
L0D6546: db $E4
L0D6547: db $82
L0D6548: db $9D
L0D6549: db $03
L0D654A: db $E4
L0D654B: db $62
L0D654C: db $A2
L0D654D: db $04
L0D654E: db $EC
L0D654F: db $4A
L0D6550: db $68
L0D6551: db $EC
L0D6552: db $70
L0D6553: db $68
L0D6554: db $EC
L0D6555: db $96
L0D6556: db $68
L0D6557: db $E4
L0D6558: db $82
L0D6559: db $B2
L0D655A: db $03
L0D655B: db $E4
L0D655C: db $62
L0D655D: db $9D
L0D655E: db $04
L0D655F: db $E4
L0D6560: db $82
L0D6561: db $AE
L0D6562: db $03
L0D6563: db $E4
L0D6564: db $62
L0D6565: db $B2
L0D6566: db $04
L0D6567: db $E4
L0D6568: db $82
L0D6569: db $A9
L0D656A: db $03
L0D656B: db $E4
L0D656C: db $62
L0D656D: db $AE
L0D656E: db $04
L0D656F: db $E4
L0D6570: db $82
L0D6571: db $A6
L0D6572: db $03
L0D6573: db $E4
L0D6574: db $62
L0D6575: db $A9
L0D6576: db $04
L0D6577: db $E4
L0D6578: db $82
L0D6579: db $AE
L0D657A: db $03
L0D657B: db $E4
L0D657C: db $62
L0D657D: db $A6
L0D657E: db $04
L0D657F: db $E4
L0D6580: db $82
L0D6581: db $A9
L0D6582: db $03
L0D6583: db $E4
L0D6584: db $62
L0D6585: db $AE
L0D6586: db $04
L0D6587: db $E4
L0D6588: db $82
L0D6589: db $A6
L0D658A: db $03
L0D658B: db $E4
L0D658C: db $62
L0D658D: db $A9
L0D658E: db $04
L0D658F: db $E4
L0D6590: db $82
L0D6591: db $A2
L0D6592: db $03
L0D6593: db $E4
L0D6594: db $62
L0D6595: db $A6
L0D6596: db $04
L0D6597: db $E4
L0D6598: db $82
L0D6599: db $A9
L0D659A: db $03
L0D659B: db $E4
L0D659C: db $62
L0D659D: db $A2
L0D659E: db $04
L0D659F: db $E4
L0D65A0: db $82
L0D65A1: db $A6
L0D65A2: db $03
L0D65A3: db $E4
L0D65A4: db $62
L0D65A5: db $A9
L0D65A6: db $04
L0D65A7: db $E4
L0D65A8: db $82
L0D65A9: db $A2
L0D65AA: db $03
L0D65AB: db $E4
L0D65AC: db $62
L0D65AD: db $A6
L0D65AE: db $04
L0D65AF: db $E4
L0D65B0: db $82
L0D65B1: db $9D
L0D65B2: db $03
L0D65B3: db $E4
L0D65B4: db $62
L0D65B5: db $A2
L0D65B6: db $04
L0D65B7: db $E4
L0D65B8: db $82
L0D65B9: db $A0
L0D65BA: db $03
L0D65BB: db $E4
L0D65BC: db $62
L0D65BD: db $9D
L0D65BE: db $04
L0D65BF: db $E4
L0D65C0: db $82
L0D65C1: db $A4
L0D65C2: db $03
L0D65C3: db $E4
L0D65C4: db $62
L0D65C5: db $A0
L0D65C6: db $04
L0D65C7: db $E4
L0D65C8: db $82
L0D65C9: db $A7
L0D65CA: db $03
L0D65CB: db $E4
L0D65CC: db $62
L0D65CD: db $A4
L0D65CE: db $04
L0D65CF: db $E4
L0D65D0: db $82
L0D65D1: db $AA
L0D65D2: db $03
L0D65D3: db $E4
L0D65D4: db $62
L0D65D5: db $A7
L0D65D6: db $04
L0D65D7: db $EC
L0D65D8: db $24
L0D65D9: db $68
L0D65DA: db $E4
L0D65DB: db $82
L0D65DC: db $AE
L0D65DD: db $03
L0D65DE: db $E4
L0D65DF: db $62
L0D65E0: db $A1
L0D65E1: db $04
L0D65E2: db $E4
L0D65E3: db $82
L0D65E4: db $A9
L0D65E5: db $03
L0D65E6: db $E4
L0D65E7: db $62
L0D65E8: db $AE
L0D65E9: db $04
L0D65EA: db $E4
L0D65EB: db $82
L0D65EC: db $A6
L0D65ED: db $03
L0D65EE: db $E4
L0D65EF: db $62
L0D65F0: db $A9
L0D65F1: db $04
L0D65F2: db $E4
L0D65F3: db $82
L0D65F4: db $A2
L0D65F5: db $03
L0D65F6: db $E4
L0D65F7: db $62
L0D65F8: db $A6
L0D65F9: db $04
L0D65FA: db $E4
L0D65FB: db $82
L0D65FC: db $A9
L0D65FD: db $03
L0D65FE: db $E4
L0D65FF: db $62
L0D6600: db $A2
L0D6601: db $04
L0D6602: db $E4
L0D6603: db $82
L0D6604: db $A6
L0D6605: db $03
L0D6606: db $E4
L0D6607: db $62
L0D6608: db $A9
L0D6609: db $04
L0D660A: db $E4
L0D660B: db $82
L0D660C: db $A2
L0D660D: db $03
L0D660E: db $E4
L0D660F: db $62
L0D6610: db $A6
L0D6611: db $04
L0D6612: db $E4
L0D6613: db $82
L0D6614: db $9D
L0D6615: db $03
L0D6616: db $E4
L0D6617: db $62
L0D6618: db $A2
L0D6619: db $04
L0D661A: db $EC
L0D661B: db $4A
L0D661C: db $68
L0D661D: db $E4
L0D661E: db $88
L0D661F: db $AB
L0D6620: db $03
L0D6621: db $E4
L0D6622: db $68
L0D6623: db $9D
L0D6624: db $04
L0D6625: db $E4
L0D6626: db $88
L0D6627: db $AB
L0D6628: db $03
L0D6629: db $E4
L0D662A: db $68
L0D662B: db $AB
L0D662C: db $04
L0D662D: db $E4
L0D662E: db $88
L0D662F: db $AB
L0D6630: db $03
L0D6631: db $E4
L0D6632: db $68
L0D6633: db $AB
L0D6634: db $04
L0D6635: db $E4
L0D6636: db $88
L0D6637: db $AB
L0D6638: db $03
L0D6639: db $E4
L0D663A: db $68
L0D663B: db $AB
L0D663C: db $04
L0D663D: db $80
L0D663E: db $03
L0D663F: db $AB
L0D6640: db $04
L0D6641: db $E4
L0D6642: db $88
L0D6643: db $AB
L0D6644: db $07
L0D6645: db $80
L0D6646: db $03
L0D6647: db $E4
L0D6648: db $68
L0D6649: db $AB
L0D664A: db $04
L0D664B: db $E4
L0D664C: db $88
L0D664D: db $A9
L0D664E: db $0B
L0D664F: db $E4
L0D6650: db $68
L0D6651: db $A9
L0D6652: db $0A
L0D6653: db $E4
L0D6654: db $88
L0D6655: db $AB
L0D6656: db $07
L0D6657: db $80
L0D6658: db $03
L0D6659: db $E4
L0D665A: db $68
L0D665B: db $AB
L0D665C: db $04
L0D665D: db $E4
L0D665E: db $88
L0D665F: db $AD
L0D6660: db $07
L0D6661: db $80
L0D6662: db $03
L0D6663: db $E4
L0D6664: db $68
L0D6665: db $AD
L0D6666: db $04
L0D6667: db $E4
L0D6668: db $88
L0D6669: db $AE
L0D666A: db $07
L0D666B: db $80
L0D666C: db $03
L0D666D: db $E4
L0D666E: db $68
L0D666F: db $AE
L0D6670: db $04
L0D6671: db $80
L0D6672: db $03
L0D6673: db $E4
L0D6674: db $48
L0D6675: db $AE
L0D6676: db $04
L0D6677: db $E4
L0D6678: db $88
L0D6679: db $B0
L0D667A: db $07
L0D667B: db $80
L0D667C: db $03
L0D667D: db $E4
L0D667E: db $68
L0D667F: db $B0
L0D6680: db $04
L0D6681: db $E4
L0D6682: db $88
L0D6683: db $B0
L0D6684: db $05
L0D6685: db $80
L0D6686: db $02
L0D6687: db $B0
L0D6688: db $05
L0D6689: db $80
L0D668A: db $02
L0D668B: db $B0
L0D668C: db $07
L0D668D: db $80
L0D668E: db $03
L0D668F: db $E4
L0D6690: db $68
L0D6691: db $B0
L0D6692: db $04
L0D6693: db $E4
L0D6694: db $88
L0D6695: db $B0
L0D6696: db $05
L0D6697: db $80
L0D6698: db $02
L0D6699: db $B0
L0D669A: db $1C
L0D669B: db $E4
L0D669C: db $68
L0D669D: db $B0
L0D669E: db $07
L0D669F: db $80
L0D66A0: db $03
L0D66A1: db $E4
L0D66A2: db $48
L0D66A3: db $B0
L0D66A4: db $04
L0D66A5: db $E4
L0D66A6: db $68
L0D66A7: db $B0
L0D66A8: db $07
L0D66A9: db $80
L0D66AA: db $EC
L0D66AB: db $BC
L0D66AC: db $68
L0D66AD: db $EC
L0D66AE: db $D6
L0D66AF: db $68
L0D66B0: db $EC
L0D66B1: db $F0
L0D66B2: db $68
L0D66B3: db $EC
L0D66B4: db $0A
L0D66B5: db $69
L0D66B6: db $EC
L0D66B7: db $BC
L0D66B8: db $68
L0D66B9: db $EC
L0D66BA: db $D6
L0D66BB: db $68
L0D66BC: db $E4
L0D66BD: db $88
L0D66BE: db $B0
L0D66BF: db $07
L0D66C0: db $AD
L0D66C1: db $03
L0D66C2: db $E4
L0D66C3: db $68
L0D66C4: db $B0
L0D66C5: db $04
L0D66C6: db $E4
L0D66C7: db $88
L0D66C8: db $A9
L0D66C9: db $07
L0D66CA: db $E4
L0D66CB: db $68
L0D66CC: db $AD
L0D66CD: db $03
L0D66CE: db $A9
L0D66CF: db $04
L0D66D0: db $E4
L0D66D1: db $88
L0D66D2: db $B0
L0D66D3: db $07
L0D66D4: db $AD
L0D66D5: db $03
L0D66D6: db $E4
L0D66D7: db $68
L0D66D8: db $B0
L0D66D9: db $04
L0D66DA: db $E4
L0D66DB: db $88
L0D66DC: db $A4
L0D66DD: db $07
L0D66DE: db $E4
L0D66DF: db $68
L0D66E0: db $AD
L0D66E1: db $03
L0D66E2: db $A4
L0D66E3: db $04
L0D66E4: db $E4
L0D66E5: db $88
L0D66E6: db $A9
L0D66E7: db $1C
L0D66E8: db $E4
L0D66E9: db $68
L0D66EA: db $A9
L0D66EB: db $07
L0D66EC: db $80
L0D66ED: db $03
L0D66EE: db $E4
L0D66EF: db $48
L0D66F0: db $A9
L0D66F1: db $04
L0D66F2: db $E4
L0D66F3: db $38
L0D66F4: db $A9
L0D66F5: db $07
L0D66F6: db $80
L0D66F7: db $E5
L0D66F8: db $75
L0D66F9: db $63
L0D66FA: db $EE
L0D66FB: db $40
L0D66FC: db $E4
L0D66FD: db $88
L0D66FE: db $9D
L0D66FF: db $0B
L0D6700: db $E4
L0D6701: db $68
L0D6702: db $9D
L0D6703: db $07
L0D6704: db $80
L0D6705: db $03
L0D6706: db $E4
L0D6707: db $88
L0D6708: db $9D
L0D6709: db $07
L0D670A: db $A1
L0D670B: db $03
L0D670C: db $E4
L0D670D: db $68
L0D670E: db $9D
L0D670F: db $04
L0D6710: db $E4
L0D6711: db $88
L0D6712: db $A2
L0D6713: db $03
L0D6714: db $E4
L0D6715: db $68
L0D6716: db $A1
L0D6717: db $04
L0D6718: db $80
L0D6719: db $03
L0D671A: db $A2
L0D671B: db $04
L0D671C: db $E4
L0D671D: db $88
L0D671E: db $A4
L0D671F: db $07
L0D6720: db $80
L0D6721: db $03
L0D6722: db $E4
L0D6723: db $68
L0D6724: db $A4
L0D6725: db $04
L0D6726: db $E4
L0D6727: db $88
L0D6728: db $AB
L0D6729: db $0B
L0D672A: db $E4
L0D672B: db $68
L0D672C: db $AB
L0D672D: db $03
L0D672E: db $E4
L0D672F: db $88
L0D6730: db $AB
L0D6731: db $07
L0D6732: db $A9
L0D6733: db $03
L0D6734: db $E4
L0D6735: db $68
L0D6736: db $AB
L0D6737: db $04
L0D6738: db $E4
L0D6739: db $88
L0D673A: db $A7
L0D673B: db $03
L0D673C: db $E4
L0D673D: db $68
L0D673E: db $A9
L0D673F: db $04
L0D6740: db $E4
L0D6741: db $88
L0D6742: db $A9
L0D6743: db $03
L0D6744: db $E4
L0D6745: db $68
L0D6746: db $A7
L0D6747: db $04
L0D6748: db $80
L0D6749: db $03
L0D674A: db $A9
L0D674B: db $04
L0D674C: db $E4
L0D674D: db $88
L0D674E: db $A7
L0D674F: db $0B
L0D6750: db $E4
L0D6751: db $68
L0D6752: db $A7
L0D6753: db $03
L0D6754: db $E4
L0D6755: db $88
L0D6756: db $A6
L0D6757: db $05
L0D6758: db $80
L0D6759: db $02
L0D675A: db $A6
L0D675B: db $1C
L0D675C: db $E4
L0D675D: db $68
L0D675E: db $A6
L0D675F: db $07
L0D6760: db $80
L0D6761: db $03
L0D6762: db $E4
L0D6763: db $48
L0D6764: db $A6
L0D6765: db $04
L0D6766: db $EE
L0D6767: db $80
L0D6768: db $E4
L0D6769: db $82
L0D676A: db $AD
L0D676B: db $07
L0D676C: db $80
L0D676D: db $03
L0D676E: db $E4
L0D676F: db $62
L0D6770: db $AD
L0D6771: db $04
L0D6772: db $E4
L0D6773: db $82
L0D6774: db $AD
L0D6775: db $07
L0D6776: db $AE
L0D6777: db $03
L0D6778: db $E4
L0D6779: db $62
L0D677A: db $AD
L0D677B: db $04
L0D677C: db $E4
L0D677D: db $82
L0D677E: db $AE
L0D677F: db $03
L0D6780: db $E4
L0D6781: db $62
L0D6782: db $AE
L0D6783: db $04
L0D6784: db $E4
L0D6785: db $82
L0D6786: db $AD
L0D6787: db $07
L0D6788: db $80
L0D6789: db $03
L0D678A: db $E4
L0D678B: db $62
L0D678C: db $AD
L0D678D: db $04
L0D678E: db $ED
L0D678F: db $EE
L0D6790: db $40
L0D6791: db $E4
L0D6792: db $88
L0D6793: db $9D
L0D6794: db $0B
L0D6795: db $E4
L0D6796: db $68
L0D6797: db $9D
L0D6798: db $07
L0D6799: db $80
L0D679A: db $03
L0D679B: db $E4
L0D679C: db $88
L0D679D: db $9D
L0D679E: db $07
L0D679F: db $A1
L0D67A0: db $03
L0D67A1: db $E4
L0D67A2: db $68
L0D67A3: db $9D
L0D67A4: db $04
L0D67A5: db $E4
L0D67A6: db $88
L0D67A7: db $A2
L0D67A8: db $03
L0D67A9: db $E4
L0D67AA: db $68
L0D67AB: db $A1
L0D67AC: db $04
L0D67AD: db $80
L0D67AE: db $03
L0D67AF: db $A2
L0D67B0: db $04
L0D67B1: db $E4
L0D67B2: db $88
L0D67B3: db $A4
L0D67B4: db $07
L0D67B5: db $80
L0D67B6: db $03
L0D67B7: db $E4
L0D67B8: db $68
L0D67B9: db $A4
L0D67BA: db $04
L0D67BB: db $E4
L0D67BC: db $88
L0D67BD: db $A9
L0D67BE: db $0B
L0D67BF: db $E4
L0D67C0: db $68
L0D67C1: db $A9
L0D67C2: db $03
L0D67C3: db $E4
L0D67C4: db $88
L0D67C5: db $A9
L0D67C6: db $07
L0D67C7: db $A9
L0D67C8: db $03
L0D67C9: db $E4
L0D67CA: db $68
L0D67CB: db $A9
L0D67CC: db $04
L0D67CD: db $E4
L0D67CE: db $88
L0D67CF: db $A8
L0D67D0: db $03
L0D67D1: db $E4
L0D67D2: db $68
L0D67D3: db $A9
L0D67D4: db $04
L0D67D5: db $E4
L0D67D6: db $88
L0D67D7: db $A9
L0D67D8: db $03
L0D67D9: db $E4
L0D67DA: db $68
L0D67DB: db $A8
L0D67DC: db $04
L0D67DD: db $E4
L0D67DE: db $88
L0D67DF: db $AB
L0D67E0: db $03
L0D67E1: db $E4
L0D67E2: db $68
L0D67E3: db $A9
L0D67E4: db $04
L0D67E5: db $E4
L0D67E6: db $88
L0D67E7: db $AD
L0D67E8: db $1C
L0D67E9: db $E4
L0D67EA: db $68
L0D67EB: db $AD
L0D67EC: db $07
L0D67ED: db $80
L0D67EE: db $03
L0D67EF: db $E4
L0D67F0: db $48
L0D67F1: db $AD
L0D67F2: db $04
L0D67F3: db $E4
L0D67F4: db $A8
L0D67F5: db $A4
L0D67F6: db $07
L0D67F7: db $A7
L0D67F8: db $03
L0D67F9: db $E4
L0D67FA: db $88
L0D67FB: db $A4
L0D67FC: db $04
L0D67FD: db $E4
L0D67FE: db $A8
L0D67FF: db $A9
L0D6800: db $03
L0D6801: db $E4
L0D6802: db $88
L0D6803: db $A7
L0D6804: db $04
L0D6805: db $80
L0D6806: db $03
L0D6807: db $A9
L0D6808: db $04
L0D6809: db $EE
L0D680A: db $80
L0D680B: db $E4
L0D680C: db $82
L0D680D: db $AE
L0D680E: db $0B
L0D680F: db $E4
L0D6810: db $62
L0D6811: db $AE
L0D6812: db $03
L0D6813: db $E4
L0D6814: db $82
L0D6815: db $AE
L0D6816: db $0B
L0D6817: db $E4
L0D6818: db $62
L0D6819: db $AE
L0D681A: db $03
L0D681B: db $E4
L0D681C: db $82
L0D681D: db $AD
L0D681E: db $0B
L0D681F: db $E4
L0D6820: db $62
L0D6821: db $AD
L0D6822: db $03
L0D6823: db $ED
L0D6824: db $E4
L0D6825: db $82
L0D6826: db $AD
L0D6827: db $03
L0D6828: db $E4
L0D6829: db $62
L0D682A: db $A1
L0D682B: db $04
L0D682C: db $E4
L0D682D: db $82
L0D682E: db $A9
L0D682F: db $03
L0D6830: db $E4
L0D6831: db $62
L0D6832: db $AD
L0D6833: db $04
L0D6834: db $E4
L0D6835: db $82
L0D6836: db $A4
L0D6837: db $03
L0D6838: db $E4
L0D6839: db $62
L0D683A: db $A9
L0D683B: db $04
L0D683C: db $E4
L0D683D: db $82
L0D683E: db $A1
L0D683F: db $03
L0D6840: db $E4
L0D6841: db $62
L0D6842: db $A4
L0D6843: db $04
L0D6844: db $E7
L0D6845: db $00
L0D6846: db $04
L0D6847: db $24
L0D6848: db $68
L0D6849: db $ED
L0D684A: db $E4
L0D684B: db $82
L0D684C: db $A9
L0D684D: db $03
L0D684E: db $E4
L0D684F: db $62
L0D6850: db $9D
L0D6851: db $04
L0D6852: db $E4
L0D6853: db $82
L0D6854: db $A6
L0D6855: db $03
L0D6856: db $E4
L0D6857: db $62
L0D6858: db $A9
L0D6859: db $04
L0D685A: db $E4
L0D685B: db $82
L0D685C: db $A2
L0D685D: db $03
L0D685E: db $E4
L0D685F: db $62
L0D6860: db $A6
L0D6861: db $04
L0D6862: db $E4
L0D6863: db $82
L0D6864: db $9D
L0D6865: db $03
L0D6866: db $E4
L0D6867: db $62
L0D6868: db $A2
L0D6869: db $04
L0D686A: db $E7
L0D686B: db $00
L0D686C: db $02
L0D686D: db $4A
L0D686E: db $68
L0D686F: db $ED
L0D6870: db $E4
L0D6871: db $82
L0D6872: db $AE
L0D6873: db $03
L0D6874: db $E4
L0D6875: db $62
L0D6876: db $A2
L0D6877: db $04
L0D6878: db $E4
L0D6879: db $82
L0D687A: db $AB
L0D687B: db $03
L0D687C: db $E4
L0D687D: db $62
L0D687E: db $AE
L0D687F: db $04
L0D6880: db $E4
L0D6881: db $82
L0D6882: db $A7
L0D6883: db $03
L0D6884: db $E4
L0D6885: db $62
L0D6886: db $AB
L0D6887: db $04
L0D6888: db $E4
L0D6889: db $82
L0D688A: db $A2
L0D688B: db $03
L0D688C: db $E4
L0D688D: db $62
L0D688E: db $A7
L0D688F: db $04
L0D6890: db $E7
L0D6891: db $00
L0D6892: db $02
L0D6893: db $70
L0D6894: db $68
L0D6895: db $ED
L0D6896: db $E4
L0D6897: db $82
L0D6898: db $A9
L0D6899: db $03
L0D689A: db $E4
L0D689B: db $62
L0D689C: db $9D
L0D689D: db $04
L0D689E: db $E4
L0D689F: db $82
L0D68A0: db $A4
L0D68A1: db $03
L0D68A2: db $E4
L0D68A3: db $62
L0D68A4: db $A9
L0D68A5: db $04
L0D68A6: db $E4
L0D68A7: db $82
L0D68A8: db $A1
L0D68A9: db $03
L0D68AA: db $E4
L0D68AB: db $62
L0D68AC: db $A4
L0D68AD: db $04
L0D68AE: db $E4
L0D68AF: db $82
L0D68B0: db $9D
L0D68B1: db $03
L0D68B2: db $E4
L0D68B3: db $62
L0D68B4: db $A1
L0D68B5: db $04
L0D68B6: db $E7
L0D68B7: db $00
L0D68B8: db $02
L0D68B9: db $96
L0D68BA: db $68
L0D68BB: db $ED
L0D68BC: db $E4
L0D68BD: db $88
L0D68BE: db $AE
L0D68BF: db $07
L0D68C0: db $AB
L0D68C1: db $03
L0D68C2: db $E4
L0D68C3: db $68
L0D68C4: db $AE
L0D68C5: db $04
L0D68C6: db $E4
L0D68C7: db $88
L0D68C8: db $A7
L0D68C9: db $07
L0D68CA: db $E4
L0D68CB: db $68
L0D68CC: db $AB
L0D68CD: db $03
L0D68CE: db $A7
L0D68CF: db $04
L0D68D0: db $E7
L0D68D1: db $00
L0D68D2: db $02
L0D68D3: db $BC
L0D68D4: db $68
L0D68D5: db $ED
L0D68D6: db $E4
L0D68D7: db $88
L0D68D8: db $B2
L0D68D9: db $07
L0D68DA: db $AE
L0D68DB: db $03
L0D68DC: db $E4
L0D68DD: db $68
L0D68DE: db $B2
L0D68DF: db $04
L0D68E0: db $E4
L0D68E1: db $88
L0D68E2: db $A9
L0D68E3: db $07
L0D68E4: db $E4
L0D68E5: db $68
L0D68E6: db $AE
L0D68E7: db $03
L0D68E8: db $A9
L0D68E9: db $04
L0D68EA: db $E7
L0D68EB: db $00
L0D68EC: db $02
L0D68ED: db $D6
L0D68EE: db $68
L0D68EF: db $ED
L0D68F0: db $E4
L0D68F1: db $88
L0D68F2: db $B0
L0D68F3: db $07
L0D68F4: db $AD
L0D68F5: db $03
L0D68F6: db $E4
L0D68F7: db $68
L0D68F8: db $B0
L0D68F9: db $04
L0D68FA: db $E4
L0D68FB: db $88
L0D68FC: db $A9
L0D68FD: db $07
L0D68FE: db $E4
L0D68FF: db $68
L0D6900: db $AD
L0D6901: db $03
L0D6902: db $A9
L0D6903: db $04
L0D6904: db $E7
L0D6905: db $00
L0D6906: db $02
L0D6907: db $F0
L0D6908: db $68
L0D6909: db $ED
L0D690A: db $E4
L0D690B: db $88
L0D690C: db $AE
L0D690D: db $07
L0D690E: db $A9
L0D690F: db $03
L0D6910: db $E4
L0D6911: db $68
L0D6912: db $AE
L0D6913: db $04
L0D6914: db $E4
L0D6915: db $88
L0D6916: db $A6
L0D6917: db $07
L0D6918: db $E4
L0D6919: db $68
L0D691A: db $A9
L0D691B: db $03
L0D691C: db $A6
L0D691D: db $04
L0D691E: db $E7
L0D691F: db $00
L0D6920: db $02
L0D6921: db $0A
L0D6922: db $69
L0D6923: db $ED
L0D6924: db $E4
L0D6925: db $20
L0D6926: db $E9
L0D6927: db $44
L0D6928: db $F3
L0D6929: db $03
L0D692A: db $F5
L0D692B: db $00
L0D692C: db $EC
L0D692D: db $C1
L0D692E: db $69
L0D692F: db $EC
L0D6930: db $F2
L0D6931: db $69
L0D6932: db $EC
L0D6933: db $C1
L0D6934: db $69
L0D6935: db $EC
L0D6936: db $0C
L0D6937: db $6A
L0D6938: db $EC
L0D6939: db $C1
L0D693A: db $69
L0D693B: db $EC
L0D693C: db $F2
L0D693D: db $69
L0D693E: db $EC
L0D693F: db $C1
L0D6940: db $69
L0D6941: db $EC
L0D6942: db $0C
L0D6943: db $6A
L0D6944: db $EC
L0D6945: db $29
L0D6946: db $6A
L0D6947: db $E6
L0D6948: db $05
L0D6949: db $EC
L0D694A: db $29
L0D694B: db $6A
L0D694C: db $E6
L0D694D: db $FB
L0D694E: db $9B
L0D694F: db $0E
L0D6950: db $93
L0D6951: db $07
L0D6952: db $95
L0D6953: db $80
L0D6954: db $96
L0D6955: db $9B
L0D6956: db $0E
L0D6957: db $9D
L0D6958: db $95
L0D6959: db $07
L0D695A: db $98
L0D695B: db $80
L0D695C: db $9B
L0D695D: db $9D
L0D695E: db $0E
L0D695F: db $EC
L0D6960: db $38
L0D6961: db $6A
L0D6962: db $95
L0D6963: db $0E
L0D6964: db $A1
L0D6965: db $07
L0D6966: db $95
L0D6967: db $80
L0D6968: db $95
L0D6969: db $A1
L0D696A: db $0E
L0D696B: db $91
L0D696C: db $9D
L0D696D: db $95
L0D696E: db $98
L0D696F: db $EC
L0D6970: db $38
L0D6971: db $6A
L0D6972: db $90
L0D6973: db $07
L0D6974: db $93
L0D6975: db $96
L0D6976: db $90
L0D6977: db $0E
L0D6978: db $96
L0D6979: db $07
L0D697A: db $80
L0D697B: db $98
L0D697C: db $13
L0D697D: db $80
L0D697E: db $02
L0D697F: db $98
L0D6980: db $0E
L0D6981: db $93
L0D6982: db $98
L0D6983: db $80
L0D6984: db $07
L0D6985: db $9D
L0D6986: db $0C
L0D6987: db $80
L0D6988: db $02
L0D6989: db $9D
L0D698A: db $07
L0D698B: db $98
L0D698C: db $9D
L0D698D: db $80
L0D698E: db $9D
L0D698F: db $91
L0D6990: db $80
L0D6991: db $91
L0D6992: db $0E
L0D6993: db $93
L0D6994: db $95
L0D6995: db $EC
L0D6996: db $46
L0D6997: db $6A
L0D6998: db $9D
L0D6999: db $0E
L0D699A: db $80
L0D699B: db $07
L0D699C: db $98
L0D699D: db $15
L0D699E: db $9D
L0D699F: db $0E
L0D69A0: db $96
L0D69A1: db $80
L0D69A2: db $07
L0D69A3: db $98
L0D69A4: db $15
L0D69A5: db $9A
L0D69A6: db $0E
L0D69A7: db $EC
L0D69A8: db $46
L0D69A9: db $6A
L0D69AA: db $8F
L0D69AB: db $07
L0D69AC: db $80
L0D69AD: db $9B
L0D69AE: db $0E
L0D69AF: db $91
L0D69B0: db $07
L0D69B1: db $80
L0D69B2: db $9D
L0D69B3: db $0E
L0D69B4: db $96
L0D69B5: db $13
L0D69B6: db $80
L0D69B7: db $02
L0D69B8: db $A2
L0D69B9: db $13
L0D69BA: db $80
L0D69BB: db $02
L0D69BC: db $A2
L0D69BD: db $0E
L0D69BE: db $E5
L0D69BF: db $24
L0D69C0: db $69
L0D69C1: db $96
L0D69C2: db $13
L0D69C3: db $80
L0D69C4: db $02
L0D69C5: db $96
L0D69C6: db $07
L0D69C7: db $80
L0D69C8: db $96
L0D69C9: db $80
L0D69CA: db $9B
L0D69CB: db $0E
L0D69CC: db $93
L0D69CD: db $9B
L0D69CE: db $05
L0D69CF: db $80
L0D69D0: db $02
L0D69D1: db $9B
L0D69D2: db $0E
L0D69D3: db $9A
L0D69D4: db $98
L0D69D5: db $13
L0D69D6: db $80
L0D69D7: db $02
L0D69D8: db $98
L0D69D9: db $07
L0D69DA: db $80
L0D69DB: db $91
L0D69DC: db $80
L0D69DD: db $9D
L0D69DE: db $0E
L0D69DF: db $98
L0D69E0: db $9D
L0D69E1: db $07
L0D69E2: db $9B
L0D69E3: db $0E
L0D69E4: db $9A
L0D69E5: db $96
L0D69E6: db $13
L0D69E7: db $80
L0D69E8: db $02
L0D69E9: db $96
L0D69EA: db $07
L0D69EB: db $80
L0D69EC: db $91
L0D69ED: db $80
L0D69EE: db $9B
L0D69EF: db $0E
L0D69F0: db $93
L0D69F1: db $ED
L0D69F2: db $9B
L0D69F3: db $07
L0D69F4: db $9A
L0D69F5: db $0E
L0D69F6: db $9B
L0D69F7: db $07
L0D69F8: db $9C
L0D69F9: db $9D
L0D69FA: db $0E
L0D69FB: db $80
L0D69FC: db $07
L0D69FD: db $9D
L0D69FE: db $91
L0D69FF: db $9D
L0D6A00: db $80
L0D6A01: db $9D
L0D6A02: db $0C
L0D6A03: db $80
L0D6A04: db $02
L0D6A05: db $9D
L0D6A06: db $07
L0D6A07: db $9B
L0D6A08: db $0E
L0D6A09: db $9A
L0D6A0A: db $98
L0D6A0B: db $ED
L0D6A0C: db $9B
L0D6A0D: db $07
L0D6A0E: db $9D
L0D6A0F: db $80
L0D6A10: db $9D
L0D6A11: db $05
L0D6A12: db $80
L0D6A13: db $02
L0D6A14: db $9D
L0D6A15: db $07
L0D6A16: db $96
L0D6A17: db $0E
L0D6A18: db $80
L0D6A19: db $07
L0D6A1A: db $96
L0D6A1B: db $80
L0D6A1C: db $91
L0D6A1D: db $93
L0D6A1E: db $94
L0D6A1F: db $80
L0D6A20: db $95
L0D6A21: db $0C
L0D6A22: db $80
L0D6A23: db $02
L0D6A24: db $95
L0D6A25: db $07
L0D6A26: db $96
L0D6A27: db $1C
L0D6A28: db $ED
L0D6A29: db $91
L0D6A2A: db $0E
L0D6A2B: db $9D
L0D6A2C: db $07
L0D6A2D: db $91
L0D6A2E: db $80
L0D6A2F: db $91
L0D6A30: db $9D
L0D6A31: db $0E
L0D6A32: db $E7
L0D6A33: db $00
L0D6A34: db $02
L0D6A35: db $29
L0D6A36: db $6A
L0D6A37: db $ED
L0D6A38: db $96
L0D6A39: db $0E
L0D6A3A: db $A2
L0D6A3B: db $07
L0D6A3C: db $96
L0D6A3D: db $80
L0D6A3E: db $96
L0D6A3F: db $A2
L0D6A40: db $0E
L0D6A41: db $96
L0D6A42: db $A2
L0D6A43: db $94
L0D6A44: db $A0
L0D6A45: db $ED
L0D6A46: db $9B
L0D6A47: db $0E
L0D6A48: db $80
L0D6A49: db $07
L0D6A4A: db $93
L0D6A4B: db $15
L0D6A4C: db $9B
L0D6A4D: db $0E
L0D6A4E: db $96
L0D6A4F: db $80
L0D6A50: db $07
L0D6A51: db $91
L0D6A52: db $15
L0D6A53: db $96
L0D6A54: db $0E
L0D6A55: db $ED
L0D6A56: db $E9
L0D6A57: db $88
L0D6A58: db $EC
L0D6A59: db $4D
L0D6A5A: db $6B
L0D6A5B: db $E4
L0D6A5C: db $C1
L0D6A5D: db $57
L0D6A5E: db $07
L0D6A5F: db $E4
L0D6A60: db $51
L0D6A61: db $17
L0D6A62: db $07
L0D6A63: db $17
L0D6A64: db $07
L0D6A65: db $17
L0D6A66: db $07
L0D6A67: db $E4
L0D6A68: db $A1
L0D6A69: db $26
L0D6A6A: db $07
L0D6A6B: db $E4
L0D6A6C: db $51
L0D6A6D: db $17
L0D6A6E: db $07
L0D6A6F: db $17
L0D6A70: db $07
L0D6A71: db $17
L0D6A72: db $07
L0D6A73: db $E4
L0D6A74: db $C1
L0D6A75: db $57
L0D6A76: db $07
L0D6A77: db $E4
L0D6A78: db $51
L0D6A79: db $17
L0D6A7A: db $07
L0D6A7B: db $17
L0D6A7C: db $07
L0D6A7D: db $17
L0D6A7E: db $07
L0D6A7F: db $E4
L0D6A80: db $A1
L0D6A81: db $26
L0D6A82: db $07
L0D6A83: db $E4
L0D6A84: db $51
L0D6A85: db $17
L0D6A86: db $07
L0D6A87: db $E4
L0D6A88: db $A1
L0D6A89: db $26
L0D6A8A: db $07
L0D6A8B: db $26
L0D6A8C: db $07
L0D6A8D: db $EC
L0D6A8E: db $4D
L0D6A8F: db $6B
L0D6A90: db $E4
L0D6A91: db $C1
L0D6A92: db $57
L0D6A93: db $07
L0D6A94: db $E4
L0D6A95: db $51
L0D6A96: db $17
L0D6A97: db $07
L0D6A98: db $17
L0D6A99: db $07
L0D6A9A: db $17
L0D6A9B: db $07
L0D6A9C: db $E4
L0D6A9D: db $A1
L0D6A9E: db $26
L0D6A9F: db $07
L0D6AA0: db $E4
L0D6AA1: db $51
L0D6AA2: db $17
L0D6AA3: db $07
L0D6AA4: db $17
L0D6AA5: db $07
L0D6AA6: db $17
L0D6AA7: db $07
L0D6AA8: db $E4
L0D6AA9: db $A1
L0D6AAA: db $26
L0D6AAB: db $07
L0D6AAC: db $E4
L0D6AAD: db $51
L0D6AAE: db $17
L0D6AAF: db $07
L0D6AB0: db $E4
L0D6AB1: db $A1
L0D6AB2: db $26
L0D6AB3: db $07
L0D6AB4: db $E4
L0D6AB5: db $51
L0D6AB6: db $17
L0D6AB7: db $07
L0D6AB8: db $E4
L0D6AB9: db $A1
L0D6ABA: db $26
L0D6ABB: db $07
L0D6ABC: db $E4
L0D6ABD: db $51
L0D6ABE: db $17
L0D6ABF: db $07
L0D6AC0: db $E4
L0D6AC1: db $A1
L0D6AC2: db $26
L0D6AC3: db $07
L0D6AC4: db $26
L0D6AC5: db $07
L0D6AC6: db $EC
L0D6AC7: db $8B
L0D6AC8: db $6B
L0D6AC9: db $EC
L0D6ACA: db $56
L0D6ACB: db $6B
L0D6ACC: db $EC
L0D6ACD: db $8B
L0D6ACE: db $6B
L0D6ACF: db $EC
L0D6AD0: db $56
L0D6AD1: db $6B
L0D6AD2: db $EC
L0D6AD3: db $8B
L0D6AD4: db $6B
L0D6AD5: db $EC
L0D6AD6: db $56
L0D6AD7: db $6B
L0D6AD8: db $EC
L0D6AD9: db $8B
L0D6ADA: db $6B
L0D6ADB: db $E4
L0D6ADC: db $C1
L0D6ADD: db $57
L0D6ADE: db $07
L0D6ADF: db $E4
L0D6AE0: db $A1
L0D6AE1: db $26
L0D6AE2: db $07
L0D6AE3: db $E4
L0D6AE4: db $53
L0D6AE5: db $13
L0D6AE6: db $07
L0D6AE7: db $E4
L0D6AE8: db $A1
L0D6AE9: db $26
L0D6AEA: db $07
L0D6AEB: db $26
L0D6AEC: db $07
L0D6AED: db $26
L0D6AEE: db $07
L0D6AEF: db $E4
L0D6AF0: db $53
L0D6AF1: db $13
L0D6AF2: db $07
L0D6AF3: db $E4
L0D6AF4: db $A1
L0D6AF5: db $26
L0D6AF6: db $07
L0D6AF7: db $26
L0D6AF8: db $07
L0D6AF9: db $E4
L0D6AFA: db $51
L0D6AFB: db $17
L0D6AFC: db $07
L0D6AFD: db $E4
L0D6AFE: db $A1
L0D6AFF: db $26
L0D6B00: db $07
L0D6B01: db $E4
L0D6B02: db $51
L0D6B03: db $17
L0D6B04: db $07
L0D6B05: db $E4
L0D6B06: db $A1
L0D6B07: db $26
L0D6B08: db $07
L0D6B09: db $26
L0D6B0A: db $07
L0D6B0B: db $E4
L0D6B0C: db $91
L0D6B0D: db $34
L0D6B0E: db $07
L0D6B0F: db $E4
L0D6B10: db $91
L0D6B11: db $35
L0D6B12: db $07
L0D6B13: db $EC
L0D6B14: db $56
L0D6B15: db $6B
L0D6B16: db $EC
L0D6B17: db $56
L0D6B18: db $6B
L0D6B19: db $EC
L0D6B1A: db $56
L0D6B1B: db $6B
L0D6B1C: db $E4
L0D6B1D: db $C1
L0D6B1E: db $57
L0D6B1F: db $07
L0D6B20: db $E4
L0D6B21: db $51
L0D6B22: db $17
L0D6B23: db $07
L0D6B24: db $17
L0D6B25: db $07
L0D6B26: db $17
L0D6B27: db $07
L0D6B28: db $E4
L0D6B29: db $A1
L0D6B2A: db $26
L0D6B2B: db $07
L0D6B2C: db $E4
L0D6B2D: db $51
L0D6B2E: db $17
L0D6B2F: db $07
L0D6B30: db $17
L0D6B31: db $07
L0D6B32: db $17
L0D6B33: db $07
L0D6B34: db $E4
L0D6B35: db $A1
L0D6B36: db $26
L0D6B37: db $07
L0D6B38: db $26
L0D6B39: db $07
L0D6B3A: db $E4
L0D6B3B: db $91
L0D6B3C: db $35
L0D6B3D: db $07
L0D6B3E: db $E4
L0D6B3F: db $53
L0D6B40: db $13
L0D6B41: db $0E
L0D6B42: db $E4
L0D6B43: db $91
L0D6B44: db $35
L0D6B45: db $07
L0D6B46: db $E4
L0D6B47: db $91
L0D6B48: db $37
L0D6B49: db $0E
L0D6B4A: db $E5
L0D6B4B: db $56
L0D6B4C: db $6A
L0D6B4D: db $EC
L0D6B4E: db $56
L0D6B4F: db $6B
L0D6B50: db $E7
L0D6B51: db $00
L0D6B52: db $07
L0D6B53: db $4D
L0D6B54: db $6B
L0D6B55: db $ED
L0D6B56: db $E4
L0D6B57: db $C1
L0D6B58: db $57
L0D6B59: db $07
L0D6B5A: db $E4
L0D6B5B: db $51
L0D6B5C: db $17
L0D6B5D: db $07
L0D6B5E: db $17
L0D6B5F: db $07
L0D6B60: db $17
L0D6B61: db $07
L0D6B62: db $E4
L0D6B63: db $A1
L0D6B64: db $26
L0D6B65: db $07
L0D6B66: db $E4
L0D6B67: db $51
L0D6B68: db $17
L0D6B69: db $07
L0D6B6A: db $17
L0D6B6B: db $07
L0D6B6C: db $17
L0D6B6D: db $07
L0D6B6E: db $E4
L0D6B6F: db $C1
L0D6B70: db $57
L0D6B71: db $07
L0D6B72: db $E4
L0D6B73: db $51
L0D6B74: db $17
L0D6B75: db $07
L0D6B76: db $17
L0D6B77: db $07
L0D6B78: db $17
L0D6B79: db $07
L0D6B7A: db $E4
L0D6B7B: db $A1
L0D6B7C: db $26
L0D6B7D: db $07
L0D6B7E: db $E4
L0D6B7F: db $51
L0D6B80: db $17
L0D6B81: db $07
L0D6B82: db $E4
L0D6B83: db $53
L0D6B84: db $13
L0D6B85: db $07
L0D6B86: db $E4
L0D6B87: db $C1
L0D6B88: db $57
L0D6B89: db $07
L0D6B8A: db $ED
L0D6B8B: db $E4
L0D6B8C: db $C1
L0D6B8D: db $57
L0D6B8E: db $07
L0D6B8F: db $E4
L0D6B90: db $51
L0D6B91: db $17
L0D6B92: db $07
L0D6B93: db $17
L0D6B94: db $07
L0D6B95: db $17
L0D6B96: db $07
L0D6B97: db $E4
L0D6B98: db $A1
L0D6B99: db $26
L0D6B9A: db $07
L0D6B9B: db $E4
L0D6B9C: db $51
L0D6B9D: db $17
L0D6B9E: db $07
L0D6B9F: db $17
L0D6BA0: db $07
L0D6BA1: db $17
L0D6BA2: db $07
L0D6BA3: db $E4
L0D6BA4: db $C1
L0D6BA5: db $57
L0D6BA6: db $07
L0D6BA7: db $E4
L0D6BA8: db $51
L0D6BA9: db $17
L0D6BAA: db $07
L0D6BAB: db $17
L0D6BAC: db $07
L0D6BAD: db $17
L0D6BAE: db $07
L0D6BAF: db $E4
L0D6BB0: db $A1
L0D6BB1: db $26
L0D6BB2: db $07
L0D6BB3: db $E4
L0D6BB4: db $51
L0D6BB5: db $17
L0D6BB6: db $07
L0D6BB7: db $E4
L0D6BB8: db $A1
L0D6BB9: db $26
L0D6BBA: db $07
L0D6BBB: db $E4
L0D6BBC: db $53
L0D6BBD: db $13
L0D6BBE: db $07
L0D6BBF: db $ED
SndHeader_BGM_0A: db $04
L0D6BC1: db $80
L0D6BC2: db $13
L0D6BC3: db $D9
L0D6BC4: db $6B
L0D6BC5: db $00
L0D6BC6: db $81
L0D6BC7: db $80
L0D6BC8: db $18
L0D6BC9: db $EB
L0D6BCA: db $6F
L0D6BCB: db $00
L0D6BCC: db $81
L0D6BCD: db $80
L0D6BCE: db $1D
L0D6BCF: db $E0
L0D6BD0: db $71
L0D6BD1: db $00
L0D6BD2: db $81
L0D6BD3: db $80
L0D6BD4: db $22
L0D6BD5: db $86
L0D6BD6: db $73
L0D6BD7: db $00
L0D6BD8: db $81
L0D6BD9: db $E4
L0D6BDA: db $A8
L0D6BDB: db $E9
L0D6BDC: db $11
L0D6BDD: db $EE
L0D6BDE: db $80
L0D6BDF: db $F1
L0D6BE0: db $03
L0D6BE1: db $EC
L0D6BE2: db $5B
L0D6BE3: db $6D
L0D6BE4: db $EC
L0D6BE5: db $E4
L0D6BE6: db $6D
L0D6BE7: db $EC
L0D6BE8: db $5C
L0D6BE9: db $6E
L0D6BEA: db $E6
L0D6BEB: db $05
L0D6BEC: db $EC
L0D6BED: db $5C
L0D6BEE: db $6E
L0D6BEF: db $E6
L0D6BF0: db $FB
L0D6BF1: db $EC
L0D6BF2: db $AA
L0D6BF3: db $6E
L0D6BF4: db $EC
L0D6BF5: db $42
L0D6BF6: db $6F
L0D6BF7: db $EC
L0D6BF8: db $FD
L0D6BF9: db $6B
L0D6BFA: db $E5
L0D6BFB: db $D9
L0D6BFC: db $6B
L0D6BFD: db $EC
L0D6BFE: db $F8
L0D6BFF: db $6E
L0D6C00: db $E4
L0D6C01: db $A8
L0D6C02: db $97
L0D6C03: db $0A
L0D6C04: db $9A
L0D6C05: db $05
L0D6C06: db $80
L0D6C07: db $02
L0D6C08: db $E4
L0D6C09: db $88
L0D6C0A: db $9A
L0D6C0B: db $03
L0D6C0C: db $E4
L0D6C0D: db $A8
L0D6C0E: db $97
L0D6C0F: db $0A
L0D6C10: db $9C
L0D6C11: db $9A
L0D6C12: db $05
L0D6C13: db $80
L0D6C14: db $02
L0D6C15: db $E4
L0D6C16: db $88
L0D6C17: db $9A
L0D6C18: db $03
L0D6C19: db $E4
L0D6C1A: db $A8
L0D6C1B: db $97
L0D6C1C: db $14
L0D6C1D: db $EC
L0D6C1E: db $F8
L0D6C1F: db $6E
L0D6C20: db $E4
L0D6C21: db $A8
L0D6C22: db $97
L0D6C23: db $0A
L0D6C24: db $9A
L0D6C25: db $05
L0D6C26: db $80
L0D6C27: db $02
L0D6C28: db $E4
L0D6C29: db $88
L0D6C2A: db $9A
L0D6C2B: db $03
L0D6C2C: db $E4
L0D6C2D: db $A8
L0D6C2E: db $97
L0D6C2F: db $0A
L0D6C30: db $9D
L0D6C31: db $9C
L0D6C32: db $9A
L0D6C33: db $05
L0D6C34: db $80
L0D6C35: db $02
L0D6C36: db $E4
L0D6C37: db $88
L0D6C38: db $9A
L0D6C39: db $03
L0D6C3A: db $E4
L0D6C3B: db $A8
L0D6C3C: db $9C
L0D6C3D: db $0A
L0D6C3E: db $EC
L0D6C3F: db $F8
L0D6C40: db $6E
L0D6C41: db $E4
L0D6C42: db $A8
L0D6C43: db $97
L0D6C44: db $0A
L0D6C45: db $9A
L0D6C46: db $05
L0D6C47: db $80
L0D6C48: db $02
L0D6C49: db $E4
L0D6C4A: db $88
L0D6C4B: db $9A
L0D6C4C: db $03
L0D6C4D: db $E4
L0D6C4E: db $A8
L0D6C4F: db $97
L0D6C50: db $0A
L0D6C51: db $9C
L0D6C52: db $9A
L0D6C53: db $05
L0D6C54: db $80
L0D6C55: db $02
L0D6C56: db $E4
L0D6C57: db $88
L0D6C58: db $9A
L0D6C59: db $03
L0D6C5A: db $E4
L0D6C5B: db $A8
L0D6C5C: db $97
L0D6C5D: db $14
L0D6C5E: db $EC
L0D6C5F: db $F8
L0D6C60: db $6E
L0D6C61: db $E4
L0D6C62: db $A8
L0D6C63: db $A3
L0D6C64: db $0F
L0D6C65: db $80
L0D6C66: db $02
L0D6C67: db $E4
L0D6C68: db $88
L0D6C69: db $A3
L0D6C6A: db $03
L0D6C6B: db $E4
L0D6C6C: db $A8
L0D6C6D: db $A3
L0D6C6E: db $0F
L0D6C6F: db $80
L0D6C70: db $02
L0D6C71: db $E4
L0D6C72: db $88
L0D6C73: db $A3
L0D6C74: db $03
L0D6C75: db $E4
L0D6C76: db $A8
L0D6C77: db $A3
L0D6C78: db $0F
L0D6C79: db $80
L0D6C7A: db $02
L0D6C7B: db $E4
L0D6C7C: db $88
L0D6C7D: db $A3
L0D6C7E: db $03
L0D6C7F: db $E4
L0D6C80: db $A8
L0D6C81: db $A3
L0D6C82: db $0F
L0D6C83: db $80
L0D6C84: db $02
L0D6C85: db $E4
L0D6C86: db $88
L0D6C87: db $A3
L0D6C88: db $03
L0D6C89: db $E4
L0D6C8A: db $A8
L0D6C8B: db $A8
L0D6C8C: db $05
L0D6C8D: db $80
L0D6C8E: db $02
L0D6C8F: db $E4
L0D6C90: db $88
L0D6C91: db $A8
L0D6C92: db $03
L0D6C93: db $E4
L0D6C94: db $A8
L0D6C95: db $A8
L0D6C96: db $08
L0D6C97: db $80
L0D6C98: db $02
L0D6C99: db $A8
L0D6C9A: db $08
L0D6C9B: db $80
L0D6C9C: db $02
L0D6C9D: db $A8
L0D6C9E: db $0A
L0D6C9F: db $A9
L0D6CA0: db $AA
L0D6CA1: db $14
L0D6CA2: db $EC
L0D6CA3: db $28
L0D6CA4: db $6F
L0D6CA5: db $E6
L0D6CA6: db $FB
L0D6CA7: db $EC
L0D6CA8: db $28
L0D6CA9: db $6F
L0D6CAA: db $E6
L0D6CAB: db $05
L0D6CAC: db $E4
L0D6CAD: db $A8
L0D6CAE: db $9C
L0D6CAF: db $05
L0D6CB0: db $80
L0D6CB1: db $02
L0D6CB2: db $E4
L0D6CB3: db $88
L0D6CB4: db $9C
L0D6CB5: db $03
L0D6CB6: db $E4
L0D6CB7: db $A8
L0D6CB8: db $9A
L0D6CB9: db $0F
L0D6CBA: db $80
L0D6CBB: db $02
L0D6CBC: db $E4
L0D6CBD: db $88
L0D6CBE: db $9A
L0D6CBF: db $03
L0D6CC0: db $E4
L0D6CC1: db $A8
L0D6CC2: db $9C
L0D6CC3: db $05
L0D6CC4: db $80
L0D6CC5: db $02
L0D6CC6: db $E4
L0D6CC7: db $88
L0D6CC8: db $9C
L0D6CC9: db $03
L0D6CCA: db $E4
L0D6CCB: db $A8
L0D6CCC: db $9A
L0D6CCD: db $0F
L0D6CCE: db $80
L0D6CCF: db $02
L0D6CD0: db $E4
L0D6CD1: db $88
L0D6CD2: db $9A
L0D6CD3: db $03
L0D6CD4: db $E6
L0D6CD5: db $F4
L0D6CD6: db $EC
L0D6CD7: db $28
L0D6CD8: db $6F
L0D6CD9: db $E6
L0D6CDA: db $0C
L0D6CDB: db $EC
L0D6CDC: db $28
L0D6CDD: db $6F
L0D6CDE: db $E6
L0D6CDF: db $FB
L0D6CE0: db $EC
L0D6CE1: db $28
L0D6CE2: db $6F
L0D6CE3: db $E6
L0D6CE4: db $05
L0D6CE5: db $E4
L0D6CE6: db $A8
L0D6CE7: db $9C
L0D6CE8: db $05
L0D6CE9: db $80
L0D6CEA: db $02
L0D6CEB: db $E4
L0D6CEC: db $88
L0D6CED: db $9C
L0D6CEE: db $03
L0D6CEF: db $E4
L0D6CF0: db $A8
L0D6CF1: db $9A
L0D6CF2: db $0F
L0D6CF3: db $80
L0D6CF4: db $02
L0D6CF5: db $E4
L0D6CF6: db $88
L0D6CF7: db $9A
L0D6CF8: db $03
L0D6CF9: db $E4
L0D6CFA: db $A8
L0D6CFB: db $9C
L0D6CFC: db $05
L0D6CFD: db $80
L0D6CFE: db $02
L0D6CFF: db $E4
L0D6D00: db $88
L0D6D01: db $9C
L0D6D02: db $03
L0D6D03: db $E4
L0D6D04: db $A8
L0D6D05: db $9A
L0D6D06: db $0F
L0D6D07: db $80
L0D6D08: db $02
L0D6D09: db $E4
L0D6D0A: db $88
L0D6D0B: db $9A
L0D6D0C: db $03
L0D6D0D: db $E6
L0D6D0E: db $F4
L0D6D0F: db $E4
L0D6D10: db $A8
L0D6D11: db $A6
L0D6D12: db $05
L0D6D13: db $80
L0D6D14: db $02
L0D6D15: db $E4
L0D6D16: db $88
L0D6D17: db $A6
L0D6D18: db $03
L0D6D19: db $E4
L0D6D1A: db $A8
L0D6D1B: db $A3
L0D6D1C: db $0F
L0D6D1D: db $80
L0D6D1E: db $02
L0D6D1F: db $E4
L0D6D20: db $88
L0D6D21: db $A3
L0D6D22: db $03
L0D6D23: db $E6
L0D6D24: db $0C
L0D6D25: db $E4
L0D6D26: db $A8
L0D6D27: db $9A
L0D6D28: db $05
L0D6D29: db $80
L0D6D2A: db $02
L0D6D2B: db $E4
L0D6D2C: db $88
L0D6D2D: db $9A
L0D6D2E: db $03
L0D6D2F: db $E4
L0D6D30: db $A8
L0D6D31: db $97
L0D6D32: db $1E
L0D6D33: db $E4
L0D6D34: db $88
L0D6D35: db $97
L0D6D36: db $05
L0D6D37: db $80
L0D6D38: db $02
L0D6D39: db $E4
L0D6D3A: db $68
L0D6D3B: db $97
L0D6D3C: db $03
L0D6D3D: db $E4
L0D6D3E: db $A8
L0D6D3F: db $A1
L0D6D40: db $14
L0D6D41: db $E4
L0D6D42: db $88
L0D6D43: db $A1
L0D6D44: db $05
L0D6D45: db $80
L0D6D46: db $02
L0D6D47: db $E4
L0D6D48: db $68
L0D6D49: db $A1
L0D6D4A: db $03
L0D6D4B: db $E4
L0D6D4C: db $A8
L0D6D4D: db $9C
L0D6D4E: db $05
L0D6D4F: db $80
L0D6D50: db $02
L0D6D51: db $E4
L0D6D52: db $68
L0D6D53: db $9C
L0D6D54: db $03
L0D6D55: db $E4
L0D6D56: db $A8
L0D6D57: db $9D
L0D6D58: db $0A
L0D6D59: db $9E
L0D6D5A: db $ED
L0D6D5B: db $E4
L0D6D5C: db $A8
L0D6D5D: db $9E
L0D6D5E: db $0A
L0D6D5F: db $9D
L0D6D60: db $05
L0D6D61: db $80
L0D6D62: db $02
L0D6D63: db $E4
L0D6D64: db $88
L0D6D65: db $9D
L0D6D66: db $03
L0D6D67: db $E4
L0D6D68: db $A8
L0D6D69: db $9A
L0D6D6A: db $0A
L0D6D6B: db $97
L0D6D6C: db $14
L0D6D6D: db $E4
L0D6D6E: db $88
L0D6D6F: db $97
L0D6D70: db $05
L0D6D71: db $80
L0D6D72: db $02
L0D6D73: db $E4
L0D6D74: db $68
L0D6D75: db $97
L0D6D76: db $03
L0D6D77: db $E4
L0D6D78: db $A8
L0D6D79: db $9E
L0D6D7A: db $0A
L0D6D7B: db $9D
L0D6D7C: db $05
L0D6D7D: db $80
L0D6D7E: db $02
L0D6D7F: db $E4
L0D6D80: db $88
L0D6D81: db $9D
L0D6D82: db $03
L0D6D83: db $E4
L0D6D84: db $A8
L0D6D85: db $9A
L0D6D86: db $0A
L0D6D87: db $97
L0D6D88: db $E4
L0D6D89: db $88
L0D6D8A: db $97
L0D6D8B: db $05
L0D6D8C: db $80
L0D6D8D: db $02
L0D6D8E: db $E4
L0D6D8F: db $68
L0D6D90: db $97
L0D6D91: db $03
L0D6D92: db $E4
L0D6D93: db $A8
L0D6D94: db $9E
L0D6D95: db $14
L0D6D96: db $97
L0D6D97: db $05
L0D6D98: db $80
L0D6D99: db $02
L0D6D9A: db $E4
L0D6D9B: db $88
L0D6D9C: db $97
L0D6D9D: db $03
L0D6D9E: db $E4
L0D6D9F: db $A8
L0D6DA0: db $9A
L0D6DA1: db $0A
L0D6DA2: db $9D
L0D6DA3: db $E4
L0D6DA4: db $A8
L0D6DA5: db $9E
L0D6DA6: db $0A
L0D6DA7: db $A0
L0D6DA8: db $05
L0D6DA9: db $80
L0D6DAA: db $02
L0D6DAB: db $E4
L0D6DAC: db $88
L0D6DAD: db $A0
L0D6DAE: db $03
L0D6DAF: db $E4
L0D6DB0: db $A8
L0D6DB1: db $A1
L0D6DB2: db $0A
L0D6DB3: db $A3
L0D6DB4: db $14
L0D6DB5: db $A1
L0D6DB6: db $0A
L0D6DB7: db $A0
L0D6DB8: db $9E
L0D6DB9: db $E4
L0D6DBA: db $A8
L0D6DBB: db $A0
L0D6DBC: db $05
L0D6DBD: db $80
L0D6DBE: db $02
L0D6DBF: db $E4
L0D6DC0: db $88
L0D6DC1: db $A0
L0D6DC2: db $03
L0D6DC3: db $E4
L0D6DC4: db $A8
L0D6DC5: db $9D
L0D6DC6: db $05
L0D6DC7: db $80
L0D6DC8: db $02
L0D6DC9: db $E4
L0D6DCA: db $88
L0D6DCB: db $9D
L0D6DCC: db $03
L0D6DCD: db $E4
L0D6DCE: db $A8
L0D6DCF: db $9E
L0D6DD0: db $0A
L0D6DD1: db $A0
L0D6DD2: db $A1
L0D6DD3: db $A3
L0D6DD4: db $E4
L0D6DD5: db $A8
L0D6DD6: db $A5
L0D6DD7: db $0F
L0D6DD8: db $80
L0D6DD9: db $02
L0D6DDA: db $E4
L0D6DDB: db $88
L0D6DDC: db $A5
L0D6DDD: db $03
L0D6DDE: db $E7
L0D6DDF: db $00
L0D6DE0: db $02
L0D6DE1: db $5B
L0D6DE2: db $6D
L0D6DE3: db $ED
L0D6DE4: db $E4
L0D6DE5: db $A8
L0D6DE6: db $A6
L0D6DE7: db $05
L0D6DE8: db $80
L0D6DE9: db $02
L0D6DEA: db $E4
L0D6DEB: db $88
L0D6DEC: db $A6
L0D6DED: db $03
L0D6DEE: db $E4
L0D6DEF: db $A8
L0D6DF0: db $A3
L0D6DF1: db $14
L0D6DF2: db $E4
L0D6DF3: db $A8
L0D6DF4: db $A6
L0D6DF5: db $05
L0D6DF6: db $80
L0D6DF7: db $02
L0D6DF8: db $E4
L0D6DF9: db $88
L0D6DFA: db $A6
L0D6DFB: db $03
L0D6DFC: db $E4
L0D6DFD: db $A8
L0D6DFE: db $A3
L0D6DFF: db $14
L0D6E00: db $E4
L0D6E01: db $A8
L0D6E02: db $A7
L0D6E03: db $05
L0D6E04: db $80
L0D6E05: db $02
L0D6E06: db $E4
L0D6E07: db $88
L0D6E08: db $A7
L0D6E09: db $03
L0D6E0A: db $E4
L0D6E0B: db $A8
L0D6E0C: db $A3
L0D6E0D: db $14
L0D6E0E: db $E4
L0D6E0F: db $A8
L0D6E10: db $A7
L0D6E11: db $05
L0D6E12: db $80
L0D6E13: db $02
L0D6E14: db $E4
L0D6E15: db $88
L0D6E16: db $A7
L0D6E17: db $03
L0D6E18: db $E4
L0D6E19: db $A8
L0D6E1A: db $A3
L0D6E1B: db $14
L0D6E1C: db $E4
L0D6E1D: db $A8
L0D6E1E: db $A8
L0D6E1F: db $05
L0D6E20: db $80
L0D6E21: db $02
L0D6E22: db $E4
L0D6E23: db $88
L0D6E24: db $A8
L0D6E25: db $03
L0D6E26: db $E4
L0D6E27: db $A8
L0D6E28: db $A3
L0D6E29: db $14
L0D6E2A: db $E4
L0D6E2B: db $A8
L0D6E2C: db $A8
L0D6E2D: db $05
L0D6E2E: db $80
L0D6E2F: db $02
L0D6E30: db $E4
L0D6E31: db $88
L0D6E32: db $A8
L0D6E33: db $03
L0D6E34: db $E4
L0D6E35: db $A8
L0D6E36: db $A3
L0D6E37: db $14
L0D6E38: db $E4
L0D6E39: db $A8
L0D6E3A: db $A9
L0D6E3B: db $0A
L0D6E3C: db $E4
L0D6E3D: db $88
L0D6E3E: db $A9
L0D6E3F: db $05
L0D6E40: db $80
L0D6E41: db $02
L0D6E42: db $E4
L0D6E43: db $68
L0D6E44: db $A9
L0D6E45: db $03
L0D6E46: db $E4
L0D6E47: db $A8
L0D6E48: db $A9
L0D6E49: db $0A
L0D6E4A: db $AA
L0D6E4B: db $14
L0D6E4C: db $E4
L0D6E4D: db $88
L0D6E4E: db $AA
L0D6E4F: db $05
L0D6E50: db $80
L0D6E51: db $02
L0D6E52: db $E4
L0D6E53: db $68
L0D6E54: db $AA
L0D6E55: db $03
L0D6E56: db $E7
L0D6E57: db $00
L0D6E58: db $02
L0D6E59: db $E4
L0D6E5A: db $6D
L0D6E5B: db $ED
L0D6E5C: db $E4
L0D6E5D: db $A8
L0D6E5E: db $A6
L0D6E5F: db $05
L0D6E60: db $80
L0D6E61: db $02
L0D6E62: db $E4
L0D6E63: db $88
L0D6E64: db $A6
L0D6E65: db $03
L0D6E66: db $E4
L0D6E67: db $A8
L0D6E68: db $A6
L0D6E69: db $0A
L0D6E6A: db $A5
L0D6E6B: db $14
L0D6E6C: db $80
L0D6E6D: db $03
L0D6E6E: db $E4
L0D6E6F: db $88
L0D6E70: db $A5
L0D6E71: db $05
L0D6E72: db $80
L0D6E73: db $02
L0D6E74: db $E4
L0D6E75: db $68
L0D6E76: db $A5
L0D6E77: db $05
L0D6E78: db $80
L0D6E79: db $02
L0D6E7A: db $E4
L0D6E7B: db $48
L0D6E7C: db $A5
L0D6E7D: db $03
L0D6E7E: db $E4
L0D6E7F: db $A8
L0D6E80: db $A6
L0D6E81: db $05
L0D6E82: db $80
L0D6E83: db $A6
L0D6E84: db $0A
L0D6E85: db $A5
L0D6E86: db $14
L0D6E87: db $E4
L0D6E88: db $88
L0D6E89: db $A5
L0D6E8A: db $05
L0D6E8B: db $80
L0D6E8C: db $02
L0D6E8D: db $E4
L0D6E8E: db $68
L0D6E8F: db $A5
L0D6E90: db $03
L0D6E91: db $E4
L0D6E92: db $A8
L0D6E93: db $A6
L0D6E94: db $14
L0D6E95: db $E4
L0D6E96: db $88
L0D6E97: db $A5
L0D6E98: db $05
L0D6E99: db $80
L0D6E9A: db $02
L0D6E9B: db $E4
L0D6E9C: db $68
L0D6E9D: db $A5
L0D6E9E: db $03
L0D6E9F: db $E4
L0D6EA0: db $A8
L0D6EA1: db $A3
L0D6EA2: db $0A
L0D6EA3: db $A5
L0D6EA4: db $E7
L0D6EA5: db $00
L0D6EA6: db $02
L0D6EA7: db $5C
L0D6EA8: db $6E
L0D6EA9: db $ED
L0D6EAA: db $E4
L0D6EAB: db $A8
L0D6EAC: db $AA
L0D6EAD: db $05
L0D6EAE: db $80
L0D6EAF: db $02
L0D6EB0: db $E4
L0D6EB1: db $88
L0D6EB2: db $AA
L0D6EB3: db $03
L0D6EB4: db $E4
L0D6EB5: db $A8
L0D6EB6: db $AA
L0D6EB7: db $0A
L0D6EB8: db $A8
L0D6EB9: db $14
L0D6EBA: db $80
L0D6EBB: db $03
L0D6EBC: db $E4
L0D6EBD: db $88
L0D6EBE: db $A8
L0D6EBF: db $05
L0D6EC0: db $80
L0D6EC1: db $02
L0D6EC2: db $E4
L0D6EC3: db $68
L0D6EC4: db $A8
L0D6EC5: db $05
L0D6EC6: db $80
L0D6EC7: db $02
L0D6EC8: db $E4
L0D6EC9: db $48
L0D6ECA: db $A8
L0D6ECB: db $03
L0D6ECC: db $E4
L0D6ECD: db $A8
L0D6ECE: db $AA
L0D6ECF: db $05
L0D6ED0: db $80
L0D6ED1: db $AA
L0D6ED2: db $0A
L0D6ED3: db $A8
L0D6ED4: db $14
L0D6ED5: db $E4
L0D6ED6: db $88
L0D6ED7: db $A8
L0D6ED8: db $05
L0D6ED9: db $80
L0D6EDA: db $02
L0D6EDB: db $E4
L0D6EDC: db $68
L0D6EDD: db $A8
L0D6EDE: db $03
L0D6EDF: db $E4
L0D6EE0: db $A8
L0D6EE1: db $AA
L0D6EE2: db $14
L0D6EE3: db $E4
L0D6EE4: db $88
L0D6EE5: db $A8
L0D6EE6: db $05
L0D6EE7: db $80
L0D6EE8: db $02
L0D6EE9: db $E4
L0D6EEA: db $68
L0D6EEB: db $A8
L0D6EEC: db $03
L0D6EED: db $E4
L0D6EEE: db $A8
L0D6EEF: db $A7
L0D6EF0: db $0A
L0D6EF1: db $A8
L0D6EF2: db $E7
L0D6EF3: db $00
L0D6EF4: db $02
L0D6EF5: db $AA
L0D6EF6: db $6E
L0D6EF7: db $ED
L0D6EF8: db $E4
L0D6EF9: db $A8
L0D6EFA: db $9E
L0D6EFB: db $05
L0D6EFC: db $80
L0D6EFD: db $02
L0D6EFE: db $E4
L0D6EFF: db $88
L0D6F00: db $9E
L0D6F01: db $03
L0D6F02: db $E4
L0D6F03: db $A8
L0D6F04: db $9E
L0D6F05: db $0A
L0D6F06: db $A0
L0D6F07: db $05
L0D6F08: db $80
L0D6F09: db $02
L0D6F0A: db $E4
L0D6F0B: db $88
L0D6F0C: db $A0
L0D6F0D: db $03
L0D6F0E: db $E4
L0D6F0F: db $A8
L0D6F10: db $A0
L0D6F11: db $0A
L0D6F12: db $A1
L0D6F13: db $05
L0D6F14: db $80
L0D6F15: db $02
L0D6F16: db $E4
L0D6F17: db $88
L0D6F18: db $A1
L0D6F19: db $03
L0D6F1A: db $E4
L0D6F1B: db $A8
L0D6F1C: db $A1
L0D6F1D: db $0A
L0D6F1E: db $A2
L0D6F1F: db $A3
L0D6F20: db $0F
L0D6F21: db $80
L0D6F22: db $02
L0D6F23: db $E4
L0D6F24: db $88
L0D6F25: db $A3
L0D6F26: db $03
L0D6F27: db $ED
L0D6F28: db $E4
L0D6F29: db $A8
L0D6F2A: db $A6
L0D6F2B: db $05
L0D6F2C: db $80
L0D6F2D: db $02
L0D6F2E: db $E4
L0D6F2F: db $88
L0D6F30: db $A6
L0D6F31: db $03
L0D6F32: db $E4
L0D6F33: db $A8
L0D6F34: db $A3
L0D6F35: db $0F
L0D6F36: db $80
L0D6F37: db $02
L0D6F38: db $E4
L0D6F39: db $88
L0D6F3A: db $A3
L0D6F3B: db $03
L0D6F3C: db $E7
L0D6F3D: db $00
L0D6F3E: db $02
L0D6F3F: db $28
L0D6F40: db $6F
L0D6F41: db $ED
L0D6F42: db $E4
L0D6F43: db $A8
L0D6F44: db $A8
L0D6F45: db $0F
L0D6F46: db $E4
L0D6F47: db $88
L0D6F48: db $A8
L0D6F49: db $05
L0D6F4A: db $E4
L0D6F4B: db $A8
L0D6F4C: db $A7
L0D6F4D: db $0F
L0D6F4E: db $E4
L0D6F4F: db $88
L0D6F50: db $A7
L0D6F51: db $05
L0D6F52: db $E4
L0D6F53: db $A8
L0D6F54: db $A6
L0D6F55: db $05
L0D6F56: db $80
L0D6F57: db $02
L0D6F58: db $E4
L0D6F59: db $88
L0D6F5A: db $A6
L0D6F5B: db $03
L0D6F5C: db $E4
L0D6F5D: db $A8
L0D6F5E: db $A5
L0D6F5F: db $0F
L0D6F60: db $E4
L0D6F61: db $88
L0D6F62: db $A5
L0D6F63: db $05
L0D6F64: db $E4
L0D6F65: db $A8
L0D6F66: db $A4
L0D6F67: db $0F
L0D6F68: db $80
L0D6F69: db $02
L0D6F6A: db $E4
L0D6F6B: db $88
L0D6F6C: db $A4
L0D6F6D: db $03
L0D6F6E: db $E4
L0D6F6F: db $A8
L0D6F70: db $A3
L0D6F71: db $0F
L0D6F72: db $E4
L0D6F73: db $88
L0D6F74: db $A3
L0D6F75: db $05
L0D6F76: db $E4
L0D6F77: db $A8
L0D6F78: db $A2
L0D6F79: db $0F
L0D6F7A: db $E4
L0D6F7B: db $88
L0D6F7C: db $A2
L0D6F7D: db $05
L0D6F7E: db $E4
L0D6F7F: db $A8
L0D6F80: db $A1
L0D6F81: db $05
L0D6F82: db $80
L0D6F83: db $02
L0D6F84: db $E4
L0D6F85: db $88
L0D6F86: db $A1
L0D6F87: db $03
L0D6F88: db $E4
L0D6F89: db $A8
L0D6F8A: db $A0
L0D6F8B: db $0F
L0D6F8C: db $E4
L0D6F8D: db $88
L0D6F8E: db $A0
L0D6F8F: db $05
L0D6F90: db $E4
L0D6F91: db $A8
L0D6F92: db $9F
L0D6F93: db $0F
L0D6F94: db $E4
L0D6F95: db $88
L0D6F96: db $9F
L0D6F97: db $05
L0D6F98: db $E4
L0D6F99: db $A8
L0D6F9A: db $9E
L0D6F9B: db $0F
L0D6F9C: db $E4
L0D6F9D: db $88
L0D6F9E: db $9E
L0D6F9F: db $05
L0D6FA0: db $E4
L0D6FA1: db $A8
L0D6FA2: db $9D
L0D6FA3: db $05
L0D6FA4: db $80
L0D6FA5: db $02
L0D6FA6: db $E4
L0D6FA7: db $88
L0D6FA8: db $9D
L0D6FA9: db $03
L0D6FAA: db $E4
L0D6FAB: db $A8
L0D6FAC: db $9C
L0D6FAD: db $0F
L0D6FAE: db $E4
L0D6FAF: db $88
L0D6FB0: db $9C
L0D6FB1: db $05
L0D6FB2: db $E4
L0D6FB3: db $A8
L0D6FB4: db $9B
L0D6FB5: db $0F
L0D6FB6: db $80
L0D6FB7: db $02
L0D6FB8: db $E4
L0D6FB9: db $88
L0D6FBA: db $9B
L0D6FBB: db $03
L0D6FBC: db $E4
L0D6FBD: db $A8
L0D6FBE: db $9A
L0D6FBF: db $0F
L0D6FC0: db $E4
L0D6FC1: db $88
L0D6FC2: db $9A
L0D6FC3: db $05
L0D6FC4: db $E4
L0D6FC5: db $A8
L0D6FC6: db $99
L0D6FC7: db $0F
L0D6FC8: db $E4
L0D6FC9: db $88
L0D6FCA: db $99
L0D6FCB: db $05
L0D6FCC: db $E4
L0D6FCD: db $A8
L0D6FCE: db $98
L0D6FCF: db $05
L0D6FD0: db $80
L0D6FD1: db $02
L0D6FD2: db $E4
L0D6FD3: db $88
L0D6FD4: db $98
L0D6FD5: db $03
L0D6FD6: db $E4
L0D6FD7: db $A8
L0D6FD8: db $99
L0D6FD9: db $05
L0D6FDA: db $80
L0D6FDB: db $02
L0D6FDC: db $E4
L0D6FDD: db $88
L0D6FDE: db $99
L0D6FDF: db $03
L0D6FE0: db $E4
L0D6FE1: db $A8
L0D6FE2: db $9A
L0D6FE3: db $05
L0D6FE4: db $80
L0D6FE5: db $02
L0D6FE6: db $E4
L0D6FE7: db $88
L0D6FE8: db $9A
L0D6FE9: db $03
L0D6FEA: db $ED
L0D6FEB: db $E4
L0D6FEC: db $A8
L0D6FED: db $E9
L0D6FEE: db $22
L0D6FEF: db $EE
L0D6FF0: db $C0
L0D6FF1: db $EC
L0D6FF2: db $36
L0D6FF3: db $70
L0D6FF4: db $EE
L0D6FF5: db $00
L0D6FF6: db $EC
L0D6FF7: db $BF
L0D6FF8: db $70
L0D6FF9: db $E6
L0D6FFA: db $01
L0D6FFB: db $EC
L0D6FFC: db $BF
L0D6FFD: db $70
L0D6FFE: db $E6
L0D6FFF: db $01
L0D7000: db $EC
L0D7001: db $BF
L0D7002: db $70
L0D7003: db $E6
L0D7004: db $FE
L0D7005: db $EC
L0D7006: db $F5
L0D7007: db $70
L0D7008: db $EC
L0D7009: db $BF
L0D700A: db $70
L0D700B: db $E6
L0D700C: db $01
L0D700D: db $EC
L0D700E: db $BF
L0D700F: db $70
L0D7010: db $E6
L0D7011: db $01
L0D7012: db $EC
L0D7013: db $BF
L0D7014: db $70
L0D7015: db $E6
L0D7016: db $FE
L0D7017: db $EC
L0D7018: db $F5
L0D7019: db $70
L0D701A: db $EC
L0D701B: db $56
L0D701C: db $71
L0D701D: db $EC
L0D701E: db $84
L0D701F: db $71
L0D7020: db $EC
L0D7021: db $B2
L0D7022: db $71
L0D7023: db $EE
L0D7024: db $C0
L0D7025: db $E6
L0D7026: db $03
L0D7027: db $EC
L0D7028: db $42
L0D7029: db $6F
L0D702A: db $E6
L0D702B: db $FD
L0D702C: db $E6
L0D702D: db $FB
L0D702E: db $EC
L0D702F: db $FD
L0D7030: db $6B
L0D7031: db $E6
L0D7032: db $05
L0D7033: db $E5
L0D7034: db $EB
L0D7035: db $6F
L0D7036: db $E4
L0D7037: db $A8
L0D7038: db $97
L0D7039: db $0A
L0D703A: db $96
L0D703B: db $05
L0D703C: db $80
L0D703D: db $02
L0D703E: db $E4
L0D703F: db $88
L0D7040: db $96
L0D7041: db $03
L0D7042: db $E4
L0D7043: db $A8
L0D7044: db $95
L0D7045: db $0A
L0D7046: db $92
L0D7047: db $14
L0D7048: db $E4
L0D7049: db $88
L0D704A: db $92
L0D704B: db $05
L0D704C: db $80
L0D704D: db $02
L0D704E: db $E4
L0D704F: db $68
L0D7050: db $92
L0D7051: db $03
L0D7052: db $E4
L0D7053: db $A8
L0D7054: db $97
L0D7055: db $0A
L0D7056: db $96
L0D7057: db $05
L0D7058: db $80
L0D7059: db $02
L0D705A: db $E4
L0D705B: db $88
L0D705C: db $96
L0D705D: db $03
L0D705E: db $E4
L0D705F: db $A8
L0D7060: db $95
L0D7061: db $0A
L0D7062: db $92
L0D7063: db $E4
L0D7064: db $88
L0D7065: db $92
L0D7066: db $05
L0D7067: db $80
L0D7068: db $02
L0D7069: db $E4
L0D706A: db $68
L0D706B: db $92
L0D706C: db $03
L0D706D: db $E4
L0D706E: db $A8
L0D706F: db $97
L0D7070: db $14
L0D7071: db $92
L0D7072: db $05
L0D7073: db $80
L0D7074: db $02
L0D7075: db $E4
L0D7076: db $88
L0D7077: db $92
L0D7078: db $03
L0D7079: db $E4
L0D707A: db $A8
L0D707B: db $95
L0D707C: db $0A
L0D707D: db $97
L0D707E: db $E4
L0D707F: db $A8
L0D7080: db $9A
L0D7081: db $0A
L0D7082: db $9C
L0D7083: db $05
L0D7084: db $80
L0D7085: db $02
L0D7086: db $E4
L0D7087: db $88
L0D7088: db $9C
L0D7089: db $03
L0D708A: db $E4
L0D708B: db $A8
L0D708C: db $9E
L0D708D: db $0A
L0D708E: db $A0
L0D708F: db $14
L0D7090: db $9E
L0D7091: db $0A
L0D7092: db $9C
L0D7093: db $9A
L0D7094: db $E4
L0D7095: db $A8
L0D7096: db $9D
L0D7097: db $05
L0D7098: db $80
L0D7099: db $02
L0D709A: db $E4
L0D709B: db $88
L0D709C: db $9D
L0D709D: db $03
L0D709E: db $E4
L0D709F: db $A8
L0D70A0: db $99
L0D70A1: db $05
L0D70A2: db $80
L0D70A3: db $02
L0D70A4: db $E4
L0D70A5: db $88
L0D70A6: db $99
L0D70A7: db $03
L0D70A8: db $E4
L0D70A9: db $A8
L0D70AA: db $9B
L0D70AB: db $0A
L0D70AC: db $9D
L0D70AD: db $9E
L0D70AE: db $A0
L0D70AF: db $E4
L0D70B0: db $A8
L0D70B1: db $A1
L0D70B2: db $0F
L0D70B3: db $80
L0D70B4: db $02
L0D70B5: db $E4
L0D70B6: db $88
L0D70B7: db $A1
L0D70B8: db $03
L0D70B9: db $E7
L0D70BA: db $00
L0D70BB: db $02
L0D70BC: db $36
L0D70BD: db $70
L0D70BE: db $ED
L0D70BF: db $E4
L0D70C0: db $A2
L0D70C1: db $AD
L0D70C2: db $02
L0D70C3: db $E4
L0D70C4: db $82
L0D70C5: db $A8
L0D70C6: db $03
L0D70C7: db $E4
L0D70C8: db $A2
L0D70C9: db $AA
L0D70CA: db $02
L0D70CB: db $E4
L0D70CC: db $82
L0D70CD: db $AD
L0D70CE: db $03
L0D70CF: db $E4
L0D70D0: db $A2
L0D70D1: db $B1
L0D70D2: db $02
L0D70D3: db $E4
L0D70D4: db $82
L0D70D5: db $AA
L0D70D6: db $03
L0D70D7: db $E4
L0D70D8: db $A2
L0D70D9: db $AA
L0D70DA: db $02
L0D70DB: db $E4
L0D70DC: db $82
L0D70DD: db $B1
L0D70DE: db $03
L0D70DF: db $E4
L0D70E0: db $A2
L0D70E1: db $AD
L0D70E2: db $02
L0D70E3: db $E4
L0D70E4: db $82
L0D70E5: db $AA
L0D70E6: db $03
L0D70E7: db $E4
L0D70E8: db $A2
L0D70E9: db $A8
L0D70EA: db $02
L0D70EB: db $E4
L0D70EC: db $82
L0D70ED: db $AD
L0D70EE: db $03
L0D70EF: db $E7
L0D70F0: db $00
L0D70F1: db $02
L0D70F2: db $BF
L0D70F3: db $70
L0D70F4: db $ED
L0D70F5: db $E4
L0D70F6: db $A2
L0D70F7: db $B0
L0D70F8: db $02
L0D70F9: db $E4
L0D70FA: db $82
L0D70FB: db $AA
L0D70FC: db $03
L0D70FD: db $E4
L0D70FE: db $A2
L0D70FF: db $AD
L0D7100: db $02
L0D7101: db $E4
L0D7102: db $82
L0D7103: db $B0
L0D7104: db $03
L0D7105: db $E4
L0D7106: db $A2
L0D7107: db $B3
L0D7108: db $02
L0D7109: db $E4
L0D710A: db $82
L0D710B: db $AD
L0D710C: db $03
L0D710D: db $E4
L0D710E: db $A2
L0D710F: db $AD
L0D7110: db $02
L0D7111: db $E4
L0D7112: db $82
L0D7113: db $B3
L0D7114: db $03
L0D7115: db $E4
L0D7116: db $A2
L0D7117: db $B0
L0D7118: db $02
L0D7119: db $E4
L0D711A: db $82
L0D711B: db $AD
L0D711C: db $03
L0D711D: db $E4
L0D711E: db $A2
L0D711F: db $AD
L0D7120: db $02
L0D7121: db $E4
L0D7122: db $82
L0D7123: db $B0
L0D7124: db $03
L0D7125: db $E4
L0D7126: db $A2
L0D7127: db $B3
L0D7128: db $02
L0D7129: db $E4
L0D712A: db $82
L0D712B: db $AD
L0D712C: db $03
L0D712D: db $E4
L0D712E: db $A2
L0D712F: db $B2
L0D7130: db $02
L0D7131: db $E4
L0D7132: db $82
L0D7133: db $B3
L0D7134: db $03
L0D7135: db $E4
L0D7136: db $A2
L0D7137: db $B1
L0D7138: db $02
L0D7139: db $E4
L0D713A: db $82
L0D713B: db $B2
L0D713C: db $03
L0D713D: db $E4
L0D713E: db $A2
L0D713F: db $B0
L0D7140: db $02
L0D7141: db $E4
L0D7142: db $82
L0D7143: db $B1
L0D7144: db $03
L0D7145: db $E4
L0D7146: db $A2
L0D7147: db $AF
L0D7148: db $02
L0D7149: db $E4
L0D714A: db $82
L0D714B: db $B0
L0D714C: db $03
L0D714D: db $E4
L0D714E: db $A2
L0D714F: db $AE
L0D7150: db $02
L0D7151: db $E4
L0D7152: db $82
L0D7153: db $AF
L0D7154: db $03
L0D7155: db $ED
L0D7156: db $E4
L0D7157: db $A2
L0D7158: db $AD
L0D7159: db $05
L0D715A: db $80
L0D715B: db $02
L0D715C: db $E4
L0D715D: db $82
L0D715E: db $AD
L0D715F: db $03
L0D7160: db $E4
L0D7161: db $A2
L0D7162: db $AA
L0D7163: db $05
L0D7164: db $80
L0D7165: db $02
L0D7166: db $E4
L0D7167: db $82
L0D7168: db $AA
L0D7169: db $03
L0D716A: db $E4
L0D716B: db $A2
L0D716C: db $B1
L0D716D: db $05
L0D716E: db $80
L0D716F: db $02
L0D7170: db $E4
L0D7171: db $82
L0D7172: db $B1
L0D7173: db $03
L0D7174: db $E4
L0D7175: db $A2
L0D7176: db $AA
L0D7177: db $05
L0D7178: db $80
L0D7179: db $02
L0D717A: db $E4
L0D717B: db $82
L0D717C: db $AA
L0D717D: db $03
L0D717E: db $E7
L0D717F: db $00
L0D7180: db $08
L0D7181: db $56
L0D7182: db $71
L0D7183: db $ED
L0D7184: db $E4
L0D7185: db $A2
L0D7186: db $AF
L0D7187: db $05
L0D7188: db $80
L0D7189: db $02
L0D718A: db $E4
L0D718B: db $82
L0D718C: db $AF
L0D718D: db $03
L0D718E: db $E4
L0D718F: db $A2
L0D7190: db $B7
L0D7191: db $05
L0D7192: db $80
L0D7193: db $02
L0D7194: db $E4
L0D7195: db $82
L0D7196: db $B7
L0D7197: db $03
L0D7198: db $E4
L0D7199: db $A2
L0D719A: db $B2
L0D719B: db $05
L0D719C: db $80
L0D719D: db $02
L0D719E: db $E4
L0D719F: db $82
L0D71A0: db $B2
L0D71A1: db $03
L0D71A2: db $E4
L0D71A3: db $A2
L0D71A4: db $B7
L0D71A5: db $05
L0D71A6: db $80
L0D71A7: db $02
L0D71A8: db $E4
L0D71A9: db $82
L0D71AA: db $B7
L0D71AB: db $03
L0D71AC: db $E7
L0D71AD: db $00
L0D71AE: db $08
L0D71AF: db $84
L0D71B0: db $71
L0D71B1: db $ED
L0D71B2: db $E4
L0D71B3: db $A2
L0D71B4: db $AD
L0D71B5: db $05
L0D71B6: db $80
L0D71B7: db $02
L0D71B8: db $E4
L0D71B9: db $82
L0D71BA: db $AD
L0D71BB: db $03
L0D71BC: db $E4
L0D71BD: db $A2
L0D71BE: db $AC
L0D71BF: db $05
L0D71C0: db $80
L0D71C1: db $02
L0D71C2: db $E4
L0D71C3: db $82
L0D71C4: db $AC
L0D71C5: db $03
L0D71C6: db $E4
L0D71C7: db $A2
L0D71C8: db $B2
L0D71C9: db $05
L0D71CA: db $80
L0D71CB: db $02
L0D71CC: db $E4
L0D71CD: db $82
L0D71CE: db $B2
L0D71CF: db $03
L0D71D0: db $E4
L0D71D1: db $A2
L0D71D2: db $AC
L0D71D3: db $05
L0D71D4: db $80
L0D71D5: db $02
L0D71D6: db $E4
L0D71D7: db $82
L0D71D8: db $AC
L0D71D9: db $03
L0D71DA: db $E7
L0D71DB: db $00
L0D71DC: db $08
L0D71DD: db $B2
L0D71DE: db $71
L0D71DF: db $ED
L0D71E0: db $E4
L0D71E1: db $20
L0D71E2: db $E9
L0D71E3: db $44
L0D71E4: db $F3
L0D71E5: db $03
L0D71E6: db $F5
L0D71E7: db $00
L0D71E8: db $EC
L0D71E9: db $8F
L0D71EA: db $72
L0D71EB: db $EC
L0D71EC: db $CC
L0D71ED: db $72
L0D71EE: db $EC
L0D71EF: db $0A
L0D71F0: db $73
L0D71F1: db $EC
L0D71F2: db $17
L0D71F3: db $73
L0D71F4: db $EC
L0D71F5: db $24
L0D71F6: db $73
L0D71F7: db $97
L0D71F8: db $0A
L0D71F9: db $92
L0D71FA: db $05
L0D71FB: db $80
L0D71FC: db $98
L0D71FD: db $0A
L0D71FE: db $92
L0D71FF: db $05
L0D7200: db $80
L0D7201: db $99
L0D7202: db $80
L0D7203: db $9A
L0D7204: db $0D
L0D7205: db $80
L0D7206: db $07
L0D7207: db $9B
L0D7208: db $14
L0D7209: db $9C
L0D720A: db $0A
L0D720B: db $97
L0D720C: db $9D
L0D720D: db $0D
L0D720E: db $80
L0D720F: db $07
L0D7210: db $9E
L0D7211: db $0A
L0D7212: db $9F
L0D7213: db $14
L0D7214: db $9E
L0D7215: db $0A
L0D7216: db $97
L0D7217: db $05
L0D7218: db $80
L0D7219: db $9D
L0D721A: db $0A
L0D721B: db $97
L0D721C: db $05
L0D721D: db $80
L0D721E: db $9C
L0D721F: db $80
L0D7220: db $9B
L0D7221: db $0D
L0D7222: db $80
L0D7223: db $07
L0D7224: db $9A
L0D7225: db $14
L0D7226: db $99
L0D7227: db $05
L0D7228: db $80
L0D7229: db $92
L0D722A: db $80
L0D722B: db $98
L0D722C: db $14
L0D722D: db $97
L0D722E: db $05
L0D722F: db $80
L0D7230: db $96
L0D7231: db $0A
L0D7232: db $92
L0D7233: db $EC
L0D7234: db $31
L0D7235: db $73
L0D7236: db $97
L0D7237: db $0A
L0D7238: db $92
L0D7239: db $05
L0D723A: db $80
L0D723B: db $97
L0D723C: db $0A
L0D723D: db $92
L0D723E: db $05
L0D723F: db $80
L0D7240: db $97
L0D7241: db $80
L0D7242: db $99
L0D7243: db $14
L0D7244: db $9A
L0D7245: db $0F
L0D7246: db $80
L0D7247: db $05
L0D7248: db $9A
L0D7249: db $0F
L0D724A: db $80
L0D724B: db $05
L0D724C: db $9A
L0D724D: db $0F
L0D724E: db $80
L0D724F: db $05
L0D7250: db $9A
L0D7251: db $0F
L0D7252: db $80
L0D7253: db $05
L0D7254: db $9A
L0D7255: db $0F
L0D7256: db $80
L0D7257: db $05
L0D7258: db $9C
L0D7259: db $08
L0D725A: db $80
L0D725B: db $02
L0D725C: db $9C
L0D725D: db $05
L0D725E: db $80
L0D725F: db $9C
L0D7260: db $80
L0D7261: db $9C
L0D7262: db $0A
L0D7263: db $9D
L0D7264: db $05
L0D7265: db $80
L0D7266: db $9E
L0D7267: db $14
L0D7268: db $EC
L0D7269: db $53
L0D726A: db $73
L0D726B: db $97
L0D726C: db $05
L0D726D: db $80
L0D726E: db $97
L0D726F: db $0A
L0D7270: db $9E
L0D7271: db $97
L0D7272: db $08
L0D7273: db $80
L0D7274: db $02
L0D7275: db $EC
L0D7276: db $53
L0D7277: db $73
L0D7278: db $97
L0D7279: db $05
L0D727A: db $80
L0D727B: db $97
L0D727C: db $0A
L0D727D: db $9E
L0D727E: db $97
L0D727F: db $14
L0D7280: db $92
L0D7281: db $0A
L0D7282: db $9E
L0D7283: db $14
L0D7284: db $80
L0D7285: db $0A
L0D7286: db $98
L0D7287: db $05
L0D7288: db $80
L0D7289: db $99
L0D728A: db $0A
L0D728B: db $9A
L0D728C: db $E5
L0D728D: db $E0
L0D728E: db $71
L0D728F: db $97
L0D7290: db $0A
L0D7291: db $92
L0D7292: db $05
L0D7293: db $80
L0D7294: db $92
L0D7295: db $0A
L0D7296: db $9A
L0D7297: db $92
L0D7298: db $05
L0D7299: db $80
L0D729A: db $92
L0D729B: db $0A
L0D729C: db $97
L0D729D: db $92
L0D729E: db $05
L0D729F: db $80
L0D72A0: db $92
L0D72A1: db $0A
L0D72A2: db $9A
L0D72A3: db $92
L0D72A4: db $05
L0D72A5: db $80
L0D72A6: db $92
L0D72A7: db $0A
L0D72A8: db $97
L0D72A9: db $05
L0D72AA: db $80
L0D72AB: db $97
L0D72AC: db $0A
L0D72AD: db $9A
L0D72AE: db $97
L0D72AF: db $95
L0D72B0: db $14
L0D72B1: db $92
L0D72B2: db $0A
L0D72B3: db $9A
L0D72B4: db $12
L0D72B5: db $80
L0D72B6: db $02
L0D72B7: db $92
L0D72B8: db $0A
L0D72B9: db $94
L0D72BA: db $14
L0D72BB: db $91
L0D72BC: db $0A
L0D72BD: db $9A
L0D72BE: db $1E
L0D72BF: db $95
L0D72C0: db $05
L0D72C1: db $80
L0D72C2: db $95
L0D72C3: db $0A
L0D72C4: db $9A
L0D72C5: db $95
L0D72C6: db $E7
L0D72C7: db $00
L0D72C8: db $02
L0D72C9: db $8F
L0D72CA: db $72
L0D72CB: db $ED
L0D72CC: db $97
L0D72CD: db $05
L0D72CE: db $80
L0D72CF: db $97
L0D72D0: db $0A
L0D72D1: db $92
L0D72D2: db $97
L0D72D3: db $05
L0D72D4: db $80
L0D72D5: db $97
L0D72D6: db $0A
L0D72D7: db $92
L0D72D8: db $98
L0D72D9: db $05
L0D72DA: db $80
L0D72DB: db $98
L0D72DC: db $0A
L0D72DD: db $92
L0D72DE: db $98
L0D72DF: db $05
L0D72E0: db $80
L0D72E1: db $98
L0D72E2: db $0A
L0D72E3: db $92
L0D72E4: db $99
L0D72E5: db $05
L0D72E6: db $80
L0D72E7: db $99
L0D72E8: db $0A
L0D72E9: db $92
L0D72EA: db $99
L0D72EB: db $05
L0D72EC: db $80
L0D72ED: db $99
L0D72EE: db $0A
L0D72EF: db $92
L0D72F0: db $9A
L0D72F1: db $05
L0D72F2: db $80
L0D72F3: db $9A
L0D72F4: db $08
L0D72F5: db $80
L0D72F6: db $02
L0D72F7: db $9A
L0D72F8: db $05
L0D72F9: db $80
L0D72FA: db $9B
L0D72FB: db $05
L0D72FC: db $80
L0D72FD: db $9B
L0D72FE: db $08
L0D72FF: db $80
L0D7300: db $02
L0D7301: db $9B
L0D7302: db $05
L0D7303: db $80
L0D7304: db $E7
L0D7305: db $00
L0D7306: db $02
L0D7307: db $CC
L0D7308: db $72
L0D7309: db $ED
L0D730A: db $92
L0D730B: db $05
L0D730C: db $80
L0D730D: db $92
L0D730E: db $0A
L0D730F: db $90
L0D7310: db $95
L0D7311: db $E7
L0D7312: db $00
L0D7313: db $08
L0D7314: db $0A
L0D7315: db $73
L0D7316: db $ED
L0D7317: db $93
L0D7318: db $05
L0D7319: db $80
L0D731A: db $93
L0D731B: db $0A
L0D731C: db $90
L0D731D: db $92
L0D731E: db $E7
L0D731F: db $00
L0D7320: db $08
L0D7321: db $17
L0D7322: db $73
L0D7323: db $ED
L0D7324: db $94
L0D7325: db $05
L0D7326: db $80
L0D7327: db $94
L0D7328: db $0A
L0D7329: db $90
L0D732A: db $92
L0D732B: db $E7
L0D732C: db $00
L0D732D: db $08
L0D732E: db $24
L0D732F: db $73
L0D7330: db $ED
L0D7331: db $97
L0D7332: db $0A
L0D7333: db $92
L0D7334: db $05
L0D7335: db $80
L0D7336: db $97
L0D7337: db $0A
L0D7338: db $92
L0D7339: db $05
L0D733A: db $80
L0D733B: db $97
L0D733C: db $80
L0D733D: db $99
L0D733E: db $14
L0D733F: db $9A
L0D7340: db $92
L0D7341: db $05
L0D7342: db $80
L0D7343: db $9A
L0D7344: db $80
L0D7345: db $95
L0D7346: db $0F
L0D7347: db $80
L0D7348: db $05
L0D7349: db $96
L0D734A: db $0A
L0D734B: db $97
L0D734C: db $92
L0D734D: db $E7
L0D734E: db $00
L0D734F: db $03
L0D7350: db $31
L0D7351: db $73
L0D7352: db $ED
L0D7353: db $97
L0D7354: db $05
L0D7355: db $80
L0D7356: db $97
L0D7357: db $0A
L0D7358: db $9A
L0D7359: db $97
L0D735A: db $08
L0D735B: db $80
L0D735C: db $02
L0D735D: db $97
L0D735E: db $05
L0D735F: db $80
L0D7360: db $97
L0D7361: db $0A
L0D7362: db $9A
L0D7363: db $97
L0D7364: db $08
L0D7365: db $80
L0D7366: db $02
L0D7367: db $97
L0D7368: db $05
L0D7369: db $80
L0D736A: db $97
L0D736B: db $0A
L0D736C: db $9C
L0D736D: db $97
L0D736E: db $08
L0D736F: db $80
L0D7370: db $02
L0D7371: db $97
L0D7372: db $05
L0D7373: db $80
L0D7374: db $97
L0D7375: db $0A
L0D7376: db $9C
L0D7377: db $97
L0D7378: db $08
L0D7379: db $80
L0D737A: db $02
L0D737B: db $97
L0D737C: db $05
L0D737D: db $80
L0D737E: db $97
L0D737F: db $0A
L0D7380: db $9D
L0D7381: db $97
L0D7382: db $08
L0D7383: db $80
L0D7384: db $02
L0D7385: db $ED
L0D7386: db $E9
L0D7387: db $88
L0D7388: db $EC
L0D7389: db $C7
L0D738A: db $74
L0D738B: db $E4
L0D738C: db $C1
L0D738D: db $57
L0D738E: db $0A
L0D738F: db $E4
L0D7390: db $51
L0D7391: db $17
L0D7392: db $0A
L0D7393: db $E4
L0D7394: db $A1
L0D7395: db $26
L0D7396: db $0A
L0D7397: db $E4
L0D7398: db $53
L0D7399: db $13
L0D739A: db $0A
L0D739B: db $E4
L0D739C: db $C1
L0D739D: db $57
L0D739E: db $05
L0D739F: db $E4
L0D73A0: db $51
L0D73A1: db $17
L0D73A2: db $05
L0D73A3: db $E4
L0D73A4: db $C1
L0D73A5: db $57
L0D73A6: db $0A
L0D73A7: db $E4
L0D73A8: db $A1
L0D73A9: db $26
L0D73AA: db $0A
L0D73AB: db $E4
L0D73AC: db $53
L0D73AD: db $13
L0D73AE: db $0A
L0D73AF: db $E4
L0D73B0: db $C1
L0D73B1: db $57
L0D73B2: db $0A
L0D73B3: db $E4
L0D73B4: db $51
L0D73B5: db $17
L0D73B6: db $0A
L0D73B7: db $E4
L0D73B8: db $A1
L0D73B9: db $26
L0D73BA: db $0A
L0D73BB: db $E4
L0D73BC: db $53
L0D73BD: db $13
L0D73BE: db $0A
L0D73BF: db $E4
L0D73C0: db $C1
L0D73C1: db $57
L0D73C2: db $05
L0D73C3: db $E4
L0D73C4: db $51
L0D73C5: db $17
L0D73C6: db $05
L0D73C7: db $E4
L0D73C8: db $C1
L0D73C9: db $57
L0D73CA: db $0A
L0D73CB: db $E4
L0D73CC: db $A1
L0D73CD: db $26
L0D73CE: db $05
L0D73CF: db $26
L0D73D0: db $05
L0D73D1: db $26
L0D73D2: db $05
L0D73D3: db $26
L0D73D4: db $05
L0D73D5: db $EC
L0D73D6: db $19
L0D73D7: db $75
L0D73D8: db $E4
L0D73D9: db $A1
L0D73DA: db $26
L0D73DB: db $05
L0D73DC: db $26
L0D73DD: db $05
L0D73DE: db $E4
L0D73DF: db $91
L0D73E0: db $34
L0D73E1: db $05
L0D73E2: db $34
L0D73E3: db $05
L0D73E4: db $E4
L0D73E5: db $91
L0D73E6: db $35
L0D73E7: db $05
L0D73E8: db $35
L0D73E9: db $05
L0D73EA: db $EC
L0D73EB: db $19
L0D73EC: db $75
L0D73ED: db $E4
L0D73EE: db $A1
L0D73EF: db $26
L0D73F0: db $05
L0D73F1: db $26
L0D73F2: db $05
L0D73F3: db $E4
L0D73F4: db $C1
L0D73F5: db $57
L0D73F6: db $05
L0D73F7: db $57
L0D73F8: db $05
L0D73F9: db $E4
L0D73FA: db $A1
L0D73FB: db $26
L0D73FC: db $05
L0D73FD: db $26
L0D73FE: db $05
L0D73FF: db $EC
L0D7400: db $2B
L0D7401: db $75
L0D7402: db $EC
L0D7403: db $77
L0D7404: db $75
L0D7405: db $E4
L0D7406: db $A1
L0D7407: db $26
L0D7408: db $0A
L0D7409: db $E4
L0D740A: db $C1
L0D740B: db $57
L0D740C: db $0A
L0D740D: db $E4
L0D740E: db $A1
L0D740F: db $26
L0D7410: db $05
L0D7411: db $E4
L0D7412: db $C1
L0D7413: db $57
L0D7414: db $0A
L0D7415: db $57
L0D7416: db $05
L0D7417: db $E4
L0D7418: db $A1
L0D7419: db $26
L0D741A: db $0A
L0D741B: db $E4
L0D741C: db $C1
L0D741D: db $57
L0D741E: db $0A
L0D741F: db $E4
L0D7420: db $A1
L0D7421: db $26
L0D7422: db $0A
L0D7423: db $E4
L0D7424: db $C1
L0D7425: db $57
L0D7426: db $0A
L0D7427: db $EC
L0D7428: db $77
L0D7429: db $75
L0D742A: db $E4
L0D742B: db $A1
L0D742C: db $26
L0D742D: db $0A
L0D742E: db $E4
L0D742F: db $C1
L0D7430: db $57
L0D7431: db $0A
L0D7432: db $E4
L0D7433: db $A1
L0D7434: db $26
L0D7435: db $05
L0D7436: db $E4
L0D7437: db $C1
L0D7438: db $57
L0D7439: db $0A
L0D743A: db $E4
L0D743B: db $A1
L0D743C: db $26
L0D743D: db $05
L0D743E: db $E4
L0D743F: db $A1
L0D7440: db $26
L0D7441: db $0A
L0D7442: db $26
L0D7443: db $0A
L0D7444: db $26
L0D7445: db $0A
L0D7446: db $26
L0D7447: db $0A
L0D7448: db $EC
L0D7449: db $9A
L0D744A: db $75
L0D744B: db $E4
L0D744C: db $C1
L0D744D: db $57
L0D744E: db $0A
L0D744F: db $E4
L0D7450: db $51
L0D7451: db $17
L0D7452: db $0A
L0D7453: db $E4
L0D7454: db $A1
L0D7455: db $26
L0D7456: db $0A
L0D7457: db $E4
L0D7458: db $51
L0D7459: db $17
L0D745A: db $0A
L0D745B: db $E4
L0D745C: db $C1
L0D745D: db $57
L0D745E: db $0A
L0D745F: db $57
L0D7460: db $0A
L0D7461: db $E4
L0D7462: db $A1
L0D7463: db $26
L0D7464: db $0A
L0D7465: db $E4
L0D7466: db $C1
L0D7467: db $57
L0D7468: db $0A
L0D7469: db $E4
L0D746A: db $51
L0D746B: db $17
L0D746C: db $0A
L0D746D: db $E4
L0D746E: db $53
L0D746F: db $13
L0D7470: db $0A
L0D7471: db $E4
L0D7472: db $A1
L0D7473: db $26
L0D7474: db $0A
L0D7475: db $E4
L0D7476: db $53
L0D7477: db $13
L0D7478: db $0A
L0D7479: db $E4
L0D747A: db $A1
L0D747B: db $26
L0D747C: db $0A
L0D747D: db $E4
L0D747E: db $53
L0D747F: db $13
L0D7480: db $0A
L0D7481: db $E4
L0D7482: db $A1
L0D7483: db $26
L0D7484: db $0A
L0D7485: db $E4
L0D7486: db $53
L0D7487: db $13
L0D7488: db $0A
L0D7489: db $E4
L0D748A: db $51
L0D748B: db $17
L0D748C: db $0A
L0D748D: db $E4
L0D748E: db $A1
L0D748F: db $26
L0D7490: db $0A
L0D7491: db $26
L0D7492: db $0A
L0D7493: db $26
L0D7494: db $0A
L0D7495: db $26
L0D7496: db $0A
L0D7497: db $26
L0D7498: db $0A
L0D7499: db $26
L0D749A: db $0A
L0D749B: db $E4
L0D749C: db $C1
L0D749D: db $57
L0D749E: db $0A
L0D749F: db $EC
L0D74A0: db $DC
L0D74A1: db $75
L0D74A2: db $E4
L0D74A3: db $C1
L0D74A4: db $57
L0D74A5: db $05
L0D74A6: db $E4
L0D74A7: db $51
L0D74A8: db $17
L0D74A9: db $05
L0D74AA: db $E4
L0D74AB: db $C1
L0D74AC: db $57
L0D74AD: db $05
L0D74AE: db $E4
L0D74AF: db $51
L0D74B0: db $17
L0D74B1: db $05
L0D74B2: db $E4
L0D74B3: db $A1
L0D74B4: db $26
L0D74B5: db $0A
L0D74B6: db $E4
L0D74B7: db $C1
L0D74B8: db $57
L0D74B9: db $0A
L0D74BA: db $57
L0D74BB: db $0A
L0D74BC: db $E4
L0D74BD: db $A1
L0D74BE: db $26
L0D74BF: db $0A
L0D74C0: db $26
L0D74C1: db $0A
L0D74C2: db $26
L0D74C3: db $0A
L0D74C4: db $E5
L0D74C5: db $86
L0D74C6: db $73
L0D74C7: db $E4
L0D74C8: db $C1
L0D74C9: db $57
L0D74CA: db $0A
L0D74CB: db $E4
L0D74CC: db $51
L0D74CD: db $17
L0D74CE: db $0A
L0D74CF: db $E4
L0D74D0: db $A1
L0D74D1: db $26
L0D74D2: db $0A
L0D74D3: db $E4
L0D74D4: db $53
L0D74D5: db $13
L0D74D6: db $0A
L0D74D7: db $E4
L0D74D8: db $C1
L0D74D9: db $57
L0D74DA: db $05
L0D74DB: db $E4
L0D74DC: db $51
L0D74DD: db $17
L0D74DE: db $05
L0D74DF: db $E4
L0D74E0: db $C1
L0D74E1: db $57
L0D74E2: db $0A
L0D74E3: db $E4
L0D74E4: db $A1
L0D74E5: db $26
L0D74E6: db $0A
L0D74E7: db $E4
L0D74E8: db $53
L0D74E9: db $13
L0D74EA: db $0A
L0D74EB: db $E4
L0D74EC: db $C1
L0D74ED: db $57
L0D74EE: db $0A
L0D74EF: db $E4
L0D74F0: db $51
L0D74F1: db $17
L0D74F2: db $0A
L0D74F3: db $E4
L0D74F4: db $A1
L0D74F5: db $26
L0D74F6: db $0A
L0D74F7: db $E4
L0D74F8: db $53
L0D74F9: db $13
L0D74FA: db $0A
L0D74FB: db $E4
L0D74FC: db $C1
L0D74FD: db $57
L0D74FE: db $05
L0D74FF: db $E4
L0D7500: db $51
L0D7501: db $17
L0D7502: db $05
L0D7503: db $E4
L0D7504: db $C1
L0D7505: db $57
L0D7506: db $0A
L0D7507: db $E4
L0D7508: db $A1
L0D7509: db $26
L0D750A: db $0A
L0D750B: db $E4
L0D750C: db $53
L0D750D: db $13
L0D750E: db $05
L0D750F: db $E4
L0D7510: db $A1
L0D7511: db $26
L0D7512: db $05
L0D7513: db $E7
L0D7514: db $00
L0D7515: db $03
L0D7516: db $C7
L0D7517: db $74
L0D7518: db $ED
L0D7519: db $E4
L0D751A: db $A1
L0D751B: db $26
L0D751C: db $05
L0D751D: db $26
L0D751E: db $05
L0D751F: db $E4
L0D7520: db $C1
L0D7521: db $57
L0D7522: db $0A
L0D7523: db $57
L0D7524: db $0A
L0D7525: db $E7
L0D7526: db $00
L0D7527: db $07
L0D7528: db $19
L0D7529: db $75
L0D752A: db $ED
L0D752B: db $E4
L0D752C: db $C1
L0D752D: db $57
L0D752E: db $0A
L0D752F: db $E4
L0D7530: db $51
L0D7531: db $17
L0D7532: db $0A
L0D7533: db $E4
L0D7534: db $A1
L0D7535: db $26
L0D7536: db $0A
L0D7537: db $E4
L0D7538: db $51
L0D7539: db $17
L0D753A: db $0A
L0D753B: db $E4
L0D753C: db $C1
L0D753D: db $57
L0D753E: db $05
L0D753F: db $E4
L0D7540: db $51
L0D7541: db $17
L0D7542: db $05
L0D7543: db $E4
L0D7544: db $C1
L0D7545: db $57
L0D7546: db $0A
L0D7547: db $E4
L0D7548: db $A1
L0D7549: db $26
L0D754A: db $0A
L0D754B: db $26
L0D754C: db $0A
L0D754D: db $E4
L0D754E: db $C1
L0D754F: db $57
L0D7550: db $0A
L0D7551: db $E4
L0D7552: db $51
L0D7553: db $17
L0D7554: db $0A
L0D7555: db $E4
L0D7556: db $A1
L0D7557: db $26
L0D7558: db $0A
L0D7559: db $E4
L0D755A: db $C1
L0D755B: db $57
L0D755C: db $0A
L0D755D: db $E4
L0D755E: db $51
L0D755F: db $17
L0D7560: db $05
L0D7561: db $E4
L0D7562: db $53
L0D7563: db $13
L0D7564: db $05
L0D7565: db $E4
L0D7566: db $C1
L0D7567: db $57
L0D7568: db $0A
L0D7569: db $E4
L0D756A: db $A1
L0D756B: db $26
L0D756C: db $0A
L0D756D: db $E4
L0D756E: db $53
L0D756F: db $13
L0D7570: db $0A
L0D7571: db $E7
L0D7572: db $00
L0D7573: db $06
L0D7574: db $2B
L0D7575: db $75
L0D7576: db $ED
L0D7577: db $E4
L0D7578: db $A1
L0D7579: db $26
L0D757A: db $0A
L0D757B: db $E4
L0D757C: db $C1
L0D757D: db $57
L0D757E: db $0A
L0D757F: db $E4
L0D7580: db $A1
L0D7581: db $26
L0D7582: db $0A
L0D7583: db $E4
L0D7584: db $C1
L0D7585: db $57
L0D7586: db $05
L0D7587: db $57
L0D7588: db $05
L0D7589: db $E4
L0D758A: db $A1
L0D758B: db $26
L0D758C: db $0A
L0D758D: db $E4
L0D758E: db $C1
L0D758F: db $57
L0D7590: db $0A
L0D7591: db $E4
L0D7592: db $A1
L0D7593: db $26
L0D7594: db $0A
L0D7595: db $E4
L0D7596: db $C1
L0D7597: db $57
L0D7598: db $0A
L0D7599: db $ED
L0D759A: db $E4
L0D759B: db $C1
L0D759C: db $57
L0D759D: db $0A
L0D759E: db $E4
L0D759F: db $51
L0D75A0: db $17
L0D75A1: db $0A
L0D75A2: db $E4
L0D75A3: db $A1
L0D75A4: db $26
L0D75A5: db $0A
L0D75A6: db $E4
L0D75A7: db $51
L0D75A8: db $17
L0D75A9: db $0A
L0D75AA: db $E4
L0D75AB: db $C1
L0D75AC: db $57
L0D75AD: db $0A
L0D75AE: db $57
L0D75AF: db $0A
L0D75B0: db $E4
L0D75B1: db $A1
L0D75B2: db $26
L0D75B3: db $0A
L0D75B4: db $E4
L0D75B5: db $C1
L0D75B6: db $57
L0D75B7: db $0A
L0D75B8: db $E4
L0D75B9: db $51
L0D75BA: db $17
L0D75BB: db $0A
L0D75BC: db $E4
L0D75BD: db $C1
L0D75BE: db $57
L0D75BF: db $0A
L0D75C0: db $E4
L0D75C1: db $A1
L0D75C2: db $26
L0D75C3: db $0A
L0D75C4: db $E4
L0D75C5: db $53
L0D75C6: db $13
L0D75C7: db $0A
L0D75C8: db $E4
L0D75C9: db $C1
L0D75CA: db $57
L0D75CB: db $0A
L0D75CC: db $57
L0D75CD: db $0A
L0D75CE: db $E4
L0D75CF: db $A1
L0D75D0: db $26
L0D75D1: db $0A
L0D75D2: db $E4
L0D75D3: db $53
L0D75D4: db $13
L0D75D5: db $0A
L0D75D6: db $E7
L0D75D7: db $00
L0D75D8: db $03
L0D75D9: db $9A
L0D75DA: db $75
L0D75DB: db $ED
L0D75DC: db $E4
L0D75DD: db $C1
L0D75DE: db $57
L0D75DF: db $05
L0D75E0: db $E4
L0D75E1: db $51
L0D75E2: db $17
L0D75E3: db $05
L0D75E4: db $E4
L0D75E5: db $C1
L0D75E6: db $57
L0D75E7: db $05
L0D75E8: db $E4
L0D75E9: db $51
L0D75EA: db $17
L0D75EB: db $05
L0D75EC: db $E4
L0D75ED: db $A1
L0D75EE: db $26
L0D75EF: db $0A
L0D75F0: db $E4
L0D75F1: db $C1
L0D75F2: db $57
L0D75F3: db $05
L0D75F4: db $E4
L0D75F5: db $53
L0D75F6: db $13
L0D75F7: db $05
L0D75F8: db $E7
L0D75F9: db $00
L0D75FA: db $0C
L0D75FB: db $DC
L0D75FC: db $75
L0D75FD: db $ED
SndHeader_BGM_09: db $03
L0D75FF: db $80
L0D7600: db $13
L0D7601: db $17
L0D7602: db $76
L0D7603: db $03
L0D7604: db $81
L0D7605: db $80
L0D7606: db $18
L0D7607: db $71
L0D7608: db $78
L0D7609: db $03
L0D760A: db $81
L0D760B: db $80
L0D760C: db $1D
L0D760D: db $10
L0D760E: db $7E
L0D760F: db $03
L0D7610: db $81
L0D7611: db $80;X
L0D7612: db $22;X
L0D7613: db $C9;X
L0D7614: db $7F;X
L0D7615: db $00;X
L0D7616: db $81;X
L0D7617: db $E4
L0D7618: db $11
L0D7619: db $E9
L0D761A: db $11
L0D761B: db $EE
L0D761C: db $80
L0D761D: db $F1
L0D761E: db $03
L0D761F: db $E4
L0D7620: db $A8
L0D7621: db $97
L0D7622: db $20
L0D7623: db $E4
L0D7624: db $88
L0D7625: db $97
L0D7626: db $08
L0D7627: db $80
L0D7628: db $04
L0D7629: db $E4
L0D762A: db $68
L0D762B: db $97
L0D762C: db $E4
L0D762D: db $A8
L0D762E: db $92
L0D762F: db $08
L0D7630: db $94
L0D7631: db $95
L0D7632: db $10
L0D7633: db $97
L0D7634: db $99
L0D7635: db $08
L0D7636: db $9A
L0D7637: db $9C
L0D7638: db $9E
L0D7639: db $9F
L0D763A: db $20
L0D763B: db $80
L0D763C: db $04
L0D763D: db $E4
L0D763E: db $88
L0D763F: db $9F
L0D7640: db $04
L0D7641: db $E4
L0D7642: db $A8
L0D7643: db $95
L0D7644: db $08
L0D7645: db $98
L0D7646: db $9C
L0D7647: db $A1
L0D7648: db $18
L0D7649: db $80
L0D764A: db $04
L0D764B: db $E4
L0D764C: db $88
L0D764D: db $A1
L0D764E: db $E4
L0D764F: db $A8
L0D7650: db $9F
L0D7651: db $10
L0D7652: db $A1
L0D7653: db $9E
L0D7654: db $20
L0D7655: db $80
L0D7656: db $04
L0D7657: db $E4
L0D7658: db $88
L0D7659: db $9E
L0D765A: db $04
L0D765B: db $E4
L0D765C: db $A8
L0D765D: db $A3
L0D765E: db $08
L0D765F: db $A2
L0D7660: db $A3
L0D7661: db $A5
L0D7662: db $9E
L0D7663: db $AA
L0D7664: db $10
L0D7665: db $9E
L0D7666: db $A0
L0D7667: db $A1
L0D7668: db $20
L0D7669: db $80
L0D766A: db $04
L0D766B: db $E4
L0D766C: db $88
L0D766D: db $A1
L0D766E: db $E4
L0D766F: db $A8
L0D7670: db $A8
L0D7671: db $08
L0D7672: db $A1
L0D7673: db $A5
L0D7674: db $A8
L0D7675: db $A1
L0D7676: db $AD
L0D7677: db $10
L0D7678: db $A8
L0D7679: db $AA
L0D767A: db $EE
L0D767B: db $C0
L0D767C: db $EC
L0D767D: db $F7
L0D767E: db $77
L0D767F: db $EC
L0D7680: db $02
L0D7681: db $78
L0D7682: db $EC
L0D7683: db $5F
L0D7684: db $78
L0D7685: db $B3
L0D7686: db $06
L0D7687: db $80
L0D7688: db $02
L0D7689: db $B3
L0D768A: db $06
L0D768B: db $80
L0D768C: db $02
L0D768D: db $B4
L0D768E: db $06
L0D768F: db $80
L0D7690: db $02
L0D7691: db $B5
L0D7692: db $06
L0D7693: db $80
L0D7694: db $02
L0D7695: db $B6
L0D7696: db $60
L0D7697: db $E4
L0D7698: db $88
L0D7699: db $B6
L0D769A: db $08
L0D769B: db $80
L0D769C: db $04
L0D769D: db $E4
L0D769E: db $68
L0D769F: db $B6
L0D76A0: db $E4
L0D76A1: db $48
L0D76A2: db $B6
L0D76A3: db $08
L0D76A4: db $80
L0D76A5: db $04
L0D76A6: db $E4
L0D76A7: db $38
L0D76A8: db $B6
L0D76A9: db $EE
L0D76AA: db $80
L0D76AB: db $EC
L0D76AC: db $0D
L0D76AD: db $78
L0D76AE: db $A9
L0D76AF: db $40
L0D76B0: db $FA
L0D76B1: db $08
L0D76B2: db $E4
L0D76B3: db $88
L0D76B4: db $A9
L0D76B5: db $80
L0D76B6: db $04
L0D76B7: db $E4
L0D76B8: db $68
L0D76B9: db $A9
L0D76BA: db $E4
L0D76BB: db $48
L0D76BC: db $A9
L0D76BD: db $08
L0D76BE: db $80
L0D76BF: db $04
L0D76C0: db $E4
L0D76C1: db $38
L0D76C2: db $A9
L0D76C3: db $E4
L0D76C4: db $98
L0D76C5: db $99
L0D76C6: db $08
L0D76C7: db $E4
L0D76C8: db $48
L0D76C9: db $9B
L0D76CA: db $E4
L0D76CB: db $68
L0D76CC: db $99
L0D76CD: db $9B
L0D76CE: db $E4
L0D76CF: db $88
L0D76D0: db $99
L0D76D1: db $9B
L0D76D2: db $E4
L0D76D3: db $A8
L0D76D4: db $99
L0D76D5: db $9B
L0D76D6: db $E4
L0D76D7: db $C8
L0D76D8: db $99
L0D76D9: db $9B
L0D76DA: db $E4
L0D76DB: db $D8
L0D76DC: db $99
L0D76DD: db $9B
L0D76DE: db $E4
L0D76DF: db $E8
L0D76E0: db $99
L0D76E1: db $9B
L0D76E2: db $E4
L0D76E3: db $F8
L0D76E4: db $99
L0D76E5: db $9B
L0D76E6: db $EC
L0D76E7: db $0D
L0D76E8: db $78
L0D76E9: db $AC
L0D76EA: db $40
L0D76EB: db $FA
L0D76EC: db $08
L0D76ED: db $E4
L0D76EE: db $88
L0D76EF: db $AC
L0D76F0: db $80
L0D76F1: db $04
L0D76F2: db $E4
L0D76F3: db $68
L0D76F4: db $AC
L0D76F5: db $E4
L0D76F6: db $48
L0D76F7: db $AC
L0D76F8: db $08
L0D76F9: db $80
L0D76FA: db $04
L0D76FB: db $E4
L0D76FC: db $38
L0D76FD: db $AC
L0D76FE: db $E4
L0D76FF: db $98
L0D7700: db $9B
L0D7701: db $08
L0D7702: db $E4
L0D7703: db $48
L0D7704: db $9D
L0D7705: db $E4
L0D7706: db $68
L0D7707: db $9B
L0D7708: db $9D
L0D7709: db $E4
L0D770A: db $88
L0D770B: db $9B
L0D770C: db $9D
L0D770D: db $E4
L0D770E: db $A8
L0D770F: db $9B
L0D7710: db $9D
L0D7711: db $E4
L0D7712: db $C8
L0D7713: db $9B
L0D7714: db $9D
L0D7715: db $E4
L0D7716: db $D8
L0D7717: db $9E
L0D7718: db $A0
L0D7719: db $E4
L0D771A: db $E8
L0D771B: db $A2
L0D771C: db $A4
L0D771D: db $E4
L0D771E: db $F8
L0D771F: db $A5
L0D7720: db $A7
L0D7721: db $EC
L0D7722: db $2F
L0D7723: db $78
L0D7724: db $E6
L0D7725: db $01
L0D7726: db $EC
L0D7727: db $2F
L0D7728: db $78
L0D7729: db $E6
L0D772A: db $02
L0D772B: db $EC
L0D772C: db $2F
L0D772D: db $78
L0D772E: db $E6
L0D772F: db $02
L0D7730: db $EC
L0D7731: db $2F
L0D7732: db $78
L0D7733: db $E6
L0D7734: db $FB
L0D7735: db $EC
L0D7736: db $4B
L0D7737: db $78
L0D7738: db $E6
L0D7739: db $03
L0D773A: db $EC
L0D773B: db $4B
L0D773C: db $78
L0D773D: db $E6
L0D773E: db $FD
L0D773F: db $9B
L0D7740: db $08
L0D7741: db $9D
L0D7742: db $9F
L0D7743: db $A0
L0D7744: db $9D
L0D7745: db $9F
L0D7746: db $A0
L0D7747: db $A2
L0D7748: db $9E
L0D7749: db $A0
L0D774A: db $A2
L0D774B: db $A4
L0D774C: db $A2
L0D774D: db $A4
L0D774E: db $A5
L0D774F: db $A7
L0D7750: db $A1
L0D7751: db $A3
L0D7752: db $A5
L0D7753: db $A6
L0D7754: db $A3
L0D7755: db $A5
L0D7756: db $A6
L0D7757: db $A8
L0D7758: db $A5
L0D7759: db $A7
L0D775A: db $A9
L0D775B: db $AA
L0D775C: db $AC
L0D775D: db $AE
L0D775E: db $AF
L0D775F: db $B0
L0D7760: db $E4
L0D7761: db $A8
L0D7762: db $AA
L0D7763: db $10
L0D7764: db $80
L0D7765: db $04
L0D7766: db $E4
L0D7767: db $88
L0D7768: db $AA
L0D7769: db $E4
L0D776A: db $A8
L0D776B: db $A7
L0D776C: db $10
L0D776D: db $80
L0D776E: db $04
L0D776F: db $E4
L0D7770: db $88
L0D7771: db $A7
L0D7772: db $E4
L0D7773: db $A8
L0D7774: db $AA
L0D7775: db $08
L0D7776: db $80
L0D7777: db $04
L0D7778: db $E4
L0D7779: db $88
L0D777A: db $AA
L0D777B: db $E4
L0D777C: db $A8
L0D777D: db $AC
L0D777E: db $10
L0D777F: db $80
L0D7780: db $04
L0D7781: db $E4
L0D7782: db $88
L0D7783: db $AC
L0D7784: db $E4
L0D7785: db $A8
L0D7786: db $A9
L0D7787: db $10
L0D7788: db $80
L0D7789: db $04
L0D778A: db $E4
L0D778B: db $88
L0D778C: db $A9
L0D778D: db $E4
L0D778E: db $A8
L0D778F: db $AC
L0D7790: db $08
L0D7791: db $80
L0D7792: db $04
L0D7793: db $E4
L0D7794: db $88
L0D7795: db $AC
L0D7796: db $E4
L0D7797: db $A8
L0D7798: db $AE
L0D7799: db $30
L0D779A: db $E4
L0D779B: db $88
L0D779C: db $AE
L0D779D: db $08
L0D779E: db $80
L0D779F: db $04
L0D77A0: db $E4
L0D77A1: db $68
L0D77A2: db $AA
L0D77A3: db $E4
L0D77A4: db $A8
L0D77A5: db $B3
L0D77A6: db $10
L0D77A7: db $B0
L0D77A8: db $AC
L0D77A9: db $A7
L0D77AA: db $E4
L0D77AB: db $A8
L0D77AC: db $AA
L0D77AD: db $10
L0D77AE: db $80
L0D77AF: db $04
L0D77B0: db $E4
L0D77B1: db $88
L0D77B2: db $AA
L0D77B3: db $E4
L0D77B4: db $A8
L0D77B5: db $A7
L0D77B6: db $10
L0D77B7: db $80
L0D77B8: db $04
L0D77B9: db $E4
L0D77BA: db $88
L0D77BB: db $A7
L0D77BC: db $E4
L0D77BD: db $A8
L0D77BE: db $AA
L0D77BF: db $08
L0D77C0: db $80
L0D77C1: db $04
L0D77C2: db $E4
L0D77C3: db $88
L0D77C4: db $AA
L0D77C5: db $E4
L0D77C6: db $A8
L0D77C7: db $AC
L0D77C8: db $18
L0D77C9: db $80
L0D77CA: db $04
L0D77CB: db $E4
L0D77CC: db $88
L0D77CD: db $AC
L0D77CE: db $E4
L0D77CF: db $A8
L0D77D0: db $AC
L0D77D1: db $06
L0D77D2: db $80
L0D77D3: db $02
L0D77D4: db $AC
L0D77D5: db $06
L0D77D6: db $80
L0D77D7: db $02
L0D77D8: db $AC
L0D77D9: db $06
L0D77DA: db $80
L0D77DB: db $02
L0D77DC: db $AC
L0D77DD: db $06
L0D77DE: db $80
L0D77DF: db $02
L0D77E0: db $B3
L0D77E1: db $60
L0D77E2: db $E4
L0D77E3: db $88
L0D77E4: db $B3
L0D77E5: db $08
L0D77E6: db $80
L0D77E7: db $04
L0D77E8: db $E4
L0D77E9: db $68
L0D77EA: db $B3
L0D77EB: db $E4
L0D77EC: db $48
L0D77ED: db $B3
L0D77EE: db $08
L0D77EF: db $80
L0D77F0: db $04
L0D77F1: db $E4
L0D77F2: db $38
L0D77F3: db $B3
L0D77F4: db $E5
L0D77F5: db $17
L0D77F6: db $76
L0D77F7: db $AE
L0D77F8: db $08
L0D77F9: db $A7
L0D77FA: db $B1
L0D77FB: db $AC
L0D77FC: db $E7
L0D77FD: db $00
L0D77FE: db $04
L0D77FF: db $F7
L0D7800: db $77
L0D7801: db $ED
L0D7802: db $AD
L0D7803: db $08
L0D7804: db $A8
L0D7805: db $B2
L0D7806: db $AD
L0D7807: db $E7
L0D7808: db $00
L0D7809: db $04
L0D780A: db $02
L0D780B: db $78
L0D780C: db $ED
L0D780D: db $E4
L0D780E: db $A8
L0D780F: db $9D
L0D7810: db $20
L0D7811: db $E4
L0D7812: db $88
L0D7813: db $9D
L0D7814: db $08
L0D7815: db $80
L0D7816: db $04
L0D7817: db $E4
L0D7818: db $68
L0D7819: db $9D
L0D781A: db $E4
L0D781B: db $A8
L0D781C: db $A0
L0D781D: db $08
L0D781E: db $9E
L0D781F: db $E7
L0D7820: db $00
L0D7821: db $03
L0D7822: db $0D
L0D7823: db $78
L0D7824: db $9D
L0D7825: db $10
L0D7826: db $A0
L0D7827: db $A5
L0D7828: db $A9
L0D7829: db $AA
L0D782A: db $A9
L0D782B: db $06
L0D782C: db $80
L0D782D: db $02
L0D782E: db $ED
L0D782F: db $E4
L0D7830: db $A8
L0D7831: db $A9
L0D7832: db $10
L0D7833: db $80
L0D7834: db $04
L0D7835: db $E4
L0D7836: db $88
L0D7837: db $A9
L0D7838: db $E4
L0D7839: db $A8
L0D783A: db $A9
L0D783B: db $10
L0D783C: db $80
L0D783D: db $04
L0D783E: db $E4
L0D783F: db $88
L0D7840: db $A9
L0D7841: db $E4
L0D7842: db $A8
L0D7843: db $A9
L0D7844: db $08
L0D7845: db $80
L0D7846: db $04
L0D7847: db $E4
L0D7848: db $88
L0D7849: db $A9
L0D784A: db $ED
L0D784B: db $E4
L0D784C: db $A8
L0D784D: db $9B
L0D784E: db $08
L0D784F: db $A0
L0D7850: db $A4
L0D7851: db $A7
L0D7852: db $18
L0D7853: db $A4
L0D7854: db $08
L0D7855: db $A0
L0D7856: db $A9
L0D7857: db $20
L0D7858: db $A7
L0D7859: db $E7
L0D785A: db $00
L0D785B: db $02
L0D785C: db $4B
L0D785D: db $78
L0D785E: db $ED
L0D785F: db $B3
L0D7860: db $06
L0D7861: db $80
L0D7862: db $02
L0D7863: db $B3
L0D7864: db $06
L0D7865: db $80
L0D7866: db $02
L0D7867: db $B3
L0D7868: db $0E
L0D7869: db $80
L0D786A: db $02
L0D786B: db $E7
L0D786C: db $00
L0D786D: db $03
L0D786E: db $5F
L0D786F: db $78
L0D7870: db $ED
L0D7871: db $E4
L0D7872: db $A8
L0D7873: db $E9
L0D7874: db $22
L0D7875: db $EE
L0D7876: db $C0
L0D7877: db $E4
L0D7878: db $A8
L0D7879: db $92
L0D787A: db $20
L0D787B: db $E4
L0D787C: db $88
L0D787D: db $92
L0D787E: db $08
L0D787F: db $80
L0D7880: db $04
L0D7881: db $E4
L0D7882: db $68
L0D7883: db $92
L0D7884: db $E4
L0D7885: db $A8
L0D7886: db $8D
L0D7887: db $08
L0D7888: db $8F
L0D7889: db $04
L0D788A: db $E4
L0D788B: db $88
L0D788C: db $8D
L0D788D: db $E4
L0D788E: db $A8
L0D788F: db $90
L0D7890: db $08
L0D7891: db $80
L0D7892: db $04
L0D7893: db $E4
L0D7894: db $88
L0D7895: db $90
L0D7896: db $E4
L0D7897: db $A8
L0D7898: db $92
L0D7899: db $08
L0D789A: db $80
L0D789B: db $04
L0D789C: db $E4
L0D789D: db $88
L0D789E: db $92
L0D789F: db $E4
L0D78A0: db $A8
L0D78A1: db $95
L0D78A2: db $08
L0D78A3: db $97
L0D78A4: db $04
L0D78A5: db $E4
L0D78A6: db $88
L0D78A7: db $95
L0D78A8: db $E4
L0D78A9: db $A8
L0D78AA: db $99
L0D78AB: db $E4
L0D78AC: db $88
L0D78AD: db $97
L0D78AE: db $E4
L0D78AF: db $A8
L0D78B0: db $9A
L0D78B1: db $E4
L0D78B2: db $88
L0D78B3: db $99
L0D78B4: db $E4
L0D78B5: db $A8
L0D78B6: db $9C
L0D78B7: db $20
L0D78B8: db $80
L0D78B9: db $04
L0D78BA: db $E4
L0D78BB: db $88
L0D78BC: db $9C
L0D78BD: db $E4
L0D78BE: db $A8
L0D78BF: db $92
L0D78C0: db $80
L0D78C1: db $95
L0D78C2: db $04
L0D78C3: db $E4
L0D78C4: db $88
L0D78C5: db $92
L0D78C6: db $E4
L0D78C7: db $A8
L0D78C8: db $98
L0D78C9: db $E4
L0D78CA: db $88
L0D78CB: db $95
L0D78CC: db $E4
L0D78CD: db $A8
L0D78CE: db $9A
L0D78CF: db $10
L0D78D0: db $E4
L0D78D1: db $88
L0D78D2: db $9A
L0D78D3: db $0C
L0D78D4: db $80
L0D78D5: db $04
L0D78D6: db $E4
L0D78D7: db $A8
L0D78D8: db $98
L0D78D9: db $08
L0D78DA: db $80
L0D78DB: db $04
L0D78DC: db $E4
L0D78DD: db $88
L0D78DE: db $98
L0D78DF: db $E4
L0D78E0: db $A8
L0D78E1: db $9A
L0D78E2: db $08
L0D78E3: db $80
L0D78E4: db $04
L0D78E5: db $E4
L0D78E6: db $88
L0D78E7: db $9A
L0D78E8: db $E4
L0D78E9: db $A8
L0D78EA: db $99
L0D78EB: db $20
L0D78EC: db $80
L0D78ED: db $04
L0D78EE: db $E4
L0D78EF: db $88
L0D78F0: db $99
L0D78F1: db $E4
L0D78F2: db $A8
L0D78F3: db $9E
L0D78F4: db $08
L0D78F5: db $9D
L0D78F6: db $04
L0D78F7: db $E4
L0D78F8: db $88
L0D78F9: db $9E
L0D78FA: db $E4
L0D78FB: db $A8
L0D78FC: db $9E
L0D78FD: db $E4
L0D78FE: db $88
L0D78FF: db $9D
L0D7900: db $E4
L0D7901: db $A8
L0D7902: db $A0
L0D7903: db $E4
L0D7904: db $88
L0D7905: db $9E
L0D7906: db $E4
L0D7907: db $A8
L0D7908: db $99
L0D7909: db $E4
L0D790A: db $88
L0D790B: db $A0
L0D790C: db $E4
L0D790D: db $A8
L0D790E: db $A5
L0D790F: db $0C
L0D7910: db $E4
L0D7911: db $88
L0D7912: db $A5
L0D7913: db $04
L0D7914: db $E4
L0D7915: db $A8
L0D7916: db $99
L0D7917: db $0C
L0D7918: db $E4
L0D7919: db $88
L0D791A: db $99
L0D791B: db $04
L0D791C: db $E4
L0D791D: db $A8
L0D791E: db $9B
L0D791F: db $0C
L0D7920: db $E4
L0D7921: db $88
L0D7922: db $9B
L0D7923: db $04
L0D7924: db $E4
L0D7925: db $A8
L0D7926: db $9C
L0D7927: db $20
L0D7928: db $80
L0D7929: db $04
L0D792A: db $E4
L0D792B: db $88
L0D792C: db $9C
L0D792D: db $E4
L0D792E: db $A8
L0D792F: db $A3
L0D7930: db $08
L0D7931: db $9E
L0D7932: db $04
L0D7933: db $E4
L0D7934: db $88
L0D7935: db $A3
L0D7936: db $E4
L0D7937: db $A8
L0D7938: db $A1
L0D7939: db $E4
L0D793A: db $88
L0D793B: db $9E
L0D793C: db $E4
L0D793D: db $A8
L0D793E: db $A3
L0D793F: db $E4
L0D7940: db $88
L0D7941: db $A1
L0D7942: db $E4
L0D7943: db $A8
L0D7944: db $9E
L0D7945: db $E4
L0D7946: db $88
L0D7947: db $A3
L0D7948: db $E4
L0D7949: db $A8
L0D794A: db $A5
L0D794B: db $08
L0D794C: db $80
L0D794D: db $04
L0D794E: db $E4
L0D794F: db $88
L0D7950: db $A5
L0D7951: db $E4
L0D7952: db $A8
L0D7953: db $A3
L0D7954: db $08
L0D7955: db $80
L0D7956: db $04
L0D7957: db $E4
L0D7958: db $88
L0D7959: db $A3
L0D795A: db $E4
L0D795B: db $A8
L0D795C: db $A6
L0D795D: db $08
L0D795E: db $80
L0D795F: db $04
L0D7960: db $E4
L0D7961: db $88
L0D7962: db $A6
L0D7963: db $EC
L0D7964: db $96
L0D7965: db $7C
L0D7966: db $E4
L0D7967: db $A8
L0D7968: db $A3
L0D7969: db $04
L0D796A: db $E4
L0D796B: db $88
L0D796C: db $9B
L0D796D: db $E4
L0D796E: db $A8
L0D796F: db $A4
L0D7970: db $E4
L0D7971: db $88
L0D7972: db $A3
L0D7973: db $E4
L0D7974: db $A8
L0D7975: db $A6
L0D7976: db $E4
L0D7977: db $88
L0D7978: db $A4
L0D7979: db $E4
L0D797A: db $A8
L0D797B: db $9F
L0D797C: db $E4
L0D797D: db $88
L0D797E: db $A6
L0D797F: db $EC
L0D7980: db $B5
L0D7981: db $7C
L0D7982: db $E4
L0D7983: db $A8
L0D7984: db $A3
L0D7985: db $04
L0D7986: db $E4
L0D7987: db $88
L0D7988: db $9F
L0D7989: db $E4
L0D798A: db $A8
L0D798B: db $A2
L0D798C: db $04
L0D798D: db $E4
L0D798E: db $88
L0D798F: db $A3
L0D7990: db $E4
L0D7991: db $A8
L0D7992: db $A3
L0D7993: db $08
L0D7994: db $80
L0D7995: db $04
L0D7996: db $E4
L0D7997: db $88
L0D7998: db $A3
L0D7999: db $E7
L0D799A: db $00
L0D799B: db $03
L0D799C: db $82
L0D799D: db $79
L0D799E: db $E4
L0D799F: db $A8
L0D79A0: db $A3
L0D79A1: db $08
L0D79A2: db $A4
L0D79A3: db $04
L0D79A4: db $E4
L0D79A5: db $88
L0D79A6: db $A3
L0D79A7: db $E4
L0D79A8: db $A8
L0D79A9: db $A5
L0D79AA: db $E4
L0D79AB: db $88
L0D79AC: db $A4
L0D79AD: db $E4
L0D79AE: db $A8
L0D79AF: db $A6
L0D79B0: db $E4
L0D79B1: db $88
L0D79B2: db $A5
L0D79B3: db $E4
L0D79B4: db $98
L0D79B5: db $A7
L0D79B6: db $05
L0D79B7: db $A3
L0D79B8: db $06
L0D79B9: db $A7
L0D79BA: db $05
L0D79BB: db $AF
L0D79BC: db $A7
L0D79BD: db $06
L0D79BE: db $A3
L0D79BF: db $05
L0D79C0: db $E4
L0D79C1: db $88
L0D79C2: db $A7
L0D79C3: db $05
L0D79C4: db $A3
L0D79C5: db $06
L0D79C6: db $A7
L0D79C7: db $05
L0D79C8: db $AF
L0D79C9: db $A7
L0D79CA: db $06
L0D79CB: db $A3
L0D79CC: db $05
L0D79CD: db $E4
L0D79CE: db $78
L0D79CF: db $A7
L0D79D0: db $05
L0D79D1: db $A3
L0D79D2: db $06
L0D79D3: db $A7
L0D79D4: db $05
L0D79D5: db $AF
L0D79D6: db $A7
L0D79D7: db $06
L0D79D8: db $A3
L0D79D9: db $05
L0D79DA: db $E4
L0D79DB: db $58
L0D79DC: db $A7
L0D79DD: db $05
L0D79DE: db $A3
L0D79DF: db $06
L0D79E0: db $A7
L0D79E1: db $05
L0D79E2: db $AF
L0D79E3: db $A7
L0D79E4: db $06
L0D79E5: db $A3
L0D79E6: db $05
L0D79E7: db $EC
L0D79E8: db $D4
L0D79E9: db $7C
L0D79EA: db $EC
L0D79EB: db $1F
L0D79EC: db $7D
L0D79ED: db $E4
L0D79EE: db $A8
L0D79EF: db $94
L0D79F0: db $E4
L0D79F1: db $88
L0D79F2: db $99
L0D79F3: db $EC
L0D79F4: db $1F
L0D79F5: db $7D
L0D79F6: db $E4
L0D79F7: db $A8
L0D79F8: db $A0
L0D79F9: db $E4
L0D79FA: db $88
L0D79FB: db $99
L0D79FC: db $E4
L0D79FD: db $A8
L0D79FE: db $91
L0D79FF: db $E4
L0D7A00: db $88
L0D7A01: db $92
L0D7A02: db $E4
L0D7A03: db $58
L0D7A04: db $92
L0D7A05: db $E4
L0D7A06: db $38
L0D7A07: db $91
L0D7A08: db $E4
L0D7A09: db $68
L0D7A0A: db $94
L0D7A0B: db $E4
L0D7A0C: db $48
L0D7A0D: db $92
L0D7A0E: db $E4
L0D7A0F: db $68
L0D7A10: db $92
L0D7A11: db $E4
L0D7A12: db $48
L0D7A13: db $94
L0D7A14: db $E4
L0D7A15: db $78
L0D7A16: db $91
L0D7A17: db $E4
L0D7A18: db $58
L0D7A19: db $92
L0D7A1A: db $E4
L0D7A1B: db $78
L0D7A1C: db $92
L0D7A1D: db $E4
L0D7A1E: db $58
L0D7A1F: db $91
L0D7A20: db $E4
L0D7A21: db $88
L0D7A22: db $94
L0D7A23: db $E4
L0D7A24: db $68
L0D7A25: db $92
L0D7A26: db $E4
L0D7A27: db $88
L0D7A28: db $92
L0D7A29: db $E4
L0D7A2A: db $68
L0D7A2B: db $94
L0D7A2C: db $E4
L0D7A2D: db $98
L0D7A2E: db $91
L0D7A2F: db $E4
L0D7A30: db $78
L0D7A31: db $92
L0D7A32: db $E4
L0D7A33: db $98
L0D7A34: db $92
L0D7A35: db $E4
L0D7A36: db $78
L0D7A37: db $91
L0D7A38: db $E4
L0D7A39: db $A8
L0D7A3A: db $94
L0D7A3B: db $E4
L0D7A3C: db $88
L0D7A3D: db $92
L0D7A3E: db $E4
L0D7A3F: db $B8
L0D7A40: db $92
L0D7A41: db $E4
L0D7A42: db $98
L0D7A43: db $94
L0D7A44: db $E4
L0D7A45: db $C8
L0D7A46: db $91
L0D7A47: db $E4
L0D7A48: db $A8
L0D7A49: db $92
L0D7A4A: db $E4
L0D7A4B: db $D8
L0D7A4C: db $92
L0D7A4D: db $E4
L0D7A4E: db $B8
L0D7A4F: db $91
L0D7A50: db $E4
L0D7A51: db $E8
L0D7A52: db $94
L0D7A53: db $E4
L0D7A54: db $C8
L0D7A55: db $92
L0D7A56: db $E4
L0D7A57: db $F8
L0D7A58: db $92
L0D7A59: db $E4
L0D7A5A: db $D8
L0D7A5B: db $94
L0D7A5C: db $EC
L0D7A5D: db $D4
L0D7A5E: db $7C
L0D7A5F: db $E4
L0D7A60: db $A8
L0D7A61: db $A5
L0D7A62: db $0C
L0D7A63: db $E4
L0D7A64: db $88
L0D7A65: db $A5
L0D7A66: db $04
L0D7A67: db $E4
L0D7A68: db $A8
L0D7A69: db $AC
L0D7A6A: db $80
L0D7A6B: db $B1
L0D7A6C: db $E4
L0D7A6D: db $88
L0D7A6E: db $AC
L0D7A6F: db $E4
L0D7A70: db $A8
L0D7A71: db $A7
L0D7A72: db $20
L0D7A73: db $E4
L0D7A74: db $88
L0D7A75: db $A7
L0D7A76: db $08
L0D7A77: db $80
L0D7A78: db $04
L0D7A79: db $E4
L0D7A7A: db $68
L0D7A7B: db $A7
L0D7A7C: db $E4
L0D7A7D: db $A8
L0D7A7E: db $AC
L0D7A7F: db $80
L0D7A80: db $B1
L0D7A81: db $E4
L0D7A82: db $88
L0D7A83: db $AC
L0D7A84: db $E4
L0D7A85: db $A8
L0D7A86: db $A7
L0D7A87: db $20
L0D7A88: db $E4
L0D7A89: db $A8
L0D7A8A: db $94
L0D7A8B: db $04
L0D7A8C: db $E4
L0D7A8D: db $88
L0D7A8E: db $96
L0D7A8F: db $E4
L0D7A90: db $A8
L0D7A91: db $96
L0D7A92: db $E4
L0D7A93: db $88
L0D7A94: db $94
L0D7A95: db $E7
L0D7A96: db $00
L0D7A97: db $05
L0D7A98: db $88
L0D7A99: db $7A
L0D7A9A: db $E4
L0D7A9B: db $A8
L0D7A9C: db $97
L0D7A9D: db $E4
L0D7A9E: db $88
L0D7A9F: db $96
L0D7AA0: db $E4
L0D7AA1: db $A8
L0D7AA2: db $98
L0D7AA3: db $E4
L0D7AA4: db $88
L0D7AA5: db $97
L0D7AA6: db $E4
L0D7AA7: db $A8
L0D7AA8: db $99
L0D7AA9: db $E4
L0D7AAA: db $88
L0D7AAB: db $98
L0D7AAC: db $E4
L0D7AAD: db $A8
L0D7AAE: db $9B
L0D7AAF: db $E4
L0D7AB0: db $88
L0D7AB1: db $99
L0D7AB2: db $E4
L0D7AB3: db $A8
L0D7AB4: db $9D
L0D7AB5: db $E4
L0D7AB6: db $88
L0D7AB7: db $9B
L0D7AB8: db $E4
L0D7AB9: db $A8
L0D7ABA: db $9E
L0D7ABB: db $E4
L0D7ABC: db $88
L0D7ABD: db $9D
L0D7ABE: db $EC
L0D7ABF: db $42
L0D7AC0: db $7D
L0D7AC1: db $E6
L0D7AC2: db $02
L0D7AC3: db $EC
L0D7AC4: db $42
L0D7AC5: db $7D
L0D7AC6: db $E6
L0D7AC7: db $01
L0D7AC8: db $EC
L0D7AC9: db $42
L0D7ACA: db $7D
L0D7ACB: db $E6
L0D7ACC: db $02
L0D7ACD: db $EC
L0D7ACE: db $42
L0D7ACF: db $7D
L0D7AD0: db $E6
L0D7AD1: db $FB
L0D7AD2: db $EC
L0D7AD3: db $5E
L0D7AD4: db $7D
L0D7AD5: db $EC
L0D7AD6: db $B7
L0D7AD7: db $7D
L0D7AD8: db $E4
L0D7AD9: db $A8
L0D7ADA: db $96
L0D7ADB: db $04
L0D7ADC: db $E4
L0D7ADD: db $88
L0D7ADE: db $AC
L0D7ADF: db $E4
L0D7AE0: db $A8
L0D7AE1: db $98
L0D7AE2: db $E4
L0D7AE3: db $88
L0D7AE4: db $96
L0D7AE5: db $E4
L0D7AE6: db $A8
L0D7AE7: db $9A
L0D7AE8: db $E4
L0D7AE9: db $88
L0D7AEA: db $98
L0D7AEB: db $E4
L0D7AEC: db $A8
L0D7AED: db $9B
L0D7AEE: db $E4
L0D7AEF: db $88
L0D7AF0: db $9A
L0D7AF1: db $E4
L0D7AF2: db $A8
L0D7AF3: db $98
L0D7AF4: db $E4
L0D7AF5: db $88
L0D7AF6: db $9B
L0D7AF7: db $E4
L0D7AF8: db $A8
L0D7AF9: db $9A
L0D7AFA: db $E4
L0D7AFB: db $88
L0D7AFC: db $98
L0D7AFD: db $E4
L0D7AFE: db $A8
L0D7AFF: db $9B
L0D7B00: db $E4
L0D7B01: db $88
L0D7B02: db $9A
L0D7B03: db $E4
L0D7B04: db $A8
L0D7B05: db $9D
L0D7B06: db $E4
L0D7B07: db $88
L0D7B08: db $9B
L0D7B09: db $E4
L0D7B0A: db $A8
L0D7B0B: db $9B
L0D7B0C: db $E4
L0D7B0D: db $88
L0D7B0E: db $9D
L0D7B0F: db $E4
L0D7B10: db $A8
L0D7B11: db $9D
L0D7B12: db $E4
L0D7B13: db $88
L0D7B14: db $9B
L0D7B15: db $E4
L0D7B16: db $A8
L0D7B17: db $9F
L0D7B18: db $E4
L0D7B19: db $88
L0D7B1A: db $9D
L0D7B1B: db $E4
L0D7B1C: db $A8
L0D7B1D: db $A0
L0D7B1E: db $E4
L0D7B1F: db $88
L0D7B20: db $9F
L0D7B21: db $E4
L0D7B22: db $A8
L0D7B23: db $9D
L0D7B24: db $E4
L0D7B25: db $88
L0D7B26: db $A0
L0D7B27: db $E4
L0D7B28: db $A8
L0D7B29: db $9F
L0D7B2A: db $E4
L0D7B2B: db $88
L0D7B2C: db $9D
L0D7B2D: db $E4
L0D7B2E: db $A8
L0D7B2F: db $A0
L0D7B30: db $E4
L0D7B31: db $88
L0D7B32: db $9F
L0D7B33: db $E4
L0D7B34: db $A8
L0D7B35: db $A2
L0D7B36: db $E4
L0D7B37: db $88
L0D7B38: db $A0
L0D7B39: db $E4
L0D7B3A: db $A8
L0D7B3B: db $9A
L0D7B3C: db $E4
L0D7B3D: db $88
L0D7B3E: db $A2
L0D7B3F: db $E4
L0D7B40: db $A8
L0D7B41: db $9C
L0D7B42: db $E4
L0D7B43: db $88
L0D7B44: db $9A
L0D7B45: db $E4
L0D7B46: db $A8
L0D7B47: db $9E
L0D7B48: db $E4
L0D7B49: db $88
L0D7B4A: db $9C
L0D7B4B: db $E4
L0D7B4C: db $A8
L0D7B4D: db $9F
L0D7B4E: db $E4
L0D7B4F: db $88
L0D7B50: db $9E
L0D7B51: db $E4
L0D7B52: db $A8
L0D7B53: db $9F
L0D7B54: db $E4
L0D7B55: db $88
L0D7B56: db $9F
L0D7B57: db $E4
L0D7B58: db $A8
L0D7B59: db $A1
L0D7B5A: db $E4
L0D7B5B: db $88
L0D7B5C: db $9F
L0D7B5D: db $E4
L0D7B5E: db $A8
L0D7B5F: db $A3
L0D7B60: db $E4
L0D7B61: db $88
L0D7B62: db $A1
L0D7B63: db $E4
L0D7B64: db $A8
L0D7B65: db $A5
L0D7B66: db $E4
L0D7B67: db $88
L0D7B68: db $A3
L0D7B69: db $E4
L0D7B6A: db $A8
L0D7B6B: db $A2
L0D7B6C: db $E4
L0D7B6D: db $88
L0D7B6E: db $A5
L0D7B6F: db $E4
L0D7B70: db $A8
L0D7B71: db $A3
L0D7B72: db $E4
L0D7B73: db $88
L0D7B74: db $A2
L0D7B75: db $E4
L0D7B76: db $A8
L0D7B77: db $A5
L0D7B78: db $E4
L0D7B79: db $88
L0D7B7A: db $A3
L0D7B7B: db $E4
L0D7B7C: db $A8
L0D7B7D: db $A7
L0D7B7E: db $E4
L0D7B7F: db $88
L0D7B80: db $A5
L0D7B81: db $E4
L0D7B82: db $A8
L0D7B83: db $A9
L0D7B84: db $E4
L0D7B85: db $88
L0D7B86: db $A7
L0D7B87: db $E4
L0D7B88: db $A8
L0D7B89: db $AA
L0D7B8A: db $E4
L0D7B8B: db $88
L0D7B8C: db $A9
L0D7B8D: db $E4
L0D7B8E: db $A8
L0D7B8F: db $AC
L0D7B90: db $E4
L0D7B91: db $88
L0D7B92: db $AA
L0D7B93: db $E4
L0D7B94: db $A8
L0D7B95: db $AD
L0D7B96: db $E4
L0D7B97: db $88
L0D7B98: db $AC
L0D7B99: db $E4
L0D7B9A: db $A8
L0D7B9B: db $A7
L0D7B9C: db $10
L0D7B9D: db $80
L0D7B9E: db $04
L0D7B9F: db $E4
L0D7BA0: db $88
L0D7BA1: db $A7
L0D7BA2: db $E4
L0D7BA3: db $A8
L0D7BA4: db $A3
L0D7BA5: db $10
L0D7BA6: db $80
L0D7BA7: db $04
L0D7BA8: db $E4
L0D7BA9: db $88
L0D7BAA: db $A3
L0D7BAB: db $E4
L0D7BAC: db $A8
L0D7BAD: db $A7
L0D7BAE: db $08
L0D7BAF: db $80
L0D7BB0: db $04
L0D7BB1: db $E4
L0D7BB2: db $88
L0D7BB3: db $A7
L0D7BB4: db $E4
L0D7BB5: db $A8
L0D7BB6: db $A9
L0D7BB7: db $10
L0D7BB8: db $80
L0D7BB9: db $04
L0D7BBA: db $E4
L0D7BBB: db $88
L0D7BBC: db $A9
L0D7BBD: db $E4
L0D7BBE: db $A8
L0D7BBF: db $A5
L0D7BC0: db $10
L0D7BC1: db $80
L0D7BC2: db $04
L0D7BC3: db $E4
L0D7BC4: db $88
L0D7BC5: db $A5
L0D7BC6: db $E4
L0D7BC7: db $A8
L0D7BC8: db $A9
L0D7BC9: db $08
L0D7BCA: db $80
L0D7BCB: db $04
L0D7BCC: db $E4
L0D7BCD: db $88
L0D7BCE: db $A9
L0D7BCF: db $E4
L0D7BD0: db $A8
L0D7BD1: db $AB
L0D7BD2: db $04
L0D7BD3: db $80
L0D7BD4: db $AE
L0D7BD5: db $E4
L0D7BD6: db $88
L0D7BD7: db $AB
L0D7BD8: db $E4
L0D7BD9: db $A8
L0D7BDA: db $B3
L0D7BDB: db $E4
L0D7BDC: db $88
L0D7BDD: db $AE
L0D7BDE: db $E4
L0D7BDF: db $A8
L0D7BE0: db $B8
L0D7BE1: db $E4
L0D7BE2: db $88
L0D7BE3: db $B3
L0D7BE4: db $E4
L0D7BE5: db $A8
L0D7BE6: db $B7
L0D7BE7: db $E4
L0D7BE8: db $88
L0D7BE9: db $B8
L0D7BEA: db $E4
L0D7BEB: db $A8
L0D7BEC: db $B3
L0D7BED: db $E4
L0D7BEE: db $88
L0D7BEF: db $B7
L0D7BF0: db $E4
L0D7BF1: db $A8
L0D7BF2: db $AE
L0D7BF3: db $E4
L0D7BF4: db $88
L0D7BF5: db $B3
L0D7BF6: db $E4
L0D7BF7: db $A8
L0D7BF8: db $AB
L0D7BF9: db $E4
L0D7BFA: db $88
L0D7BFB: db $AE
L0D7BFC: db $E4
L0D7BFD: db $A8
L0D7BFE: db $AC
L0D7BFF: db $08
L0D7C00: db $80
L0D7C01: db $04
L0D7C02: db $E4
L0D7C03: db $88
L0D7C04: db $AC
L0D7C05: db $E4
L0D7C06: db $A8
L0D7C07: db $A7
L0D7C08: db $08
L0D7C09: db $80
L0D7C0A: db $04
L0D7C0B: db $E4
L0D7C0C: db $88
L0D7C0D: db $A7
L0D7C0E: db $E4
L0D7C0F: db $A8
L0D7C10: db $A4
L0D7C11: db $08
L0D7C12: db $80
L0D7C13: db $04
L0D7C14: db $E4
L0D7C15: db $88
L0D7C16: db $A4
L0D7C17: db $E4
L0D7C18: db $A8
L0D7C19: db $A0
L0D7C1A: db $08
L0D7C1B: db $80
L0D7C1C: db $04
L0D7C1D: db $E4
L0D7C1E: db $88
L0D7C1F: db $A0
L0D7C20: db $E4
L0D7C21: db $A8
L0D7C22: db $A7
L0D7C23: db $10
L0D7C24: db $80
L0D7C25: db $04
L0D7C26: db $E4
L0D7C27: db $88
L0D7C28: db $A7
L0D7C29: db $E4
L0D7C2A: db $A8
L0D7C2B: db $A3
L0D7C2C: db $10
L0D7C2D: db $80
L0D7C2E: db $04
L0D7C2F: db $E4
L0D7C30: db $88
L0D7C31: db $A3
L0D7C32: db $E4
L0D7C33: db $A8
L0D7C34: db $A7
L0D7C35: db $08
L0D7C36: db $80
L0D7C37: db $04
L0D7C38: db $E4
L0D7C39: db $88
L0D7C3A: db $A7
L0D7C3B: db $E4
L0D7C3C: db $A8
L0D7C3D: db $A9
L0D7C3E: db $10
L0D7C3F: db $E4
L0D7C40: db $88
L0D7C41: db $A9
L0D7C42: db $08
L0D7C43: db $80
L0D7C44: db $04
L0D7C45: db $E4
L0D7C46: db $68
L0D7C47: db $A9
L0D7C48: db $E4
L0D7C49: db $A8
L0D7C4A: db $A9
L0D7C4B: db $06
L0D7C4C: db $80
L0D7C4D: db $02
L0D7C4E: db $A9
L0D7C4F: db $04
L0D7C50: db $E4
L0D7C51: db $88
L0D7C52: db $A9
L0D7C53: db $E4
L0D7C54: db $A8
L0D7C55: db $A9
L0D7C56: db $E4
L0D7C57: db $88
L0D7C58: db $A9
L0D7C59: db $E4
L0D7C5A: db $A8
L0D7C5B: db $A9
L0D7C5C: db $E4
L0D7C5D: db $88
L0D7C5E: db $A9
L0D7C5F: db $E4
L0D7C60: db $98
L0D7C61: db $AE
L0D7C62: db $05
L0D7C63: db $B3
L0D7C64: db $06
L0D7C65: db $B5
L0D7C66: db $05
L0D7C67: db $BA
L0D7C68: db $B5
L0D7C69: db $06
L0D7C6A: db $B3
L0D7C6B: db $05
L0D7C6C: db $E4
L0D7C6D: db $88
L0D7C6E: db $AE
L0D7C6F: db $05
L0D7C70: db $B3
L0D7C71: db $06
L0D7C72: db $B5
L0D7C73: db $05
L0D7C74: db $BA
L0D7C75: db $B5
L0D7C76: db $06
L0D7C77: db $B3
L0D7C78: db $05
L0D7C79: db $E4
L0D7C7A: db $78
L0D7C7B: db $AE
L0D7C7C: db $05
L0D7C7D: db $B3
L0D7C7E: db $06
L0D7C7F: db $B5
L0D7C80: db $05
L0D7C81: db $BA
L0D7C82: db $B5
L0D7C83: db $06
L0D7C84: db $B3
L0D7C85: db $05
L0D7C86: db $E4
L0D7C87: db $58
L0D7C88: db $AE
L0D7C89: db $05
L0D7C8A: db $B3
L0D7C8B: db $06
L0D7C8C: db $B5
L0D7C8D: db $05
L0D7C8E: db $BA
L0D7C8F: db $B5
L0D7C90: db $06
L0D7C91: db $B3
L0D7C92: db $05
L0D7C93: db $E5
L0D7C94: db $71
L0D7C95: db $78
L0D7C96: db $E4
L0D7C97: db $A8
L0D7C98: db $9E
L0D7C99: db $04
L0D7C9A: db $E4
L0D7C9B: db $88
L0D7C9C: db $9B
L0D7C9D: db $E4
L0D7C9E: db $A8
L0D7C9F: db $A2
L0D7CA0: db $E4
L0D7CA1: db $88
L0D7CA2: db $9E
L0D7CA3: db $E4
L0D7CA4: db $A8
L0D7CA5: db $A5
L0D7CA6: db $E4
L0D7CA7: db $88
L0D7CA8: db $A2
L0D7CA9: db $E4
L0D7CAA: db $A8
L0D7CAB: db $9B
L0D7CAC: db $E4
L0D7CAD: db $88
L0D7CAE: db $A5
L0D7CAF: db $E7
L0D7CB0: db $00
L0D7CB1: db $04
L0D7CB2: db $96
L0D7CB3: db $7C
L0D7CB4: db $ED
L0D7CB5: db $E4
L0D7CB6: db $A8
L0D7CB7: db $A3
L0D7CB8: db $04
L0D7CB9: db $E4
L0D7CBA: db $88
L0D7CBB: db $9F
L0D7CBC: db $E4
L0D7CBD: db $A8
L0D7CBE: db $A4
L0D7CBF: db $E4
L0D7CC0: db $88
L0D7CC1: db $A3
L0D7CC2: db $E4
L0D7CC3: db $A8
L0D7CC4: db $A6
L0D7CC5: db $E4
L0D7CC6: db $88
L0D7CC7: db $A4
L0D7CC8: db $E4
L0D7CC9: db $A8
L0D7CCA: db $9F
L0D7CCB: db $E4
L0D7CCC: db $88
L0D7CCD: db $A6
L0D7CCE: db $E7
L0D7CCF: db $00
L0D7CD0: db $03
L0D7CD1: db $B5
L0D7CD2: db $7C
L0D7CD3: db $ED
L0D7CD4: db $E4
L0D7CD5: db $A8
L0D7CD6: db $99
L0D7CD7: db $08
L0D7CD8: db $80
L0D7CD9: db $04
L0D7CDA: db $E4
L0D7CDB: db $88
L0D7CDC: db $99
L0D7CDD: db $E4
L0D7CDE: db $A8
L0D7CDF: db $AC
L0D7CE0: db $80
L0D7CE1: db $B1
L0D7CE2: db $E4
L0D7CE3: db $88
L0D7CE4: db $AC
L0D7CE5: db $E4
L0D7CE6: db $A8
L0D7CE7: db $99
L0D7CE8: db $E4
L0D7CE9: db $88
L0D7CEA: db $B1
L0D7CEB: db $80
L0D7CEC: db $99
L0D7CED: db $E4
L0D7CEE: db $A8
L0D7CEF: db $9D
L0D7CF0: db $08
L0D7CF1: db $9B
L0D7CF2: db $04
L0D7CF3: db $E4
L0D7CF4: db $88
L0D7CF5: db $9D
L0D7CF6: db $E7
L0D7CF7: db $00
L0D7CF8: db $03
L0D7CF9: db $D4
L0D7CFA: db $7C
L0D7CFB: db $E4
L0D7CFC: db $A8
L0D7CFD: db $99
L0D7CFE: db $08
L0D7CFF: db $80
L0D7D00: db $04
L0D7D01: db $E4
L0D7D02: db $88
L0D7D03: db $99
L0D7D04: db $E4
L0D7D05: db $A8
L0D7D06: db $A9
L0D7D07: db $80
L0D7D08: db $B1
L0D7D09: db $E4
L0D7D0A: db $88
L0D7D0B: db $A9
L0D7D0C: db $E4
L0D7D0D: db $A8
L0D7D0E: db $A0
L0D7D0F: db $08
L0D7D10: db $80
L0D7D11: db $04
L0D7D12: db $E4
L0D7D13: db $88
L0D7D14: db $A0
L0D7D15: db $E4
L0D7D16: db $A8
L0D7D17: db $A5
L0D7D18: db $08
L0D7D19: db $80
L0D7D1A: db $04
L0D7D1B: db $E4
L0D7D1C: db $88
L0D7D1D: db $A5
L0D7D1E: db $ED
L0D7D1F: db $E4
L0D7D20: db $A8
L0D7D21: db $A5
L0D7D22: db $0C
L0D7D23: db $E4
L0D7D24: db $88
L0D7D25: db $A5
L0D7D26: db $04
L0D7D27: db $E4
L0D7D28: db $A8
L0D7D29: db $AC
L0D7D2A: db $80
L0D7D2B: db $B1
L0D7D2C: db $E4
L0D7D2D: db $88
L0D7D2E: db $AC
L0D7D2F: db $E4
L0D7D30: db $A8
L0D7D31: db $A0
L0D7D32: db $E4
L0D7D33: db $88
L0D7D34: db $B1
L0D7D35: db $E4
L0D7D36: db $A8
L0D7D37: db $9D
L0D7D38: db $E4
L0D7D39: db $88
L0D7D3A: db $A0
L0D7D3B: db $E4
L0D7D3C: db $A8
L0D7D3D: db $99
L0D7D3E: db $E4
L0D7D3F: db $88
L0D7D40: db $9D
L0D7D41: db $ED
L0D7D42: db $E4
L0D7D43: db $A8
L0D7D44: db $A0
L0D7D45: db $10
L0D7D46: db $80
L0D7D47: db $04
L0D7D48: db $E4
L0D7D49: db $88
L0D7D4A: db $A0
L0D7D4B: db $E4
L0D7D4C: db $A8
L0D7D4D: db $99
L0D7D4E: db $10
L0D7D4F: db $80
L0D7D50: db $04
L0D7D51: db $E4
L0D7D52: db $88
L0D7D53: db $99
L0D7D54: db $E4
L0D7D55: db $A8
L0D7D56: db $A0
L0D7D57: db $08
L0D7D58: db $80
L0D7D59: db $04
L0D7D5A: db $E4
L0D7D5B: db $88
L0D7D5C: db $A0
L0D7D5D: db $ED
L0D7D5E: db $E4
L0D7D5F: db $A8
L0D7D60: db $98
L0D7D61: db $04
L0D7D62: db $E4
L0D7D63: db $88
L0D7D64: db $AC
L0D7D65: db $E4
L0D7D66: db $A8
L0D7D67: db $9B
L0D7D68: db $E4
L0D7D69: db $88
L0D7D6A: db $98
L0D7D6B: db $E4
L0D7D6C: db $A8
L0D7D6D: db $A0
L0D7D6E: db $E4
L0D7D6F: db $88
L0D7D70: db $9B
L0D7D71: db $E4
L0D7D72: db $A8
L0D7D73: db $A4
L0D7D74: db $E4
L0D7D75: db $88
L0D7D76: db $A0
L0D7D77: db $E4
L0D7D78: db $A8
L0D7D79: db $AE
L0D7D7A: db $E4
L0D7D7B: db $88
L0D7D7C: db $A4
L0D7D7D: db $E4
L0D7D7E: db $A8
L0D7D7F: db $AC
L0D7D80: db $E4
L0D7D81: db $88
L0D7D82: db $AE
L0D7D83: db $E4
L0D7D84: db $A8
L0D7D85: db $A0
L0D7D86: db $E4
L0D7D87: db $88
L0D7D88: db $AC
L0D7D89: db $E4
L0D7D8A: db $A8
L0D7D8B: db $9B
L0D7D8C: db $E4
L0D7D8D: db $88
L0D7D8E: db $A0
L0D7D8F: db $E4
L0D7D90: db $A8
L0D7D91: db $A6
L0D7D92: db $08
L0D7D93: db $80
L0D7D94: db $04
L0D7D95: db $E4
L0D7D96: db $88
L0D7D97: db $A6
L0D7D98: db $E4
L0D7D99: db $A8
L0D7D9A: db $AE
L0D7D9B: db $80
L0D7D9C: db $AC
L0D7D9D: db $E4
L0D7D9E: db $88
L0D7D9F: db $AE
L0D7DA0: db $E4
L0D7DA1: db $A8
L0D7DA2: db $A4
L0D7DA3: db $08
L0D7DA4: db $80
L0D7DA5: db $04
L0D7DA6: db $E4
L0D7DA7: db $88
L0D7DA8: db $A4
L0D7DA9: db $E4
L0D7DAA: db $A8
L0D7DAB: db $AE
L0D7DAC: db $80
L0D7DAD: db $AC
L0D7DAE: db $E4
L0D7DAF: db $88
L0D7DB0: db $AE
L0D7DB1: db $E7
L0D7DB2: db $00
L0D7DB3: db $02
L0D7DB4: db $5E
L0D7DB5: db $7D
L0D7DB6: db $ED
L0D7DB7: db $E4
L0D7DB8: db $A8
L0D7DB9: db $9B
L0D7DBA: db $04
L0D7DBB: db $E4
L0D7DBC: db $88
L0D7DBD: db $AC
L0D7DBE: db $E4
L0D7DBF: db $A8
L0D7DC0: db $9E
L0D7DC1: db $E4
L0D7DC2: db $88
L0D7DC3: db $9B
L0D7DC4: db $E4
L0D7DC5: db $A8
L0D7DC6: db $A3
L0D7DC7: db $E4
L0D7DC8: db $88
L0D7DC9: db $9E
L0D7DCA: db $E4
L0D7DCB: db $A8
L0D7DCC: db $A7
L0D7DCD: db $E4
L0D7DCE: db $88
L0D7DCF: db $A3
L0D7DD0: db $E4
L0D7DD1: db $A8
L0D7DD2: db $AE
L0D7DD3: db $E4
L0D7DD4: db $88
L0D7DD5: db $A7
L0D7DD6: db $E4
L0D7DD7: db $A8
L0D7DD8: db $AC
L0D7DD9: db $E4
L0D7DDA: db $88
L0D7DDB: db $AE
L0D7DDC: db $E4
L0D7DDD: db $A8
L0D7DDE: db $A3
L0D7DDF: db $E4
L0D7DE0: db $88
L0D7DE1: db $AC
L0D7DE2: db $E4
L0D7DE3: db $A8
L0D7DE4: db $9E
L0D7DE5: db $E4
L0D7DE6: db $88
L0D7DE7: db $A3
L0D7DE8: db $E4
L0D7DE9: db $A8
L0D7DEA: db $A9
L0D7DEB: db $08
L0D7DEC: db $80
L0D7DED: db $04
L0D7DEE: db $E4
L0D7DEF: db $88
L0D7DF0: db $A9
L0D7DF1: db $E4
L0D7DF2: db $A8
L0D7DF3: db $AE
L0D7DF4: db $80
L0D7DF5: db $AC
L0D7DF6: db $E4
L0D7DF7: db $88
L0D7DF8: db $AE
L0D7DF9: db $E4
L0D7DFA: db $A8
L0D7DFB: db $A7
L0D7DFC: db $08
L0D7DFD: db $80
L0D7DFE: db $04
L0D7DFF: db $E4
L0D7E00: db $88
L0D7E01: db $A7
L0D7E02: db $E4
L0D7E03: db $A8
L0D7E04: db $AE
L0D7E05: db $80
L0D7E06: db $AC
L0D7E07: db $E4
L0D7E08: db $88
L0D7E09: db $AE
L0D7E0A: db $E7
L0D7E0B: db $00
L0D7E0C: db $02
L0D7E0D: db $B7
L0D7E0E: db $7D
L0D7E0F: db $ED
L0D7E10: db $E4
L0D7E11: db $20
L0D7E12: db $E9
L0D7E13: db $44
L0D7E14: db $F3
L0D7E15: db $03
L0D7E16: db $F5
L0D7E17: db $00
L0D7E18: db $9B
L0D7E19: db $40
L0D7E1A: db $99
L0D7E1B: db $10
L0D7E1C: db $9B
L0D7E1D: db $9C
L0D7E1E: db $08
L0D7E1F: db $9E
L0D7E20: db $9F
L0D7E21: db $A1
L0D7E22: db $9A
L0D7E23: db $20
L0D7E24: db $FA
L0D7E25: db $06
L0D7E26: db $80
L0D7E27: db $02
L0D7E28: db $9A
L0D7E29: db $06
L0D7E2A: db $80
L0D7E2B: db $02
L0D7E2C: db $9A
L0D7E2D: db $06
L0D7E2E: db $80
L0D7E2F: db $02
L0D7E30: db $9A
L0D7E31: db $08
L0D7E32: db $9E
L0D7E33: db $20
L0D7E34: db $9C
L0D7E35: db $10
L0D7E36: db $9E
L0D7E37: db $97
L0D7E38: db $08
L0D7E39: db $9E
L0D7E3A: db $A3
L0D7E3B: db $10
L0D7E3C: db $97
L0D7E3D: db $08
L0D7E3E: db $9E
L0D7E3F: db $A3
L0D7E40: db $10
L0D7E41: db $97
L0D7E42: db $08
L0D7E43: db $9E
L0D7E44: db $A3
L0D7E45: db $10
L0D7E46: db $97
L0D7E47: db $08
L0D7E48: db $9E
L0D7E49: db $A3
L0D7E4A: db $10
L0D7E4B: db $9A
L0D7E4C: db $08
L0D7E4D: db $95
L0D7E4E: db $9E
L0D7E4F: db $10
L0D7E50: db $9A
L0D7E51: db $08
L0D7E52: db $95
L0D7E53: db $9E
L0D7E54: db $10
L0D7E55: db $9A
L0D7E56: db $08
L0D7E57: db $95
L0D7E58: db $9E
L0D7E59: db $10
L0D7E5A: db $9A
L0D7E5B: db $08
L0D7E5C: db $95
L0D7E5D: db $9E
L0D7E5E: db $10
L0D7E5F: db $97
L0D7E60: db $20
L0D7E61: db $80
L0D7E62: db $10
L0D7E63: db $97
L0D7E64: db $06
L0D7E65: db $80
L0D7E66: db $02
L0D7E67: db $97
L0D7E68: db $08
L0D7E69: db $9E
L0D7E6A: db $40
L0D7E6B: db $9A
L0D7E6C: db $20
L0D7E6D: db $80
L0D7E6E: db $10
L0D7E6F: db $9A
L0D7E70: db $06
L0D7E71: db $80
L0D7E72: db $02
L0D7E73: db $9A
L0D7E74: db $08
L0D7E75: db $A1
L0D7E76: db $40
L0D7E77: db $9E
L0D7E78: db $08
L0D7E79: db $92
L0D7E7A: db $9E
L0D7E7B: db $0E
L0D7E7C: db $80
L0D7E7D: db $02
L0D7E7E: db $9E
L0D7E7F: db $08
L0D7E80: db $92
L0D7E81: db $9E
L0D7E82: db $0E
L0D7E83: db $80
L0D7E84: db $02
L0D7E85: db $9E
L0D7E86: db $08
L0D7E87: db $92
L0D7E88: db $9E
L0D7E89: db $0E
L0D7E8A: db $80
L0D7E8B: db $02
L0D7E8C: db $92
L0D7E8D: db $08
L0D7E8E: db $97
L0D7E8F: db $9B
L0D7E90: db $9C
L0D7E91: db $97
L0D7E92: db $1E
L0D7E93: db $80
L0D7E94: db $02
L0D7E95: db $95
L0D7E96: db $1E
L0D7E97: db $80
L0D7E98: db $02
L0D7E99: db $94
L0D7E9A: db $1E
L0D7E9B: db $80
L0D7E9C: db $02
L0D7E9D: db $92
L0D7E9E: db $1E
L0D7E9F: db $80
L0D7EA0: db $02
L0D7EA1: db $99
L0D7EA2: db $10
L0D7EA3: db $94
L0D7EA4: db $99
L0D7EA5: db $94
L0D7EA6: db $99
L0D7EA7: db $94
L0D7EA8: db $99
L0D7EA9: db $94
L0D7EAA: db $08
L0D7EAB: db $96
L0D7EAC: db $97
L0D7EAD: db $10
L0D7EAE: db $92
L0D7EAF: db $97
L0D7EB0: db $92
L0D7EB1: db $97
L0D7EB2: db $92
L0D7EB3: db $97
L0D7EB4: db $92
L0D7EB5: db $08
L0D7EB6: db $97
L0D7EB7: db $96
L0D7EB8: db $10
L0D7EB9: db $92
L0D7EBA: db $96
L0D7EBB: db $92
L0D7EBC: db $96
L0D7EBD: db $92
L0D7EBE: db $96
L0D7EBF: db $92
L0D7EC0: db $08
L0D7EC1: db $96
L0D7EC2: db $92
L0D7EC3: db $0E
L0D7EC4: db $80
L0D7EC5: db $02
L0D7EC6: db $92
L0D7EC7: db $10
L0D7EC8: db $91
L0D7EC9: db $92
L0D7ECA: db $94
L0D7ECB: db $8F
L0D7ECC: db $94
L0D7ECD: db $98
L0D7ECE: db $99
L0D7ECF: db $10
L0D7ED0: db $91
L0D7ED1: db $08
L0D7ED2: db $94
L0D7ED3: db $99
L0D7ED4: db $10
L0D7ED5: db $91
L0D7ED6: db $08
L0D7ED7: db $94
L0D7ED8: db $99
L0D7ED9: db $10
L0D7EDA: db $91
L0D7EDB: db $08
L0D7EDC: db $94
L0D7EDD: db $99
L0D7EDE: db $10
L0D7EDF: db $91
L0D7EE0: db $08
L0D7EE1: db $94
L0D7EE2: db $97
L0D7EE3: db $10
L0D7EE4: db $91
L0D7EE5: db $08
L0D7EE6: db $94
L0D7EE7: db $97
L0D7EE8: db $10
L0D7EE9: db $91
L0D7EEA: db $08
L0D7EEB: db $94
L0D7EEC: db $97
L0D7EED: db $10
L0D7EEE: db $91
L0D7EEF: db $08
L0D7EF0: db $94
L0D7EF1: db $97
L0D7EF2: db $10
L0D7EF3: db $91
L0D7EF4: db $08
L0D7EF5: db $94
L0D7EF6: db $92
L0D7EF7: db $0E
L0D7EF8: db $80
L0D7EF9: db $02
L0D7EFA: db $92
L0D7EFB: db $06
L0D7EFC: db $80
L0D7EFD: db $02
L0D7EFE: db $92
L0D7EFF: db $06
L0D7F00: db $80
L0D7F01: db $02
L0D7F02: db $92
L0D7F03: db $0E
L0D7F04: db $80
L0D7F05: db $02
L0D7F06: db $92
L0D7F07: db $06
L0D7F08: db $80
L0D7F09: db $02
L0D7F0A: db $92
L0D7F0B: db $06
L0D7F0C: db $80
L0D7F0D: db $02
L0D7F0E: db $92
L0D7F0F: db $0E
L0D7F10: db $80
L0D7F11: db $02
L0D7F12: db $92
L0D7F13: db $06
L0D7F14: db $80
L0D7F15: db $02
L0D7F16: db $92
L0D7F17: db $06
L0D7F18: db $80
L0D7F19: db $02
L0D7F1A: db $92
L0D7F1B: db $0E
L0D7F1C: db $80
L0D7F1D: db $02
L0D7F1E: db $92
L0D7F1F: db $06
L0D7F20: db $80
L0D7F21: db $02
L0D7F22: db $92
L0D7F23: db $06
L0D7F24: db $80
L0D7F25: db $02
L0D7F26: db $94
L0D7F27: db $10
L0D7F28: db $8F
L0D7F29: db $94
L0D7F2A: db $8F
L0D7F2B: db $94
L0D7F2C: db $96
L0D7F2D: db $97
L0D7F2E: db $98
L0D7F2F: db $99
L0D7F30: db $18
L0D7F31: db $91
L0D7F32: db $8D
L0D7F33: db $10
L0D7F34: db $9B
L0D7F35: db $18
L0D7F36: db $92
L0D7F37: db $9B
L0D7F38: db $10
L0D7F39: db $9C
L0D7F3A: db $18
L0D7F3B: db $94
L0D7F3C: db $9C
L0D7F3D: db $10
L0D7F3E: db $9E
L0D7F3F: db $18
L0D7F40: db $96
L0D7F41: db $9E
L0D7F42: db $10
L0D7F43: db $EC
L0D7F44: db $B2
L0D7F45: db $7F
L0D7F46: db $E6
L0D7F47: db $03
L0D7F48: db $EC
L0D7F49: db $B2
L0D7F4A: db $7F
L0D7F4B: db $E6
L0D7F4C: db $FD
L0D7F4D: db $9B
L0D7F4E: db $08
L0D7F4F: db $96
L0D7F50: db $9B
L0D7F51: db $0E
L0D7F52: db $80
L0D7F53: db $02
L0D7F54: db $9B
L0D7F55: db $08
L0D7F56: db $96
L0D7F57: db $9B
L0D7F58: db $0E
L0D7F59: db $80
L0D7F5A: db $02
L0D7F5B: db $9B
L0D7F5C: db $08
L0D7F5D: db $96
L0D7F5E: db $9B
L0D7F5F: db $0E
L0D7F60: db $80
L0D7F61: db $02
L0D7F62: db $9B
L0D7F63: db $08
L0D7F64: db $96
L0D7F65: db $9B
L0D7F66: db $0E
L0D7F67: db $80
L0D7F68: db $02
L0D7F69: db $9C
L0D7F6A: db $08
L0D7F6B: db $97
L0D7F6C: db $9C
L0D7F6D: db $0E
L0D7F6E: db $80
L0D7F6F: db $02
L0D7F70: db $9C
L0D7F71: db $08
L0D7F72: db $97
L0D7F73: db $9C
L0D7F74: db $0E
L0D7F75: db $80
L0D7F76: db $02
L0D7F77: db $9E
L0D7F78: db $08
L0D7F79: db $94
L0D7F7A: db $96
L0D7F7B: db $97
L0D7F7C: db $99
L0D7F7D: db $9B
L0D7F7E: db $9C
L0D7F7F: db $9E
L0D7F80: db $A3
L0D7F81: db $18
L0D7F82: db $9E
L0D7F83: db $A3
L0D7F84: db $10
L0D7F85: db $A5
L0D7F86: db $18
L0D7F87: db $A0
L0D7F88: db $A5
L0D7F89: db $10
L0D7F8A: db $9B
L0D7F8B: db $96
L0D7F8C: db $9B
L0D7F8D: db $A2
L0D7F8E: db $A7
L0D7F8F: db $A4
L0D7F90: db $A0
L0D7F91: db $9B
L0D7F92: db $A3
L0D7F93: db $18
L0D7F94: db $9E
L0D7F95: db $A3
L0D7F96: db $10
L0D7F97: db $99
L0D7F98: db $20
L0D7F99: db $A5
L0D7F9A: db $08
L0D7F9B: db $A9
L0D7F9C: db $AC
L0D7F9D: db $A5
L0D7F9E: db $AB
L0D7F9F: db $60
L0D7FA0: db $E4
L0D7FA1: db $40
L0D7FA2: db $AB
L0D7FA3: db $08
L0D7FA4: db $80
L0D7FA5: db $04
L0D7FA6: db $AB
L0D7FA7: db $08
L0D7FA8: db $80
L0D7FA9: db $04
L0D7FAA: db $E4
L0D7FAB: db $60
L0D7FAC: db $AB
L0D7FAD: db $04
L0D7FAE: db $AB
L0D7FAF: db $E5
L0D7FB0: db $10
L0D7FB1: db $7E
L0D7FB2: db $94
L0D7FB3: db $08
L0D7FB4: db $96
L0D7FB5: db $98
L0D7FB6: db $9A
L0D7FB7: db $9B
L0D7FB8: db $9D
L0D7FB9: db $9B
L0D7FBA: db $9A
L0D7FBB: db $A0
L0D7FBC: db $9F
L0D7FBD: db $9D
L0D7FBE: db $9B
L0D7FBF: db $9D
L0D7FC0: db $9F
L0D7FC1: db $A1
L0D7FC2: db $A2
L0D7FC3: db $E7
L0D7FC4: db $00
L0D7FC5: db $02
L0D7FC6: db $B2
L0D7FC7: db $7F
L0D7FC8: db $ED
L0D7FC9: db $E9;X
L0D7FCA: db $88;X
L0D7FCB: db $E3;X
L0D7FCC: db $FF;X
L0D7FCD: db $FF;X
L0D7FCE: db $FF;X
L0D7FCF: db $FF;X
L0D7FD0: db $FF;X
L0D7FD1: db $FF;X
L0D7FD2: db $FF;X
L0D7FD3: db $FF;X
L0D7FD4: db $FF;X
L0D7FD5: db $FF;X
L0D7FD6: db $FF;X
L0D7FD7: db $FF;X
L0D7FD8: db $FF;X
L0D7FD9: db $FF;X
L0D7FDA: db $FF;X
L0D7FDB: db $FF;X
L0D7FDC: db $FF;X
L0D7FDD: db $FF;X
L0D7FDE: db $FF;X
L0D7FDF: db $FF;X
L0D7FE0: db $FF;X
L0D7FE1: db $FF;X
L0D7FE2: db $FF;X
L0D7FE3: db $FF;X
L0D7FE4: db $FF;X
L0D7FE5: db $FF;X
L0D7FE6: db $FF;X
L0D7FE7: db $FF;X
L0D7FE8: db $FF;X
L0D7FE9: db $FF;X
L0D7FEA: db $FF;X
L0D7FEB: db $FF;X
L0D7FEC: db $FF;X
L0D7FED: db $FF;X
L0D7FEE: db $FF;X
L0D7FEF: db $FF;X
L0D7FF0: db $FF;X
L0D7FF1: db $FF;X
L0D7FF2: db $FF;X
L0D7FF3: db $FF;X
L0D7FF4: db $FF;X
L0D7FF5: db $FF;X
L0D7FF6: db $FF;X
L0D7FF7: db $FF;X
L0D7FF8: db $FF;X
L0D7FF9: db $FF;X
L0D7FFA: db $FF;X
L0D7FFB: db $FF;X
L0D7FFC: db $FF;X
L0D7FFD: db $FF;X
L0D7FFE: db $FF;X
