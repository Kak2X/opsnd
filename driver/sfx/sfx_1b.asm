SndHeader_SFX_1B:
	db $03 ; Number of channels
.ch2:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH2_PTR ; Sound channel ptr
	dw SndData_SFX_1B_Ch2 ; Data ptr
	dnote F_,2 ; Base note
	db $81 ; Unused
.ch3:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH3_PTR ; Sound channel ptr
	dw SndData_SFX_1B_Ch3 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_1B_Ch4 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
SndData_SFX_1B_Ch2:
	envelope $F8
	panning $22
	duty_cycle 3
	note C_,3, 1
	note B_,2
	note A#,2
	note A_,2
	note G#,2
	note G_,2
	note F#,2
	note F_,2
	note E_,2
	note D#,2
	note D_,2
	note C#,2
	note C_,2
	chan_stop
SndData_SFX_1B_Ch3:
	wave_vol $40
	panning $44
	wave_id $03
	chan_stop
SndData_SFX_1B_Ch4:
	envelope $B2
	panning $88
	wait 53
	wait 1
	lock_envelope
	wait 52
	wait 1
	wait 51
	wait 1
	wait 50
	wait 1
	wait 49
	wait 1
	wait 48
	wait 1
	wait 32
	wait 1
	wait 33
	wait 1
	wait 34
	wait 1
	wait 35
	wait 1
	wait 36
	wait 1
	wait 37
	wait 1
	wait 38
	wait 1
	wait 39
	wait 1
	chan_stop
