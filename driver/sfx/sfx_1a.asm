SndHeader_SFX_1A:
	db $03 ; Number of channels
.ch2:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH2_PTR ; Sound channel ptr
	dw SndData_SFX_1A_Ch2 ; Data ptr
	db 12 ; Initial fine tune
	db $81 ; Unused
.ch3:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH3_PTR ; Sound channel ptr
	dw SndData_SFX_1A_Ch3 ; Data ptr
	db 0 ; Initial fine tune
	db $81 ; Unused
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_1A_Ch4 ; Data ptr
	db 0 ; Initial fine tune
	db $81 ; Unused
SndData_SFX_1A_Ch2:
	envelope $B8
	panning $22
	duty_cycle 2
	note C_,3, 1
	note C#,3
	note D_,3
	note D#,3
	note E_,3
	note F_,3
	note F#,3
	note G_,3
	note G#,3
	note A_,3
	note A#,3
	note B_,3
	note C_,4
	note C#,4
	note D_,4
	note D#,4
	note E_,4
	note F_,4
	note F#,4
	note G_,4
	note G#,4
	note A_,4
	note A#,4
	note B_,4
	chan_stop
SndData_SFX_1A_Ch3:
	wave_vol $40
	panning $44
	wave_id $03
	chan_stop
SndData_SFX_1A_Ch4:
	envelope $F8
	panning $88
	wait 87
	wait 2
	lock_envelope
	wait 86
	wait 2
	wait 85
	wait 2
	wait 84
	wait 2
	wait 83
	wait 2
	wait 82
	wait 2
	wait 81
	wait 2
	wait 80
	wait 2
	wait 55
	wait 2
	wait 54
	wait 2
	wait 53
	wait 2
	wait 52
	wait 2
	wait 51
	wait 2
	wait 50
	wait 2
	wait 49
	wait 2
	unlock_envelope
	envelope $F3
	wait 48
	wait 50
	chan_stop
