SndHeader_SFX_36:
	db $03 ; Number of channels
.ch2:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH2_PTR ; Sound channel ptr
	dw SndData_SFX_36_Ch2 ; Data ptr
	db 4 ; Initial fine tune
	db $81 ; Unused
.ch3:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH3_PTR ; Sound channel ptr
	dw SndData_SFX_36_Ch3 ; Data ptr
	db 0 ; Initial fine tune
	db $81 ; Unused
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_36_Ch4 ; Data ptr
	db 0 ; Initial fine tune
	db $81 ; Unused
SndData_SFX_36_Ch2:
	envelope $F8
	panning $22
	duty_cycle 1
	note D_,3, 4
	note D_,2
	note D_,3, 1
	lock_envelope
	note C#,3
	note C_,3
	note B_,2
	note A#,2
	unlock_envelope
	envelope $98
	note D_,3, 1
	lock_envelope
	note C#,3
	note C_,3
	note B_,2
	note A#,2
	unlock_envelope
	envelope $38
	note D_,3, 1
	lock_envelope
	note C#,3
	note C_,3
	note B_,2
	note A#,2
	unlock_envelope
	chan_stop
SndData_SFX_36_Ch3:
	chan_stop
SndData_SFX_36_Ch4:
	envelope $F8
	panning $88
	wait 71
	wait 1
	lock_envelope
	wait 87
	wait 1
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
	unlock_envelope
	envelope $F4
	wait 68
	wait 20
	chan_stop
