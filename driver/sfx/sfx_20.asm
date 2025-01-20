SndHeader_SFX_20:
	db $03 ; Number of channels
.ch2:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH2_PTR ; Sound channel ptr
	dw SndData_SFX_20_Ch2 ; Data ptr
	db 12 ; Initial fine tune
	db $81 ; Unused
.ch3:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH3_PTR ; Sound channel ptr
	dw SndData_SFX_20_Ch3 ; Data ptr
	db 0 ; Initial fine tune
	db $81 ; Unused
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_20_Ch4 ; Data ptr
	db 0 ; Initial fine tune
	db $81 ; Unused
SndData_SFX_20_Ch2:
	envelope $A8
	panning $22
	duty_cycle 3
	note C_,4, 2
	note B_,3
	note A#,3
	note A_,3
	note G#,3
	note G_,3
	note F#,3
	note F_,3
	note E_,3
	note D#,3
	note D_,3
	note C#,3
	note C_,3
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
SndData_SFX_20_Ch3:
	wave_vol $40
	panning $44
	wave_id $03
	chan_stop
SndData_SFX_20_Ch4:
	envelope $F8
	panning $88
	wait 87
	wait 6
	lock_envelope
	wait 86
	wait 6
	wait 85
	wait 6
	wait 84
	wait 6
	wait 83
	wait 6
	wait 82
	wait 6
	wait 81
	wait 6
	unlock_envelope
	envelope $F3
	wait 80
	wait 40
	chan_stop
