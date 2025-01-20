SndHeader_SFX_3B:
	db $03 ; Number of channels
.ch2:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH2_PTR ; Sound channel ptr
	dw SndData_SFX_3B_Ch2 ; Data ptr
	db 12 ; Initial fine tune
	db $81 ; Unused
.ch3:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH3_PTR ; Sound channel ptr
	dw SndData_SFX_3B_Ch3 ; Data ptr
	db 0 ; Initial fine tune
	db $81 ; Unused
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_3B_Ch4 ; Data ptr
	db 0 ; Initial fine tune
	db $81 ; Unused
SndData_SFX_3B_Ch2:
	envelope $F8
	panning $22
	duty_cycle 2
	note D_,3, 6
	note G_,3
	note C_,4
	envelope $C8
	note D_,3
	note G_,3
	note C_,4
	envelope $98
	note D_,3
	note G_,3
	note C_,4
	envelope $68
	note D_,3
	note G_,3
	note C_,4
	envelope $38
	note D_,3
	note G_,3
	note C_,4
	chan_stop
SndData_SFX_3B_Ch3:
	wave_vol $40
	panning $44
	chan_stop
SndData_SFX_3B_Ch4:
	envelope $F2
	panning $88
	snd_call SndCall_SFX_3B_Ch4_0
	snd_call SndCall_SFX_3B_Ch4_1
	note4x $80, 15 ; Nearest: B_,3
	snd_call SndCall_SFX_3B_Ch4_0
	snd_call SndCall_SFX_3B_Ch4_0
	snd_call SndCall_SFX_3B_Ch4_1
	note4x $80, 5 ; Nearest: B_,3
	snd_call SndCall_SFX_3B_Ch4_0
	chan_stop
SndCall_SFX_3B_Ch4_0:
	wait 64
	wait 1
	lock_envelope
	wait 65
	wait 1
	wait 66
	wait 1
	wait 67
	wait 1
	wait 68
	wait 1
	wait 69
	wait 1
	wait 70
	wait 1
	wait 71
	wait 1
	unlock_envelope
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
	snd_ret
SndCall_SFX_3B_Ch4_1:
	wait 64
	wait 1
	lock_envelope
	wait 65
	wait 1
	wait 66
	wait 1
	wait 67
	wait 1
	wait 68
	wait 1
	wait 69
	wait 1
	wait 70
	wait 1
	wait 71
	wait 1
	unlock_envelope
	snd_ret
