SndHeader_SFX_29:
	db $01 ; Number of channels
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_29_Ch4 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
SndData_SFX_29_Ch4:
	envelope $F2
	panning $88
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
	chan_stop
