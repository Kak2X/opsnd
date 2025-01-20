SndHeader_SFX_2A:
	db $01 ; Number of channels
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_2A_Ch4 ; Data ptr
	db 0 ; Initial fine tune
	db $81 ; Unused
SndData_SFX_2A_Ch4:
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
	envelope $F6
	wait 48
	wait 100
	chan_stop
