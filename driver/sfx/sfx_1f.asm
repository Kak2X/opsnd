SndHeader_SFX_1F:
	db $01 ; Number of channels
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_1F_Ch4 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
SndData_SFX_1F_Ch4:
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
	wait 48
	wait 8
	wait 49
	wait 8
	wait 50
	wait 8
	wait 51
	wait 8
	wait 52
	wait 8
	wait 53
	wait 8
	wait 54
	wait 8
	wait 55
	wait 8
	wait 54
	wait 8
	wait 53
	wait 8
	wait 52
	wait 8
	wait 51
	wait 8
	wait 50
	wait 8
	wait 49
	wait 8
	wait 48
	wait 8
	unlock_envelope
	chan_stop
