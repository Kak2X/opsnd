SndHeader_SFX_18:
	db $01 ; Number of channels
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_18_Ch4 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
SndData_SFX_18_Ch4:
	envelope $F6
	panning $88
	wait 85
	wait 3
	wait 71
	wait 3
	wait 85
	wait 3
	lock_envelope
	wait 55
	wait 3
	wait 71
	wait 8
	unlock_envelope
	envelope $F3
	wait 71
	wait 10
	envelope $83
	wait 71
	wait 10
	envelope $33
	wait 71
	wait 10
	chan_stop
