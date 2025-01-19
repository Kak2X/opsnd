SndHeader_SFX_33:
	db $01 ; Number of channels
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_33_Ch4 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
SndData_SFX_33_Ch4:
	envelope $F2
	panning $88
	wait 81
	wait 3
	envelope $39
	wait 17
	wait 1
	lock_envelope
	wait 18
	wait 1
	wait 19
	wait 1
	unlock_envelope
	envelope $F2
	wait 8
	wait 3
	chan_stop
