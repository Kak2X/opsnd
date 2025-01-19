SndHeader_SFX_21:
	db $01 ; Number of channels
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_21_Ch4 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
SndData_SFX_21_Ch4:
	envelope $A7
	panning $88
	wait 71
	wait 1
	lock_envelope
	wait 52
	wait 1
	wait 85
	wait 1
	unlock_envelope
	chan_stop
