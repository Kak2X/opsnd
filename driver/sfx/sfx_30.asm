SndHeader_SFX_30:
	db $01 ; Number of channels
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_30_Ch4 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
SndData_SFX_30_Ch4:
	envelope $39
	panning $88
	wait 71
	wait 2
	lock_envelope
	wait 70
	wait 2
	wait 69
	wait 2
	wait 68
	wait 2
	wait 67
	wait 2
	wait 66
	wait 2
	wait 65
	wait 2
	wait 64
	wait 2
	unlock_envelope
	chan_stop
