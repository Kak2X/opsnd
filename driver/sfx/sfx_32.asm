SndHeader_SFX_32:
	db $01 ; Number of channels
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_32_Ch4 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
SndData_SFX_32_Ch4:
	envelope $29
	panning $88
	wait 32
	wait 1
	lock_envelope
	wait 33
	wait 1
	wait 34
	wait 1
	wait 35
	wait 1
	wait 36
	wait 1
	wait 37
	wait 1
	wait 38
	wait 1
	wait 39
	wait 1
	wait 39
	wait 1
	lock_envelope
	wait 38
	wait 1
	wait 37
	wait 1
	wait 36
	wait 1
	wait 35
	wait 1
	wait 34
	wait 1
	wait 33
	wait 1
	wait 32
	wait 1
	unlock_envelope
	chan_stop
