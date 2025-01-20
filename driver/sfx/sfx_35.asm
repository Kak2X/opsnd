SndHeader_SFX_35:
	db $01 ; Number of channels
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_35_Ch4 ; Data ptr
	db 0 ; Initial fine tune
	db $81 ; Unused
SndData_SFX_35_Ch4:
	envelope $29
	panning $88
	wait 48
	wait 1
	lock_envelope
	wait 49
	wait 1
	wait 50
	wait 1
	wait 51
	wait 1
	wait 52
	wait 1
	wait 53
	wait 1
	wait 54
	wait 2
	wait 55
	wait 1
	wait 54
	wait 1
	wait 53
	wait 1
	wait 52
	wait 1
	wait 51
	wait 1
	wait 50
	wait 1
	wait 49
	wait 1
	unlock_envelope
	chan_stop
