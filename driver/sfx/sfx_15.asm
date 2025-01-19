SndHeader_SFX_15:
	db $01 ; Number of channels
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_15_Ch4 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
SndData_SFX_15_Ch4:
	envelope $39
	panning $88
	wait 71
	wait 1
	lock_envelope
	wait 70
	wait 1
	wait 69
	wait 1
	wait 68
	wait 1
	wait 67
	wait 1
	wait 66
	wait 2
	wait 65
	wait 1
	wait 64
	wait 1
	unlock_envelope
	snd_loop SndData_SFX_15_Ch4, $00, 6
	chan_stop
