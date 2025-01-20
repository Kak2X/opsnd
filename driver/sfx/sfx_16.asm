SndHeader_SFX_16:
	db $01 ; Number of channels
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_16_Ch4 ; Data ptr
	db 0 ; Initial fine tune
	db $81 ; Unused
SndData_SFX_16_Ch4:
	envelope $F8
	panning $88
	wait 32
	wait 2
	lock_envelope
	wait 33
	wait 2
	wait 34
	wait 2
	wait 35
	wait 2
	wait 36
	wait 2
	wait 37
	wait 2
	wait 38
	wait 2
	unlock_envelope
	envelope $F1
	wait 39
	wait 6
	chan_stop SNP_SFX4
