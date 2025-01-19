SndHeader_SFX_24:
	db $01 ; Number of channels
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_24_Ch4 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
SndData_SFX_24_Ch4:
	envelope $F8
	panning $88
	wait 97
	wait 2
	wait 84
	wait 1
	wait 68
	wait 1
	wait 55
	wait 1
	envelope $A3
	wait 66
	wait 40
	chan_stop
