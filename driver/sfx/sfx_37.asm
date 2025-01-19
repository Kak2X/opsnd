SndHeader_SFX_37:
	db $01 ; Number of channels
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_37_Ch4 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
SndData_SFX_37_Ch4:
	chan_stop
