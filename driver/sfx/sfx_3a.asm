SndHeader_SFX_3A:
	db $02 ; Number of channels
.ch1:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH1_PTR ; Sound channel ptr
	dw SndData_SFX_3A_Ch2 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
.ch2:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH2_PTR ; Sound channel ptr
	dw SndData_SFX_3A_Ch2 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
SndData_SFX_3A_Ch2:
	chan_stop
