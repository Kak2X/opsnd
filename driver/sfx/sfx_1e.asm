SndHeader_SFX_1E:
	db $04 ; Number of channels
.ch1:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH1_PTR ; Sound channel ptr
	dw SndData_SFX_1E_Ch1 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
.ch2:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH2_PTR ; Sound channel ptr
	dw SndData_SFX_1E_Ch2 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
.ch3:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH3_PTR ; Sound channel ptr
	dw SndData_SFX_1E_Ch3 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_1E_Ch4 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
SndData_SFX_1E_Ch1:
	envelope $F7
	panning $11
	duty_cycle 2
	note C_,2, 5
	silence 1
	note C_,3, 6
	vibrato_on $18
	note C_,5, 120
	wait2 120
	chan_stop
SndData_SFX_1E_Ch2:
	envelope $F7
	panning $22
	duty_cycle 2
	note G_,2, 5
	silence 1
	note G_,3, 6
	vibrato_on $18
	note G_,4, 120
	wait2 120
	chan_stop
SndData_SFX_1E_Ch3:
	wave_vol $40
	panning $44
	wave_id $03
	chan_stop
SndData_SFX_1E_Ch4:
	envelope $F8
	panning $88
	wait 113
	wait 5
	wait 0
	wait 1
	wait 53
	wait 6
	chan_stop
