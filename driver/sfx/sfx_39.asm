SndHeader_SFX_39:
	db $04 ; Number of channels
.ch1:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH1_PTR ; Sound channel ptr
	dw SndData_SFX_39_Ch1 ; Data ptr
	dnote C_,1 ; Base note
	db $81 ; Unused
.ch2:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH2_PTR ; Sound channel ptr
	dw SndData_SFX_39_Ch2 ; Data ptr
	dnote C_,1 ; Base note
	db $81 ; Unused
.ch3:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH3_PTR ; Sound channel ptr
	dw SndData_SFX_39_Ch3 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
.ch4:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_SFX_39_Ch4 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
SndData_SFX_39_Ch1:
	envelope $F1
	panning $11
	duty_cycle 3
SndCall_SFX_39_Ch2_0:
	silence 30
	note C_,4, 1
	note E_,4
	note G_,4
	note D_,4
	note F_,4
	note A_,4
	note E_,4
	note G_,4
	note B_,4
	note F_,4
	note A_,4
	note C_,5
	note G_,4
	note B_,4
	note D_,5
	note A_,4
	note C_,5
	note E_,5
	note B_,4
	note D_,5
	note F_,5
	note C_,5
	note E_,5
	note G_,5
	note D_,5
	note F_,5
	note A_,5
	note E_,5
	note G_,5
	note B_,5
	note F_,5
	note A_,5
	note C_,6
	note G_,5
	note B_,5
	note D_,6
	note A_,5
	note C_,6
	note E_,6
	note B_,5
	note D_,6
	note F_,6
	note C_,6
	note E_,6
	note G_,6
	note D_,6
	note F_,6
	note A_,6
	note E_,6
	note G_,6
	note B_,6
	note F_,6
	note A_,6
	note C_,7
	note G_,6
	note B_,6
	note D_,7
	note A_,6
	note C_,7
	note E_,7
	note B_,6
	note D_,7
	note F_,7
	note C_,7
	note E_,7
	note G_,7
	note D_,7
	note F_,7
	note A_,7
	note E_,7
	note G_,7
	note B_,7
	chan_stop
SndData_SFX_39_Ch2:
	envelope $D1
	panning $22
	duty_cycle 3
	snd_call SndCall_SFX_39_Ch2_0
	chan_stop
SndData_SFX_39_Ch3:
	wave_vol $40
	panning $44
	chan_stop
SndData_SFX_39_Ch4:
	envelope $F8
	panning $88
	wait 87
	wait 2
	lock_envelope
	wait 86
	wait 2
	wait 85
	wait 2
	wait 84
	wait 2
	wait 83
	wait 2
	wait 82
	wait 2
	wait 81
	wait 2
	wait 80
	wait 2
	wait 55
	wait 2
	wait 54
	wait 2
	wait 53
	wait 2
	wait 52
	wait 2
	wait 51
	wait 2
	wait 50
	wait 2
	wait 49
	wait 2
	wait 48
	wait 8
	wait 49
	wait 8
	wait 50
	wait 8
	wait 51
	wait 8
	wait 52
	wait 8
	wait 53
	wait 8
	wait 54
	wait 8
	wait 55
	wait 8
	wait 54
	wait 8
	wait 53
	wait 8
	wait 52
	wait 8
	wait 51
	wait 8
	wait 50
	wait 8
	wait 49
	wait 8
	wait 48
	wait 8
	unlock_envelope
	chan_stop
