SndHeader_SFX_2C:
	db $01 ; Number of channels
.ch2:
	db SIS_SFX|SIS_ENABLED ; Initial playback status
	db SND_CH2_PTR ; Sound channel ptr
	dw SndData_SFX_2C_Ch2 ; Data ptr
	dnote 0 ; Base note
	db $81 ; Unused
SndData_SFX_2C_Ch2:
	envelope $1C
	panning $22
	duty_cycle 3
	note A_,5, 1
	lock_envelope
	note A#,5
	snd_loop SndData_SFX_2C_Ch2, $00, 30
	chan_stop
