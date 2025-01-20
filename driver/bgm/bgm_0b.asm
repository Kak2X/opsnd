SndHeader_BGM_0B:
	db $04 ; Number of channels
.ch1:
	db SIS_ENABLED ; Initial playback status
	db SND_CH1_PTR ; Sound channel ptr
	dw SndData_BGM_0B_Ch1 ; Data ptr
	db 0 ; Initial fine tune
	db $81 ; Unused
.ch2:
	db SIS_ENABLED ; Initial playback status
	db SND_CH2_PTR ; Sound channel ptr
	dw SndData_BGM_0B_Ch2 ; Data ptr
	db 0 ; Initial fine tune
	db $81 ; Unused
.ch3:
	db SIS_ENABLED ; Initial playback status
	db SND_CH3_PTR ; Sound channel ptr
	dw SndData_BGM_0B_Ch3 ; Data ptr
	db 0 ; Initial fine tune
	db $81 ; Unused
.ch4:
	db SIS_ENABLED ; Initial playback status
	db SND_CH4_PTR ; Sound channel ptr
	dw SndData_BGM_0B_Ch4 ; Data ptr
	db 0 ; Initial fine tune
	db $81 ; Unused
SndData_BGM_0B_Ch1:
	envelope $A8
	panning $11
	duty_cycle 3
	vibrato_on $03
	snd_call SndCall_BGM_0B_Ch1_0
	snd_call SndCall_BGM_0B_Ch1_1
	snd_call SndCall_BGM_0B_Ch1_0
	snd_call SndCall_BGM_0B_Ch1_2
	snd_call SndCall_BGM_0B_Ch1_0
	snd_call SndCall_BGM_0B_Ch1_1
	snd_call SndCall_BGM_0B_Ch1_0
	snd_call SndCall_BGM_0B_Ch1_3
	snd_call SndCall_BGM_0B_Ch1_4
	envelope $A8
	note F#,6, 14
	silence 7
	note E_,6
	note D_,6
	note C#,6
	silence
	note D_,6, 21
	note C#,6, 7
	silence
	note D_,6
	note D#,6
	silence
	note E_,6, 56
	wait2 7
	envelope $88
	note E_,6, 7
	silence 3
	envelope $68
	note E_,6, 4
	envelope $48
	note E_,6, 7
	silence 3
	envelope $38
	note E_,6, 4
	silence 28
	snd_call SndCall_BGM_0B_Ch1_4
	envelope $A8
	note B_,5, 5
	silence 2
	note B_,5, 5
	silence 2
	note B_,5, 5
	silence 2
	note B_,5, 7
	silence
	note A_,5
	silence
	note G#,5, 19
	silence 2
	note A_,5, 14
	note B_,5
	note C#,6
	silence 3
	envelope $88
	note C#,6, 4
	envelope $A8
	note D_,6, 7
	silence
	note D_,6, 5
	silence 2
	note D_,6, 5
	silence 2
	note D#,6, 7
	silence
	note D#,6
	note E_,6, 28
	envelope $88
	note E_,6, 7
	silence 3
	envelope $68
	note E_,6, 4
	envelope $48
	note E_,6, 7
	silence 3
	envelope $38
	note E_,6, 4
	envelope $A8
	note D_,6, 7
	note A_,5
	note F#,5, 14
	note D_,6, 7
	note A_,5
	note F#,5, 14
	note E_,6, 7
	note C#,6
	note A_,5, 14
	note E_,6, 7
	note C#,6
	note A_,5, 14
	note D_,6, 7
	note B_,5
	note G#,5, 14
	note D_,6, 7
	note B_,5
	note G#,5, 14
	note C#,6, 7
	note A_,5
	note E_,5, 14
	note C#,6, 7
	note A_,5
	note E_,5, 14
	note D_,6, 7
	note A_,5
	note F#,5, 14
	note D_,6, 7
	note A_,5
	note F#,5, 14
	note E_,6, 7
	note C#,6
	note A_,5, 14
	note E_,6, 7
	note C#,6
	note A_,5, 14
	note D_,6, 7
	note B_,5
	note E_,5, 14
	note D_,6, 7
	note B_,5
	note E_,5, 14
	note A_,5, 28
	envelope $88
	note A_,5, 7
	silence 3
	envelope $68
	note A_,5, 4
	envelope $48
	note A_,5, 7
	silence 3
	envelope $38
	note A_,5, 4
	snd_loop SndData_BGM_0B_Ch1
SndCall_BGM_0B_Ch1_0:
	envelope $A8
	duty_cycle 3
	note A_,4, 14
	silence 7
	note A_,4
	note B_,4
	note C#,5
	silence
	note D_,5
	silence
	note A_,5, 12
	silence 2
	note A_,5, 7
	note G#,5
	note F#,5
	note G#,5, 14
	note F#,5
	note E_,5, 5
	silence 2
	note E_,5, 28
	envelope $88
	note E_,5, 7
	silence 3
	envelope $68
	note E_,5, 4
	envelope $82
	duty_cycle 2
	note E_,6, 7
	silence
	note E_,6
	note D_,6
	note C#,6
	note B_,5, 14
	snd_ret
SndCall_BGM_0B_Ch1_1:
	envelope $A8
	duty_cycle 3
	note A_,4, 14
	silence 7
	note A_,4
	note B_,4
	note C#,5
	silence
	note D_,5
	silence
	note A_,5, 12
	silence 2
	note A_,5, 7
	note G#,5
	note F#,5
	note G#,5
	note A_,5
	note B_,5, 42
	envelope $88
	note B_,5, 7
	silence 3
	envelope $68
	note B_,5, 4
	envelope $48
	note B_,5, 7
	silence 3
	envelope $38
	note B_,5, 4
	envelope $82
	duty_cycle 2
	note D_,6, 14
	note C#,6
	note B_,5
	snd_ret
SndCall_BGM_0B_Ch1_2:
	envelope $A8
	duty_cycle 3
	note A_,4, 14
	silence 7
	note A_,4
	note B_,4
	note C#,5
	silence
	note D_,5
	silence
	note D_,6, 12
	silence 2
	note D_,6, 7
	note C#,6
	note B_,5
	note C#,6
	note D_,6
	note A_,5, 56
	envelope $88
	note A_,5, 7
	silence 3
	envelope $68
	note A_,5, 4
	envelope $48
	note A_,5, 7
	silence 3
	envelope $38
	note A_,5, 4
	silence 28
	snd_ret
SndCall_BGM_0B_Ch1_3:
	envelope $A8
	duty_cycle 3
	note A_,4, 14
	silence 7
	note A_,4
	note B_,4
	note C#,5
	silence
	note D_,5
	silence
	note D_,6, 12
	silence 2
	note D_,6, 7
	note C#,6
	note E_,5
	note F#,5
	note G#,5
	note A_,5, 56
	envelope $88
	note A_,5, 7
	silence 3
	envelope $68
	note A_,5, 4
	envelope $48
	note A_,5, 7
	silence 3
	envelope $38
	note A_,5, 4
	silence 28
	snd_ret
SndCall_BGM_0B_Ch1_4:
	envelope $A8
	duty_cycle 3
	note B_,5, 14
	silence 7
	note A_,5
	note B_,5
	note C#,6
	silence
	note C#,6, 1
	note D_,6, 20
	note C#,6, 7
	silence
	note D_,6
	note E_,6
	silence
	note C_,6, 1
	note C#,6, 20
	note A_,5, 42
	envelope $88
	note A_,5, 7
	silence 3
	envelope $68
	note A_,5, 4
	envelope $48
	note A_,5, 7
	silence 3
	envelope $38
	note A_,5, 4
	silence 28
	snd_ret
SndData_BGM_0B_Ch2:
	envelope $A8
	panning $22
	duty_cycle 1
	vibrato_on $03
	snd_call SndCall_BGM_0B_Ch2_0
	snd_call SndCall_BGM_0B_Ch2_1
	snd_call SndCall_BGM_0B_Ch2_0
	duty_cycle 1
	envelope $88
	note E_,4, 11
	envelope $68
	note E_,4, 7
	silence 3
	envelope $88
	note E_,4, 7
	note G#,4, 3
	envelope $68
	note E_,4, 4
	envelope $88
	note A_,4, 3
	envelope $68
	note G#,4, 4
	silence 3
	note A_,4, 4
	envelope $88
	note B_,4, 7
	silence 3
	envelope $68
	note B_,4, 4
	envelope $88
	note E_,5, 11
	envelope $68
	note E_,5, 3
	envelope $88
	note E_,5, 7
	note F#,5, 3
	envelope $68
	note E_,5, 4
	envelope $88
	note F#,5, 3
	envelope $68
	note F#,5, 4
	envelope $88
	note G_,5, 3
	envelope $68
	note F#,5, 4
	envelope $88
	note G#,5, 3
	envelope $68
	note G_,5, 4
	envelope $A8
	note E_,5, 3
	envelope $68
	note G#,5, 4
	envelope $A8
	note C#,5, 3
	envelope $88
	note E_,5, 4
	envelope $A8
	note A_,4, 3
	envelope $88
	note C#,5, 4
	envelope $A8
	note E_,4, 3
	envelope $88
	note A_,4, 4
	envelope $A8
	note D_,4, 3
	envelope $88
	note E_,4, 4
	envelope $A8
	note C#,4, 3
	envelope $88
	note D_,4, 4
	envelope $A8
	note D_,4, 3
	envelope $88
	note C#,4, 4
	envelope $A8
	note E_,4, 3
	envelope $88
	note D_,4, 4
	envelope $A8
	note D_,4, 3
	envelope $88
	note E_,4, 4
	envelope $A8
	note C#,4, 3
	envelope $88
	note D_,4, 4
	silence 3
	note C#,4, 4
	envelope $A8
	note A_,3, 21
	envelope $88
	note A_,3, 7
	silence 3
	envelope $68
	note A_,3, 4
	snd_call SndCall_BGM_0B_Ch2_0
	snd_call SndCall_BGM_0B_Ch2_1
	snd_call SndCall_BGM_0B_Ch2_0
	duty_cycle 1
	envelope $88
	note E_,4, 11
	envelope $68
	note E_,4, 7
	silence 3
	envelope $88
	note E_,4, 7
	note G#,4, 3
	envelope $68
	note E_,4, 4
	envelope $88
	note A_,4, 3
	envelope $68
	note G#,4, 4
	silence 3
	note A_,4, 4
	envelope $88
	note B_,4, 7
	silence 3
	envelope $68
	note B_,4, 4
	envelope $88
	note A_,5, 11
	envelope $68
	note A_,5, 3
	envelope $88
	note A_,5, 5
	silence 2
	note A_,5, 3
	envelope $68
	note A_,5, 4
	envelope $88
	note C#,5, 3
	envelope $68
	note A_,5, 4
	envelope $88
	note D_,5, 3
	envelope $68
	note C#,5, 4
	envelope $88
	note E_,5, 3
	envelope $68
	note D_,5, 4
	envelope $A8
	note C#,5, 3
	envelope $68
	note E_,5, 4
	envelope $A8
	note E_,5, 3
	envelope $88
	note C#,5, 4
	envelope $A8
	note A_,4, 3
	envelope $88
	note E_,5, 4
	envelope $A8
	note C#,5, 3
	envelope $88
	note A_,4, 4
	envelope $A8
	note E_,4, 3
	envelope $88
	note C#,5, 4
	envelope $A8
	note A_,4, 3
	envelope $88
	note E_,4, 4
	envelope $A8
	note C#,4, 3
	envelope $88
	note A_,4, 4
	envelope $A8
	note E_,4, 3
	envelope $88
	note C#,4, 4
	envelope $A8
	note A_,3, 3
	envelope $88
	note E_,4, 4
	envelope $A8
	note C#,4, 3
	envelope $88
	note A_,3, 4
	envelope $A8
	note E_,4, 3
	envelope $88
	note C#,4, 4
	envelope $A8
	note A_,4, 14
	silence 3
	envelope $88
	note A_,4, 4
	envelope $A8
	note C#,5, 7
	silence 3
	envelope $88
	note C#,5, 4
	snd_call SndCall_BGM_0B_Ch2_2
	envelope $82
	note A_,5, 3
	envelope $62
	note G#,4, 4
	envelope $82
	note E_,5, 3
	envelope $62
	note A_,5, 4
	envelope $82
	note C#,5, 3
	envelope $62
	note E_,5, 4
	envelope $82
	note A_,4, 3
	envelope $62
	note C#,5, 4
	envelope $82
	note E_,5, 3
	envelope $62
	note A_,4, 4
	envelope $82
	note C#,5, 3
	envelope $62
	note E_,5, 4
	envelope $82
	note A_,4, 3
	envelope $62
	note C#,5, 4
	envelope $82
	note E_,4, 3
	envelope $62
	note A_,4, 4
	snd_call SndCall_BGM_0B_Ch2_3
	snd_call SndCall_BGM_0B_Ch2_4
	snd_call SndCall_BGM_0B_Ch2_5
	envelope $82
	note C#,6, 3
	envelope $62
	note E_,4, 4
	envelope $82
	note A_,5, 3
	envelope $62
	note C#,6, 4
	envelope $82
	note E_,5, 3
	envelope $62
	note A_,5, 4
	envelope $82
	note C#,5, 3
	envelope $62
	note E_,5, 4
	envelope $82
	note A_,5, 3
	envelope $62
	note C#,5, 4
	envelope $82
	note E_,5, 3
	envelope $62
	note A_,5, 4
	envelope $82
	note C#,5, 3
	envelope $62
	note E_,5, 4
	envelope $82
	note A_,4, 3
	envelope $62
	note C#,5, 4
	envelope $82
	note E_,5, 3
	envelope $62
	note A_,4, 4
	envelope $82
	note C#,5, 3
	envelope $62
	note E_,5, 4
	envelope $82
	note A_,4, 3
	envelope $62
	note C#,5, 4
	envelope $82
	note E_,4, 3
	envelope $62
	note A_,4, 4
	envelope $82
	note G_,4, 3
	envelope $62
	note E_,4, 4
	envelope $82
	note B_,4, 3
	envelope $62
	note G_,4, 4
	envelope $82
	note D_,5, 3
	envelope $62
	note B_,4, 4
	envelope $82
	note F_,5, 3
	envelope $62
	note D_,5, 4
	snd_call SndCall_BGM_0B_Ch2_2
	envelope $82
	note A_,5, 3
	envelope $62
	note G#,4, 4
	envelope $82
	note E_,5, 3
	envelope $62
	note A_,5, 4
	envelope $82
	note C#,5, 3
	envelope $62
	note E_,5, 4
	envelope $82
	note A_,4, 3
	envelope $62
	note C#,5, 4
	envelope $82
	note E_,5, 3
	envelope $62
	note A_,4, 4
	envelope $82
	note C#,5, 3
	envelope $62
	note E_,5, 4
	envelope $82
	note A_,4, 3
	envelope $62
	note C#,5, 4
	envelope $82
	note E_,4, 3
	envelope $62
	note A_,4, 4
	snd_call SndCall_BGM_0B_Ch2_3
	envelope $88
	note F#,5, 3
	envelope $68
	note E_,4, 4
	envelope $88
	note F#,5, 3
	envelope $68
	note F#,5, 4
	envelope $88
	note F#,5, 3
	envelope $68
	note F#,5, 4
	envelope $88
	note F#,5, 3
	envelope $68
	note F#,5, 4
	silence 3
	note F#,5, 4
	envelope $88
	note F#,5, 7
	silence 3
	envelope $68
	note F#,5, 4
	envelope $88
	note E_,5, 11
	envelope $68
	note E_,5, 10
	envelope $88
	note F#,5, 7
	silence 3
	envelope $68
	note F#,5, 4
	envelope $88
	note G#,5, 7
	silence 3
	envelope $68
	note G#,5, 4
	envelope $88
	note A_,5, 7
	silence 3
	envelope $68
	note A_,5, 4
	silence 3
	envelope $48
	note A_,5, 4
	envelope $88
	note B_,5, 7
	silence 3
	envelope $68
	note B_,5, 4
	envelope $88
	note B_,5, 5
	silence 2
	note B_,5, 5
	silence 2
	note B_,5, 7
	silence 3
	envelope $68
	note B_,5, 4
	envelope $88
	note B_,5, 5
	silence 2
	note B_,5, 28
	envelope $68
	note B_,5, 7
	silence 3
	envelope $48
	note B_,5, 4
	envelope $68
	note B_,5, 7
	silence
	snd_call SndCall_BGM_0B_Ch2_6
	snd_call SndCall_BGM_0B_Ch2_7
	snd_call SndCall_BGM_0B_Ch2_8
	snd_call SndCall_BGM_0B_Ch2_9
	snd_call SndCall_BGM_0B_Ch2_6
	snd_call SndCall_BGM_0B_Ch2_7
	envelope $88
	note B_,5, 7
	note G#,5, 3
	envelope $68
	note B_,5, 4
	envelope $88
	note E_,5, 7
	envelope $68
	note G#,5, 3
	note E_,5, 4
	envelope $88
	note B_,5, 7
	note G#,5, 3
	envelope $68
	note B_,5, 4
	envelope $88
	note B_,4, 7
	envelope $68
	note G#,5, 3
	note B_,4, 4
	envelope $88
	note E_,5, 28
	envelope $68
	note E_,5, 7
	silence 3
	envelope $48
	note E_,5, 4
	envelope $38
	note E_,5, 7
	silence
	snd_loop SndData_BGM_0B_Ch2
SndCall_BGM_0B_Ch2_0:
	duty_cycle 1
	envelope $88
	note E_,4, 11
	envelope $68
	note E_,4, 7
	silence 3
	envelope $88
	note E_,4, 7
	note G#,4, 3
	envelope $68
	note E_,4, 4
	envelope $88
	note A_,4, 3
	envelope $68
	note G#,4, 4
	silence 3
	note A_,4, 4
	envelope $88
	note B_,4, 7
	silence 3
	envelope $68
	note B_,4, 4
	envelope $88
	note F#,5, 11
	envelope $68
	note F#,5, 3
	envelope $88
	note F#,5, 7
	note E_,5, 3
	envelope $68
	note F#,5, 4
	envelope $88
	note D_,5, 3
	envelope $68
	note E_,5, 4
	envelope $88
	note E_,5, 3
	envelope $68
	note D_,5, 4
	silence 3
	note E_,5, 4
	envelope $88
	note D_,5, 11
	envelope $68
	note D_,5, 3
	envelope $88
	note C#,5, 5
	silence 2
	note C#,5, 28
	envelope $68
	note C#,5, 7
	silence 3
	envelope $48
	note C#,5, 4
	duty_cycle 2
	envelope $82
	note G#,5, 7
	silence 3
	envelope $62
	note G#,5, 4
	envelope $82
	note G#,5, 7
	note A_,5, 3
	envelope $62
	note G#,5, 4
	envelope $82
	note A_,5, 3
	envelope $62
	note A_,5, 4
	envelope $82
	note G#,5, 7
	silence 3
	envelope $62
	note G#,5, 4
	snd_ret
SndCall_BGM_0B_Ch2_1:
	duty_cycle 1
	envelope $88
	note E_,4, 11
	envelope $68
	note E_,4, 7
	silence 3
	envelope $88
	note E_,4, 7
	note G#,4, 3
	envelope $68
	note E_,4, 4
	envelope $88
	note A_,4, 3
	envelope $68
	note G#,4, 4
	silence 3
	note A_,4, 4
	envelope $88
	note B_,4, 7
	silence 3
	envelope $68
	note B_,4, 4
	envelope $88
	note E_,5, 11
	envelope $68
	note E_,5, 3
	envelope $88
	note E_,5, 7
	note E_,5, 3
	envelope $68
	note E_,5, 4
	envelope $88
	note D#,5, 3
	envelope $68
	note E_,5, 4
	envelope $88
	note E_,5, 3
	envelope $68
	note D#,5, 4
	envelope $88
	note F#,5, 3
	envelope $68
	note E_,5, 4
	envelope $88
	note G#,5, 28
	envelope $68
	note G#,5, 7
	silence 3
	envelope $48
	note G#,5, 4
	envelope $A8
	note B_,4, 7
	note D_,5, 3
	envelope $88
	note B_,4, 4
	envelope $A8
	note E_,5, 3
	envelope $88
	note D_,5, 4
	silence 3
	note E_,5, 4
	duty_cycle 2
	envelope $82
	note A_,5, 11
	envelope $62
	note A_,5, 3
	envelope $82
	note A_,5, 11
	envelope $62
	note A_,5, 3
	envelope $82
	note G#,5, 11
	envelope $62
	note G#,5, 3
	snd_ret
SndCall_BGM_0B_Ch2_2:
	envelope $82
	note G#,5, 3
	envelope $62
	note G#,4, 4
	envelope $82
	note E_,5, 3
	envelope $62
	note G#,5, 4
	envelope $82
	note B_,4, 3
	envelope $62
	note E_,5, 4
	envelope $82
	note G#,4, 3
	envelope $62
	note B_,4, 4
	snd_loop SndCall_BGM_0B_Ch2_2, $00, 4
	snd_ret
SndCall_BGM_0B_Ch2_3:
	envelope $82
	note E_,5, 3
	envelope $62
	note E_,4, 4
	envelope $82
	note C#,5, 3
	envelope $62
	note E_,5, 4
	envelope $82
	note A_,4, 3
	envelope $62
	note C#,5, 4
	envelope $82
	note E_,4, 3
	envelope $62
	note A_,4, 4
	snd_loop SndCall_BGM_0B_Ch2_3, $00, 2
	snd_ret
SndCall_BGM_0B_Ch2_4:
	envelope $82
	note A_,5, 3
	envelope $62
	note A_,4, 4
	envelope $82
	note F#,5, 3
	envelope $62
	note A_,5, 4
	envelope $82
	note D_,5, 3
	envelope $62
	note F#,5, 4
	envelope $82
	note A_,4, 3
	envelope $62
	note D_,5, 4
	snd_loop SndCall_BGM_0B_Ch2_4, $00, 2
	snd_ret
SndCall_BGM_0B_Ch2_5:
	envelope $82
	note E_,5, 3
	envelope $62
	note E_,4, 4
	envelope $82
	note B_,4, 3
	envelope $62
	note E_,5, 4
	envelope $82
	note G#,4, 3
	envelope $62
	note B_,4, 4
	envelope $82
	note E_,4, 3
	envelope $62
	note G#,4, 4
	snd_loop SndCall_BGM_0B_Ch2_5, $00, 2
	snd_ret
SndCall_BGM_0B_Ch2_6:
	envelope $88
	note A_,5, 7
	note F#,5, 3
	envelope $68
	note A_,5, 4
	envelope $88
	note D_,5, 7
	envelope $68
	note F#,5, 3
	note D_,5, 4
	snd_loop SndCall_BGM_0B_Ch2_6, $00, 2
	snd_ret
SndCall_BGM_0B_Ch2_7:
	envelope $88
	note C#,6, 7
	note A_,5, 3
	envelope $68
	note C#,6, 4
	envelope $88
	note E_,5, 7
	envelope $68
	note A_,5, 3
	note E_,5, 4
	snd_loop SndCall_BGM_0B_Ch2_7, $00, 2
	snd_ret
SndCall_BGM_0B_Ch2_8:
	envelope $88
	note B_,5, 7
	note G#,5, 3
	envelope $68
	note B_,5, 4
	envelope $88
	note E_,5, 7
	envelope $68
	note G#,5, 3
	note E_,5, 4
	snd_loop SndCall_BGM_0B_Ch2_8, $00, 2
	snd_ret
SndCall_BGM_0B_Ch2_9:
	envelope $88
	note A_,5, 7
	note E_,5, 3
	envelope $68
	note A_,5, 4
	envelope $88
	note C#,5, 7
	envelope $68
	note E_,5, 3
	note C#,5, 4
	snd_loop SndCall_BGM_0B_Ch2_9, $00, 2
	snd_ret
SndData_BGM_0B_Ch3:
	wave_vol $40
	panning $44
	wave_id $03
	wave_cutoff 0
	snd_call SndCall_BGM_0B_Ch3_0
	snd_call SndCall_BGM_0B_Ch3_1
	snd_call SndCall_BGM_0B_Ch3_0
	snd_call SndCall_BGM_0B_Ch3_2
	snd_call SndCall_BGM_0B_Ch3_0
	snd_call SndCall_BGM_0B_Ch3_1
	snd_call SndCall_BGM_0B_Ch3_0
	snd_call SndCall_BGM_0B_Ch3_2
	snd_call SndCall_BGM_0B_Ch3_3
	fine_tune 5
	snd_call SndCall_BGM_0B_Ch3_3
	fine_tune -5
	note D_,4, 14
	note F#,3, 7
	note G#,3
	silence
	note A_,3
	note D_,4, 14
	note E_,4
	note G#,3, 7
	note B_,3
	silence
	note D_,4
	note E_,4, 14
	snd_call SndCall_BGM_0B_Ch3_4
	note G#,3, 14
	note G#,4, 7
	note G#,3
	silence
	note G#,3
	note G#,4, 14
	note E_,3
	note E_,4
	note G#,3
	note B_,3
	snd_call SndCall_BGM_0B_Ch3_4
	note D#,3, 7
	note F#,3
	note A_,3
	note D#,3, 14
	note A_,3, 7
	silence
	note B_,3, 19
	silence 2
	note B_,3, 14
	note F#,3
	note B_,3
	silence 7
	note E_,4, 12
	silence 2
	note E_,4, 7
	note B_,3
	note E_,4
	silence
	note E_,4
	note E_,3
	silence
	note E_,3, 14
	note F#,3
	note G#,3
	snd_call SndCall_BGM_0B_Ch3_5
	note E_,4, 14
	silence 7
	note B_,3, 21
	note E_,4, 14
	note A_,3
	silence 7
	note B_,3, 21
	note C#,4, 14
	snd_call SndCall_BGM_0B_Ch3_5
	note D_,3, 7
	silence
	note D_,4, 14
	note E_,3, 7
	silence
	note E_,4, 14
	note A_,3, 19
	silence 2
	note A_,4, 19
	silence 2
	note A_,4, 14
	snd_loop SndData_BGM_0B_Ch3
SndCall_BGM_0B_Ch3_0:
	note A_,3, 19
	silence 2
	note A_,3, 7
	silence
	note A_,3
	silence
	note D_,4, 14
	note F#,3
	note D_,4, 5
	silence 2
	note D_,4, 14
	note C#,4
	note B_,3, 19
	silence 2
	note B_,3, 7
	silence
	note E_,3
	silence
	note E_,4, 14
	note B_,3
	note E_,4, 7
	note D_,4, 14
	note C#,4
	note A_,3, 19
	silence 2
	note A_,3, 7
	silence
	note E_,3
	silence
	note D_,4, 14
	note F#,3
	snd_ret
SndCall_BGM_0B_Ch3_1:
	note D_,4, 7
	note C#,4, 14
	note D_,4, 7
	note D#,4
	note E_,4, 14
	silence 7
	note E_,4
	note E_,3
	note E_,4
	silence
	note E_,4, 12
	silence 2
	note E_,4, 7
	note D_,4, 14
	note C#,4
	note B_,3
	snd_ret
SndCall_BGM_0B_Ch3_2:
	note D_,4, 7
	note E_,4
	silence
	note E_,4, 5
	silence 2
	note E_,4, 7
	note A_,3, 14
	silence 7
	note A_,3
	silence
	note E_,3
	note F#,3
	note G_,3
	silence
	note G#,3, 12
	silence 2
	note G#,3, 7
	note A_,3, 28
	snd_ret
SndCall_BGM_0B_Ch3_3:
	note E_,3, 14
	note E_,4, 7
	note E_,3
	silence
	note E_,3
	note E_,4, 14
	snd_loop SndCall_BGM_0B_Ch3_3, $00, 2
	snd_ret
SndCall_BGM_0B_Ch3_4:
	note A_,3, 14
	note A_,4, 7
	note A_,3
	silence
	note A_,3
	note A_,4, 14
	note A_,3
	note A_,4
	note G_,3
	note G_,4
	snd_ret
SndCall_BGM_0B_Ch3_5:
	note D_,4, 14
	silence 7
	note F#,3, 21
	note D_,4, 14
	note A_,3
	silence 7
	note E_,3, 21
	note A_,3, 14
	snd_ret
SndData_BGM_0B_Ch4:
	panning $88
	snd_call SndCall_BGM_0B_Ch4_0
	envelope $C1
	wait 87
	wait 7
	envelope $51
	wait 23
	wait 7
	wait 23
	wait 7
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	envelope $51
	wait 23
	wait 7
	wait 23
	wait 7
	wait 23
	wait 7
	envelope $C1
	wait 87
	wait 7
	envelope $51
	wait 23
	wait 7
	wait 23
	wait 7
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	envelope $51
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	wait 38
	wait 7
	snd_call SndCall_BGM_0B_Ch4_0
	envelope $C1
	wait 87
	wait 7
	envelope $51
	wait 23
	wait 7
	wait 23
	wait 7
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	envelope $51
	wait 23
	wait 7
	wait 23
	wait 7
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	envelope $51
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	envelope $51
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	envelope $51
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	wait 38
	wait 7
	snd_call SndCall_BGM_0B_Ch4_2
	snd_call SndCall_BGM_0B_Ch4_1
	snd_call SndCall_BGM_0B_Ch4_2
	snd_call SndCall_BGM_0B_Ch4_1
	snd_call SndCall_BGM_0B_Ch4_2
	snd_call SndCall_BGM_0B_Ch4_1
	snd_call SndCall_BGM_0B_Ch4_2
	envelope $C1
	wait 87
	wait 7
	envelope $A1
	wait 38
	wait 7
	envelope $53
	wait 19
	wait 7
	envelope $A1
	wait 38
	wait 7
	wait 38
	wait 7
	wait 38
	wait 7
	envelope $53
	wait 19
	wait 7
	envelope $A1
	wait 38
	wait 7
	wait 38
	wait 7
	envelope $51
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	envelope $51
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	wait 38
	wait 7
	envelope $91
	wait 52
	wait 7
	envelope $91
	wait 53
	wait 7
	snd_call SndCall_BGM_0B_Ch4_1
	snd_call SndCall_BGM_0B_Ch4_1
	snd_call SndCall_BGM_0B_Ch4_1
	envelope $C1
	wait 87
	wait 7
	envelope $51
	wait 23
	wait 7
	wait 23
	wait 7
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	envelope $51
	wait 23
	wait 7
	wait 23
	wait 7
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	wait 38
	wait 7
	envelope $91
	wait 53
	wait 7
	envelope $53
	wait 19
	wait 14
	envelope $91
	wait 53
	wait 7
	envelope $91
	wait 55
	wait 14
	snd_loop SndData_BGM_0B_Ch4
SndCall_BGM_0B_Ch4_0:
	snd_call SndCall_BGM_0B_Ch4_1
	snd_loop SndCall_BGM_0B_Ch4_0, $00, 7
	snd_ret
SndCall_BGM_0B_Ch4_1:
	envelope $C1
	wait 87
	wait 7
	envelope $51
	wait 23
	wait 7
	wait 23
	wait 7
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	envelope $51
	wait 23
	wait 7
	wait 23
	wait 7
	wait 23
	wait 7
	envelope $C1
	wait 87
	wait 7
	envelope $51
	wait 23
	wait 7
	wait 23
	wait 7
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	envelope $51
	wait 23
	wait 7
	envelope $53
	wait 19
	wait 7
	envelope $C1
	wait 87
	wait 7
	snd_ret
SndCall_BGM_0B_Ch4_2:
	envelope $C1
	wait 87
	wait 7
	envelope $51
	wait 23
	wait 7
	wait 23
	wait 7
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	envelope $51
	wait 23
	wait 7
	wait 23
	wait 7
	wait 23
	wait 7
	envelope $C1
	wait 87
	wait 7
	envelope $51
	wait 23
	wait 7
	wait 23
	wait 7
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	envelope $51
	wait 23
	wait 7
	envelope $A1
	wait 38
	wait 7
	envelope $53
	wait 19
	wait 7
	snd_ret
