; =============== Sound_WaveSetPtrTable ===============
; Sets of Wave data for channel 3, copied directly to the rWave registers.
; [TCRF] More than half of the sets are unused!
Sound_WaveSetPtrTable:
	dw Sound_WaveSet_Unused_0
	dw Sound_WaveSet1
	dw Sound_WaveSet2
	dw Sound_WaveSet_Unused_3
	dw Sound_WaveSet_Unused_4
	dw Sound_WaveSet5
	dw Sound_WaveSet_Unused_6
	dw Sound_WaveSet_Unused_7
	dw Sound_WaveSet_Unused_8
	dw Sound_WaveSet_Unused_9

Sound_WaveSet_Unused_0: db $01,$23,$45,$67,$89,$AB,$CD,$EF,$ED,$CB,$A9,$87,$65,$43,$21,$00
Sound_WaveSet1: db $FF,$EE,$DD,$CC,$BB,$AA,$99,$88,$77,$66,$55,$44,$33,$22,$11,$00
Sound_WaveSet2: db $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
Sound_WaveSet_Unused_3: db $FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
Sound_WaveSet_Unused_4: db $02,$46,$89,$AB,$CC,$DD,$EE,$FF,$FE,$ED,$DD,$CC,$BA,$98,$64,$31
Sound_WaveSet5: db $CF,$AF,$30,$12,$21,$01,$7F,$C2,$EA,$07,$FC,$62,$12,$5B,$FB,$12
Sound_WaveSet_Unused_6: db $86,$33,$22,$11,$00,$0B,$EF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$EE
Sound_WaveSet_Unused_7: db $87,$76,$65,$54,$43,$32,$21,$10,$0F,$FE,$ED,$DC,$CB,$BA,$A9,$98
Sound_WaveSet_Unused_8: db $18,$F2,$68,$4E,$18,$76,$1A,$4C,$85,$43,$2E,$DC,$FF,$12,$84,$48
Sound_WaveSet_Unused_9: db $CC,$6B,$93,$77,$28,$BA,$6E,$35,$51,$C3,$9C,$ED,$B8,$2B,$29,$E2