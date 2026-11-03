SECTION "Sound Driver Interface for the player", ROM0
SoundInt_Init: mHomeCallRet Sound_StopAll_Main
SoundInt_Do: mHomeCallRet Sound_Do_Main
SoundInt_ReqPlayId:	mHomeCallRet Sound_ReqPlayId_Main
SoundInt_SetVolume: mHomeCallRet Sound_SetVolume_Main
IF !TRIM_UNMAPPED
SoundInt_StartSlide: mHomeCallRet Sound_StartSlide_Main
ENDC
Bankswitch:
	mBankswitch
	ret