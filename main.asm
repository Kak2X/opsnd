INCLUDE "hardware.asm"
IF GBS_MODE
INCLUDE "player/gbs.asm"
ELSE
INCLUDE "player/main.asm"
ENDC
INCLUDE "driver/main.asm"