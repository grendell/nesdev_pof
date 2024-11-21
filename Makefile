AS = ca65
LD = ld65
AS_FLAGS =
LD_FLAGS = -C nrom.cfg
OBJ = obj

nesdev.nes: $(OBJ) $(OBJ)/nesdev.o
	$(LD) $(LD_FLAGS) $(OBJ)/nesdev.o -o nesdev.nes

$(OBJ):
	mkdir $(OBJ)

$(OBJ)/nesdev.o: nesdev.s chars.inc system.inc nrom.cfg
	$(AS) $(AS_FLAGS) nesdev.s -o $(OBJ)/nesdev.o

.PHONY: clean
clean:
	rm -rf $(OBJ) nesdev.nes