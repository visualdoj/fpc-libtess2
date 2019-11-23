ifeq ($(wildcard Makefile.node),)

default :
	fpc -Fu../../src -FU. simple.pas
	@./simple
ifeq ($(OS),Windows_NT)
clean :
	del *.map *.ppu *.o *.s *.out $(PROGNAME).exe
else
clean :
	rm *.map *.ppu *.o *.s *.out $(PROGNAME)
endif




else # Doj's environment

default : build run;

SOURCE_PATHS := ports/libtess2

include Makefile.node

build   : fpcprog;
run     : progrun;
clean   : cleancur;
test    : fpctest;

endif
