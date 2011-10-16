GHCMAKE = $(GHC) --make
GHCFLAGS =-O2 -XForeignFunctionInterface -fglasgow-exts -rtsopts -i./lib 
OFILES=./lib/trie_lib.c ./lib/rts.c
SALDO_TMP=$(SALDO)-bin$(EXEEXT)
DALIN_TMP=$(DALIN)-bin$(EXEEXT)
FSV_TMP=$(FSV)-bin$(EXEEXT)
SALDOP=$(SALDO)$(EXEEXT)
DALINP=$(DALIN)$(EXEEXT)
FSVP=$(FSV)$(EXEEXT)

all: ./lib/rts.c ./lib/trie_lib.c ./lib/trie_lib.h
	$(GHCMAKE) ./saldo/Main.hs $(OFILES) $(GHCFLAGS) -i./saldo -o $(SALDO_TMP)
	$(GHCMAKE) ./dalin/Main.hs $(OFILES) $(GHCFLAGS) -i./dalin -o $(DALIN_TMP)
	$(GHCMAKE) ./fsv/Main.hs $(OFILES) $(GHCFLAGS) -i./fsv -o $(FSV_TMP)
	strip $(SALDO_TMP)
	strip $(DALIN_TMP)
	strip $(FSV_TMP)
	mv $(SALDO_TMP) ../bin/$(SALDOP)
	mv $(DALIN_TMP) ../bin/$(DALINP)
	mv $(FSV_TMP) ../bin/$(FSVP)
