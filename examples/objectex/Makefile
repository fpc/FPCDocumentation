#######################################################################
#
# Makefile to compile all examples and convert them to LaTeX
# 
#######################################################################

# Compiler
ifndef FPC
ifdef PP
FPC=$(PP)
endif
endif
ifndef FPC
FPCPROG:=$(strip $(wildcard $(addsuffix /fpc$(SRCEXEEXT),$(SEARCHPATH))))
ifneq ($(FPCPROG),)
FPCPROG:=$(firstword $(FPCPROG))
FPC:=$(shell $(FPCPROG) -PB)
ifneq ($(findstring Error,$(FPC)),)
override FPC=ppc386
endif
else
override FPC=ppc386
endif
endif
override FPC:=$(subst $(SRCEXEEXT),,$(FPC))
override FPC:=$(subst \,/,$(FPC))$(SRCEXEEXT)
# Unit directory
# UNITDIR=/usr/lib/ppc/0.99.0/linuxunits


# Any options you wish to pass.
PPOPTS=

# Script to convert the programs to LaTeX examples which can be included.
PP2TEX=../pp2tex

# Script to collect all examples in 1 file.
MAKETEX=make1tex

#######################################################################
# No need to edit after this line.
#######################################################################

ifdef UNITDIR
PPOPTS:=$(PPOPTS) -Up$(UNITDIR);
endif
ifdef inUnix
EXECPREFIX=./
else
EXECPREFIX=
endif

.SUFFIXES: .pp .tex

.PHONY: all tex clean execute

OBJECTS=ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 \
        ex11 ex12 ex13 ex14 ex15 ex16 ex17 ex18 ex19 ex20 \
        ex21 ex22 ex23 ex24 ex25 ex26 ex27 ex28 ex29 ex30 \
        ex31 ex32 ex33 ex34 ex35 ex36 ex37 ex38 ex39 ex40

# This might not be the same list as objects, since some of the
# tests might be interactive.
TOTEST=ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 \
        ex11 ex12 ex13 ex14 ex15 ex16 ex17 ex18 ex19 ex20 \
        ex21 ex22 ex23 ex24 ex25 ex26 ex27 ex28 ex29 ex30 \
        ex31 ex32 ex33 ex34 ex35 ex37 ex38 ex39 ex40

LOGFILES=$(addsuffix .log, $(TOTEST))


TEXOBJECTS=$(addsuffix .tex, $(OBJECTS))

EXEOBJECTS=$(addsuffix $(EXEEXT), $(OBJECTS))

all : $(EXEOBJECTS)

tex : $(TEXOBJECTS) myobject.tex mysortc.tex

execute: $(LOGFILES)


onetex : tex
	$(MAKETEX) $(TEXOBJECTS)

clean : 
	-rm -f *.o *.s $(OBJECTS) $(TEXOBJECTS) mysortc.ppu myobject.ppu mysortc.tex myobject.tex test.dat
	-rm -f *.ow *.sw *.exe *.dll *.ppw *.log *.tmp 
 
$(OBJECTS): %: %.pp
	$(FPC) $(PPOPTS) $*

$(TEXOBJECTS) myobject.tex mysortc.tex: %.tex: %.pp head.tex foot.tex
	$(PP2TEX) $*

$(LOGFILES): %.log: %$(EXEEXT)
	$(EXECPREFIX)$*$(EXEEXT) > $*.log
