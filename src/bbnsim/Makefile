############################################################
# Makefile for BBN /src/bbnsim/ source directory
#
#   $Author: bucknerk $
#   $Id: Makefile,v 1.2 2007/07/13 19:50:03 bucknerk Exp $
#   $Log: Makefile,v $
#   Revision 1.2  2007/07/13 19:50:03  bucknerk
#   Just fixed some stupid errors like target names.
#
#   Revision 1.1  2007/07/13 19:07:53  bucknerk
#   These are all new so that they work with the top-level make and
#   hopefully simplify things.  OBTW we NOT autogenerating anything anymore.
#
#
###########################################################

#NOTE do not need this here but it helps to make sure things work right

ifndef $(DB_PW)
verybad:
	@echo "Must run this from the top-level directory (bbn)"
endif

.PHONY: bbn bbndev

VPATH= $(BIN_DIR)

bbn bbndev: run_bbnsim bbnsim
	
bbnsim:	nucb.f  nuccommcb.f  nucintmcb.f  nucratb.f  ran.f
	f77 -O -o $(BIN_DIR)/bbnsim *.f

run_bbnsim: run_bbnsim.pl
	cp -pu run_bbnsim.pl $(BIN_DIR)/run_bbnsim

clean:
	/bin/rm -f *.o
