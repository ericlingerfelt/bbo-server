############################################################
# Makefile for BBN /src/constraint/ source directory
#
#   $Author: bucknerk $
#   $Id: Makefile,v 1.2 2007/07/13 19:50:07 bucknerk Exp $
#   $Log: Makefile,v $
#   Revision 1.2  2007/07/13 19:50:07  bucknerk
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

bbn bbndev: constraint
	
constraint: cosmology6.f 
	f77 -O -o $(BIN_DIR)/constraint cosmology6.f

clean:
	/bin/rm -f *.o
