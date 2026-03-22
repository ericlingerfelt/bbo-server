############################################################
# Makefile for BBN source directory
#
#   $Author: bucknerk $
#   $Id: Makefile,v 1.5 2008/04/29 19:24:02 bucknerk Exp $
#   $Log: Makefile,v $
#   Revision 1.5  2008/04/29 19:24:02  bucknerk
#   Fixed these so that they would compile/install nicely on new machine. I think
#   that they should work now.
#
#   Revision 1.4  2007/07/13 19:49:33  bucknerk
#   Fixed this to use the command name to choose the module path.
#   Hopefully this is not a buggy option.  Must finish checking the
#   dev version in operation.
#
#   Revision 1.3  2007/07/13 19:07:32  bucknerk
#   These are all new so that they work with the top-level make and
#   hopefully simplify things.  OBTW we NOT autogenerating anything anymore.
#
#
###########################################################

ifndef $(DB_PW)
verybad:
	@echo "Must run this from the top-level directory (bbn)"
endif

.PHONY: bbn bbndev bbn_info bbndev_info

bbn: bbn_info
	@cd bbnsim && $(MAKE) $@
	@cd constraint && $(MAKE) $@
	cp -up ag_main.pl $(BIN_DIR)/bbn

bbndev: bbndev_info
	@cd bbnsim && $(MAKE) $@
	@cd constraint && $(MAKE) $@
	cp -up ag_main.pl $(BIN_DIR)/bbndev


bbn_info:
	@echo -e "package ag_db;\n  use warnings;\n  use strict;\n\n \$$ag_db::dbh = '';\n  \$$ag_db::username = 'bbn';\n  \$$ag_db::pw = '$(DB_PW)';\n  \$$ag_db::dir = '/home/www/bbn/';\n\n  1;\n" > ag_db.pm


bbndev_info:
	@echo -e "package ag_db;\n  use warnings;\n  use strict;\n\n \$$ag_db::dbh = '';\n  \$$ag_db::username = 'bbndev';\n  \$$ag_db::pw = '$(DB_PW)d';\n  \$$ag_db::dir = '/home/www/bbndev/';\n\n  1;\n" > ag_db.pm


clean:
	@cd bbnsim && $(MAKE) clean
	@cd constraint && $(MAKE) clean
	/bin/rm -f ag_db.pm
