PREFIX?=/usr/local
INSTALLDIR=$(PREFIX)/lib

all:

install: $(INSTALLDIR)
	        cp HunlexMakefile $(INSTALLDIR)

uninstall:
	        rm -f $(INSTALLDIR)/HunlexMakefile

$(INSTALLDIR):
	install -d $(INSTALLDIR)
