############################################################
# This is the toplevel makefile for hunlex
# 
# for more on hunlex compilation, see hunlex dir
PWD=$(shell pwd)
SRCDIR=$(PWD)/src
DOCDIR=$(PWD)/doc
ADMDIR=$(PWD)/adm
EXSDIR=$(PWD)/exs
PREFIX?=/usr/local
INSTALL_PREFIX?=$(PREFIX)

all:   all_src all_adm all_doc all_exs 

all_src:
	cd $(SRCDIR); make

all_adm:
	cd $(ADMDIR); make

all_doc: 
	cd $(DOCDIR); make

all_exs: 
	cd $(EXSDIR); make



clean:  clean_src clean_doc #clean_examples

clean_src:
	cd $(SRCDIR); make clean

clean_exs: 
	cd $(EXSDIR); make clean

clean_doc:
	cd $(DOCDIR); make clean


install:
	([ -e install_prefix ] && echo "already installed under " && \
		cat install_prefix && exit 1) || \
	(\
	cd $(SRCDIR) && make PREFIX=$(INSTALL_PREFIX) install && \
	cd $(ADMDIR) && make PREFIX=$(INSTALL_PREFIX) install && \
	cd $(EXSDIR) && make PREFIX=$(INSTALL_PREFIX) install && \
	cd $(DOCDIR) && make PREFIX=$(INSTALL_PREFIX) install && \
	echo $(INSTALL_PREFIX) > $(PWD)/install_prefix\
	)

uninstall:
	([ -e install_prefix ] && \
	cd $(SRCDIR) && make PREFIX=`cat $(PWD)/install_prefix` uninstall &&\
	cd $(EXSDIR) && make PREFIX=`cat $(PWD)/install_prefix` uninstall && \
	cd $(DOCDIR) && make PREFIX=`cat $(PWD)/install_prefix` uninstall && \
	echo "uninstalled" &&\
	rm -f $(PWD)/install_prefix) || ( echo "not installed" && exit 1 )

reinstall: all uninstall install
