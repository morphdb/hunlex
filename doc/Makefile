# you may want to select doc formats
FORMATS?=pdf bightml info
# pdf info
AVAILABLE_FORMATS?=info plaintext html dvi ps pdf tex bightml
#available: man info plaintext html dvi ps pdf
#tex available only under makeinfo >=4.8 tex
#docbook xml also available in principle, hack your rules, sorry

PROJECT=hunlex

PREFIX?=/usr/local
INSTALLDIR?=$(PREFIX)/share/hunlex
MANINSTALLDIR?=$(PREFIX)/share/man/man1
INFOINSTALLDIR?=$(PREFIX)/info

TEXINFO_DIR=texinfo
TEXINFO_FILES=$(TEXINFO_DIR)/$(PROJECT).texinfo $(wildcard $(TEXINFO_DIR)/*.texi) # $(TEXINFO_DIR)/$(PROJECT).man

MANPAGE=$(PROJECT).1

MAKEINFO=makeinfo --force --enable-encoding --fill-column=72 -P $(TEXINFO_DIR)

TEX2DVIFLAFS=-t @smallbook

vpath $(PROJECT).html html
vpath $(PROJECT).bightml bightml
vpath $(PROJECT).pdf pdf
vpath $(PROJECT).ps ps
vpath $(PROJECT).tex tex
vpath $(PROJECT).dvi dvi
vpath $(PROJECT).info info

all: $(FORMATS) $(FORMATS:%=$(PROJECT).%)

install: $(INSTALLDIR)  $(INFOINSTALLDIR) all # $(MANINSTALLDIR)
	echo $^
	cp -R $(FORMATS) $(INSTALLDIR)
#	cp man/$(MANPAGE) $(MANINSTALLDIR)
	if [ -w /var/backups ]; then install-info  --description="hunlex" --menuentry=hunlex --dir-file=$(PREFIX)/info info/$(PROJECT).info; else echo "WARNING: can't install info centrally: no write privileges to /var/backups"; fi
	ln -sf $(INSTALLDIR)/info/$(PROJECT).info $(INFOINSTALLDIR)/$(PROJECT).info

uninstall:
	rm -rf $(INSTALLDIR)
	rm -f $(MANINSTALLDIR)/$(MANPAGE)
	if [ -w /var/backups ]; then install-info --dir-file=$(PREFIX)/info --remove $(INFOINSTALLDIR)/$(PROJECT).info ; fi
	rm -f $(INFOINSTALLDIR)/$(PROJECT).info

$(INFOINSTALLDIR):
	install -d $(INFOINSTALLDIR)

clean:
	rm -rf $(AVAILABLE_FORMATS)


# the manpage is just the command line options
# and included verbatim as a chapter in the texinfo manual

#man/$(MANPAGE): ../src/hunlex_wrapper.ml man
#	if [ -n "$(REMAN)" ]; then help2man ../src/hunlex > $@; fi

#$(TEXINFO_DIR)/$(PROJECT).man: man/$(MANPAGE)
#	man $^ | col -b > $@

# extra options
# --css-include=FILE --no-split

$(PROJECT).html: $(TEXINFO_FILES) # html
	$(MAKEINFO) --html --output=html $<
	ln -fs html/index.html html/$(PROJECT).html

$(PROJECT).bightml: $(TEXINFO_FILES) bightml
	$(MAKEINFO) --html --no-split --output=bightml/index.html $<
	ln -fs bightml/index.html bightml/$(PROJECT).bightml


$(PROJECT).pdf: $(TEXINFO_FILES) pdf
	texi2pdf -o pdf/$(PROJECT).pdf $<

$(PROJECT).ps: dvi/$(PROJECT).dvi ps
	dvips -o ps/$(PROJECT).ps $<

$(PROJECT).dvi: $(TEXINFO_FILES) dvi
	texi2dvi -c -o $@ $(TEX2DVIFLAFS) $<

# not yet available in texinfo 4.6
texdoc: $(TEXINFO_FILES)
	$(MAKEINFO) --tex --output=tex $<

$(PROJECT).info: $(TEXINFO_FILES) info
	$(MAKEINFO) --output=info/$(PROJECT).info $<

$(PROJECT).txt: $(TEXINFO_FILES) txt
	$(MAKEINFO) --no-split --no-headers  $< > txt/$(PROJECT).txt

man $(AVAILABLE_FORMATS) $(INSTALLDIR) $(MANINSTALLDIR):
	mkdir -p $@

