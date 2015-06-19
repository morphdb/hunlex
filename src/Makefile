###############################################################
# OCAML related BINARIES are assumed to reside in the user path
# you can create a local link to your favouritve version
OCAMLBINDIR?=
PREFIX?=/usr/local/
# must add local OCAML distribution to path
SHELL=`which bash`
PATH = $(OCAMLBINDIR):$(shell echo $$PATH)
export PATH

#############################################################
# we are using the opt version of the compiler (it is faster)
# if it is available (not available for any platform)
OPT?=$(shell export PATH=$(PATH); which ocamlc.opt > /dev/null 2>&1 && echo .opt)
# by specifying, use of a particular version of OCaml can be enforced

OCAMLC		?=	ocamlc$(OPT)
OCAMLOPT	?=	ocamlopt$(OPT)
OCAMLDEP?	?=	ocamldep$(OPT)
OCAMLDOC?	?=	ocamldoc$(OPT)
OCAMLLEX?	?=	ocamllex$(OPT)
OCAMLYACC?	?=	ocamlyacc   # why is there no ocamlyacc.opt?


#################################################################
# assume local OCamlMakefile 
# This is meant to be a link
# define it with full path cause this is passed on to OCamlMakefile
# wherever it resides in actuality
OCAMLMAKEFILE=OCamlMakefile

#################################################################
# set libpaths and incpaths 
SHELL?=/bin/bash
# this is in principle set in OCamlMakefile the same way
# but we need it to find OCamlMakefile itself
OCAMLLIBPATH = $(shell $(OCAMLC) -where)
# and other nested libdirs (This really should be automatic...)
#LIBDIRS = $(shell ls -1dp $(OCAMLLIBPATH)/* | grep '/$$')
#INCDIRS = $(LIBDIRS)

#################################################################
# source files: order is relevant for linking 
SOURCES = \
 utils.ml bv.ml \
 kr.ml \
 kr_parser.mly kr_lexer.mll \
 fs.ml \
 grammar_defs.ml \
 tag.ml	\
 output.ml \
 flag.ml close.ml \
 morph_parser.mly morph_lexer.mll \
 phono_parser.mly phono_lexer.mll \
 usage_parser.mly usage_lexer.mll \
 annot_parser.mly annot_lexer.mll \
 grammar_parser.mly grammar_lexer.mll \
 hunlex.ml \
 hunlex_wrapper.ml

# str library comes with ocaml distribution
LIBS = str unix

# main executable 
RESULT = hunlex

INSTALLDIR?=$(PREFIX)/bin

#################################################################
# default target is native-code
# but it is not available on some platforms
TARGET?=native-code
# note that we check whether OCamlMakefile is available
all: hal checkOCamlMakefile $(TARGET)

hal: 
	echo OCAMLBINDIR=$(OCAMLBINDIR)
	echo $(PATH)
	echo $(OCAMLC)

#* LIBINSTALL_FILES - the files of a library that should be installed
#                    using "findlib". Default:#
#
#                         $(RESULT).mli $(RESULT).cmi $(RESULT).cma
#                         $(RESULT).cmxa $(RESULT).a lib$(RESULT).a

# include main Makefile to do the job  
# OCamlMakefile (courtesy of Markus Mottl)
# http://www.ai.univie.ac.at/~markus/home/ocaml_sources.html#OCamlMakefile

install: $(TARGET) 
	$(QUIET)echo "INSTALLDIR=$(INSTALLDIR)" 
	$(QUIET)install -d $(INSTALLDIR) && cp $(RESULT) $(INSTALLDIR)

uninstall: 
	$(QUIET)echo "remove from $(INSTALLDIR)"
	$(QUIET)rm -f $(INSTALLDIR)/$(RESULT)

-include $(OCAMLMAKEFILE)

#################################################################
# This is irrelevant if OCamlMakefile is a local file
# which it is ideally not 

checkOCamlMakefile: 
	$(QUIET)if [ -s $(OCAMLMAKEFILE) ]; \
	then echo "OCamlMakefile ok"; \
	else echo "resetting local link to OCamlMakefile in distribution"; \
		make $(OCAMLMAKEFILE); \
	fi


# if there is no local copy, assume it is under OCAMLLIBPATH
# if this fails, you have to create your own local link
$(OCAMLMAKEFILE):
	$(QUIET)OCAMLMAKEFILE_DISTRIB=$(OCAMLLIBPATH)/OCamlMakefile; \
	if [ -s $$OCAMLMAKEFILE_DISTRIB ]; \
	then ln -sf $$OCAMLMAKEFILE_DISTRIB OCamlMakefile; \
	else echo "no OCamlMakefile found, create your own link"; \
	fi

