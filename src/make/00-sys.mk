#-*- mode:makefile-gmake; -*-
.DEFAULT_GOAL=help
VERBOSITY ?= 2 # [0, 3]

# Helper
SHUTUP = > /dev/null 2> /dev/null

init: sanity-check quicklisp

sanity-check: $(ROOT)/$(TARGET).asd $(LISP_BIN) $(QL_SETUP)
	@echo "!> Environment looks sane. I'll allow this."
