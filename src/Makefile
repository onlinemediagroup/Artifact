##
## Artifact Data Store
##
## Copyright (c) 2012 Zephyr Pellerin
## All rights reserved.
##
## Redistribution and use in source and binary forms are permitted
## provided that the above copyright notice and this paragraph are
## duplicated in all such forms and that any documentation,
## advertising materials, and other materials related to such
## distribution and use acknowledge that the software was developed
## by Zephyr Pellerin.  My name cannot be used in endorsement of any product
## derived from this software. Buy me a beer sometime if you liked this
## THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
## IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. 
##

EMULATOR=beam
EBIN = $(ROOT)/ebin
INCLUDE = $(ROOT)/include

APP_TARGET = $(EBIN)/artifact.app

ERL_COMPILE_FLAGS += \
	+warn_unused_vars +nowarn_shadow_vars +warn_unused_import \
	+debug_info

SOURCES = \
	artifact_config artifact_log artifact_hash artifact_store artifact_store_ets artifact_store_dets \
	artifact_stat artifact_version artifact_connection artifact_sync artifact_membership \
	artifact_coordinator artifact_tcp_server \
	artifact_tcp_server_supervisor artifact_tcp_server_acceptor artifact_tcp_server_monitor \
	artifact_rpc artifact_memcache artifact_supervisor artifact vclock

MODS = ${SOURCES:%=$(EBIN)/%.$(EMULATOR)} $(APP_TARGET)

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -pa $(EBIN) -W $(ERL_COMPILE_FLAGS) -I$(INCLUDE) -o$(EBIN) $<

all: $(MODS)

$(MODS): $(INCLUDE)/artifact.hrl

dialyze:
	dialyzer --succ_typings -c ${SOURCES:%=%.erl}

clean:	
	rm -rf $(EBIN)/*.$(EMULATOR) $(APP_TARGET) $(EBIN)/erl_crash.dump *~

$(APP_TARGET): artifact.app.src Makefile
	sed -e 's;%VSN%;$(artifact_VSN);' $< > $@
