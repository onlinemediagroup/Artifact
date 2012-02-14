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
	artifact_tcp_server_sup artifact_tcp_server_acceptor artifact_tcp_server_monitor \
	artifact_rpc artifact_memcache artifact_sup artifact vclock

MODS = ${SOURCES:%=$(EBIN)/%.$(EMULATOR)} $(APP_TARGET)

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -pa $(EBIN) -W $(ERL_COMPILE_FLAGS) -I$(INCLUDE) -o$(EBIN) $<

all: $(MODS)

$(MODS): $(INCLUDE)/artifact.hrl

clean:	
	rm -rf $(EBIN)/*.$(EMULATOR) $(APP_TARGET) $(EBIN)/erl_crash.dump *~

$(APP_TARGET): artifact.app.src Makefile
	sed -e 's;%VSN%;$(artifact_VSN);' $< > $@
