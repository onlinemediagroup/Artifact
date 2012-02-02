artifact_VSN=0.4.0

ifndef ROOT
	ROOT=$(shell pwd)
endif

COMMON_TEST_LIB = $(shell erl -noshell -eval 'io:format("~s~n", [code:lib_dir(common_test)]).' -s init stop)
TEST_SERVER_LIB = $(shell erl -noshell -eval 'io:format("~s~n", [code:lib_dir(test_server)]).' -s init stop)
RUN_TEST_CMD = $(COMMON_TEST_LIB)/priv/bin/run_test

all: subdirs

subdirs:
	cd src; artifact_VSN=$(artifact_VSN) ROOT=$(ROOT) make

test: test_do

test_compile: subdirs
	cd test; \
		ROOT=$(ROOT) TEST_SERVER=$(TEST_SERVER_LIB) RUN_TEST=$(COMMON_TEST_LIB) make

test_do: test_compile
	mkdir -p test/log
	${RUN_TEST_CMD} -dir . \
		-logdir test/log -cover test/artifact.coverspec \
		-I$(ROOT)/include -pa $(ROOT)/ebin

test_single: test_compile
	mkdir -p test/log
	${RUN_TEST_CMD} -suite $(SUITE) \
		-logdir test/log -cover test/artifact.coverspec \
		-I$(ROOT)/include -pa $(ROOT)/ebin

docs:
	erl -noshell -run edoc_run application "'artifact'" \
		'"."' '[{def,{vsn, "$(artifact_VSN)"}}]'

dialyze:
	cd src; make dialyze

clean:	
	rm -rf *.beam erl_crash.dump *~
	rm -rf test/log
	rm -rf doc
	cd src; ROOT=$(ROOT) make clean
	cd test; ROOT=$(ROOT) make clean

