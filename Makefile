.PHONY: all compile clean start tests deps erlang rebar profile
REBAR=rebar/rebar
KERL=kerl/kerl
ERL_VERSION=18.0
ERL_INSTALL=${HOME}/erlangs/${ERL_VERSION}
ERL_ACTIVATE="source ${ERL_INSTALL}/activate"
BASH_PROFILE=$(if ifeq($(findstring, "darwin", ${OSTYPE}), "darwin"), ${HOME}/.bash_profile, ${HOME}/.bashrc )
HOST=$(shell hostname)

all: compile
compile: rebar
	$(REBAR) skip_deps=true compile
clean: rebar
	$(REBAR) skip_deps=true clean
start: compile
	erl -pa ebin deps/*/ebin -eval "application:ensure_all_started(clc_v2, permanent)."
tests: rebar
	$(REBAR) skip_deps=true eunit

deps: rebar rebar.config.lock ${ERL_INSTALL}/activate
	$(REBAR) -C rebar.config.lock get-deps
	$(REBAR) -C rebar.config.lock compile

rebar.config.lock:
	$(REBAR) get-deps
	$(REBAR) compile
	$(REBAR) lock-deps

rebar: $(REBAR)
profile:
	echo ${BASH_PROFILE}

$(KERL):
		git submodule update --init --recursive

$(REBAR):
		git submodule update --init --recursive
		@cd rebar && ./bootstrap

${ERL_INSTALL}/activate: $(KERL)
	-$(KERL) build $(ERL_VERSION) $(ERL_VERSION)
	$(KERL) install $(ERL_VERSION) ${HOME}/erlangs/$(ERL_VERSION)
	fgrep -q ${ERL_ACTIVATE} ${BASH_PROFILE} || echo ${ERL_ACTIVATE} >> ${BASH_PROFILE}
	echo "NOTICE: You will need to re-source your bash profile to run erlang or compile"
