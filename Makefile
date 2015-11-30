.PHONY: all compile clean start tests uats deps mock_compile mock_deps erlang rebar profile
REBAR=rebar/rebar
KERL=kerl/kerl
MOCK_DIR=uats/mock_clc
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
uats: rebar compile mock_compile
	$(REBAR) skip_deps=true ct

deps: rebar rebar.config.lock
	$(REBAR) -C rebar.config.lock get-deps
	$(REBAR) -C rebar.config.lock compile

rebar.config.lock:
	$(REBAR) get-deps
	$(REBAR) compile
	$(REBAR) lock-deps

mock_compile: rebar mock_deps
	cd $(MOCK_DIR) && \
	../../$(REBAR) compile

mock_deps: rebar
	cd $(MOCK_DIR) && \
	../../$(REBAR) get-deps && \
	../../$(REBAR) compile && \
	../../$(REBAR) lock-deps

rebar: $(REBAR)
profile:
	echo ${BASH_PROFILE}
erlang: ${ERL_INSTALL}/activate

$(KERL):
		git submodule update --init --recursive

$(REBAR):
		git submodule update --init --recursive
		@cd rebar && ./bootstrap

${ERL_INSTALL}/activate: $(KERL)
	$(KERL) update releases
	-$(KERL) build $(ERL_VERSION) $(ERL_VERSION)
	$(KERL) install $(ERL_VERSION) ${HOME}/erlangs/$(ERL_VERSION)
	fgrep -q ${ERL_ACTIVATE} ${BASH_PROFILE} || echo ${ERL_ACTIVATE} >> ${BASH_PROFILE}
	echo "NOTICE: You will need to re-source your bash profile to run erlang or compile"
