REBAR=`which rebar || ./rebar`

all: deps compile

deps:
	@$(REBAR) get-deps
compile:
	@$(REBAR) compile
test:
	@$(REBAR) skip_deps=true eunit
clean:
	@$(REBAR) clean
check:
	dialyzer --src -r src -I ebin \
	-Wunmatched_returns \
	-Werror_handling \
	-Wrace_conditions \
	-Wbehaviours \
	-Wunderspecs