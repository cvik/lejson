ERLC_OPTS = +debug_info
EUNIT_OPTS = verbose,{report,{eunit_surefire,[{dir,"test"}]}}

include erlang.mk

clean ::
	@rm test/TEST-*
