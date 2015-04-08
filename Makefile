ERLC_OPTS = +debug_info
EUNIT_OPTS = verbose,{report,{eunit_surefire,[{dir,"."}]}}

include erlang.mk
