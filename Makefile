.PHONY: all compile xref dialyzer test

all: xref

compile:
	@rebar3 compile

xref:
	@rebar3 xref

dialyzer:
	@rebar3 dialyzer

tests:
	@rebar3 eunit

test: clean xref tests dialyzer

clean:
	@rebar3 clean

distclean: clean
	@rm -rfv _build
