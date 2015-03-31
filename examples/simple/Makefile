REBAR = rebar
RELX  = relx

.PHONY: all deps compile rel

all: rel

deps:
	$(REBAR) get-deps

compile: deps
	$(REBAR) compile

rel: compile
	$(RELX)
