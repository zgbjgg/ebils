
.PHONY: test 

all: compile

compile:
	@./rebar3 compile

clean:
	@./rebar3 clean

test:
	@mkdir -p log && mkdir -p log/ct && ./rebar3 ct --readable=false
