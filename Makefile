
.PHONY: test 

all: compile

compile:
	@./rebar compile

clean:
	@./rebar clean

test:
	@mkdir -p log && mkdir -p log/ct && ./rebar ct
