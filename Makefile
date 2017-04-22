coverage-report: $(shell ls -1rt `find logs -type f -name \*.coverdata 2>/dev/null` | tail -n1)
	@ erl -noshell -pa ebin deps/*/ebin -eval 'ecoveralls:travis_ci("$?"), init:stop()'

.PHONY: coverage-report
