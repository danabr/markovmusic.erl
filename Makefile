.PHONY: compile dialyzer shell

compile:
	erlc -o ebin/ -Werror +debug_info src/*.erl

dialyzer.plt:
	dialyzer --build_plt --output_plt dialyzer.plt --apps compiler dialyzer erts kernel runtime_tools stdlib syntax_tools test_server tools common_test

dialyzer: compile dialyzer.plt
	dialyzer --plt dialyzer.plt ebin/*.beam

shell: compile
	erl -pa ebin/
