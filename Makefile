.PHONY: shell compile

compile:
	erlc -o ebin/ -Werror src/*.erl

shell: compile
	erl -pa ebin/
