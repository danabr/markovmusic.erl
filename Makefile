.PHONY: shell

ebin/*.beam:
	erlc -o ebin/ src/*.erl

shell:
	erl -pa ebin/
