ERL=erl +A 1
PS3_SHELL=-pa ${PWD}/ebin -boot start_sasl -config ${PWD}/priv/elog -pa /usr/local/lib/yaws/ebin -yaws embedded true
HOSTNAME = $(shell hostname)

all: compile 

compile:
	mkdir -p ebin
	mkdir -p test/ebin
	erl -make
	cp ./src/*.app ./ebin
clean:
	rm -rf ./ebin
	rm -rf ./test/ebin
	rm erl_crash.dump
	mkdir ./ebin
	mkdir ./test/ebin

createPS3DB:
	${CMD_NOSHELL} -name ${NODE}@${HOSTNAME} -s mnesia_setup -s init stop;

setup:
	sudo mkdir -p /var/log/error_logs
	sudo chmod 777 /var/log/error_logs
	# setup the .hosts.erlang file if needed
	./hostsSetup

ps3: 
	${ERL} -name ${NODE}@${HOSTNAME} ${PS3_SHELL}
