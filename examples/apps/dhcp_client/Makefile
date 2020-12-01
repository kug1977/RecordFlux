generate:
	rflx generate -d generated specs/dhcp_client.rflx

build: generate
	gprbuild -Pdhcp_client

test: build
	sudo tests/run.sh

clean:
	rm -rf generated/* obj
