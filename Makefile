all:
	@echo "make targets: poly, mlton, clean."

poly: ev net-server net-server/os-constants.sml
	polyc -o t-poly t.mlp

mlton: ev net-server net-server/os-constants.sml
	mlton -default-ann 'allowFFI true' -output t-mlton t.mlb

net-server/os-constants.sml:
	cd net-server; make os-constants.sml

ev:
	git clone https://github.com/kni/sml-ev.git ev

net-server:
	git clone https://github.com/kni/sml-net-server-ev.git net-server

clean:
	rm -rf t-poly t-mlton
	test -h ev || rm -rf ev
	test -h net-server || rm -rf ev
