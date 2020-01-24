USER_SML_LIB?=${HOME}/SML

all:
	@echo "make targets: poly mlton clean"
	@echo "make depends && make USER_SML_LIB=lib poly mlton"

poly: ${USER_SML_LIB}/net-server-ev/os-constants.sml
	env USER_SML_LIB=${USER_SML_LIB} polyc -o t-poly t.mlp

mlton: ${USER_SML_LIB}/net-server-ev/os-constants.sml
	mlton -mlb-path-var 'USER_SML_LIB ${USER_SML_LIB}' -default-ann 'allowFFI true' -output t-mlton t.mlb


${USER_SML_LIB}/net-server-ev/os-constants.sml:
	cd ${USER_SML_LIB}/net-server-ev; make os-constants.sml


depends: lib lib/ev lib/net-server-ev

lib:
	mkdir lib

lib/ev:
	git clone https://github.com/kni/sml-ev.git lib/ev

lib/net-server-ev:
	git clone https://github.com/kni/sml-net-server-ev.git lib/net-server-ev
	cd lib/net-server-ev; make os-constants.sml

clean:
	rm -rf lib t-poly t-mlton
