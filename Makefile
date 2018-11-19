all:
	@echo "make targets: poly, mlton, clean."

poly:
	polyc -o t-poly t.mlp

mlton:
	mlton -default-ann 'allowFFI true' -output t-mlton t.mlb

clean:
	rm -rf t-poly t-mlton
