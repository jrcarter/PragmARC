all:

compile:
	gprbuild -P pragmarc.gpr

test:
	${MALKE} -C tests


install:

clean:
	git clen . -xdf
