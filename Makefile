TAR=tar

CFLAGS += -principal -color always -safe-string
PACKAGES=cmdliner containers
NAME=kittylang
EXTENSION=native

all:
	eval `opam config env`
	ocamlbuild -use-ocamlfind \
		-pkgs '$(PACKAGES)' \
		-use-menhir \
		-cflags '$(CFLAGS)' $(NAME).$(EXTENSION)

deps:
	opam update
	opam install $(PACKAGES)

# For giving out to student prefer giving an url to the git project.
archiveLastest:
	git archive -o cmicrojsML-${shell git rev-parse HEAD}.zip HEAD

clean:
	ocamlbuild -clean

