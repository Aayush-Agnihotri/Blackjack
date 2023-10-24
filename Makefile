build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop lib

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

blackjack:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f blackjack.zip
	zip -r blackjack.zip . -x@exclude.lst

clean:
	dune clean
	rm -f blackjack.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh