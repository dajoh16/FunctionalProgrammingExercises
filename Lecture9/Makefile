lec09:
	ocamlbuild -use-ocamlfind -package qcheck exercise.byte
	ocamlbuild -use-ocamlfind -package qcheck exercise.cma

stmtlang:
	ocamlbuild -use-ocamlfind -package qcheck stmtlang.byte
	ocamlbuild -use-ocamlfind -package qcheck stmtlang.cma

clean:
	rm -f tmp.bc output.txt
	ocamlbuild -clean

