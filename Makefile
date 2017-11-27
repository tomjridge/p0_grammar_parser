all:
	ocamlfind ocamlc -package p0,re,re.emacs -linkpkg p0_grammar_parser.ml
