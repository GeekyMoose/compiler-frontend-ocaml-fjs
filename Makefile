# ------------------------------------------------------------------------------
TARGET = program-launcher
SOURCE = lexer.mll parser.mly main.ml



# ------------------------------------------------------------------------------

$(TARGET) : parser.cmo lexer.cmo main.cmo
	ocamlc -o $(TARGET) parser.cmo lexer.cmo main.cmo


parser.mli: parser.mly
	ocamlyacc parser.mly
parser.ml: parser.mli
	ocamlc -c parser.mli
parser.cmo: parser.ml
	ocamlc -c parser.ml


lexer.ml: lexer.mll
	ocamllex lexer.mll
lexer.cmo: lexer.ml
	ocamlc -c lexer.ml


main.cmo: main.ml
	ocamlc -c main.ml






# ------------------------------------------------------------------------------
.PHONY: clean
clean:
	-rm *.cmo *.cmi *.mli
	-rm lexer.ml
	-rm parser.ml
