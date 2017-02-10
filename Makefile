# ------------------------------------------------------------------------------
OCAMLC = ocamlc
OCAMLY = ocamlyacc
OCAMLL = ocamllex
RM = rm
TARGET = program-launcher
OBJS = exp.cmo parser.cmo lexer.cmo main.cmo 


# ------------------------------------------------------------------------------
$(TARGET) : $(OBJS)
	$(OCAMLC) -o $(TARGET) $^

%.mli: %.mly
	$(OCAMLY) $<
%.ml: %.mli
	$(OCAMLC) -c $<

%.ml: %.mll
	$(OCAMLL) $<
%.cmo: %.ml
	$(OCAMLC) -c $<


# ------------------------------------------------------------------------------
.PHONY: clean
clean:
	$(RM) -f *.cmo *.cmi *.mli
	$(RM) -f *.out
	$(RM) -f lexer.ml
	$(RM) -f parser.ml
	$(RM) -f $(OBJS)
	$(RM) -f $(TARGET)
