OCAMLMAKEFILE = ../OCamlMakefile

USE_CAMLP4 = yes

INCDIRS = +camlp4

SOURCES = token.mli token.ml \
	lexer.mli lexer.ml \
	program_options.ml \
	return.ml \
	format_builder.mli format_builder.ml \
	formatter.mli formatter.ml \
	erlint.ml

RESULT  = erlint

#PACKS = str core_extended lwt lwt.unix lwt.syntax seq ocaml_bio

-include $(OCAMLMAKEFILE)
