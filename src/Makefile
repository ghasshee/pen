DEPEND += lexer.ml parser.ml
OBJS = 	misc.cmo indexedList.cmo \
		hexa.cmo typeEffect.cmo \
		syntax.cmo \
		label.cmo \
		typeEnv.cmo \
		imm.cmo \
		location.cmo \
		evm.cmo \
		ethereum.cmo \
		abi.cmo \
		locationEnv.cmo \
		storLayout.cmo \
		contract.cmo \
		type.cmo \
		codegenEnv.cmo \
		codegen.cmo \
		lexer.cmo \
		parser.cmo 
OCAMLC=ocamlfind ocamlc -linkpkg -package rpclib -package batteries -package cryptokit -package hex -package menhirLib -package rope cryptokit.cma batteries.cma hex.cma rope.cma
OCAMLDEP=ocamlfind ocamldep -package batteries -package cryptokit -package hex -package menhirLib -package rope 

all: $(DEPEND)  $(OBJS) pen

parser : parser.mli
	${OCAMLC} -c parser.mli 

include .depend 

depend:: $(DEPEND) 
	${OCAMLDEP} *.mli *.ml > .depend 


%.ml %.mli: %.mll
	@rm -f $@
	ocamllex $<
	@chmod -w $@

parser.ml parser.mli: parser.mly
	@rm -f parser.ml parser.mli 
	menhir parser.mly
	@chmod -w parser.ml parser.mli

%.cmi : %.mli 
	${OCAMLC} -c $<

%.cmo : %.ml
	${OCAMLC} -c $<

pen : $(OBJS) pen.cmo 
	echo Linking $@
	$(OCAMLC)  -o $@ $(OBJS) pen.cmo 

doc/spec.pdf: doc/spec.tex
	(cd doc; pdflatex -halt-on-error spec.tex; pdflatex -halt-on-error spec.tex)

##############

clean:
	rm -rf *.cmo *.cmi rm -rf pen _build lexer.ml parser.ml parser.mli 
	# unlink allinone.native
