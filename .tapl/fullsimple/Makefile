# 
# Rules for compiling and linking the typechecker/evaluator
#
# Type
#   make         to rebuild the executable file f
#   make windows to rebuild the executable file f.exe
#   make test    to rebuild the executable and run it on input file test.f
#   make clean   to remove all intermediate and temporary files
#   make depend  to rebuild the intermodule dependency graph that is used
#                  by make to determine which order to schedule 
#	           compilations.  You should not need to do this unless
#                  you add new modules or new dependencies between 
#                  existing modules.  (The graph is stored in the file
#                  .depend)

# These are the object files needed to rebuild the main executable file
OBJS = support.cmo syntax.cmo lexer.cmo type.cmo eval.cmo parser.cmo 

# Files that need to be generated from other files
DEPEND += lexer.ml parser.ml   

# When "make" is invoked with no arguments, we build an executable 
# typechecker, after building everything that it depends on
all: $(DEPEND) $(OBJS) f g 

# Include an automatically generated list of dependencies between source files
include .depend

# Rebuild intermodule dependencies
depend:: $(DEPEND) 
	ocamldep $(INCLUDE) *.mli *.ml > .depend

# lexer
%.ml %.mli: %.mll
	@rm -f $@
	ocamllex $<
	@chmod -w $@


# parser
parser.ml parser.mli: parser.mly
	@rm -f parser.ml parser.mli
	ocamlyacc -v parser.mly
	@chmod -w parser.ml parser.mli

# Compile an ML module interface
%.cmi : %.mli
	ocamlc -c $< 			# $< denotes the required file, here %.mli 

# Compile an ML module implementation
%.cmo : %.ml
	ocamlc -c $<

# Build an executable typechecker
f: $(OBJS) main.cmo 
	@echo Linking $@  					# Here, $@ denotes f 
	ocamlc -o $@ $(OBJS) main.cmo # $(COMMONOBJS) is null.

# Build an interpreter 
g: $(OBJS) imain.cmo
	@echo Linking $@
	ocamlc -o $@ $(OBJS) imain.cmo



#######################
#######################
#######################
# Build and test
test: all
	./f test.f

# Clean up the directory
clean::
	rm -rf lexer.ml parser.ml parser.mli *.o *.cmo *.cmi parser.output \
	   f g TAGS *~ *.bak

