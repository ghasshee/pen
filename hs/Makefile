


.PHONY: clean pappy

pappy: 
	#cd Pappy && make 
	#ln -s Pappy/Pos.hs ./
	#ln -s Pappy/Parse.hs ./
	#Pappy/pappy Parser.pappy
	
	alex Lexer.x 	# generates Lexer.hs 
	happy Parser.y 
	
	ghc Pen


clean: 
	#rm Pappy/*.hi Pappy/*.o
	rm *.hi *.o Parser.hs
	rm Lexer.hs 
	rm Pen
	#rm -rf Pos.hs Parse.hs
	#unlink Pos.hs
	#unlink Parse.hs


# 
# .DEFAULT_GOAL := all
# .PHONY: ${MAKECMDGOALS}
# $(filter-out all,${MAKECMDGOALS}) all: .forward-all ; @:
# .forward-all:
#         ${MAKE} -C build ${MAKECMDGOALS}
# # Never try to remake this makefile.
# ${MAKEFILE_LIST}: ;
# .SUFFIXES:
