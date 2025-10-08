nix: 
	nix-shell 


.PHONY: clean pappy

all: pappy
	ghc Pen


pappy: 
	#cd Pappy && make 
	#ln -s Pappy/Pos.hs ./
	#ln -s Pappy/Parse.hs ./
	#Pappy/pappy Parser.pappy
	
	alex Lexer.x 	# generates Lexer.hs 
	happy Parser.y --ghc  


clean:   
	#rm Pappy/*.hi Pappy/*.o
	rm *.hi *.o Parser.hs
	rm Lexer.hs 
	rm Pen
	#rm -rf Pos.hs Parse.hs
	#unlink Pos.hs
	#unlink Parse.hs


test:  all
	./Pen eg/count.pen 
	./Pen eg/count2.pen 
	./Pen eg/count3.pen 
	./Pen eg/count4.pen 
	./Pen eg/count5.pen 
	./Pen eg/count6.pen 
	./Pen eg/count7.pen 
	./Pen eg/count8.pen 
	./Pen eg/count9.pen 
	./Pen eg/count10.pen 
	./Pen eg/count11.pen 
	./Pen eg/count12.pen 

# 
# .DEFAULT_GOAL := all
# .PHONY: ${MAKECMDGOALS}
# $(filter-out all,${MAKECMDGOALS}) all: .forward-all ; @:
# .forward-all:
#         ${MAKE} -C build ${MAKECMDGOALS}
# # Never try to remake this makefile.
# ${MAKEFILE_LIST}: ;
# .SUFFIXES:



