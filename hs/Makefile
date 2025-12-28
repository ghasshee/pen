

.PHONY: clean pappy

all: pappy
	ghc Pen

nix: 
	nix-shell 

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
	cat   eg/count.pen 
	./Pen eg/count.pen 
	cat   eg/count2.pen 
	./Pen eg/count2.pen 
	cat   eg/count3.pen 
	./Pen eg/count3.pen 
	cat   eg/count4.pen 
	./Pen eg/count4.pen 
	cat   eg/count5.pen 
	./Pen eg/count5.pen 
	cat   eg/count6.pen 
	./Pen eg/count6.pen 
	cat   eg/count7.pen 
	./Pen eg/count7.pen 
	cat   eg/count8.pen 
	./Pen eg/count8.pen 
	cat   eg/count9.pen 
	./Pen eg/count9.pen 
	cat   eg/count10.pen 
	./Pen eg/count10.pen 
	cat   eg/count11.pen 
	./Pen eg/count11.pen 
	cat   eg/count12.pen 
	./Pen eg/count12.pen 
	cat   eg/data.pen
	./Pen eg/data.pen

# 
# .DEFAULT_GOAL := all
# .PHONY: ${MAKECMDGOALS}
# $(filter-out all,${MAKECMDGOALS}) all: .forward-all ; @:
# .forward-all:
#         ${MAKE} -C build ${MAKECMDGOALS}
# # Never try to remake this makefile.
# ${MAKEFILE_LIST}: ;
# .SUFFIXES:



