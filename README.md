# Pen 

Pen is a Compiler that compiles the High-Level Language into EVM bytecodes. 


## Build 

Pen is written by [OCaml](https://ocaml.org/), 
whose package manager is [OPAM](https://opam.ocaml.org/), which you need to install first of all.  

After `opam init`, 
install all dependencies; 

```
$ opam install batteries
$ opam install cryptokit  ## (>= 1.12)
$ opam install hex
$ opam install menhirLib
$ opam install rope
$ opam install rpclib
```

Then, pull this repo from github and compile the comipler !

```
$ git pull https://www.github.com/ghasshee/pen
$ cd pen/src
$ ./compile.sh ../examples/count.pen
```

## Deploy a contract on Geth

If you successfully compile it with the above command, 
    you will find the following instruction on stdIO . 

As mentioned there, please type; 

```
$ ../eth/geth.sh 
```

then enter the password which is Empty String "",  
and follow the rest of the instruction. 


Also, 
We can use the `pen` command like; 

```
$ ./pen       < ../examples/count.pen
$ ./pen --abi < ../examples/count.pen   ## shows ABI
$ ./pen --asm < ../examples/count.pen   ## shows Assembler Code  

```






# DEVM

Pen has another tool which decompiles EVM-bytecode into [Guarded Command](https://en.wikipedia.org/wiki/Guarded_Command_Language)-like Language (GCLL). 
The decompiler is written in [Haskell](https://www.haskell.org/), which is purely functional programming language.  

Currently we have 3 analysis tools: DASM, ASM, and, DEVM . 

```
$ cd devm 
$ make
$ ../src/pen < ../examples/count.pen | ./DEVM           ## Decompiler 
$ ../src/pen < ../examples/count.pen | ./DASM           ## Disassembler
$ ../src/pen < ../examples/count.pen | ./DASM  | ./ASM  ## Assembler
``` 

