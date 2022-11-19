# pen 

pen is a high-level language compiled into EVM bytecodes. 


## Build 

pen is written by OCaml Programming Language, 
whose package manager is OPAM. 
So you need to install opam first of all.  

After opam init, install all dependencies with; 

* opam install batteries
* opam install cryptokit (>= 1.12)
* opam install hex
* opam install menhirLib
* opam install rope
* opam install rpclib

Then, after installation, type the above and install the all dependencies.

```
$ git pull https://www.github.com/ghasshee/pen
$ cd pen/src
$ ./compile.sh ../examples/count.pen
```



We can also use the `pen` command like; 

```
$ ./pen       < ../examples/count.pen
$ ./pen --abi < ../examples/count.pen   ## shows ABI
$ ./pen --asm < ../examples/count.pen   ## shows Assembler Code  

```



# Decompiler 

pen has another tool which decompiles EVM-bytecode into [Guarded Command](https://en.wikipedia.org/wiki/Guarded_Command_Language)-like Language . 

```
$ cd asm
$ make
$ ../src/pen < ../examples/count.pen | ./Devm
``` 

