# Pen 

Pen is a high-level language compiled into EVM bytecodes. 


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






# Decompiler 

Pen has another tool which decompiles EVM-bytecode into [Guarded Command](https://en.wikipedia.org/wiki/Guarded_Command_Language)-like Language (GCLL). 

The decompiler is written in Haskell, which you might be familiar with! 

```
$ cd asm
$ make
$ ../src/pen < ../examples/count.pen | ./Devm
``` 

