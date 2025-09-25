# Pen 

Pen is a Compiler that compiles the High-Level Language into EVM bytecodes. 





## Build 

You might need Haskell compiler  and some libraries. 

Goto `pen/hs/` directory and then type `make` and push enter key. 


## Program Analysis 


### News : Type Inference & Higher Order Function is Now supported in pen compiler !!


```
$ cd hs 
$ make 
$ ./Pen count9.pen
``` 

The above command might analyze the pen source code and show the resulting program flow graph matrix. 


![](/type_inference.png) 




### Program Graph and Matrix representation 

The above command might analyze the pen source code and show the resulting program graph and its matrix representation. 

![](/program_graph_and_matrix.png) 



## Deploying a contract 

Currently Not Supported 




## History 

### Old Project (deprecated bamboo modification and its History) 

Original motivation of Pen compiler starts with modification of bamboo. 
It halved unreadable and unusable source code of bamboo and had new features that bamboo did not have. 
However, finally, it turned out that the original design was too untidy to build a nice compiler. 
 
Still the codes, design, and the architecture of bamboo seemed terribly bad towards formal language, 
it had some philosophy, which we might appreciate. 

1. contract oriented : contract oriented means as follows. 
Every time a contract is called, the contract returns another new contract or the same contract. 
Which means contract can evolve itself. Bamboo architecture embed the constructor code in a deployed contract.
This is @pirapira(bamboo creator) 's innovative idea, which, I do not know, will be useful really. 

2. Type is embeded into Term : this feature seems bad. Because, 
it makes us difficult to separate AST of terms and AST of types. 
But, However, this design enable avoiding full type system, which seems expensive for designing. 


#### Build 

Old Pen is written by [OCaml](https://ocaml.org/), 
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
$ cd pen/old
$ ./compile.sh ../examples/count.pen
```

#### Deploy a contract on Geth

If you successfully compile it with the above command, 
    you will find the following instruction which is displayed on STD I/O . 

As mentioned there, please type; 

```
$ ../eth/geth.sh 
```

then enter the password which is Empty String "",  
and follow the rest of the instruction. 


Also, 
We can use the `pen` command, like; 

```
$ ./pen       < ../examples/count.pen
$ ./pen --abi < ../examples/count.pen   ## shows ABI
$ ./pen --asm < ../examples/count.pen   ## shows Assembler Code  

```








### New Project (Written from Sctratch in Haskell) 


Deploying the heavily-modified old project's compiling EVM bytecode into blockchain, we should have checked the specification and run some tests, and 
then we must have modified the compiler, but the old architecture turned out too untidy to modify, finally. 

We started new compiler from scratch in Haskell. Haskell is a purely functional programming language. 
Haskell is more expensive than OCaml in writing codes. You know, OCaml is a nice language because Coq Proof Assistant and its transpiler (Extraction Mechanism) gives us more formality than Haskell's code. However, Haskell gives us a lot of useful or visibly nice interpreter. 
In developping, we use a lot Haskell's "Show Instance" for visibility of analysis, which gives us formality directly and intuitively. 





# DEVM

Pen has another tool which decompiles EVM-bytecode into [Guarded Command](https://en.wikipedia.org/wiki/Guarded_Command_Language)-like Language (GCLL). 
The decompiler is written in [Haskell](https://www.haskell.org/), which is purely functional programming language.  

Currently we have 3 analysis tools: DASM, ASM, and, DEVM . 

```
$ cd devm 
$ make
$ ../old/pen < ../examples/count.pen | ./DEVM           ## Decompiler 
$ ../old/pen < ../examples/count.pen | ./DASM           ## Disassembler
$ ../old/pen < ../examples/count.pen | ./DASM  | ./ASM  ## Assembler
``` 

Of course `DEVM` can decompile [Solidity](https://github.com/ethereum/solidity/) EVM-bytecode . 
```
$ ./DEVM < ../solc/Count.hex 
```



