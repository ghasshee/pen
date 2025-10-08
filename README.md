# Pen 

Pen is a Compiler that compiles the High-Level Language into EVM bytecodes. 

To build the compiler, you might need Haskell compiler  and some libraries. 

Goto `pen/hs/` directory and then type `make` and push enter key. 


## Type Inference 

Pen compiler support Hindley-Milnor Type Inference. (See [TaPL (Chapter 21,22)](https://www.cis.upenn.edu/~bcpierce/tapl/).)
Type Inference is one of the goal of Statically Type Checking implementation which is used for Type Safety of the programs. 
Additionally, in Pen compiler, for the sake of pointing out which functions and variables are "Higher Order" in the source code, we need to get the information of their "types".  

## Static Higher Order Functions 

The simplest way to achieve Higher Order Functions are `Lisp's Closures`. It handles functions as values, that is we can push functions on the stack and pass as a returning value to callers. `Closure` has the data consisting of function address and function argument informattions, and its environment (context, i.e. free variables of the function). In this Closure style's Higher Order Function is dynamic. Dynamic Function Call is to be avoided. 
So, we use Higher Order Function statically. That is, we statically connect function callers and function callees. No dynamic call is used. For more detail, see `pen/hs/PG.hs` .  


## Program Analysis 


### Program Graph and Matrix representation 


We analyze programs visually and mathematically, on "semiring matrix" representation of program graphs.
Program Graph is a weighted graph whose weight is called "Action" which compose a semiring whose "addition" is "Logical OR" and whose "multiplication" is to "execute sequencially". Zero of Action is "Error" and one is "Skip" i.e. do nothing and just execute the next Action. With this semantics, we can make a semiring matrix M . Starting point is `V = (1,0,0,0, .. , 0)^T` and we could analysing the whole behavior with `star`ing the matrix M, i.e. `M^* T`. 
For more detail of the theory, you could refer to [Automata Theory](https://ems.press/books/standalone/174) and [Program Graph](https://arxiv.org/abs/2012.10086). 


![Analyzing the pen source code and showing the resulting program graph and its matrix representation.](/images/program_graph_and_matrix.png) 




### Build / Run 

```
$ cd hs 
$ make 
$ ./Pen eg/count9.pen
``` 

The above command might analyze the pen source code and show the resulting program flow graph matrix. 

![](/images/type_inference.png) 


### Singularity Analysis ~ Bifurcations and Confluences 

After Semiring Operation `*`, we could multiply sequence of Actions. And the matrix and simplified and we could go to the 
analysis of singular points. After that singularity analysis, we could get branching(bifurcation) points and conflueces. 
Most of the analysis is developped in the `pen/hs/Analysis.hs` file, and Matrix operation is defined in `Mat.hs`. 
After the analysis, we define Branching data structure in `Branch.hs` file, which is used to create a branch i.e. `if _ then _ else _` structure. 



### Kripke Semantics 

After matrices are analyzed and organized very well, we could munipulate matrices by Kripke worlds. 
The points of graphs are manipulated in order to fit various situations. 
And this Matrix Manipulation, is the formal verification itself analytically. 

Currently, Kripke world is not implemented. 




### Code Generation 

Owing to Matrix Analysis and Branching, code generation is very simple. 
We could make a virtual stack on memeory, and control from which we entered the branching points. 
If we entered the branching points by recording `A`, then we push `A` to the stack. So, if we come accross a checkpoints, 
we have to select the branch whose starting Action is `AcCheck A`. This is the semantics. of `AcRecord` and `AcCheck`, which determines function call statically. 


Under Construction. 


### Deploying a contract 

After the bytecode generation, we have to make ABI information in order to deploy the bytecode into the Blockchain.  
For the detail, see solidity documentation or, the old project. 

Currently Not Supported. 




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

![](/images/devm_solc.png)


