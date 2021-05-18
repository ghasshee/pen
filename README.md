# pen 

pen is a dialect of bamboo.
Owing from ancient mathematician, haskell curry, inria, ocaml ... to bamboo@pirapira, pen is to be developed or used by everyone.

First aim is to interpret bamboo. This will take a while.

## Build 

pen is written by OCaml Programming Language, 
whose package manager is OPAM. 
So you need to install opam first of all.  

* opam install batteries
* opam install cryptokit (>= 1.12)
* opam install hex
* opam install menhirLib
* opam install rope
* opam install rpclib

Then, after installation, type the above and install the all dependencies.

```
$ git pull https://www.github.com/ghasshee/pen
$ cd pen 
$ cd src
$ make
$ ./pen < ../examples/006auction_first_case.bbo
```

# --- BAMBOO README ---  

## Bamboo: a language for morphing smart contracts

See [manifest](doc/manifest.md) for the motivation, or [tutorial](doc/tutorial.md) if you want to deploy something first, or [semantics](doc/semantics.md) if you need something resembling a definition.

## Example Bamboo Code

* [A payment channel](./examples/00h_payment_channel.bbo)
* [An ERC20 contract](./examples/01b_erc20better.bbo)
* [A vault](https://medium.com/@pirapira/implementing-a-vault-in-bamboo-9c08241b6755)

## Test

* install [opam](http://opam.ocaml.org/doc/Install.html) with OCaml 4.04.1
* opam install bamboo

```
./pen < ../examples/006auction_first_case.bbo
```
produces a bytecode. Do not trust the output as the compiler still contains bugs probably.

```
bamboo --abi < src/parse/examples/006auction_first_case.bbo
```
prints ABI.
```
[{"type": "constructor", "inputs":[{"name": "_beneficiary", "type": "address"},{"name": "_bidding_time", "type": "uint256"},{"name": "_highest_bid", "type": "uint256"}], "name": "auction", "outputs":[], "payable": true},{"type":"fallback","inputs": [],"outputs": [],"payable": true}]
```



## How to Contribute to Bamboo

* notice problems and point them out. [create issues](https://github.com/pirapira/bamboo/issues/new).
* test the bytecode like [this](doc/tutorial.md), but using other examples.  You might find bugs in the compiler.
* write new Bamboo code and test the compiler.
* join the [Gitter channel](https://gitter.im/bbo-dev/Lobby).
* spread a rumor to your friends who are into programming languages.


