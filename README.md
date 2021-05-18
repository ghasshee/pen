# pen 

pen is a high-level language compiled into EVM bytecodes. 


## Build 

pen is written by OCaml Programming Language, 
whose package manager is OPAM. 
So you need to install opam first of all.  
And after installation, switch to version `4.04.2` as; 

```
$ opam switch 4.04.2
```
and then install all dependencies; 

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
$ ./pen < ../examples/erc20.pen
```


## The Original Model Bamboo Motivations 

See [manifest](doc/manifest.md) for the motivation, or [tutorial](doc/tutorial.md) if you want to deploy something first, or [semantics](doc/semantics.md) if you need something resembling a definition.

## Examples

* [A payment channel](./examples/00h_payment_channel.bbo)
* [A vault](https://medium.com/@pirapira/implementing-a-vault-in-bamboo-9c08241b6755)

* [An ERC20 contract](./examples/erc20.pen)


## Test

* install [opam](http://opam.ocaml.org/doc/Install.html) with OCaml 4.04.1
* opam install bamboo

```
./pen < ../examples/erc20.pen
```
produces a bytecode. Do not trust the output as the compiler still contains bugs probably.

```
./pen --abi < ../examples/erc20.pen
```

prints ABI.

```
[{"type": "constructor", "inputs":[{"name": "_beneficiary", "type": "address"},{"name": "_bidding_time", "type": "uint256"},{"name": "_highest_bid", "type": "uint256"}], "name": "auction", "outputs":[], "payable": true},{"type":"fallback","inputs": [],"outputs": [],"payable": true}]
```



