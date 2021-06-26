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
$ cd pen 
$ cd src
$ make
$ ./pen < ../examples/erc20.pen
```



## Examples

* [A payment channel](./examples/00h_payment_channel.bbo)
* [A vault](https://medium.com/@pirapira/implementing-a-vault-in-bamboo-9c08241b6755)

* [An ERC20 contract](./examples/erc20.pen)


## Test


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



