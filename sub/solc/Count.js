var abi       = [
  {
    "inputs": [],
    "name": "get",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  },
  {
    "inputs": [],
    "name": "inc",
    "outputs": [],
    "stateMutability": "nonpayable",
    "type": "function"
  }
];
var code      = "0x608060405234801561001057600080fd5b5060be8061001f6000396000f3fe6080604052348015600f57600080fd5b506004361060325760003560e01c8063371303c01460375780636d4ce63c14603f575b600080fd5b603d6054565b005b60005460405190815260200160405180910390f35b60005460609060016065565b600055565b60008219821115608357634e487b7160e01b81526011600452602481fd5b50019056fea2646970667358221220ebbf9e6530745c2246c5ec9df0f41280941ab72d344700ccf6bff96f2df6469c64736f6c63430008020033";
var contract  = eth.contract(abi);
var object    = {from:eth.accounts[0], data:code, gas:1000000};
var gas       = {from:eth.accounts[0], gas:1000000};
var instance  = contract.new(object);
