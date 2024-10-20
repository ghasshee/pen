var abi       = [
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "i",
        "type": "uint256"
      }
    ],
    "name": "fac",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "nonpayable",
    "type": "function"
  }
];
var code      = "0x608060405234801561000f575f80fd5b5060f78061001c5f395ff3fe6080604052348015600e575f80fd5b50600436106026575f3560e01c8063c766526714602a575b5f80fd5b603960353660046073565b604b565b60405190815260200160405180910390f35b5f815f03605a57506001919050565b60656035600184609d565b606d908360ad565b92915050565b5f602082840312156082575f80fd5b5035919050565b634e487b7160e01b5f52601160045260245ffd5b81810381811115606d57606d6089565b8082028115828204841417606d57606d608956fea264697066735822122009aabe92854b8bbd5ef2b747dca0e42f39c741463c64001091b7624179753ade64736f6c63430008150033";
var contract  = eth.contract(abi);
var object    = {from:eth.accounts[0], data:code, gas:1000000};
var gas       = {from:eth.accounts[0], gas:1000000};
var instance  = contract.new(object);
