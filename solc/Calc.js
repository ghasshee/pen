var abi       = [
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "a",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "b",
        "type": "uint256"
      }
    ],
    "name": "multiply",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "pure",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "base",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "exp",
        "type": "uint256"
      }
    ],
    "name": "power",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "pure",
    "type": "function"
  }
]
[
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "a",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "b",
        "type": "uint256"
      }
    ],
    "name": "mul",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "pure",
    "type": "function"
  },
  {
    "inputs": [
      {
        "internalType": "uint256",
        "name": "base",
        "type": "uint256"
      },
      {
        "internalType": "uint256",
        "name": "exp",
        "type": "uint256"
      }
    ],
    "name": "pow",
    "outputs": [
      {
        "internalType": "uint256",
        "name": "",
        "type": "uint256"
      }
    ],
    "stateMutability": "pure",
    "type": "function"
  }
];
var code      = "0x6080604052348015600e575f5ffd5b506101a18061001c5f395ff3fe608060405234801561000f575f5ffd5b5060043610610034575f3560e01c8063165c4a1614610038578063c04f01fc1461005d575b5f5ffd5b61004b610046366004610134565b610070565b60405190815260200160405180910390f35b61004b61006b366004610134565b6100f3565b6040516332292b2760e21b815260048101839052602481018290525f9073__$1f0c349bde6ebfcae8b9c6f081c0ea69b5$__9063c8a4ac9c906044015b602060405180830381865af41580156100c8573d5f5f3e3d5ffd5b505050506040513d601f19601f820116820180604052508101906100ec9190610154565b9392505050565b604051632e4c697f60e01b815260048101839052602481018290525f9073__$1f0c349bde6ebfcae8b9c6f081c0ea69b5$__90632e4c697f906044016100ad565b5f5f60408385031215610145575f5ffd5b50508035926020909101359150565b5f60208284031215610164575f5ffd5b505191905056fea26469706673582212208cb86d445ea9a119aa60d3747de2ec6b827261d451d51e9fdf712a565c0574fe64736f6c634300081c0033 61011e610034600b8282823980515f1a607314602857634e487b7160e01b5f525f60045260245ffd5b305f52607381538281f3fe7300000000000000000000000000000000000000003014608060405260043610603c575f3560e01c80632e4c697f146040578063c8a4ac9c146061575b5f5ffd5b604f604b36600460a7565b6070565b60405190815260200160405180910390f35b604f606c36600460a7565b6097565b5f6001815b83811015608d576084858360c6565b91506001016075565b5090505b92915050565b5f60a0828460c6565b9392505050565b5f5f6040838503121560b7575f5ffd5b50508035926020909101359150565b8082028115828204841417609157634e487b7160e01b5f52601160045260245ffdfea2646970667358221220c494c3e0aee574078afa2f9e6bd4b5fbfe616ddae6c7bed18cef7732b2e5848e64736f6c634300081c0033";
var contract  = eth.contract(abi);
var object    = {from:eth.accounts[0], data:code, gas:1000000};
var gas       = {from:eth.accounts[0], gas:1000000};
var instance  = contract.new(object);
