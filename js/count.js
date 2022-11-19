var abi      = [{
"inputs"  : [{
"name"    : "_counter", 
"type"    : "uint256"
}], 
"name"    : "Counter", 
"outputs" : [], 
"payable" : true,
"type"    : "constructor"
},{
"inputs"  : [],
"name"    : "inc",
"outputs" : [{
"name"    : "", 
"type"    : "void"
}],
"payable" : true,
"type"    : "function"
},{
"inputs"  : [],
"name"    : "get",
"outputs" : [{
"name"    : "", 
"type"    : "uint256"
}],
"payable" : true,
"type"    : "function"
}];
var code     = "0x63000100006040526001546100145760016001555b6100006000556100a2604051806100a2016040526100a26100338239f3fe5b630001000060405260003560e01c8063371303c0146100295780636d4ce63c1461004457506000565b60043614156000576100006000556001600254016002550060005b600436141560005761000060005560025460025560025460208060405180920160405291825290f3600063000100006040526001546100145760016001555b6100006000556100a2604051806100a2016040526100a26100338239f3fe";
var contract = eth.contract(abi);
var object   = {from:eth.accounts[0], data:code, gas:1000000};
var gas      = {from:eth.accounts[0], gas:1000000};
var instance = contract.new(object);
