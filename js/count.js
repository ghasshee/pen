var abi      = [{"type": "constructor", "inputs":[{"name": "_counter", "type": "uint256"}], "name": "Counter", "outputs":[], "payable": true},{"type":"function","name":"inc","inputs": [],"outputs": [{"name": "", "type": "void"}],"payable": true},{"type":"function","name":"get","inputs": [],"outputs": [{"name": "", "type": "uint256"}],"payable": true}];
var code     = "0x630100000060405260015461001357600180555b6100136000556100a3806040805180920190528161002f8239f3fe600054565b630100000060405260003560e0901c8063371303c01461002e5780636d4ce63c1461004957506000565b60043614156000576100136000556001600254016002550060005b600436141560005761001360005560025460025560025460208060408051809201905291825290f36000630100000060405260015461001357600180555b6100136000556100a3806040805180920190528161002f8239f3fe";
var contract = eth.contract(abi);
var object   = {from:eth.accounts[0], data:code, gas:1000000};
var gas      = {from:eth.accounts[0], gas:1000000};
var instance = contract.new(object);
