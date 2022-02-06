var abi      = [{"type": "constructor", "inputs":[{"name": "_counter", "type": "uint256"}], "name": "Counter", "outputs":[], "payable": true},{"type":"function","name":"inc","inputs": [],"outputs": [{"name": "", "type": "void"}],"payable": true},{"type":"function","name":"get","inputs": [],"outputs": [{"name": "", "type": "uint256"}],"payable": true}];
var code     = "0x630100000060405260015461001357600180555b6100396000556100ac806040805180920190528161002e8239f3610009565b610004565b600054565b630100000060405260003560e0901c8063371303c0146100385780636f57c5e31461005357506003565b60043614156003576100396000556001600254016002550060005b600436141560035761003960005560025460025560025460208060408051809201905291825290f36000630100000060405260015461001357600180555b6100396000556100ac806040805180920190528161002e8239f3";
var contract = eth.contract(abi);
var object   = {from:eth.accounts[0], data:code, gas:1000000};
var gas      = {from:eth.accounts[0], gas:1000000};
var instance = contract.new(object);
