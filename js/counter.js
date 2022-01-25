var abi      = [{"type": "constructor", "inputs":[{"name": "_counter", "type": "uint256"}], "name": "Counter", "outputs":[], "payable": true},{"type":"function","name":"init","inputs": [],"outputs": [{"name": "", "type": "void"}],"payable": true},{"type":"function","name":"inc","inputs": [],"outputs": [{"name": "", "type": "void"}],"payable": true},{"type":"function","name":"reset","inputs": [],"outputs": [{"name": "", "type": "void"}],"payable": true},{"type":"function","name":"get","inputs": [],"outputs": [{"name": "", "type": "uint256"}],"payable": true}];
var code     = "0x60606040527f0000000000000000000000000000000000000000000000000000000000000020806040805180920190528180380382397f00000000000000000000000000000000000000000000000000000000000005ef3814156003577f00000000000000000000000000000000000000000000000000000000000000025b821563000000dd57815181556020909203917f000000000000000000000000000000000000000000000000000000000000000101907f00000000000000000000000000000000000000000000000000000000000000200190630000007e565b50505060015463000000ee57600180555b7f00000000000000000000000000000000000000000000000000000000000000046000557f000000000000000000000000000000000000000000000000000000000000046c80604080518092019052817f00000000000000000000000000000000000000000000000000000000000001638239f3600054565b60606040527f00000000000000000000000000000000000000000000000000000000000000003560e060020a90048063e1c7392a147f00000000000000000000000000000000000000000000000000000000000000db578063462a2a19147f0000000000000000000000000000000000000000000000000000000000000151578063ce6745ec147f00000000000000000000000000000000000000000000000000000000000001ea5780634297d5c6147f000000000000000000000000000000000000000000000000000000000000026057506003565b63000000043614156003577f00000000000000000000000000000000000000000000000000000000000000046000557f00000000000000000000000000000000000000000000000000000000000000007f0000000000000000000000000000000000000000000000000000000000000002550060005b63000000043614156003577f00000000000000000000000000000000000000000000000000000000000000046000557f00000000000000000000000000000000000000000000000000000000000000017f000000000000000000000000000000000000000000000000000000000000000254017f0000000000000000000000000000000000000000000000000000000000000002550060005b63000000043614156003577f00000000000000000000000000000000000000000000000000000000000000046000557f00000000000000000000000000000000000000000000000000000000000000017f0000000000000000000000000000000000000000000000000000000000000002550060005b63000000043614156003577f00000000000000000000000000000000000000000000000000000000000000046000557f0000000000000000000000000000000000000000000000000000000000000002547f0000000000000000000000000000000000000000000000000000000000000002557f00000000000000000000000000000000000000000000000000000000000000025460208060408051809201905291825290f3600060606040527f0000000000000000000000000000000000000000000000000000000000000020806040805180920190528180380382397f00000000000000000000000000000000000000000000000000000000000005ef3814156003577f00000000000000000000000000000000000000000000000000000000000000025b821563000000dd57815181556020909203917f000000000000000000000000000000000000000000000000000000000000000101907f00000000000000000000000000000000000000000000000000000000000000200190630000007e565b50505060015463000000ee57600180555b7f00000000000000000000000000000000000000000000000000000000000000046000557f000000000000000000000000000000000000000000000000000000000000046c80604080518092019052817f00000000000000000000000000000000000000000000000000000000000001638239f3";
var contract = eth.contract(abi);
var object   = {from:eth.accounts[0], data:code, gas:1000000};
var gas      = {from:eth.accounts[0], gas:1000000};
var instance = contract.new(object);