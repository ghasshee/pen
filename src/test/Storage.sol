pragma solidity ^0.7.1;

contract Storage {
    uint256 storedData;

    function set(uint256 data) public {
        storedData = data; 
    }

    function get() public view returns (uint256) {
        return storedData;
    }
}

contract Counter { 
    uint256 counter;

    function init() public { 
        counter = 0;
    }

    function inc() public {
        counter++;
    }

    function reset() public {
        counter = 0;
    }
}
