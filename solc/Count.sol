pragma solidity ^0.8.2;

contract Count { 
    uint256 counter;

    function inc() public {
        counter = counter + 1;
    }

    function get() public returns (uint256) { 
        return counter;
    }
}

contract Object {
    uint256 id;

    abstract constructor() public {
        id = 100;
    }
    function get() public returns (uint256) {
        return id;
    }
}


