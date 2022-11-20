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



