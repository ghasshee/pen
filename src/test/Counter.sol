pragma solidity ^0.7.1;


contract Counter { 
    uint256 counter;

    function init() public { 
        counter = 0;
    }

    function inc() public {
        counter++;
    }

}
