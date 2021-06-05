pragma solidity ^0.7.1;


contract Counter { 
    uint256 counter;
    address owner; 

    modifier _onlyOwner () {
        require(msg.sender == owner); 
        _;
    }

    function init() _onlyOwner public { 
        counter = 0;
    }

    function inc() public {
        counter++;
    }

}
