pragma solidity ^0.8.2;

import "./Library.sol"; 

contract Storage { 
    uint256 storedData;

    function set(uint256 data) public {
        storedData = data;
    }

    function get() public returns (uint256) { 
        return storedData;
    }

    function hoge() public returns (uint256) {
        return Lib.plus42(0xff); 
    } 
}


