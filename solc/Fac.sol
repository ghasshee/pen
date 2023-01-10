pragma solidity ^0.8.2;

contract Fac { 

    function fac(uint256 i) public returns (uint256) { 
        if (i == 0) {
            return 1;
        } else {
            return i * fac(i-1);
        } 
    } 

}



