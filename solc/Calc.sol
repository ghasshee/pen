pragma solidity ^0.8.20;

library MathLib {
    // external にすることで、ライブラリコントラクトとして呼び出し可能になる
    function mul(uint256 a, uint256 b) external pure returns (uint256) {
        return a * b;
    }

    function pow(uint256 base, uint256 exp) external pure returns (uint256) {
        uint256 result = 1;
        for (uint256 i = 0; i < exp; i++) {
            result *= base;
        }
        return result;
    }
}

contract Calculator {
    // ライブラリ関数を呼ぶと、内部的には DELEGATECALL が使われる
    function multiply(uint256 a, uint256 b) external pure returns (uint256) {
        return MathLib.mul(a, b);
    }

    function power(uint256 base, uint256 exp) external pure returns (uint256) {
        return MathLib.pow(base, exp);
    }
}
