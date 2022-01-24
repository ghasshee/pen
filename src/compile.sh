#!/bin/bash 

SRC=`basename $1` 
SRC=`echo $SRC | sed -e 's/\.pen//g' `
OUT="../js/$SRC.js"

make
if  [ $SRC=="" ] 
then 
    echo ""
    echo "================================================"
    echo "USAGE : specify a pen source file to comiple. " 
    echo "================================================"
    echo ""
    echo "e.g. type; "
    echo " +---------------------------------------------+"
    echo "| $ ./compile.sh ../examples/counter.pen        |"
    echo " +---------------------------------------------+"
    echo ""
    echo "then call it on geth with starting a miner; "
    echo " +---------------------------------------------+"
    echo "| $ ../eth/geth.sh                              |"
    echo "| geth> miner.start(1);                         |"
    echo "| geth> loadScript(../js/counter.js);           |"
    echo " +---------------------------------------------+"
    exit 1; 
fi
BIN=`./pen < $1`
ABI=`./pen --abi < $1`

echo "======================================================================"
echo "CONTRACT $SRC"
echo "======================================================================"
echo $ABI
echo "======================================================================"
echo $BIN 

echo "var abi      = $ABI;"                                                  > $OUT
echo "var code     = \"$BIN\";"                                             >> $OUT
echo "var contract = eth.contract(abi);"                                    >> $OUT
echo "var object   = {from:eth.accounts[0], data:code, gas:1000000};"       >> $OUT
echo "var gas      = {from:eth.accounts[0], gas:1000000};"                  >> $OUT
echo "var instance = contract.new(object);"                                 >> $OUT

echo "======================================================================"
echo "JS file generated at: pen/js/$SRC.js"
echo "code"
echo "abi" 
echo "contract"
echo "object"
echo "instance"
echo "gas"
echo "   are defined and type below code to make an instance on geth console" 
echo "======================================================================"
echo "var receipt  = eth.getTransactionReceipt(instance.transactionHash);"  
echo "var address  = receipt.contractAddress;"                              
echo "var ${SRC}   = contract.at(address);"                                 
