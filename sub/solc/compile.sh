#!/bin/sh

NAME=Count
SRC=$NAME.sol
JS=$NAME.js

#JQ=$(jq '.contracts' | jq '.[]')
#JQ_ABI=$JQ | jq '.abi'
#JQ_BIN=$JQ | jq '.bin' 

OPT="--optimize --combined-json"
NL=/dev/null
#SOLC=$(solc --optimize --combined-json)
#SOLC_ABI="$SOLC abi $SRC 2> /dev/null"
#SOLC_BIN="$SOLC bin $SRC 2> /dev/null"

BIN=$(solc $OPT bin $SRC 2> $NL | jq '.contracts' | jq '.[]' | jq '.bin')
ABI=$(solc $OPT abi $SRC 2> $NL | jq '.contracts' | jq '.[]' | jq '.abi')

CODE=\"0x$(echo $BIN | sed -e 's/"//g' )\"
rm $JS
touch $JS
echo "var abi       = $ABI;"                                                >> $JS
echo "var code      = $CODE;"                                               >> $JS
echo "var contract  = eth.contract(abi);"                                   >> $JS
echo "var object    = {from:eth.accounts[0], data:code, gas:1000000};"      >> $JS
echo "var gas       = {from:eth.accounts[0], gas:1000000};"                 >> $JS
echo "var instance  = contract.new(object);"                                >> $JS

echo $BIN | sed -e 's/"//g' > $NAME.hex


