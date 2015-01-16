#!/bin/bash

if [ $# -ne 0 ]
then
	Cookie=$1	
	AllNodes=$2
else
	Cookie="antidote"
	AllNodes="10.20.0.78 10.20.0.79"
fi
./script/restartNodes.sh "$AllNodes"
echo "Finished restarting"
sleep 3
#sudo escript ./script/connectDCs.script $Cookie $AllNodes 
