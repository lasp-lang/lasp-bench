#!/bin/bash

AllNodes=$1
Cookie=$2	
./script/startNodes.sh "$AllNodes"
echo "Finished restarting"
if [ $3 -eq 1 ]; then
	echo "Coonect DCs"
	sudo escript ./script/connectDCs.script $Cookie $AllNodes 
else
	echo "Not connecting DCs"
fi
