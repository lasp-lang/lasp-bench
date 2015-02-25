#!/bin/bash

if [ $# -eq 0 ]
then
	AllNodes="10.20.0.78 10.20.0.79"
else
	AllNodes=$1	
fi

echo $AllNodes

./script/stopNodes.sh "$AllNodes"
./script/startNodes.sh "$AllNodes"
