#!/bin/bash

if [ $# -eq 0 ]
then
	AllNodes=`cat script/allnodes`
else
	AllNodes=$1	
fi

echo $AllNodes

./script/stopNodes.sh "$AllNodes"
./script/startNodes.sh "$AllNodes"
