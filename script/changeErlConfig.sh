#!/bin/bash

AllNodes=$1
Cookie=$2
File="./"$3
sed -i '/antidote_nodes/d' $File 
BenchConfig="{antidote_nodes, ["
for Node in $AllNodes
do
    Node=\'$Cookie@$Node\',
	BenchConfig=$BenchConfig$Node
	echo $BenchConfig
	echo $Node
done
BenchConfig=${BenchConfig::-1}"]}."
echo $BenchConfig
echo "$BenchConfig" >> $File
