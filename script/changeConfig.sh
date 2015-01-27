#!/bin/bash

AllNodes=$1
Cookie=$2
File="./examples/antidote.config"
sudo sed -i '/antidote_nodes/d' $File 
BenchConfig="{antidote_nodes, ["
for Node in $AllNodes
do
        Node=\'$Cookie@$Node\',
	BenchConfig=$BenchConfig$Node
done
BenchConfig=${BenchConfig::-1}"]}."
echo $BenchConfig
sudo echo "$BenchConfig" >> $File
