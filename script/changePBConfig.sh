#!/bin/bash

AllNodes=$1
Cookie=$2
File="./"$3
sudo sed -i '/antidote_pb_ips/d' $File 
sudo sed -i '/concurrent/d' $File
PerNodeNum=20
Thread=0
BenchConfig="{antidote_pb_ips, ["
for Node in $AllNodes
do
    Node=\'$Node\',
    BenchConfig=$BenchConfig$Node
    Thread=$((Thread+PerNodeNum))
done
BenchConfig=${BenchConfig::-1}"]}."
echo $BenchConfig
sudo echo "$BenchConfig" >> $File
sudo sed -i "5i {concurrent, $Thread}." $File
