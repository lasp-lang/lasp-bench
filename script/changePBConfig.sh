#!/bin/bash

AllNodes=$1
Cookie=$2
File="./"$3
Reads=$4
Writes=$5

sed -i '/antidote_pb_ips/d' $File 
sed -i '/concurrent/d' $File
## {operations, [{append, 1}, {read, 100}]}.
sed -i '/operations/d' $File
PerNodeNum=40
Thread=0
BenchConfig="{antidote_pb_ips, ["
for Node in $AllNodes
do
    Node=\'$Node\',
    BenchConfig=$BenchConfig$Node
    #Thread=$((Thread+PerNodeNum))
    Thread=20
done
BenchConfig=${BenchConfig::-1}"]}."
echo $BenchConfig
echo "$BenchConfig" >> $File
sed -i "5i {concurrent, $Thread}." $File
sed -i "6i {operations, [{append, $Writes}, {read, $Reads}]}." $File
