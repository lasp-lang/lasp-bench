#!/bin/bash

AllNodes=$1
Cookie=$2
File="./"$3
Reads=$4
Writes=$5


if [ $File = "orset_pb.config" ]; then
    Type = "set"
elif [ $File = "antidote_pb.config" ]; then
    Type = "counter"
else
    Type = "counter"
fi


sed -i '/antidote_pb_ips/d' $File 
sed -i '/concurrent/d' $File
## {operations, [{append, 1}, {read, 100}]}.
sed -i '/operations/d' $File
#PerNodeNum=40
Thread=20
BenchConfig="{antidote_pb_ips, ["
for Node in $AllNodes
do
    Node=\'$Node\',
    BenchConfig=$BenchConfig$Node
    #Thread=$((Thread+PerNodeNum))
done
BenchConfig=${BenchConfig::-1}"]}."
echo $BenchConfig
echo "$BenchConfig" >> $File
sed -i "5i {concurrent, $Thread}." $File

if [ $Type = "counter" ]; then
    sed -i "6i {operations, [{append, $Writes}, {read, $Reads}]}." $File
else
    sed -i "6i {operations, [{update, $Writes}, {read, $Reads}]}." $File
fi
