#!/bin/bash


#AllNodes=`head -$1 script/allnodes`
Id=$1
File=$2
Reads=$3
cd  /root/basho_bench"$Id"/basho_bench/

echo Id: $Id
echo BenchmarkFile: $File
FileName="examples/"$File


echo "Benchmarking phase..."
./basho_bench $FileName

tar cvzf ./test-"$File"-"$Reads".tar tests

#echo "Time stabilize stopped.."
#kill $Pid
