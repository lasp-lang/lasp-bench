#!/bin/bash

AllNodes=`cat script/allnodes`
Mode="pb"
./script/stablizeTime.sh &
Pid=$!
./script/changePartitions.sh 2
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 2 1 $Mode 

sudo kill $Pid
