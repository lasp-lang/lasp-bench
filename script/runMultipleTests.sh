#!/bin/bash

AllNodes=`cat script/allnodes`
Mode="pb"
./script/stablizeTime.sh &
Pid=$!
#./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 1 1 $Mode 

sudo kill $Pid
