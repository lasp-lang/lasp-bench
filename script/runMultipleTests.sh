#!/bin/bash

AllNodes=`cat script/allnodes`
Mode="pb"
./script/adjustTime.sh
sleep 120
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 4 1 $Mode 
sleep 120
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 4 1 $Mode 


./script/preciseTime.sh
sleep 120
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 4 1 $Mode 
sleep 120
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 4 1 $Mode 
