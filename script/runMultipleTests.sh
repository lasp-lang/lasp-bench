#!/bin/bash

AllNodes=`cat script/allnodes`
Mode="pb"
./script/adjustTime.sh
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 4 1 $Mode 
./script/adjustTime.sh
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 8 1 $Mode 
