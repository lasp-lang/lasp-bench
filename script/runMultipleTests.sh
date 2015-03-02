#!/bin/bash

AllNodes=`cat script/allnodes`
Mode="pb"
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 1 $Mode 
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 2 $Mode
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 4 $Mode
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 8 $Mode

./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 2 1 $Mode
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 4 1 $Mode
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 8 1 $Mode
