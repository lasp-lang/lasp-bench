#!/bin/bash

AllNodes=`cat script/allnodes`
Mode="pb"
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 2 1 $Mode 
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 1 1 $Mode 
