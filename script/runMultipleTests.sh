#!/bin/bash

AllNodes=`cat script/allnodes`
Mode="pb"
./script/preciseTime.sh
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 1 1 $Mode 
