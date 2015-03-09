#!/bin/bash

AllNodes=`cat script/allnodes`
Mode="erl"
./script/runMultiDCBenchmark.sh "$AllNodes"  antidote 1 3 1 $Mode 
