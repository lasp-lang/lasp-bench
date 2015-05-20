#!/bin/bash

FirstNode=`head -1 ./script/allnodes`
Port=8091
NodeName="antidote@"$FirstNode 
erl -s connectDCs startListening [{$NodeName, $Port}] 


