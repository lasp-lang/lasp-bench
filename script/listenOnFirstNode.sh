#!/bin/bash

FirstNode=`head -1 ./script/allnodes`
NodeName="antidote@"$FirstNode 
erl -pa script -s connectDCs startListening $NodeName 


