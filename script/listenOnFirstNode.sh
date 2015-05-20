#!/bin/bash

FirstNode=`head -1 ./script/allnodes`
NodeName="antidote@"$FirstNode 
erl -pa script -setcookie antidote -name setup@localhost -s connectDCs startListener $NodeName -run init stop 


