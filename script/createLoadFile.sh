#!/bin/bash

File1=$1
File2=$2
cp $File1 $File2
sed -i 's/duration, 3/duration, 1/g' $File2
sed -i 's/{append, 1}, {read, 10}/{append, 1}/g' $File2
