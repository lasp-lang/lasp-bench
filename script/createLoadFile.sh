#!/bin/bash

File1=$1
File2=$2
sudo cp $File1 $File2
sudo sed -i 's/duration, 3/duration, 1/g' $File2
sudo sed -i 's/{append, 1}, {read, 10}/{append, 1}/g' $File2
