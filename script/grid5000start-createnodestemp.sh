#!/bin/bash

if [ $# -eq 0 ]; then
    excho "Usage cluster reservation"
    exit
fi

Cluster=$1
Reservation=$2

#echo cluster: $1
#echo reservation: $2

#oargridstat -w -l $Reservation | sed '/^$/d' > ~/machines
oarstat -j $Reservation -p | oarprint core -P host -f - > ~/atmp
awk < ~/atmp '!seen[$0]++' > ~/machines

awk < ~/machines '/'"$Cluster"'/ { print $1 }' > ~/machines-tmp

echo Deploying `cat ~/machines-tmp`
kadeploy3 -f ~/machines-tmp -a ~/antidote_images/mywheezy-x64-base.env -k ~/.ssh/exp_key.pub

