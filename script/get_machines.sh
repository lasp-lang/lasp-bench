#!/bin/bash

Cluster=$1
Reservation=$2

#echo cluster: $1
#echo reservation: $2

#oargridstat -w -l $Reservation | sed '/^$/d' > ~/machines
oarstat -j $Reservation -p | oarprint core -P host -f - > ~/atmp
awk < ~/atmp '!seen[$0]++'
