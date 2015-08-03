#!/bin/bash

AllNodes=`cat script/allnodes`
Command3="service ntp stop"
Command4="/usr/sbin/ntpdate -b ntp2.grid5000.fr"
./script/parallel_command.sh "$AllNodes" "$Command3"	
./script/parallel_command.sh "$AllNodes" "$Command4"	
