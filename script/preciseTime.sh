#!/bin/bash

AllNodes=`cat script/allnodes`
Command3="sudo service ntp stop"
Command4="sudo /usr/sbin/ntpdate -b ntp1.grid5000.fr"
./script/parallel_command.sh "$AllNodes" "$Command3"	
./script/parallel_command.sh "$AllNodes" "$Command4"	
