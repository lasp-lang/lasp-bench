#!/bin/bash

AllNodes=`cat script/allnodes`
Command3="sudo service ntp stop"
Command4="sudo /usr/sbin/ntpdate -b time.example.com"
./script/parallel_command.sh "$AllNodes" "$Command3"	
./script/parallel_command.sh "$AllNodes" "$Command4"	
