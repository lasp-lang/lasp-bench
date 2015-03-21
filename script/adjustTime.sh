#!/bin/bash

AllNodes=`cat script/allnodes`
Command4="sudo ntpdate ntp.ubuntu.com"
./script/parallel_command.sh "$AllNodes" "$Command4"	
