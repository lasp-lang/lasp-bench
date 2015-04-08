#!/bin/bash

AllNodes=`cat script/allnodes`
IP=`hostname -I`
Command0="echo $IP time.example.com | sudo tee --append /etc/hosts"
./script/parallel_command.sh "$AllNodes" "$Command0"	
