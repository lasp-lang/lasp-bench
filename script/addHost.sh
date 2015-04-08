#!/bin/bash

AllNodes=`cat script/allnodes`
IP=`hostname -I`
Command0="echo $IP time.example.com | sudo tee --append /etc/hosts"
./script/command_to_all.sh "$AllNodes" "$Command0"	
