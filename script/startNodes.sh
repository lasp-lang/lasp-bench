#!/bin/bash

if [ $# -eq 0 ]
then
	AllNodes="10.20.0.78 10.20.0.79"
else
	AllNodes=$1	
fi

echo $AllNodes

Start="sudo antidote/rel/antidote/bin/antidote start"
./script/command_to_all.sh "$AllNodes" "$Start" 
