#!/bin/bash

if [ $# -eq 0 ]
then
	AllNodes=`cat script/allnodes` 
else
	AllNodes=$1	
fi

echo "Starting nodes:" $AllNodes

Start="antidote/rel/antidote/bin/antidote start"
#./script/command_to_all.sh "$AllNodes" "$Start" 
./script/parallel_command.sh "$AllNodes" "$Start" 
