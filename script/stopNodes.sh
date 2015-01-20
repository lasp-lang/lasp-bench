#!/bin/bash

if [ $# -eq 0 ]
then
	AllNodes="10.20.0.27 10.20.0.78 10.20.0.79"
else
	AllNodes=$1	
fi

echo $AllNodes

Stop="sudo antidote/rel/antidote/bin/antidote stop" 
Remove="sudo rm -rf antidote/rel/antidote/data/*"
./script/command_to_all.sh "$AllNodes" "$Stop" 
./script/command_to_all.sh "$AllNodes" "$Remove" 
