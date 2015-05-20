#!/bin/bash

if [ $# -eq 0 ]
then
	AllNodes=`cat script/allnodes`
else
	AllNodes=$1	
fi

echo "Stopping nodes:" $AllNodes

Stop="sudo antidote/rel/antidote/bin/antidote stop" 
RemoveData="sudo rm -rf antidote/rel/antidote/data/*"
RemoveLog="sudo rm -rf antidote/rel/antidote/log/*"
./script/command_to_all.sh "$AllNodes" "$Stop" 
./script/command_to_all.sh "$AllNodes" "$RemoveData" 
./script/command_to_all.sh "$AllNodes" "$RemoveLog" 
