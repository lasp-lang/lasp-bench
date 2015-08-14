#!/bin/bash

#if [ $# -eq 0 ]
#then
#	AllNodes=`cat script/allnodes`
#else
#	AllNodes=$1	
#fi

AllNodes=`cat script/allnodes`
echo "Stopping nodes:" $AllNodes

#Stop="antidote/rel/antidote/bin/antidote stop" 
Stop="pkill beam"
RemoveData="rm -rf antidote/rel/antidote/data/*"
RemoveLog="rm -rf antidote/rel/antidote/log/*"
./script/command_to_all.sh "$AllNodes" "$Stop" 
./script/command_to_all.sh "$AllNodes" "$RemoveData" 
./script/command_to_all.sh "$AllNodes" "$RemoveLog" 
