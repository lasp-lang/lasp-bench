#!/bin/bash

if [ $# -eq 0 ]
then
    First=`head -1 script/allnodes`
else
    First=$1
fi

Status="antidote/rel/antidote/bin/antidote-admin member-status"

while true; do
       sleep 10
       LineNum=`./script/command_to_all.sh "$First" "$Status" | grep "\-\-      'antidote" | wc -l`  
       if [ $LineNum -ne 0 ]; then
               echo "Ring joined!"
               exit
       else
               echo "Joining..."
       fi
done
