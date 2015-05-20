#!/bin/bash


if [ $# -eq 1 ]
then
    First=`head -1 ./script/allnodes`
    Others=`awk -v var="$1" 'NR>=2&&NR<=var' ./script/allnodes`
    echo Parameter:$1 , joinig $Others to $First
else
    First=$1
    Others=$2
    echo "Joinig "$Others " to "$First
fi

Join="sudo antidote/rel/antidote/bin/antidote-admin cluster join antidote@$First"
Plan="sudo antidote/rel/antidote/bin/antidote-admin cluster plan"
Commit="sudo antidote/rel/antidote/bin/antidote-admin cluster commit"
Status="sudo antidote/rel/antidote/bin/antidote-admin member-status"
./script/command_to_all.sh "$Others" "$Join"
./script/command_to_all.sh "$First" "$Plan"
./script/command_to_all.sh "$First" "$Commit"


#while true; do
#	LineNum=0
#	sleep 10
#	LineNum=`./script/command_to_all.sh "$First" "$Status" | grep "\-\-      'antidote" | wc -l`  
#	if [ $LineNum -ne 0 ]; then
#		echo "Ring joined!"
#		exit
#	else
#		echo "Joining..."
#	fi
#done
