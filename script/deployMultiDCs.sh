#!/bin/bash

function joinNodes {
        TNodes=($1)
        DCPerRing=$2
        TotalLength="${#TNodes[@]}"
	echo $TotalLength
        NumDCs=$((TotalLength / DCPerRing))
	echo $NumDCs
        for I in $(seq 1 $NumDCs);
        do
        	SI=$((I * DCPerRing - DCPerRing))
        	First=("${TNodes[@]:$SI:1}")
        	Others=("${TNodes[@]:$((SI+1)):$((DCPerRing-1))}")
		if [ -n "$Others" ]; then
			echo "Connecting" "${Others[@]}" to $First
        		sudo ./script/joinNodesToRing.sh $First "${Others[@]}"
		else
			echo "not connecting.."
		fi
        done

	for I in $(seq 1 $NumDCs);
        do
                SI=$((I * DCPerRing - DCPerRing))
                First=("${TNodes[@]:$SI:1}")
		./script/waitRingsToFinish.sh $First
        done
	echo "Ring transfer have finished..."
}

AllNodes=$1
Cookie=$2	
DCPerRing=$4
./script/startNodes.sh "$AllNodes"
echo "Finished restarting"
if [ $3 -eq 1 ]; then
	echo "Connect DCs"
	joinNodes "$AllNodes" $DCPerRing
	sudo escript ./script/connectDCs.script $Cookie $DCPerRing $AllNodes
else
	echo "Not connecting DCs"
fi

