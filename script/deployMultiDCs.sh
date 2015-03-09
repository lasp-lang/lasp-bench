#!/bin/bash

function joinNodes {
        TNodes=($1)
        NodesPerDC=$2
        TotalLength="${#TNodes[@]}"
	echo "Total nodes:" $TotalLength ", nodes per dc:"$NodesPerDC
        NumDCs=$((TotalLength / NodesPerDC))
	echo $NumDCs
        for I in $(seq 1 $NumDCs);
        do
        	SI=$((I * NodesPerDC - NodesPerDC))
        	First=("${TNodes[@]:$SI:1}")
        	Others=("${TNodes[@]:$((SI+1)):$((NodesPerDC-1))}")
		if [ -n "$Others" ]; then
			echo "Connecting" "${Others[@]}" to $First
			Others=`echo ${Others[@]}`
        		sudo ./script/joinNodesToRing.sh $First "$Others"
		else
			echo "not connecting.."
		fi
        done

	for I in $(seq 1 $NumDCs);
        do
                SI=$((I * NodesPerDC - NodesPerDC))
                First=("${TNodes[@]:$SI:1}")
		./script/waitRingsToFinish.sh $First
        done
	echo "Ring transfer have finished..."
}

AllNodes=$1
Cookie=$2	
NodesPerDC=$4
./script/startNodes.sh "$AllNodes"
echo "Finished restarting"
if [ $3 -eq 1 ]; then
	echo "Connect DCs"
	joinNodes "$AllNodes" $NodesPerDC
	sudo escript ./script/connectDCs.script $Cookie $NodesPerDC $AllNodes
else
	echo "Not connecting DCs"
fi

