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
        		./script/joinNodesToRing.sh $First "$Others"
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

#AllNodes=$1
AllNodes=`cat script/allnodes`
Cookie=$2
NodesPerDC=$4
Branch=$5
BenchmarkFile=$6
./script/startNodes.sh "$AllNodes"
echo "Finished restarting"
if [ $3 -eq 1 ]; then
	echo "Connect DCs"
	joinNodes "$AllNodes" $NodesPerDC
	erlc script/connectDCs.erl
	erl -pa script -name setter@localhost -setcookie $Cookie -run connectDCs listenAndConnect $Cookie $NodesPerDC $Branch $BenchmarkFile $AllNodes -run init stop 
else
	echo "Not connecting DCs"
fi

