#!/bin/bash


#AllNodes=`head -$1 script/allnodes`
Id=$2
File=$3
Reads=$4
Writes=$5
NumDCs=$6
NodesPerDC=$7
DcId=$8
cd  /root/basho_bench"$Id"/basho_bench/
AllNodes=`cat script/runnodes`

echo All nodes: $AllNodes
echo Type: $1
echo Id: $Id
echo BenchmarkFile: $File

rm -rf ./tests
mkdir tests

##Replace benchmark configuration to include nodes
if [ $2 -eq 0 ]; then
    FileName="examples/"$File
    ./script/changeErlConfig.sh "$AllNodes" $Cookie $FileName $Reads $Writes
else
    FileName="examples/"$File
    ./script/changePBConfig.sh "$AllNodes" antidote $FileName $Reads $Writes $NumDCs $NodesPerDC $DcId
fi


#./script/stablizeTime.sh &
#Pid=$!

#LoadFile="loadfile.config"
#./script/createLoadFile.sh $FileName $LoadFile
echo "No loading phase..."
#sudo ./basho_bench "$LoadFile"
echo "Benchmarking phase..."
./basho_bench $FileName

tar cvzf ./test-"$File"-"$Reads".tar tests

#echo "Time stabilize stopped.."
#kill $Pid
