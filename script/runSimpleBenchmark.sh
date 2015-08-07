#!/bin/bash


#AllNodes=`head -$1 script/allnodes`
Id=$2

cd  /root/basho_bench"$Id"/basho_bench/
AllNodes=`cat script/runnodes`

echo All nodes: $AllNodes
echo Type: $1
echo Id: $Id


##Replace benchmark configuration to include nodes
if [ $2 -eq 0 ]; then
    FileName="examples/antidote.config"
    ./script/changeErlConfig.sh "$AllNodes" $Cookie $FileName
else
    FileName="examples/antidote_pb.config"
    ./script/changePBConfig.sh "$AllNodes" antidote $FileName
fi


#./script/stablizeTime.sh &
#Pid=$!

#LoadFile="loadfile.config"
#./script/createLoadFile.sh $FileName $LoadFile
echo "No loading phase..."
#sudo ./basho_bench "$LoadFile"
echo "Benchmarking phase..."
./basho_bench $FileName

tar cvzf ./test.tar tests

#echo "Time stabilize stopped.."
#kill $Pid
