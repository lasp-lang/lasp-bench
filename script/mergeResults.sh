#!/bin/bash

TestDirectory=$1
TestName=$2
Branch=$3
BenchFile=$4

if [ $BenchFile = "orset_pb.config" ]; then
    Type="set"
    AppendFile="update_latencies.csv"
elif [ $BenchFile = "antidote_pb.config" ]; then
    Type="counter"
    AppendFile="append_latencies.csv"
elif [ $BenchFile = "single_key.config" ]; then
    Type="set"
    AppendFile="update_latencies.csv"
else
    Type="counter"
    AppendFile="append_latencies.csv"
fi

NumNodes=`cat ~/nodelist | wc -l`
NumBenchNodes=`cat ~/benchnodelist | wc -l`
NumDC=`cat ~/countDC`

cd $TestDirectory

echo $NumNodes > numNodes
echo $NumBenchNodes > numBenchNodes
echo $NumDC > numDCs
echo $Branch > branch

Files=`cat filenames`

I=1
for File in $Files; do
    mkdir $File
    tar -C $File -xvzf "$File".tar >> tar_output
    TestDate[$I]=`ls -l "$File"/tests/current | awk -F "/" '{print $NF}' -`
    #TestDate[$I]=$I
    I=$(($I + 1))
done


mkdir summary-"$TestName"

# Read latencies
AllFiles=""
I=1
for File in $Files; do
    echo The test date for $File is ${TestDate[$I]}
    AllFiles=""$File"/tests/"${TestDate[$I]}"/read_latencies.csv "$AllFiles""
    I=$(($I + 1))
done
echo awk -f ../basho_bench/script/mergeResults.awk $AllFiles > summary-"$TestName"/read_latencies.csv
awk -f ../basho_bench/script/mergeResults.awk $AllFiles > summary-"$TestName"/read_latencies.csv

# Append latencies
AllFiles=""
I=1
for File in $Files; do
    AllFiles=""$File"/tests/"${TestDate[$I]}"/"$AppendFile" "$AllFiles""
    I=$(($I + 1))
done
echo awk -f ../basho_bench/script/mergeResults.awk $AllFiles > summary-"$TestName"/append_latencies.csv
awk -f ../basho_bench/script/mergeResults.awk $AllFiles > summary-"$TestName"/append_latencies.csv


# Summary latencies
AllFiles=""
I=1
for File in $Files; do
    AllFiles=""$File"/tests/"${TestDate[$I]}"/summary.csv "$AllFiles""
    I=$(($I + 1))
done
echo awk -f ../basho_bench/script/mergeResultsSummary.awk $AllFiles > summary-"$TestName"/summary.csv
awk -f ../basho_bench/script/mergeResultsSummary.awk $AllFiles > summary-"$TestName"/summary.csv
