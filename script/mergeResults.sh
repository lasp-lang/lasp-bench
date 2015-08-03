#!/bin/bash

TestDirectory=$1

cd $TestDirectory

Files=`cat filenames`

I=1
for File in $Files; do
    mkdir $File
    tar -C $File -xvzf "$File".tar
    TestDate[$I]=`ls -l "$File"/tests/current | awk -F "/" '{print $NF}' -`
    #TestDate[$I]=$I
    I=$(($I + 1))
done


mkdir summary

# Read latencies
AllFiles=""
I=1
for File in $Files; do
    echo ${TestDate[$I]}
    AllFiles=""$File"/tests/"${TestDate[$I]}"/read_latencies.csv "$AllFiles""
    echo $AllFiles
    I=$(($I + 1))
done
awk -f ../basho_bench/script/mergeResults.awk $AllFiles > summary/read_latencies.csv

# Append latencies
AllFiles=""
I=1
for File in $Files; do
    echo ${TestDate[$I]}
    AllFiles=""$File"/tests/"${TestDate[$I]}"/append_latencies.csv "$AllFiles""
    echo $AllFiles
    I=$(($I + 1))
done
awk -f ../basho_bench/script/mergeResults.awk $AllFiles > summary/append_latencies.csv


# Summary latencies
AllFiles=""
I=1
for File in $Files; do
    echo ${TestDate[$I]}
    AllFiles=""$File"/tests/"${TestDate[$I]}"/summary.csv "$AllFiles""
    echo $AllFiles
    I=$(($I + 1))
done
awk -f ../basho_bench/script/mergeResultsSummary.awk $AllFiles > summary/summary.csv
