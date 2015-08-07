#!/bin/bash

TestDirectory=$1

cd $TestDirectory

Files=`cat filenames`

I=1
for File in $Files; do
    mkdir $File
    tar -C $File -xvzf "$File".tar >> tar_output
    TestDate[$I]=`ls -l "$File"/tests/current | awk -F "/" '{print $NF}' -`
    #TestDate[$I]=$I
    I=$(($I + 1))
done


mkdir summary

# Read latencies
AllFiles=""
I=1
for File in $Files; do
    echo The test date for $File is ${TestDate[$I]}
    AllFiles=""$File"/tests/"${TestDate[$I]}"/read_latencies.csv "$AllFiles""
    I=$(($I + 1))
done
echo awk -f ../basho_bench/script/mergeResults.awk $AllFiles > summary/read_latencies.csv
awk -f ../basho_bench/script/mergeResults.awk $AllFiles > summary/read_latencies.csv

# Append latencies
AllFiles=""
I=1
for File in $Files; do
    AllFiles=""$File"/tests/"${TestDate[$I]}"/append_latencies.csv "$AllFiles""
    I=$(($I + 1))
done
echo awk -f ../basho_bench/script/mergeResults.awk $AllFiles > summary/append_latencies.csv
awk -f ../basho_bench/script/mergeResults.awk $AllFiles > summary/append_latencies.csv


# Summary latencies
AllFiles=""
I=1
for File in $Files; do
    AllFiles=""$File"/tests/"${TestDate[$I]}"/summary.csv "$AllFiles""
    I=$(($I + 1))
done
echo awk -f ../basho_bench/script/mergeResultsSummary.awk $AllFiles > summary/summary.csv
awk -f ../basho_bench/script/mergeResultsSummary.awk $AllFiles > summary/summary.csv
