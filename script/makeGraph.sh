#!/bin/bash

FileName=$1

tar xvzf "$FileName".tar

cd $FileName
mkdir -p results

BenchFile=`ls -l summary-* | awk -F "-" 'FNR==1 && /^summary/{print $2}' -`
ReadPortion=`ls -l summary-* | awk -F "[-:]" '/^summary/{print $3}' - | sort -nr`

cd ..

#Basho bench grapsh
for Read in $ReadPortion; do
    Rscript --vanilla priv/summary.r -i $FileName/summary-"$BenchFile"-"$Read"
    mv $FileName/summary-"$BenchFile"-"$Read"/summary.png $FileName/results/overall-summary-"$Read".png
done

cd $FileName

# Read latencies
rm plots
AllFiles=""
I=1
sed -i '/output/d' ../script/plot/readlatencies.plot
sed -i '2i set output "results/read_latencies.png"' ../script/plot/readlatencies.plot
sed -i '/set title/d' ../script/plot/readlatencies.plot
sed -i '3i set title "Read Latencies"' ../script/plot/readlatencies.plot
for Read in $ReadPortion; do
    AllFiles="summary"-$BenchFile-$Read"/read_latencies.csv "$AllFiles""
    #echo summary-"$BenchFile"-"$Read"/read_latencies.csv
    awk -f ../script/mergeLatencies.awk summary-"$BenchFile"-"$Read"/read_latencies.csv >> plots
    sed -i "${I} s/^/${I}, /" plots
    I=$(($I + 1))
done
sed -i "1i # read_ratio, min, mean, median, 95th, 99th, 99_9th, max" plots
gnuplot ../script/plot/readlatencies.plot

# Write latencies
rm plots
AllFiles=""
I=1
sed -i '/output/d' ../script/plot/readlatencies.plot
sed -i '2i set output "results/write_latencies.png"' ../script/plot/readlatencies.plot
sed -i '/set title/d' ../script/plot/readlatencies.plot
sed -i '3i set title "Write Latencies"' ../script/plot/readlatencies.plot
for Read in $ReadPortion; do
    AllFiles="summary"-$BenchFile-$Read"/append_latencies.csv "$AllFiles""
    #echo summary-"$BenchFile"-"$Read"/read_latencies.csv
    awk -f ../script/mergeLatencies.awk summary-"$BenchFile"-"$Read"/append_latencies.csv >> plots
    sed -i "${I} s/^/${I}, /" plots
    I=$(($I + 1))
done
sed -i "1i # read_ratio, min, mean, median, 95th, 99th, 99_9th, max" plots
gnuplot ../script/plot/readlatencies.plot

# Throughput
rm plots
AllFiles=""
I=1
for Read in $ReadPortion; do
    AllFiles="summary"-$BenchFile-$Read"/summary.csv "$AllFiles""
    #echo summary-"$BenchFile"-"$Read"/read_latencies.csv
    awk -f ../script/mergeSummaries.awk summary-"$BenchFile"-"$Read"/summary.csv >> plots
    sed -i "${I} s/^/${I}, /" plots
    I=$(($I + 1))
done
sed -i "1i # total, successful, failed" plots
gnuplot ../script/plot/summary.plot

