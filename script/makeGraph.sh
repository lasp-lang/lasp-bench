#!/bin/bash

FileName=$1

tar xvzf "$FileName".tar

cd $FileName

NumNodes=`cat numNodes`
NumBenchNodes=`cat numBenchNodes`
NumDCs=`cat numDCs`
Branch=`cat branch`

mkdir -p results-"$Branch"-"$NumDCs"dcs-"$NumNodes"nodes-"$NumBenchNodes"benchNodes

BenchFile=`ls -l summary-* | awk -F "-" 'FNR==1 && /^summary/{print $2}' -`
ReadPortion=`ls -l summary-* | awk -F "[-:]" '/^summary/{print $3}' - | sort -nr`

cd ..

#Basho bench grapsh
for Read in $ReadPortion; do
    Rscript --vanilla priv/summary.r -i $FileName/summary-"$BenchFile"-"$Read"
    mv $FileName/summary-"$BenchFile"-"$Read"/summary.png $FileName/results-"$Branch"-"$NumDCs"dcs-"$NumNodes"nodes-"$NumBenchNodes"benchNodes/overall-summary-"$Read".png
done

cd $FileName

# Read latencies
rm plots
AllFiles=""
I=1
sed -i '' -e '/output/d' ../script/plot/readlatencies.plot
OutName="results-"$Branch"-"$NumDCs"dcs-"$NumNodes"nodes-"$NumBenchNodes"benchNodes/read_latencies.png"
sed -i '' -e '2i\'$'\n set output \"'$OutName'\"'$'\n' ../script/plot/readlatencies.plot
sed -i '' -e '/set title/d' ../script/plot/readlatencies.plot
Title="Read-Latencies-${NumDCs}-DCs-${NumNodes}-Nodes-${NumBenchNodes}-Bench-Nodes-Branch-${Branch}"
sed -i '' -e '3i\'$'\n set title \"'$Title'\"'$'\n' ../script/plot/readlatencies.plot
for Read in $ReadPortion; do
    AllFiles="summary"-$BenchFile-$Read"/read_latencies.csv "$AllFiles""
    #echo summary-"$BenchFile"-"$Read"/read_latencies.csv
    awk -f ../script/mergeLatencies.awk summary-"$BenchFile"-"$Read"/read_latencies.csv >> plots
    sed -i '' -e ''${I}' s/^/'${I}', /' plots
    I=$(($I + 1))
done
sed -i '' -e '1i\'$'\n # read_ratio, min, mean, median, 95th, 99th, 99_9th, max'$'\n' plots
gnuplot ../script/plot/readlatencies.plot

# Write latencies
rm plots
AllFiles=""
I=1
sed -i '' -e '/output/d' ../script/plot/readlatencies.plot
OutName="results-"$Branch"-"$NumDCs"dcs-"$NumNodes"nodes-"$NumBenchNodes"benchNodes/write_latencies.png"
sed -i '' -e '2i\'$'\n set output \"'$OutName'\"'$'\n' ../script/plot/readlatencies.plot
sed -i '' -e '/set title/d' ../script/plot/readlatencies.plot
Title="Write-Latencies-${NumDCs}-DCs-${NumNodes}-Nodes-${NumBenchNodes}-Bench-Nodes-Branch-${Branch}"
sed -i '' -e '3i\'$'\n set title \"'$Title'\"'$'\n' ../script/plot/readlatencies.plot
for Read in $ReadPortion; do
    AllFiles="summary"-$BenchFile-$Read"/append_latencies.csv "$AllFiles""
    #echo summary-"$BenchFile"-"$Read"/read_latencies.csv
    awk -f ../script/mergeLatencies.awk summary-"$BenchFile"-"$Read"/append_latencies.csv >> plots
    sed -i '' -e ''${I}' s/^/'${I}', /' plots
    I=$(($I + 1))
done
sed -i '' -e '1i\'$'\n # read_ratio, min, mean, median, 95th, 99th, 99_9th, max'$'\n' plots
gnuplot ../script/plot/readlatencies.plot

# Throughput
rm plots
AllFiles=""
I=1
sed -i '' -e '/output/d' ../script/plot/summary.plot
OutName="results-"$Branch"-"$NumDCs"dcs-"$NumNodes"nodes-"$NumBenchNodes"benchNodes/summary_overall.png"
sed -i '' -e '2i\'$'\n set output \"'$OutName'\"'$'\n' ../script/plot/summary.plot
sed -i '' -e '/set title/d' ../script/plot/summary.plot
Title="Throughput-${NumDCs}-DCs-${NumNodes}-Nodes-${NumBenchNodes}-Bench-Nodes-Branch-${Branch}"
sed -i '' -e '3i\'$'\n set title \"'$Title'\"'$'\n' ../script/plot/summary.plot
for Read in $ReadPortion; do
    AllFiles="summary"-$BenchFile-$Read"/summary.csv "$AllFiles""
    #echo summary-"$BenchFile"-"$Read"/read_latencies.csv
    awk -f ../script/mergeSummaries.awk summary-"$BenchFile"-"$Read"/summary.csv >> plots
    sed -i '' -e ''${I}' s/^/'${I}', /' plots
    I=$(($I + 1))
done
sed -i '' -e '1i\'$'\n # total, successful, failed'$'\n' plots
gnuplot ../script/plot/summary.plot

