if [ $# -eq 0 ]; then
    echo "Usage: all_nodes, cookie, number_of_dcs, nodes_per_dc, bench_nodes_per_dc, connect_dc_or_not, erl|pb, bench_parallel gridJob startTime"
    exit
else
    #AllSystemNodes=$1
    #SystemNodesArray=($AllSystemNodes)
    Cookie=$1
    NumberDC=$2
    NodesPerDC=$3
    BenchNodesPerDC=$4
    BenchNodes=`cat script/allnodesbench`
    NodesToUse=$((NumberDC * NodesPerDC))
    AllNodes=`cat script/allnodes`    
    # AllNodes=${SystemNodesArray[@]:0:$NodesToUse}
    # AllNodes=`echo ${AllNodes[@]}`
    ConnectDCs=$5
    echo "Using" $AllNodes ", will connect DCs:" $ConnectDCs
    BenchFile=$6
    # if [ "$6" = "erl" ]; then
    # 	echo "Benchmark erl"
    #     BenchmarkType=0
    # elif [ "$6" = "pb" ]; then
    # 	echo "Benchmark pb"
    #     BenchmarkType=1
    # else
    #     echo "Wrong benchmark type!"
    #     exit
    # fi
    BenchParallel=$7
    GridJob=$8
    Time=$9
    Branch=`cat script/branch`
fi

BenchmarkType=1

ReadsNumber=( 10000 99 90 75 50 1 )
WritesNumber=( 1 1 10 25 50 100 )

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


ReadWrite=2

cat script/allnodes > ./tmpnodelist
cat script/allnodesbench > ./tmpnodelistbench
for DCNum in $(seq 1 $NumberDC); do
    NodeArray[$DCNum]=`head -$NodesPerDC tmpnodelist`
    sed '1,'"$NodesPerDC"'d' tmpnodelist > tmp
    cat tmp > tmpnodelist
    
    BenchNodeArray[$DCNum]=`head -$BenchNodesPerDC tmpnodelistbench`
    sed '1,'"$BenchNodesPerDC"'d' tmpnodelistbench > tmp
    cat tmp > tmpnodelistbench
done

# Run the benchmarks in parallel
# This is not a good way to do this, should be implemented inside basho bench
for DCNum in $(seq 1 $NumberDC); do
    TmpArray=(${BenchNodeArray[$DCNum]})
    for Item in ${TmpArray[@]}; do
	for I in $(seq 1 $BenchParallel); do
	    echo Running bench $I on $Item with nodes
    	    echo ssh -t -o StrictHostKeyChecking=no -i key root@$Item /root/basho_bench"$I"/basho_bench/script/runSimpleBenchmarkDemo.sh $I $BenchFile ${ReadsNumber[$ReadWrite]}

	    echo for job $GridJob on time $Time
    	    ssh -t -o StrictHostKeyChecking=no -i key root@$Item /root/basho_bench"$I"/basho_bench/script/runSimpleBenchmarkDemo.sh $I $BenchFile ${ReadsNumber[$ReadWrite]} >> logs/"$GridJob"/runBench-"$Item"-"$I"-"$Time"-Reads"${ReadsNumber[$ReadWrite]}" &
	done
    done
done

sleep 20s

echo Starting the results calculation
while [ 1 -eq 1 ]; do

    # Summary results
    sleep 5s
    AllFiles=""
    for DCNum in $(seq 1 $NumberDC); do
	TmpArray=(${BenchNodeArray[$DCNum]})
	for Item in ${TmpArray[@]}; do
	    for I in $(seq 1 $BenchParallel); do
		echo Collecting summary results $I on $Item
		AllFiles="./"$I"a"$Item"summary.csv "$AllFiles""

		scp -o StrictHostKeyChecking=no -i key root@"$Item":/root/basho_bench"$I"/basho_bench/tests/current/summary.csv ./"$I"a"$Item"summary.csv
	    done
	done
    done
    echo awk -f ../basho_bench/script/mergeResultsSummary.awk $AllFiles > summary.csv
    awk -f ../basho_bench/script/mergeResultsSummary.awk $AllFiles > summary.csv
    
    echo awk -f ../basho_bench/script/demoSummary.awk summary.csv > summaryDemoValues
    awk -f ../basho_bench/script/demoSummary.awk summary.csv > summaryDemoValues
    # gnuplot ../basho_bench/script/plot/summaryDemo.plot

    

    # Read latencies
    AllFiles=""
    for DCNum in $(seq 1 $NumberDC); do
	TmpArray=(${BenchNodeArray[$DCNum]})
	for Item in ${TmpArray[@]}; do
	    for I in $(seq 1 $BenchParallel); do
		echo Collecting read results $I on $Item
		AllFiles="./"$I"a"$Item"read.csv "$AllFiles""

		scp -o StrictHostKeyChecking=no -i key root@"$Item":/root/basho_bench"$I"/basho_bench/tests/current/read_latencies.csv ./"$I"a"$Item"read.csv
	    done
	done
    done
    echo awk -f ../basho_bench/script/mergeResults.awk $AllFiles > reads.csv
    awk -f ../basho_bench/script/mergeResults.awk $AllFiles > reads.csv

    echo awk -f ../basho_bench/script/demoLatencies.awk reads.csv > readDemoValues
    awk -f ../basho_bench/script/demoLatencies.awk reads.csv > readDemoValues
    # gnuplot ../basho_bench/script/plot/summaryDemo.plot


    # Update latencies
    AllFiles=""
    for DCNum in $(seq 1 $NumberDC); do
	TmpArray=(${BenchNodeArray[$DCNum]})
	for Item in ${TmpArray[@]}; do
	    for I in $(seq 1 $BenchParallel); do
		echo Collecting append results $I on $Item
		AllFiles="./"$I"a"$Item"write.csv "$AllFiles""

		scp -o StrictHostKeyChecking=no -i key root@"$Item":/root/basho_bench"$I"/basho_bench/tests/current/"$AppendFile" ./"$I"a"$Item"write.csv
	    done
	done
    done
    echo awk -f ../basho_bench/script/mergeResults.awk $AllFiles > writes.csv
    awk -f ../basho_bench/script/mergeResults.awk $AllFiles > writes.csv

    echo awk -f ../basho_bench/script/demoLatencies.awk writes.csv > writeDemoValues
    awk -f ../basho_bench/script/demoLatencies.awk writes.csv > writeDemoValues
    # gnuplot ../basho_bench/script/plot/summaryDemo.plot

done



