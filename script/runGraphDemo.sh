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
    BenchmarkFile=$6
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

# Run the benchmarks in parallel
# This is not a good way to do this, should be implemented inside basho bench
for DCNum in $(seq 1 $NumberDC); do
    TmpArray=(${BenchNodeArray[$DCNum]})
    for Item in ${TmpArray[@]}; do
	for I in $(seq 1 $BenchParallel); do
	    echo Running bench $I on $Item with nodes
	    echo "${NodeArray[$DCNum]}" > ./tmp
	    echo scp -o StrictHostKeyChecking=no -i key ./tmp root@"$Item":/root/basho_bench"$I"/basho_bench/script/runnodes
	    scp -o StrictHostKeyChecking=no -i key ./tmp root@"$Item":/root/basho_bench"$I"/basho_bench/script/runnodes
    	    echo ssh -t -o StrictHostKeyChecking=no -i key root@$Item /root/basho_bench"$I"/basho_bench/script/runSimpleBenchmark.sh $BenchmarkType $I $BenchmarkFile ${ReadsNumber[$ReadWrite]} ${WritesNumber[$ReadWrite]}

	    echo for job $GridJob on time $Time
    	    ssh -t -o StrictHostKeyChecking=no -i key root@$Item /root/basho_bench"$I"/basho_bench/script/runSimpleBenchmark.sh $BenchmarkType $I $BenchmarkFile ${ReadsNumber[$ReadWrite]} ${WritesNumber[$ReadWrite]} >> logs/"$GridJob"/runBench-"$Item"-"$I"-"$Time"-Reads"${ReadsNumber[$ReadWrite]}" &
	done
    done
done



while [ 1 -eq 1 ]; do

    sleep 5s
    AllFiles=""
    for DCNum in $(seq 1 $NumberDC); do
	TmpArray=(${BenchNodeArray[$DCNum]})
	for Item in ${TmpArray[@]}; do
	    for I in $(seq 1 $BenchParallel); do
		echo Collecting results $I on $Item
		AllFiles="./"$I"a"$Item"summary.csv "$AllFiles""

		scp -o StrictHostKeyChecking=no -i key root@"$Item":/root/basho_bench"$I"/basho_bench/results/current/summary.csv ./"$I"a"$Item"summary.csv
	    done
	done
    done
    echo awk -f ../basho_bench/script/mergeResults.awk $AllFiles > summary.csv
    awk -f ../basho_bench/script/mergeResults.awk $AllFiles > summary.csv

    echo awk -f ../basho_bench/script/demoSummary.awk summary.csv > summaryDemoValues
    awk -f ../basho_bench/script/demoSummary.awk summary.csv > summaryDemoValues
    # gnuplot ../basho_bench/script/plot/summaryDemo.plot

done



