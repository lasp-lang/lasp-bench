BenchNode=$1

mkdir ~/demo
while [ 1 -eq 1 ]; do

    sleep 5s

    scp -o StrictHostKeyChecking=no -i key root@"$BenchNode":/root/basho_bench1/basho_bench/summaryDemoValues ./demo/
    scp -o StrictHostKeyChecking=no -i key root@"$BenchNode":/root/basho_bench1/basho_bench/readDemoValues ./demo/
    scp -o StrictHostKeyChecking=no -i key root@"$BenchNode":/root/basho_bench1/basho_bench/writeDemoValues ./demo/
done
