BenchNode=$1

mkdir ~/demo
while [ 1 -eq 1 ]; do

    sleep 5s

    scp -o StrictHostKeyChecking=no -i key root@"$BenchNode":/root/basho_bench1/basho_bench/summaryDemoValues ./demo/

done
