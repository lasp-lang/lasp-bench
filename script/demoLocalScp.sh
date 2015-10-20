Site=$1

while [ 1 -eq 1 ]; do

    sleep 5s

    scp access.grid5000.fr:~/"$Site"/demo/summaryDemoValues ./
    scp access.grid5000.fr:~/"$Site"/demo/readDemoValues ./
    scp access.grid5000.fr:~/"$Site"/demo/writeDemoValues ./

done
