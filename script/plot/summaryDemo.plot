#set term png 
#set output "summary_demo.png"
 set title "Throughput"
#set xtics ("99.99(.01)" 1, "99(1)" 2, "90(10)" 3, "75(25)" 4, "50(50)" 5, "1(99)" 6)
#set xtics rotate by 90 right
set xrange [0:10]
set tics out
#set logscale y
set xlabel "Elapsed time (10s per tic)"
set ylabel "Operations/Second"
plot "summaryDemoValues" using 1:2 title 'Total' with linespoints, \
"summaryDemoValues" using 1:3 title 'Successful' with linespoints, \
"summaryDemoValues" using 1:4 title 'Error' with linespoints

pause 10
reread