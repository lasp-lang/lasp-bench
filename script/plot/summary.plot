set term png 
set output "results-weak_meta_data-2dcs-2nodes-2benchNodes/summary_overall.png"
set title "Throughput-2-DCs-2-Nodes-2-Bench-Nodes-Branch-weak_meta_data"
set xtics ("99.99(.01)" 1, "99(1)" 2, "90(10)" 3, "75(25)" 4, "50(50)" 5, "1(99)" 6)
#set xtics rotate by 90 right
set tics out
#set logscale y
set xlabel "Percentage of Read(Update) Operations"
set ylabel "Operations/Second"
plot "plots" using 1:2 title 'Total' with linespoints, \
"plots" using 1:3 title 'Successful' with linespoints, \
"plots" using 1:4 title 'Error' with linespoints
