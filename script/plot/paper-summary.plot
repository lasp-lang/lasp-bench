set term png 
 set output "paper-summary.png"
 set title "Throughput"
#set xtics ("99(1)" 1, "90(10)" 2, "75(25)" 3, "50(50)" 4, "1(99)" 5)
#set xtics rotate by 90 right
#set tics out
#set boxwidth 0.25
#set style fill solid
#set logscale y
set xlabel "Percentage of Read(Update) Operations"
set ylabel "Operations/Second"

#plot "paper-summary" using 1:2 title 'Antidote' with linespoints, \
#"paper-summary" using 1:3 title 'Gentle Rain' with linespoints, \
#"paper-summary" using 1:4 title 'Eiger' with linespoints, \
#"paper-summary" using 1:5 title 'Eventual Consistency' with linespoints


#plot "paper-summary" every 1 using 1:2 title 'Antidote' with boxes ls 1, \
#"paper-summary" every 1::1 using 1:3 title 'Gentle Rain' with boxes ls 2, \
#"paper-summary" every 1::2 using 1:4 title 'Eiger' with boxes ls 3, \
#"paper-summary" every 1::3 using 1:5 title 'Eventual Consistency' with boxes ls 4

set xtics rotate out
# Select histogram data
set style data histogram
# Give the bars a plain fill pattern, and draw a solid line around them.
set style fill solid border

#set style histogram clustered
plot for [COL=2:5] 'paper-summary.tsv' using COL:xticlabels(1) title columnheader