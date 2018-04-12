set xrange [0:100]
set yrange [0:100]
set cbrange[-2:8]
set xlabel 'x'
set ylabel 'y'

# filename and n=number of lines of your data 
filename(n) = sprintf("ouy%03d", n)
n = 100

set term gif animate size 1200,1000
set output 'jet.gif' 
set key off
set cblabel 'pressure'
unset cbtics
set view map
set dgrid3d

set palette rgbformulae 33,13,10

do for [j=0:n] {
	L_time(j) = sprintf('Time Frame = %03d', j)
    set title L_time(j)
    splot filename(j) u 1:2:4 w image ; pause 0.5
}