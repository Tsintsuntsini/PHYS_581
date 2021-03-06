set xrange [-1.0:1.0]
set yrange [-0.1:1.1]
set xlabel 'x'
set ylabel 'u(x)'

# filename and n=number of lines of your data 
filename(n) = sprintf("upwind_burger%03d.txt", n)
n = 23

set term gif animate size 1200,1000
set output 'upwind.gif' 
set key autotitle columnhead
set key off

set palette rgbformulae 33,13,10

do for [j=0:n] {
	L_time(j) = sprintf('Upwind Method Shockwave = %03d', j)
    set title L_time(j)
    plot filename(j) u 1:2 w lines ; pause 0.5
}