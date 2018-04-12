set xrange [-1.0:1.0]
set yrange [-1.5:1.5]
set xlabel 'x'
set ylabel 'u(x)'

# filename and n=number of lines of your data 
filename1(n) = sprintf("lw_2step_vis%03d.txt", n)
filename2(n) = sprintf("lw_2step_inv%03d.txt", n)
n = 20

set term gif animate size 1200,1000
set output 'lw2step_vis.gif' 
set key autotitle columnhead
set key off

do for [j=0:n] {
	L_time(j) = sprintf('Lax-Wendroff 2-Step Method Shockwave = %03d', j)
  set multiplot layout 1, 2 title L_time(j)
    plot filename1(j) u 1:2 w lines #; pause 0.5
    plot filename2(j) u 1:2 w lines ; pause 0.5
  unset multiplot
}