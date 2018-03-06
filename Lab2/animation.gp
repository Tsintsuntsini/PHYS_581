set xrange [-20:20]
set yrange [-25:25]
set zrange [0:50]
set xlabel 'X Coordinate'
set ylabel 'Z Coordinate'
set ylabel 'Y Coordinate'

# filename and n=number of lines of your data 
filedata = 'lorenz_1.txt'
n = system(sprintf('cat %s | wc -l', filedata))

set term gif animate
set output 'output.gif'
set key off

set label at 8.46, 8.46, 26 "" point pt 7 ps 1.5 lc rgb "red"
set label at -8.46, -8.46, 26 "" point pt 7 ps 1.5 lc rgb "red"

do for [j=1:n] {
	L_time(j) = sprintf('Lorenz System at time = %1.2f', j*0.1)
    set title L_time(j)
    splot filedata u 2:3:4 every ::1::j w l lw 2, \
          filedata u 2:3:4 every ::j::j w p pt 7 ps 2
}