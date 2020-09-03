reset

# Power spectrum file
file='data/power.dat'

# k axis properties
kmin = 1e-3
kmax = 1e2
klab = 'k / h Mpc^{-1}'

# Power axis properties
pmin = 1e-8
pmax = 1e4
plab = '{/Symbol D}^2(k)'

# Number of scale factors to plot
na=16

# x axis
set log x
set xlabel klab
set xrange [kmin:kmax]

# y axis
set log y
set ylabel plab
set format y '10^{%T}'
set yrange [pmin:pmax]

# color axis
unset colorbox

# Do the plotting
plot for [i=1:na] file u 1:(column(1+i)):(-real(i-1)/real(na-1)) w l lw 3 lc palette noti
