reset

set log x
set xlabel 'k / h Mpc^{-1}'

set log y
set ylabel '{/Symbol D}^2(k)'
set format y '10^{%T}'

unset colorbox

na=16
file='data/power.dat'

plot for [i=1:na] file u 1:(column(1+i)):(-real(i-1)/real(na-1)) w l lw 3 lc palette noti
