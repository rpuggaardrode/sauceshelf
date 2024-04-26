procedure correctIseli: .f#, .fx#, .bx#, .fs

r# = exp#(-pi*.bx#/.fs)
omega_x# = 2*pi*.fx#/.fs
omega#  = 2*pi*.f#/.fs
a# = r# ^ 2 + 1 - 2*r#*cos#(omega_x# + omega#)
b# = r# ^ 2 + 1 - 2*r#*cos#(omega_x# - omega#)
corr# = -10*(log10#(a#)+log10#(b#))
numerator# = r# ^ 2 + 1 - 2 * r# * cos#(omega_x#)
corr# = -10*(log10#(a#)+log10#(b#)) + 20*log10#(numerator#)
.res# = corr#

endproc