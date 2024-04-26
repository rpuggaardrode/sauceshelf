procedure bwHawksMiller: .f0#, .fmt#, .numFrames

.res# = zero# (.numFrames)

kLowFreq = 165.327516
coefLowFreq# = { -6.73636734e-1, 1.80874446e-3, -4.52201682e-6, 7.49514000e-9, 
	... -4.70219241e-12 }
kHighFreq = 15.8146139
coefHighFreq# = { 8.10159009e-2, -9.79728215e-5, 5.28725064e-8, -1.07099364e-11, 
	... 7.91528509e-16 }

for frame from 1 to .numFrames
	s = 1 + 0.25 * (.f0# [frame] - 132) / 88

	if .fmt# [frame] < 500
		.res# [frame] = s * (kLowFreq + (coefLowFreq# [1] * .fmt# [frame]) + 
			... (coefLowFreq# [2] * .fmt# [frame] ^ 2) + 
			... (coefLowFreq# [3] * .fmt# [frame] ^ 3) + 
			... (coefLowFreq# [4] * .fmt# [frame] ^ 4) + 
			... (coefLowFreq# [5] * .fmt# [frame] ^5) )
	else
		.res# [frame] = s * (kHighFreq + (coefHighFreq# [1] * .fmt# [frame]) + 
			... (coefHighFreq# [2] * .fmt# [frame] ^ 2) + 
			... (coefHighFreq# [3] * .fmt# [frame] ^ 3) + 
			... (coefHighFreq# [4] * .fmt# [frame] ^ 4) + 
			... (coefHighFreq# [5] * .fmt# [frame] ^5) )
	endif
endfor

endproc