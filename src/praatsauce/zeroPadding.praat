procedure zeroPadding: .res#, .numFrames, .mostFrames

if .numFrames < mostFrames
	diffFrames = .mostFrames - .numFrames
	padStart = ceiling(diffFrames / 2)
	padEnd = floor(diffFrames / 2)
	padStart# = zero# (padStart)
	.res# = combine# (padStart#, .res#)
	if padEnd <> 0
		padEnd# = zero# (padEnd)
		.res# = combine# (.res#, padEnd#)
	endif
else
	.res# = .res#
endif

endproc