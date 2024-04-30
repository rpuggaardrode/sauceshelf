procedure cpp: .timeStep, .f0min, .f0max, .start, .end

soundID = selected("Sound")
dur = Get end time

if .start > 0
	start = .start - ((6 * (1 / .f0min)) / 2) + (.timeStep / 2)
else
	start = 0
endif

if .end < dur
	end = .end + ((6 * (1 / .f0min)) / 2) + (.timeStep / 2)
else
	end = dur
endif

Extract part: start, end, "rectangular", 1, 1
snippetID = selected("Sound")

To PowerCepstrogram: .f0min, .timeStep, 5000, 50
.times# = List all frame times
cepID = selected("PowerCepstrogram")

	## is there any particular reason why our trend is 'straight' and not
	## 'exponential decay'? I think this is a matter of whether CPP is determined
	## from a linear regression on a linear scale or log scale
	## the documentation is in favor of exponential decay

To Table (cepstral peak prominences): 0, 0, 6, 3, 0, 3, .f0min, .f0max, 0.05, "parabolic",
	... 0.001, 0.05, "Exponential decay", "Robust"
tableID = selected("Table")
.res# = Get all numbers in column: "CPP(dB)"
.numFrames = Get number of rows

select cepID
plus tableID
plus snippetID
Remove

select soundID

endproc