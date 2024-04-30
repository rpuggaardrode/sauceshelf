procedure hnr: .maxFreq, .timeStep, .f0min, .start, .end

soundID = selected("Sound")
dur = Get end time

if .start > 0
	start = .start - ((2 * (1 / .f0min)) / 2) + (.timeStep / 2)
else
	start = 0
endif

if .end < dur
	end = .end + ((2 * (1 / .f0min)) / 2) + (.timeStep / 2)
else
	end = dur
endif

Extract part: start, end, "rectangular", 1, 1
snippetID = selected("Sound")

Filter (pass Hann band): 0, .maxFreq, 100
filterID = selected("Sound")

## Legacy praatsauce has 0.1 instead of 0.00001
## This is a silence threshold, which causes most low frequencies to be treated
## as silent (= -200dB). That doesn't strike me as what we want

To Harmonicity (cc): .timeStep, .f0min, 0.00001, 1.0
hnrID = selected("Harmonicity")
.times# = List all frame times
To Matrix
matrixID = selected("Matrix")
.res# = Get all values in row: 1
.numFrames = Get number of columns

select filterID
plus hnrID
plus matrixID
plus snippetID
Remove

select soundID

endproc