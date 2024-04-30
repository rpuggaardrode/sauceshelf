procedure fmt: .measureBandwidths, .timeStep, .maxN, .maxHz, 
	... .windowLength, .preEmphFrom, .start, .end

soundID = selected("Sound")
dur = Get end time

if .start > 0
	start = .start - .windowLength + (.timeStep / 2)
else
	start = 0
endif

if .end < dur
	end = .end + .windowLength + (.timeStep / 2)
else
	end = dur
endif

Extract part: start, end, "rectangular", 1, 1
snippetID = selected("Sound")

To Formant (burg): .timeStep, .maxN, .maxHz, .windowLength, .preEmphFrom
formantID = selected("Formant")
.times# = List all frame times
.numFrames = Get number of frames
Down to Table: 0, 0, 3, 0, 3, 0, 3, .measureBandwidths
tableID = selected("Table")
.f1# = Get all numbers in column: "F1(Hz)"
.f2# = Get all numbers in column: "F2(Hz)"
.f3# = Get all numbers in column: "F3(Hz)"

if .measureBandwidths <> 0
	.b1# = Get all numbers in column: "B1(Hz)"
	.b2# = Get all numbers in column: "B2(Hz)"
	.b3# = Get all numbers in column: "B3(Hz)"
endif

select formantID
plus tableID
plus snippetID
Remove

select soundID

endproc