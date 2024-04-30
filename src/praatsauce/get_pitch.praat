procedure pitch: .timeStep, .f0min, .f0max, .start, .end

soundID = selected("Sound")
dur = Get end time

if .start > 0
	start = .start - ((3 * (1 / .f0min)) / 2) + (.timeStep / 2)
else
	start = 0
endif

if .end < dur
	end = .end + ((3 * (1 / .f0min)) / 2) + (.timeStep / 2)
else
	end = dur
endif

Extract part: start, end, "rectangular", 1, 1
snippetID = selected("Sound")

To Pitch (filtered ac): .timeStep, .f0min, .f0max, 15, 0, 0.03, 0.09, 0.5, 0.005, 0.35, 0.14
pitchOrgID = selected("Pitch")
Kill octave jumps
pitchFiltID = selected("Pitch")
.f0# = List values in all frames: "Hertz"
.times# = List all frame times
.numFrames = Get number of frames
# .f0# = List values at times: .times#, "Hertz", "linear"
select pitchOrgID
plus pitchFiltID
plus snippetID
Remove

select soundID

endproc
