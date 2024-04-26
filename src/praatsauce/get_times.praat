procedure times: .fileName$, .inputDir$, .includeTheseLabels$, .useTextGrid, .intervalTier,
	... .windowLength

soundID = selected("Sound")
baseName$ = left$("'.fileName$'", index("'.fileName$'", ".") - 1)
tgName$ = baseName$ + ".TextGrid"

if .useTextGrid = 1
	Read from file: .inputDir$ + baseName$ + ".TextGrid"
	tgID = selected("TextGrid")
	.numIntervals = Count intervals where: .intervalTier, "matches (regex)",
		... .includeTheseLabels$

	Get starting points: .intervalTier, "matches (regex)", .includeTheseLabels$
	pointsID = selected("PointProcess")
	To Matrix
	matrixID = selected("Matrix")
	start# = Get all values in row: 1
	.start# = start# - .windowLength
	select pointsID
	plus matrixID
	Remove

	select tgID
	Get end points: .intervalTier, "matches (regex)", .includeTheseLabels$
	pointsID = selected("PointProcess")
	To Matrix
	matrixID = selected("Matrix")
	end# = Get all values in row: 1
	.end# = end# + .windowLength
	select pointsID
	plus matrixID
	Remove

	select tgID
	for int from 1 to .numIntervals
		labID = Get interval at time: .intervalTier, start# [int]
		.labs$ [int] = Get label of interval: .intervalTier, labID
	endfor
	Remove
else
	.numIntervals = 1
	select soundID
	start = Get start time
	.start# = { start }
	end = Get end time
	.end# = { end }
endif

endproc
