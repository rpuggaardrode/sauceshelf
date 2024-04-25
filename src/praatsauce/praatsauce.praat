include read_params.praat
include get_times.praat
include get_formants.praat

@params: "params.csv"

Create Strings as file list: "wavs", params.inputDir$ + "*.wav"
wavsListID = selected("Strings")
Sort
numFile = Get number of strings

if params.useTextGrid = 1
	Create Strings as file list: "grids", params.inputDir$ + "*.TextGrid"
	tgListID = selected("Strings")
	Sort
	numTG = Get number of strings
endif

for thisFile from 1 to numFile
	select wavsListID
	thisWav$ = Get string: thisFile
	Read from file: params.inputDir$ + thisWav$
	soundID = selected("Sound")

	if params.resample16kHz = 1
		execute resample.praat
	endif

	if params.channel <> 0
		execute extract_channel.praat 'params.channel'
	endif
	@times: thisWav$, params.inputDir$, params.includeTheseLabels$, 
		... params.useTextGrid, params.intervalTier, params.windowLength
	
	for int from 1 to times.numIntervals
		select soundID

		intervalDur = times.end# [int] - times.start# [int]
		if params.intervalEquidistant <> 0
			timeStep = intervalDur / params.intervalEquidistant
		else
			timeStep = params.intervalFixed
		endif
		Extract part: times.start# [int], times.end# [int], "rectangular", 1, 0
		snippetID = selected("Sound")
		if params.measureFormants <> 0 
			@fmt: params.bw, timeStep, params.maxNumFormants, 
				... params.maxFormantHz, params.windowLength, 
				... params.preEmphFrom
		endif

	endfor
endfor

writeInfoLine: times.start#
appendInfoLine: times.end#
appendInfoLine: times.numIntervals
appendInfoLine: times.labs$ [1]
appendInfoLine: timeStep
appendInfoLine: fmt.b1#