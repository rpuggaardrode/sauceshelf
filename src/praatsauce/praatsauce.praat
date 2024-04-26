include read_params.praat
include get_times.praat
include get_formants.praat
include get_pitch.praat
include get_spectralMeasures.praat
include get_bwHawksMiller.praat

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

	fs = Get sampling frequency

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
		
		## Need to get times# in a different way if formants are not measured
		## Same with numFrames

		if params.measureFormants <> 0 
			@fmt: params.measureBandwidths, timeStep, params.maxNumFormants, 
				... params.maxFormantHz, params.windowLength, 
				... params.preEmphFrom
		endif

		if params.measurePitch <> 0
			@pitch: timeStep, params.f0min, params.f0max, fmt.times#
		endif

		if params.requireBandwidths <> 0
			if params.bwHawksMiller = 0
				b1# = fmt.b1#
				b2# = fmt.b2#
				b3# = fmt.b3#
			else
				@bwHawksMiller: pitch.f0#, fmt.f1#, fmt.numFrames
				b1# = bwHawksMiller.res#
				@bwHawksMiller: pitch.f0#, fmt.f2#, fmt.numFrames
				b2# = bwHawksMiller.res#
				@bwHawksMiller: pitch.f0#, fmt.f3#, fmt.numFrames
				b3# = bwHawksMiller.res#
			endif
		endif

		if params.spectralMeasures <> 0
			@spec: params.windowLength, timeStep, fmt.numFrames, fmt.times#,
				... params.measureHarmonics, params.cpp, 
				... params.measureSlope, params.slopeUncorrected,
				... params.f0min, params.f0max,
				... pitch.f0#, fmt.f1#, fmt.f2#, fmt.f3#, b1#, b2#, b3#, fs
		endif
	endfor
endfor

writeInfoLine: times.start#
appendInfoLine: times.end#
appendInfoLine: times.numIntervals
appendInfoLine: times.labs$ [1]
appendInfoLine: timeStep
appendInfoLine: fmt.times#
appendInfoLine: pitch.f0#
appendInfoLine: b3#