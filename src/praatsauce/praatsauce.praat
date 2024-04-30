include read_params.praat
include get_times.praat
include get_formants.praat
include get_pitch.praat
include get_spectralMeasures.praat
include get_bwHawksMiller.praat
include get_HNR.praat
include get_CPP.praat
include combineMatrices.praat
include resample.praat
include extract_channel.praat
include zeroPadding.praat
include initiateTable.praat
include prepareTable.praat

@params: "params.csv"
@initiateTable: params.pitch, params.formant, params.harmonicAmplitude, 
	... params.harmonicAmplitudeUncorrected, params.bw, params.slope, 
	... params.slopeUncorrected, params.cpp, params.hnr, params.outputDir$, 
	... params.outputFile$, params.useTextGrid

Create Strings as file list: "wavs", params.inputDir$ + "*.wav"
wavsListID = selected("Strings")
Sort
numFile = Get number of strings

if params.useTextGrid <> 0
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

	if params.resample16kHz <> 0
		@resample
	endif

	if params.channel <> 0
		@extract_channel: params.channel
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
		#Extract part: times.start# [int], times.end# [int], "rectangular", 1, 0
		#snippetID = selected("Sound")
		
		## Need to get times# in a different way if formants are not measured
		## Same with numFrames

		frameNums# = { 0 }

		if params.measurePitch <> 0
			@pitch: timeStep, params.f0min, params.f0max, times.start# [int],
				... times.end# [int]
			frameNums# = combine# (frameNums#, { pitch.numFrames })
		endif


		if params.measureFormants <> 0 
			@fmt: params.measureBandwidths, timeStep, params.maxNumFormants, 
				... params.maxFormantHz, params.windowLength, 
				... params.preEmphFrom, times.start# [int], times.end# [int]
			frameNums# = combine# (frameNums#, { fmt.numFrames })
		endif

		if params.hnr <> 0
			@hnr: 500, timeStep, params.f0min, times.start# [int], 
				... times.end# [int]
			hnr05# = hnr.res#
			@hnr: 1500, timeStep, params.f0min, times.start# [int], 
				... times.end# [int]
			hnr15# = hnr.res#
			@hnr: 2500, timeStep, params.f0min, times.start# [int], 
				... times.end# [int]
			hnr25# = hnr.res#
			@hnr: 3500, timeStep, params.f0min, times.start# [int], 
				... times.end# [int]
			hnr35# = hnr.res#
			frameNums# = combine# (frameNums#, { hnr.numFrames })
		endif

		if params.cpp <> 0
			@cpp: timeStep, params.f0min, params.f0max, times.start# [int],
				... times.end# [int]
			frameNums# = combine# (frameNums#, { cpp.numFrames })
		endif

		mostFrames = max(frameNums#)

		if params.measurePitch <> 0
			@zeroPadding: pitch.f0#, pitch.numFrames, mostFrames
			f0# = zeroPadding.res#
			if pitch.numFrames = mostFrames
				times# = pitch.times#
			endif
		endif

		if params.measureFormants <> 0
			@zeroPadding: fmt.f1#, fmt.numFrames, mostFrames
			f1# = zeroPadding.res#
			@zeroPadding: fmt.f2#, fmt.numFrames, mostFrames
			f2# = zeroPadding.res#
			@zeroPadding: fmt.f3#, fmt.numFrames, mostFrames
			f3# = zeroPadding.res#
			if fmt.numFrames = mostFrames
				times# = fmt.times#
			endif
		endif

		if params.hnr <> 0
			@zeroPadding: hnr05#, hnr.numFrames, mostFrames
			hnr05# = zeroPadding.res#
			@zeroPadding: hnr15#, hnr.numFrames, mostFrames
			hnr15# = zeroPadding.res#
			@zeroPadding: hnr25#, hnr.numFrames, mostFrames
			hnr25# = zeroPadding.res#
			@zeroPadding: hnr35#, hnr.numFrames, mostFrames
			hnr35# = zeroPadding.res#
			if hnr.numFrames = mostFrames
				times# = hnr.times#
			endif
		endif

		if params.cpp <> 0
			@zeroPadding: cpp.res#, cpp.numFrames, mostFrames
			cpp# = zeroPadding.res#
			if cpp.numFrames = mostFrames
				times# = cpp.times#
			endif
		endif

		if params.requireBandwidths <> 0
			if params.bwHawksMiller = 0
				@zeroPadding: fmt.b1#, fmt.numFrames, mostFrames
				b1# = zeroPadding.res#
				@zeroPadding: fmt.b2#, fmt.numFrames, mostFrames
				b2# = zeroPadding.res#
				@zeroPadding: fmt.b3#, fmt.numFrames, mostFrames
				b3# = zeroPadding.res#
			else
				@bwHawksMiller: f0#, f1#, mostFrames
				b1# = bwHawksMiller.res#
				@bwHawksMiller: f0#, f2#, mostFrames
				b2# = bwHawksMiller.res#
				@bwHawksMiller: f0#, f3#, mostFrames
				b3# = bwHawksMiller.res#
			endif
		endif

		if params.spectralMeasures <> 0
			@spec: params.windowLength, timeStep, mostFrames, times#,
				... params.measureHarmonics, params.cpp, 
				... params.measureSlope, params.slopeUncorrected,
				... params.f0min, params.f0max,
				... f0#, f1#, f2#, f3#, b1#, b2#, b3#, fs,
				... times.start# [int], times.end# [int]
		endif

	## Compile results

	Create simple Matrix from values: "results", { (round#(times# * 1000) / 1000) }

	if params.pitch <> 0
		@combineMatrices: { f0# }
	endif
	if params.formant <> 0
		@combineMatrices: { f1#, f2#, f3# }
	endif
	if params.harmonicAmplitude <> 0
		@combineMatrices: { spec.h1c#, spec.h2c#, spec.h4c#, spec.a1c#, 
			... spec.a2c#, spec.a3c#, spec.h2ku#, spec.h5ku# }
	endif
	if params.harmonicAmplitudeUncorrected <> 0
		@combineMatrices: { spec.h1u#, spec.h2u#, spec.h4u#, spec.a1u#, 
			... spec.a2u#, spec.a3u# }
	endif
	if params.bw <> 0
		@combineMatrices: { b1#, b2#, b3# }
	endif
	if params.slope <> 0
		@combineMatrices: { spec.h1h2c#, spec.h2h4c#, spec.h1a1c#, spec.h1a2c#, 
			... spec.h1a3c#, spec.h2kh5ku# }
	endif
	if params.slopeUncorrected <>0
		@combineMatrices: { spec.h1h2u#, spec.h2h4u#, spec.h1a1u#, spec.h1a2u#, 
			... spec.h1a3u# }
	endif
	if params.cpp <> 0
		@combineMatrices: { cpp# }
	endif
	if params.hnr <> 0
		@combineMatrices: { hnr05#, hnr15#, hnr25#, hnr35# }
	endif

	@prepareTable: thisWav$, params.outputDir$, params.outputFile$, params.useTextGrid,
		... times.labs$ [int]

	endfor

	select soundID
	Remove

endfor

select wavsListID
if params.useTextGrid = 1
	plus tgListID
endif
Remove