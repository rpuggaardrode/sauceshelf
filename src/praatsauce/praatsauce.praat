include read_params.praat
include get_times.praat
include get_formants.praat
include get_pitch.praat
include get_spectralMeasures.praat
include get_bwHawksMiller.praat
include get_HNR.praat
include combineMatrices.praat
include resample.praat
include extract_channel.praat
include prepareTable.praat

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

		if params.hnr <> 0
			@hnr: 500, timeStep, params.f0min, fmt.numFrames
			hnr05# = hnr.res#
			@hnr: 1500, timeStep, params.f0min, fmt.numFrames
			hnr15# = hnr.res#
			@hnr: 2500, timeStep, params.f0min, fmt.numFrames
			hnr25# = hnr.res#
			@hnr: 3500, timeStep, params.f0min, fmt.numFrames
			hnr35# = hnr.res#
		endif
	
	select snippetID
	Remove

	## Compile results

	Create simple Matrix from values: "results", { fmt.times# }

	if params.pitch <> 0
		@combineMatrices: { pitch.f0# }
	endif
	if params.formant <> 0
		@combineMatrices: { fmt.f1#, fmt.f2#, fmt.f3# }
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
		@combineMatrices: { spec.cpp# }
	endif
	if params.hnr <> 0
		@combineMatrices: { hnr05#, hnr15#, hnr25#, hnr35# }
	endif

	@prepareTable: thisWav$

	endfor

	select soundID
	Remove

endfor

select wavsListID
if params.useTextGrid = 1
	plus tgListID
endif
Remove

#Create simple Matrix from values: "results", { fmt.times#, pitch.f0#, fmt.f1#, fmt.f2#, 
#	... fmt.f3#, b1#, b2#, b3#, spec.h1u#, spec.h2u#, spec.h4u#, spec.twoku#, 
#	... spec.fiveku#, spec.a1u#, spec.a2u#, spec.a3u#, spec.h1h2u#, spec.h2h4u#,
#	... spec.h2kh5ku#, spec.h1c#, spec.h2c#, spec.h4c#, spec.a1c#, spec.a2c#, 
#	... spec.a3c#, spec.h1h2c#, spec.h2h4c#, spec.h1a1c#, spec.h1a2c#, spec.h1a3c#,
#	... spec.cpp#, hnr05#, hnr15#, hnr25#, hnr35# }
#Transpose
#To TableOfReal
#To Table: "rowLabel"
#Remove column: "rowLabel"