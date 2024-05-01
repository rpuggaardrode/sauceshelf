## import all the procedures that the script relies on

include read_params.praat
include get_times.praat
include get_formants.praat
include get_pitch.praat
include get_spectralMeasures.praat
include get_bwHawksMiller.praat
include get_HNR.praat
include get_CPP.praat
include get_intensity.praat
include combineMatrices.praat
include resample.praat
include extract_channel.praat
include zeroPadding.praat
include initiateTable.praat
include prepareTable.praat

## process parameters file
  ## this should probably be a "form"?

@params: "params.csv"

## create the first line of the results file

@initiateTable: params.pitch, params.formant, params.harmonicAmplitude,
	... params.harmonicAmplitudeUncorrected, params.bw, params.slope,
	... params.slopeUncorrected, params.cpp, params.hnr, params.intensity,
	... params.outputDir$, params.outputFile$, params.useTextGrid

## get list of files in the inputDir
## not sure if the sorting is actually necessary -- legacy PS does it

  ## bulk processing should probably be optional if we want this to play well
  ## with emuR

Create Strings as file list: "wavs", params.inputDir$ + "*.wav"
wavsListID = selected("Strings")
Sort
numFile = Get number of strings

## if using TextGrid, get list of TGs in the inputDir and sort them
  ## in the current stage, this'll crash if there are more sound files than
  ## TextGrids I reckon. should this get fixed or should we just get the script
  ## to pop an error message?

if params.useTextGrid <> 0
	Create Strings as file list: "grids", params.inputDir$ + "*.TextGrid"
	tgListID = selected("Strings")
	Sort
	numTG = Get number of strings
endif

## initiate loop through sound files

for thisFile from 1 to numFile

  ## read sound file

	select wavsListID
	thisWav$ = Get string: thisFile
	Read from file: params.inputDir$ + thisWav$
	soundID = selected("Sound")

  ## resample if needed
  ## not done by default, probably something VS does because it makes STRAIGHT
  ## work faster

	if params.resample16kHz <> 0
		@resample
	endif

	## extract channel if needed

	if params.channel <> 0
		@extract_channel: params.channel
	endif

  ## need sampling frequency later for correcting amplitudes

	fs = Get sampling frequency

  ## find out which portion(s) of the sound file to analyze

	@times: thisWav$, params.inputDir$, params.includeTheseLabels$,
		... params.useTextGrid, params.intervalTier, params.windowLength

		## should probably have a check here so following for loop only happens when
		## times.numIntervals <> 0

  ## initiate loop through intervals (this is only a "real" loop when using
  ## TextGrids, otherwise times.numIntervals always == 1)

	for int from 1 to times.numIntervals

	  ## make sure sound is selected at the beginning of this loop

		select soundID

    ## if grabbing measures at equidistant points, set time step accordingly.

      ## not sure this works as intended. looks like it results in missing
      ## values at the edges of intervals which shouldn't be the case. will
      ## have to look further into this.

		intervalDur = times.end# [int] - times.start# [int]
		if params.intervalEquidistant <> 0
			timeStep = intervalDur / params.intervalEquidistant
		else
			timeStep = params.intervalFixed
		endif

    ## initiate vector containing the number of frames in each of the derived
    ## signal objects. the first number is just a dummy. this is to make sure
    ## that we keep all measures from the signal object with the MOST measures,
    ## and subsequently append zeros to the other signal objects so they're all
    ## the same length.

		frameNums# = { 0 }

    ## get pitch values

		if params.measurePitch <> 0
			@pitch: timeStep, params.f0min, params.f0max, times.start# [int],
				... times.end# [int]
			frameNums# = combine# (frameNums#, { pitch.numFrames })
		endif

    ## get formant values

		if params.measureFormants <> 0
			@fmt: params.measureBandwidths, timeStep, params.maxNumFormants,
				... params.maxFormantHz, params.windowLength,
				... params.preEmphFrom, times.start# [int], times.end# [int],
				... params.f1ref, params.f2ref, params.f3ref
			frameNums# = combine# (frameNums#, { fmt.numFrames })
		endif

		## get hnr values

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

		## get cpp

		if params.cpp <> 0
			@cpp: timeStep, params.f0min, params.f0max, times.start# [int],
				... times.end# [int]
			frameNums# = combine# (frameNums#, { cpp.numFrames })
		endif

    ## get intensity

		if params.intensity <> 0
			@rms: timeStep, params.f0min, times.start# [int], times.end# [int]
			frameNums# = combine# (frameNums#, { rms.numFrames })
		endif

    ## which of the derived signals has the most frames?

		mostFrames = max(frameNums#)

    ## go through each of the signals, and pad them with zeros if they are
    ## NOT the signal with the most frames. base times vector on the signal
    ## with the most frames.

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

		if params.intensity <> 0
			@zeroPadding: rms.res#, rms.numFrames, mostFrames
			rms# = zeroPadding.res#
			if rms.numFrames = mostFrames
				times# = rms.times#
			endif
		endif

		## do zero padding for formant bandwidths if using empirical bandwidths,
		## otherwise using Hawks-Miller formula to calculate bandwidths

		  ## could be an issue here -- the Hawks-Miller bandwidths look like they're
		  ## misbehaving when f0 or fmts are 0 instead of --undefined--.
		  ## should actually just do --undefined-- padding instead of zero padding,
		  ## but I don't know how!

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

		## get harmonic amplitudes and spectral slope measures

		if params.spectralMeasures <> 0
			@spec: params.windowLength, timeStep, mostFrames, times#,
				... params.measureSlope, params.slopeUncorrected,
				... params.f0min, params.f0max,
				... f0#, f1#, f2#, f3#, b1#, b2#, b3#, fs,
				... times.start# [int], times.end# [int]
		endif

	## compile the results.
	## round times vector to three decimals (has to be done in a roundabout way)

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
	if params.intensity <> 0
		@combineMatrices: { rms# }
	endif

	## convert matrix to table so we can add string values

	@prepareTable: thisWav$, params.outputDir$, params.outputFile$, params.useTextGrid,
		... times.labs$ [int]

	endfor

  ## end loop through intervals

	select soundID
	Remove

## end loop through sound files

endfor

## clean up

select wavsListID
if params.useTextGrid = 1
	plus tgListID
endif
Remove
