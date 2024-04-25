## Next step: implement corrections, these are all uncorrected

procedure spec: .windowLength, .timeStep, .numFrames, .times#, .measureHarmonics, 
	... .measureCPP, .measureSlope, .f0min, .f0max, .f0#, .f1#, .f2#, .f3#

snippetID = selected("Sound")
To Spectrogram: .windowLength, 5500, .timeStep, 20, "Gaussian"
spectrogramID = selected("Spectrogram")

if .measureHarmonics <> 0
	.h1db# = zero# (.numFrames)
	.h2db# = zero# (.numFrames)
	.h4db# = zero# (.numFrames)
	.a1db# = zero# (.numFrames)
	.a2db# = zero# (.numFrames)
	.a3db# = zero# (.numFrames)
	.twokdb# = zero# (.numFrames)
	.fivekdb# = zero# (.numFrames)

	lowerbh1# = .f0# - (.f0# / 10)
	upperbh1# = .f0# + (.f0# / 10)
	lowerbh2# = (.f0# * 2) - (.f0# / 10)
	upperbh2# = (.f0# * 2) + (.f0# / 10)
	lowerbh4# = (.f0# * 4) - (.f0# / 10)
	upperbh4# = (.f0# * 4) + (.f0# / 10)

	lowerba1# = .f1# - (.f1# * 0.2)
	upperba1# = .f1# + (.f1# * 0.2)
	lowerba2# = .f2# - (.f2# * 0.1)
	upperba2# = .f2# + (.f2# * 0.1)
	lowerba3# = .f3# - (.f3# * 0.1)
	upperba3# = .f3# + (.f3# * 0.1)
endif

if .measureCPP <> 0 
	.cpp# = zero# (.numFrames)
endif

for frame from 1 to .numFrames
	select spectrogramID
	To Spectrum (slice): .times# [frame]
	spectrumID = selected("Spectrum")
	To Ltas (1-to-1)
	ltasID = selected("Ltas")
	select spectrumID
	To PowerCepstrum
	cepstrumID = selected("PowerCepstrum")
	
	## is there any particular reason why our trend is 'straight' and not 
	## 'exponential decay'? I think this is a matter of whether CPP is determined
	## from a linear regression on a linear scale or log scale
	## the documentation is in favor of exponential decay

	if .measureCPP <> 0
		.cpp# [frame] = Get peak prominence: .f0min, .f0max, "parabolic", 0.001, 
			... 0, "Straight", "Robust"
	endif

	## I use the user-specified f0min and f0max here instead of 50 and 550,
	## which is used in legacy praatsauce

	if .measureHarmonics <> 0
		peakQuef = Get quefrency of peak: .f0min, .f0max, "parabolic"
		peakFreq = 1 / peakQuef

		select ltasID
		.twokdb# [frame] = Get maximum: (2000 - peakFreq), (2000 + peakFreq), "cubic"
		.fivekdb# [frame] = Get maximum: (5000 - peakFreq), (5000 + peakFreq), "cubic"

		if (.f0# [frame] <> undefined)
			.h1db# [frame] = Get maximum: lowerbh1# [frame], upperbh1# [frame], 
				... "none"
			.h2db# [frame] = Get maximum: lowerbh2# [frame], upperbh2# [frame], 
				... "none"
			.h4db# [frame] = Get maximum: lowerbh4# [frame], upperbh4# [frame], 
				... "none"
			.a1db# [frame] = Get maximum: lowerba1# [frame], upperba1# [frame], 
				... "none"
			.a2db# [frame] = Get maximum: lowerba2# [frame], upperba2# [frame], 
				... "none"
			.a3db# [frame] = Get maximum: lowerba3# [frame], upperba3# [frame], 
				... "none"
		else
			.h1db# [frame] = 0
			.h2db# [frame] = 0
			.h4db# [frame] = 0
			.a1db# [frame] = 0
			.a2db# [frame] = 0
			.a3db# [frame] = 0
		endif

	endif

	select spectrumID
	plus ltasID
	plus cepstrumID
	Remove

endfor

if .measureSlope <> 0
	.h1h2u# = .h1db# - .h2db#
	.h2h4u# = .h2db# - .h4db#
	.h1a1u# = .h1db# - .a1db#
	.h1a2u# = .h1db# - .a2db#
	.h1a3u# = .h1db# - .a3db#
	.h2kh5ku# = .twokdb# - .fivekdb#
endif

endproc