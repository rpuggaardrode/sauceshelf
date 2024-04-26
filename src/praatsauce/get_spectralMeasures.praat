include correctIseli.praat

procedure spec: .windowLength, .timeStep, .numFrames, .times#, .measureHarmonics,
	... .measureCPP, .measureSlope, .measureSlopeUncorrected,
	... .f0min, .f0max, .f0#, .f1#, .f2#, .f3#, .b1#, .b2#, .b3#, .fs

snippetID = selected("Sound")
To Spectrogram: .windowLength, 5500, .timeStep, 20, "Gaussian"
spectrogramID = selected("Spectrogram")

if .measureHarmonics <> 0
	.h1u# = zero# (.numFrames)
	.h2u# = zero# (.numFrames)
	.h4u# = zero# (.numFrames)
	.a1u# = zero# (.numFrames)
	.a2u# = zero# (.numFrames)
	.a3u# = zero# (.numFrames)
	.h2ku# = zero# (.numFrames)
	.h5ku# = zero# (.numFrames)

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
		.h2ku# [frame] = Get maximum: (2000 - peakFreq), (2000 + peakFreq), "cubic"
		.h5ku# [frame] = Get maximum: (5000 - peakFreq), (5000 + peakFreq), "cubic"

		if (.f0# [frame] <> undefined)
			.h1u# [frame] = Get maximum: lowerbh1# [frame], upperbh1# [frame],
				... "none"
			.h2u# [frame] = Get maximum: lowerbh2# [frame], upperbh2# [frame],
				... "none"
			.h4u# [frame] = Get maximum: lowerbh4# [frame], upperbh4# [frame],
				... "none"
			.a1u# [frame] = Get maximum: lowerba1# [frame], upperba1# [frame],
				... "none"
			.a2u# [frame] = Get maximum: lowerba2# [frame], upperba2# [frame],
				... "none"
			.a3u# [frame] = Get maximum: lowerba3# [frame], upperba3# [frame],
				... "none"
		else
			.h1u# [frame] = 0
			.h2u# [frame] = 0
			.h4u# [frame] = 0
			.a1u# [frame] = 0
			.a2u# [frame] = 0
			.a3u# [frame] = 0
		endif

	endif

	select spectrumID
	plus ltasID
	plus cepstrumID
	Remove

endfor

## Not sure if I understand how this theoretically works for H2k and H5k
## Legacy praatsauce corrects for H2k by using the third formant

if .measureHarmonics <> 0
	@correctIseli: .f0#, .f1#, .b1#, .fs
	.h1c# = .h1u# - correctIseli.res#
	@correctIseli: .f0#, .f2#, .b2#, .fs
	.h1c# = .h1c# - correctIseli.res#
	@correctIseli: 2 * .f0#, .f1#, .b1#, fs
	.h2c# = .h2u# - correctIseli.res#
	@correctIseli: 2 * .f0#, .f2#, .b2#, fs
	.h2c# = .h2c# - correctIseli.res#
	@correctIseli: 4 * .f0#, .f1#, .b1#, fs
	.h4c# = .h4u# - correctIseli.res#
	@correctIseli: 4 * .f0#, .f2#, .b2#, fs
	.h4c# = .h4c# - correctIseli.res#
	@correctIseli: .f1#, .f1#, .b1#, fs
	.a1c# = .a1u# - correctIseli.res#
	@correctIseli: .f1#, .f2#, .b2#, fs
	.a1c# = .a1c# - correctIseli.res#
	@correctIseli: .f2#, .f1#, .b1#, fs
	.a2c# = .a2u# - correctIseli.res#
	@correctIseli: .f2#, .f2#, .b2#, fs
	.a2c# = .a2c# - correctIseli.res#
	@correctIseli: .f3#, .f1#, .b1#, fs
	.a3c# = .a3u# - correctIseli.res#
	@correctIseli: .f3#, .f2#, .b2#, fs
	.a3c# = .a3c# - correctIseli.res#
	@correctIseli: .f3#, .f3#, .b3#, fs
	.a3c# = .a3c# - correctIseli.res#
endif

if .measureSlope <> 0
	.h1h2c# = .h1c# - .h2c#
	.h2h4c# = .h2c# - .h4c#
	.h1a1c# = .h1c# - .a1c#
	.h1a2c# = .h1c# - .a2c#
	.h1a3c# = .h1c# - .a3c#
endif

if .measureSlopeUncorrected <> 0
	.h1h2u# = .h1u# - .h2u#
	.h2h4u# = .h2u# - .h4u#
	.h1a1u# = .h1u# - .a1u#
	.h1a2u# = .h1u# - .a2u#
	.h1a3u# = .h1u# - .a3u#
	.h2kh5ku# = .h2ku# - .h5ku#
endif

select spectrogramID
Remove

select snippetID

endproc
