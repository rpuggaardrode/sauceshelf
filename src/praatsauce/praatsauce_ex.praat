### This is a minimal working example
### Doesn't loop through folders or anything like that,
### just assumes that a sound file is already read in
### and selected.

### Also doesn't do any of the corrections.

## Set some basic values

timeStep = 0.005
windowLength = 0.025
f0min = 50
f0max = 300

## Select sound and get duration

soundID = selected("Sound")
dur = Get total duration

## Create formant object
## Extract times, formants, bandwidths as vectors

To Formant (burg): timeStep, 5, 5500, windowLength, 50
formantID = selected("Formant")
times# = List all frame times
numFrames = Get number of frames
Down to Table: 0, 1, 3, 0, 1, 0, 3, 1
tableID = selected("Table")
f1# = Get all numbers in column: "F1(Hz)"
f2# = Get all numbers in column: "F2(Hz)"
f3# = Get all numbers in column: "F3(Hz)"
b1# = Get all numbers in column: "B1(Hz)"
b2# = Get all numbers in column: "B2(Hz)"
b3# = Get all numbers in column: "B3(Hz)"
select formantID
plus tableID
Remove

## Create pitch object and extract values from the same time frame as formants

select soundID
To Pitch (filtered ac): 0, 50, 800, 15, 0, 0.03, 0.09, 0.5, 0.055, 0.35, 0.14
pitchID = selected("Pitch")
f0# = List values at times: times#, "Hertz", "linear"
select pitchID
Remove

## Generate HNR05 vector
## It's fiddly, in part because it'll for some reason generate
## one more value at the beginning & end of the sound file than needed

## This should be a procedure, almost the exact same thing has to happen
## to get the HNR15, HNR25, HNR35 vectors

select soundID
Filter (pass Hann band): 0, 500, 100
filter05ID = selected("Sound")
To Harmonicity (cc): timeStep, f0min, 0.1, 1.0
hnr05ID = selected("Harmonicity")
To Matrix
matrix05ID = selected("Matrix")
Transpose
tm05ID = selected("Matrix")
To TableOfReal
tor05ID = selected("TableOfReal")
Remove row (index): 1
Remove row (index): numFrames + 1
To Matrix
fm05ID = selected("Matrix")
hnr05# = Get all values in column: 1
select filter05ID
plus hnr05ID
plus matrix05ID
plus tm05ID
plus tor05ID
plus fm05ID
Remove

## Create spectrogram object
## Subsequent spectra are grabbed directly from this object

select soundID
To Spectrogram: windowLength, 5000, timeStep, 20, "Gaussian"
spectrogramID = selected("Spectrogram")

## Assign empty vectors

cpp# = zero# (numFrames)
twokdb# = zero# (numFrames)
fivekdb# = zero# (numFrames)
h1db# = zero# (numFrames)
h2db# = zero# (numFrames)
h4db# = zero# (numFrames)
a1db# = zero# (numFrames)
a2db# = zero# (numFrames)
a3db# = zero# (numFrames)

## Vectorize what can be vectorized

p10f0# = f0# / 10

lowerbh1# = f0# - p10f0#
upperbh1# = f0# + p10f0#
lowerbh2# = (f0# * 2) - p10f0#
upperbh2# = (f0# * 2) + p10f0#
lowerbh4# = (f0# * 4) - p10f0#
upperbh4# = (f0# * 4) + p10f0#

lowerba1# = f1# - (f1# * 0.2)
upperba1# = f1# + (f1# * 0.2)
lowerba2# = f2# - (f2# * 0.1)
upperba2# = f2# + (f2# * 0.1)
lowerba3# = f3# - (f3# * 0.1)
upperba3# = f3# + (f3# * 0.1)

## I fear the rest still has to be done in a for loop.
## The LTAS could in principle be directly extracted from the
## spectrogram object as far as I can tell,
## but with the cepstrum I don't see that there's any way around it.

# Double check some of this.
# Why is the pitch range in peak_quef not just f0min and f0max?

for i from 1 to numFrames
	select spectrogramID
	To Spectrum (slice): times# [i]
	spectrumID = selected("Spectrum")
	To Ltas (1-to-1)
	ltasID = selected("Ltas")
	select spectrumID
	To PowerCepstrum
	cepstrumID = selected("PowerCepstrum")
	cpp# [i] = Get peak prominence: f0min, f0max, "parabolic", 0.001, 0, "Straight", "Robust"
	peakQuef = Get quefrency of peak: 50, 550, "parabolic"
	peakFreq = 1 / peakQuef
	lowerb2k = 2000 - peakFreq
	upperb2k = 2000 + peakFreq
	lowerb5k = 5000 - peakFreq
	upperb5k = 5000 + peakFreq

	select ltasID
	twokdb [i] = Get maximum: lowerb2k, upperb2k, "cubic"
	fivekdb [i] = Get maximum: lowerb5k, upperb5k, "cubic"

	if (f0# [i] <> undefined)
		h1db# [i] = Get maximum: lowerbh1# [i], upperbh1# [i], "none"
		h2db# [i] = Get maximum: lowerbh2# [i], upperbh2# [i], "none"
		h4db# [i] = Get maximum: lowerbh4# [i], upperbh4# [i], "none"
		#h1hz = Get frequency of maximum: lowerbh1, upperbh1, "none"
		#h2hz = Get frequency of maximum: lowerbh2, upperbh2, "none"
		#h4hz = Get frequency of maximum: lowerbh4, upperbh4, "none"

		a1db# [i] = Get maximum: lowerba1# [i], upperba1# [i], "none"
		a2db# [i] = Get maximum: lowerba2# [i], upperba2# [i], "none"
		a3db# [i] = Get maximum: lowerba3# [i], upperba3# [i], "none"
		#a1hz = Get frequency of maximum: lowerba1, upperba1, "none"
		#a2hz = Get frequency of maximum: lowerba2, upperba2, "none"
		#a4hz = Get frequency of maximum: lowerba4, upperba4, "none"
	else
		h1db# [i] = 0
		h2db# [i] = 0
		h4db# [i] = 0
		a1db# [i] = 0
		a2db# [i] = 0
		a3db# [i] = 0
	endif

	select spectrumID
	plus ltasID
	plus cepstrumID
	Remove
endfor

## Calculate the slope measures

h1h2# = h1db# - h2db#
h2h4# = h2db# - h4db#
h1a1# = h1db# - a1db#
h1a2# = h1db# - a2db#
h1a3# = h1db# - a3db#
h2kh5k# = twokdb# - fivekdb#

## Combine vectors into a matrix, convert to table, save as csv
## The renaming business can be done in a loop

Create simple Matrix from values: "Results", {times#, f0#, f1#, f2#, f3#, b1#, b2#, b3#,
	... h1db#, h2db#, h4db#, twokdb#, fivekdb#, a1db#, a2db#, a3db#,
	... h1h2#, h2h4#, h1a1#, h1a2#, h1a3#, h2kh5k#, hnr05#}
matrixID = selected("Matrix")
Transpose
tmatrixID = selected("Matrix")
To TableOfReal
torID = selected("TableOfReal")
To Table: "rowLabel"
tableID = selected("Table")
Remove column: "rowLabel"
Rename column (by number): 1, "t"
Rename column (by number): 2, "F0"
Rename column (by number): 3, "F1"
Rename column (by number): 4, "F2"
Rename column (by number): 5, "F3"
Rename column (by number): 6, "B1"
Rename column (by number): 7, "B2"
Rename column (by number): 8, "B3"
Rename column (by number): 9, "H1u"
Rename column (by number): 10, "H2u"
Rename column (by number): 11, "H4u"
Rename column (by number): 12, "H2Ku"
Rename column (by number): 13, "H5Ku"
Rename column (by number): 14, "A1u"
Rename column (by number): 15, "A2u"
Rename column (by number): 16, "A3u"
Rename column (by number): 17, "H1-H2u"
Rename column (by number): 18, "H1-H4u"
Rename column (by number): 19, "H1-A1u"
Rename column (by number): 20, "H1-A2u"
Rename column (by number): 21, "H1-A3u"
Rename column (by number): 22, "H2K-H5Ku"
Rename column (by number): 23, "HNR05"

Save as comma-separated file: "sauceplay.csv"

## Clean-up

select spectrogramID
plus matrixID
plus tmatrixID
plus torID
plus tableID
Remove
