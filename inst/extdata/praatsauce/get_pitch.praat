### Get pitch values

include extract_snippet.praat
include restrictInterval.praat

procedure pitch: .timeStep, .f0min, .f0max, .start, .end, .method, .windowShape,
  ... .maxNoCandidates, .silenceThreshold, .voicingThreshold, .octaveCost,
  ... .octaveJumpCost, .voicedUnvoicedCost, .killOctaveJumps,
  ... .save, .saveDir$, .read, .readDir$, .basefn$

## extract padded snippet
## one analysis window corresponds ROUGHLY to three pitch cycles (i.e.
## 3 * pitch floor), because for some reason this number is rounded in some
## arcane fashion.

snd = selected("Sound")
dur = Get total duration

if .windowShape = 0
  cyclesPerWindow = 3
else
  cyclesPerWindow = 6
endif

@snippet: .start, .end, ((cyclesPerWindow * (1 / .f0min)) / 2) + (.timeStep / 2)
snippet = selected("Sound")

if .read = 0

  snippetDur = Get total duration
  if snippetDur < (1 / .f0min) * cyclesPerWindow
    .f0min = ((1 / snippetDur) * cyclesPerWindow) + 1
  endif

  if .method = 0

    ## untouched argument is the undocumented attenuation at ceiling preprocessing

    pitchOrg = To Pitch (filtered ac): .timeStep, .f0min, .f0max,
      ... .maxNoCandidates, .windowShape, 0.03, .silenceThreshold,
      ... .voicingThreshold, .octaveCost, .octaveJumpCost, .voicedUnvoicedCost

  elsif .method = 1

    pitchOrg = To Pitch (raw cc): .timeStep, .f0min, .f0max, .maxNoCandidates,
      ... .windowShape, .silenceThreshold, .voicingThreshold, .octaveCost,
      ... .octaveJumpCost, .voicedUnvoicedCost

  else

    pitchOrg = To Pitch (raw ac): .timeStep, .f0min, .f0max,
    ... .maxNoCandidates, .windowShape, .silenceThreshold, .voicingThreshold,
    ... .octaveCost, .octaveJumpCost, .voicedUnvoicedCost

  endif

  if .save <> 0

    Save as text file: .saveDir$ + .basefn$ + ".Pitch"

  endif

else

  pitchOrg = Read from file: .readDir$ + .basefn$ + ".Pitch"

endif

## Kill octave jumps
## this procedure also removes candidates from the resulting object,
## which is why it is done before potentially saving a pitch file
## seems nonsensical to write pitch files to disk without candidates

if .killOctaveJumps <> 0

  pitchFilt = Kill octave jumps

endif

.meanF0 = Get mean: 0, 0, "Hertz"

## grab all values as a vector

.f0# = List values in all frames: "Hertz"

## grab frame times and number of frames

.times# = List all frame times
.numFrames = Get number of frames

if .start > 0 | .end < dur

  @restrictInterval: .times#, .f0#, .start, .end

  .times# = restrictInterval.newTimes#
  .f0# = restrictInterval.newVals#
  .numFrames = size(restrictInterval.newTimes#)

endif

## clean up

removeObject: pitchOrg, snippet
if .killOctaveJumps <> 0
  removeObject: pitchFilt
endif

selectObject: snd

endproc
