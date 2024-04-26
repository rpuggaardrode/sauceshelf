procedure hnr: .maxFreq, .timeStep, .f0min, .numFrames

snippetID = selected("Sound")

Filter (pass Hann band): 0, .maxFreq, 100
filterID = selected("Sound")

## Legacy praatsauce has 0.1 instead of 0.00001
## This is a silence threshold, which causes most low frequencies to be treated
## as silent (= -200dB). That doesn't strike me as what we want

To Harmonicity (cc): .timeStep, .f0min, 0.00001, 1.0
hnrID = selected("Harmonicity")
To Matrix
matrixID = selected("Matrix")
Transpose
tmID = selected("Matrix")
To TableOfReal
torID = selected("TableOfReal")
Remove row (index): 1
Remove row (index): .numFrames + 1
To Matrix
fmID = selected("Matrix")
.res# = Get all values in column: 1

select filterID
plus hnrID
plus matrixID
plus tmID
plus torID
plus fmID
Remove

select snippetID

endproc