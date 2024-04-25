procedure pitch: .timeStep, .f0min, .f0max, .times#

snippetID = selected("Sound")
To Pitch (filtered ac): .timeStep, .f0min, .f0max, 15, 0, 0.03, 0.09, 0.5, 0.005, 0.35, 0.14
pitchOrgID = selected("Pitch")
Kill octave jumps
pitchFiltID = selected("Pitch")
.f0# = List values at times: .times#, "Hertz", "linear"
select pitchOrgID
plus pitchFiltID
Remove

select snippetID

endproc
