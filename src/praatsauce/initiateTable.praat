procedure initiateTable: .pitch, .formants, .harmonicAmplitude, .harmonicAmplitudeUncorrected,
	... .bw, .slope, .slopeUncorrected, .cpp, .hnr, .outputDir$, .outputFile$

firstLine$ = "file t"

if .pitch <> 0
	firstLine$ = firstLine$ + " f0"
endif

if .formants <> 0
	firstLine$ = firstLine$ + " F1 F2 F3"
endif

if .harmonicAmplitude <> 0
	firstLine$ = firstLine$ + " H1c H2c H4c A1c A2c A3c H2Ku H5Ku"
endif

if .harmonicAmplitudeUncorrected <> 0
	firstLine$ = firstLine$ + " H1u H2u H4u A1u A2u A3u"
endif

if .bw <> 0
	firstLine$ = firstLine$ + " B1 B2 B3"
endif

if .slope <> 0
	firstLine$ = firstLine$ + " H1H2c H2H4c H1A1c H1A2c H1A3c H2KH5Ku"
endif

if .slopeUncorrected <> 0
	firstLine$ = firstLine$ + " H1H2u H2H4u H1A1u H1A2u H1A3u"
endif

if .cpp <> 0
	firstLine$ = firstLine$ + " CPP"
endif

if .hnr <> 0
	firstLine$ = firstLine$ + " HNR05 HNR15 HNR25 HNR35"
endif

Create Table with column names: "init", 0, firstLine$
tableID = selected("Table")

Save as tab-separated file: "'.outputDir$''.outputFile$'"
Remove

endproc