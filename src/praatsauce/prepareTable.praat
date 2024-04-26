procedure prepareTable: .fileName$

finalMatrixID = selected("Matrix")
Transpose
tmatrixID = selected("Matrix")
To TableOfReal
torID = selected("TableOfReal")
To Table: "x"
tableID = selected("Table")

nRow = Get number of rows
for r from 1 to nRow
	Set string value: r, "x", .fileName$
endfor

results$ = List: 0
Create Strings from tokens: "results", results$, "'newline$'"
stringsID = selected("Strings")
Remove string: 1
Replace all: "$", "\n", 0, "regular expressions"

results$# = List all strings
writeFile: "test.txt", results$#

plus tableID
plus torID
plus tmatrixID
plus finalMatrixID
Remove

endproc