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

endproc

