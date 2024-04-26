procedure combineMatrices: .add##

orgMatrixID = selected("Matrix")

Create simple Matrix from values: "results", .add##
newMatrixID = selected("Matrix")
plus orgMatrixID
Merge (append rows)
combinedMatrixID = selected("Matrix")

select orgMatrixID
plus newMatrixID
Remove

select combinedMatrixID

endproc