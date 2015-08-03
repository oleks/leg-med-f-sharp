open LinAlgFct

let v0 = Vector( [1.0 .. 5.0] )
printfn "v0 in R^%d =\n%s" (length v0) (VectorToString v0)

let v1 = - v0
printfn " - v0 = v1 in R^%d =\n%s" (length v1) (VectorToString v1)

let v2 = v0 + Vector( [0.0 .. 0.1 .. 0.4] )
printfn "V0 + Vector( [0.0 .. 0.1 .. 0.4] ) = v2 in R^%d =\n%s" (length v2) (VectorToString v2)

let v3 = v0 - Vector( [0.0 .. 0.1 .. 0.4] )
printfn "v0 - Vector( [0.0 .. 0.1 .. 0.4] ) = v3 in R^%d =\n%s" (length v3) (VectorToString v3)

let v4 = 3.0*v3
printfn "3.0*v3 = v4 in R^%d =\n%s" (length v4) (VectorToString v4)

let v5 = v3*v4
printfn "v3*v4 = v5 = %f" v5

let v6 = VectorNorm v3
printfn "VectorNorm v3 = v6 = %f" v6

let n = length v0
let v7 = VectorGetSlice (v0, 0, n - 1)
printfn "VectorGetSlice (v0, 0, (length v0) - 1) = v7 = %s" (VectorToString v7)

let v8 = VectorGetSlice (v0, 0, 0)
printfn "VectorGetSlice (v0, 0, 0) = v8 = %s" (VectorToString v8)

let v9 = VectorGetSlice (v0, 1, 2)
printfn "VectorGetSlice (v0, 1, 2) = v9 = %s" (VectorToString v9)

printfn "VectorRandom 4 = %s"  (VectorToString (VectorRandom 4))

let m0 = Matrix( [[1.0; 2.0];[3.0;4.0]] )
printfn "m0 in R^(%dx%d) =\n%s" (rows m0) (cols m0) (MatrixToString m0)

let m1 = 3.0*m0
printfn "3.0*m0 = m1 in R^(%dx%d) =\n%s" (rows m1) (cols m1) (MatrixToString m1)

let m2 = - m0
printfn " - m0 = m2 in R^(%dx%d) =\n%s" (rows m2) (cols m2) (MatrixToString m2)

let m3 = m0 + Matrix( [[5.0; 6.0];[7.0;8.0]] )
printfn "m0 + Matrix( [[5.0; 6.0];[7.0;8.0]] ) = m3 in R^(%dx%d) =\n%s" (rows m3) (cols m3) (MatrixToString m3)

let m4 = m0 - Matrix( [[5.0; 6.0];[7.0;8.0]] )
printfn "m0 - Matrix( [[5.0; 6.0];[7.0;8.0]] ) = m4 in R^(%dx%d) =\n%s" (rows m4) (cols m4) (MatrixToString m4)

let m5 = transpose m0
printfn "transpose m0 = m5 in R^(%dx%d) =\n%s" (rows m5) (cols m5) (MatrixToString m5)

let m6 = m0*m5
printfn "m0*m5 = m6 in R^(%dx%d) =\n%s" (rows m6) (cols m6) (MatrixToString m6)

let r = rows m0
let c = cols m0
printfn "MatrxGetSlice (m0, 0, r - 1, 0, c - 1) = %s" (MatrixToString (MatrixGetSlice (m0, 0, r - 1, 0, c - 1)))

printfn "MatrixGetSlice (m0, 0, 0, 0, 0)  = %s" (MatrixToString (MatrixGetSlice (m0, 0, 0, 0, 0)))

printfn "MatrixGetSlice (m0, 1, 1, 0, 1)  = %s" (MatrixToString (MatrixGetSlice (m0, 1, 1, 0, 1)))

printfn "RowConcat (m0, m1) = %s" (MatrixToString (RowConcat (m0, m1)))

printfn "ColConcat (m0, m1) = %s" (MatrixToString (ColConcat (m0, m1)))

printfn "Minor (m0, 0, 0) = %s" (MatrixToString (Minor (m0, 0, 0)))

printfn "Minor (m0, 1, 0) = %s" (MatrixToString (Minor (m0, 1, 0)))

printfn "Minor (m0, - 1, 0) = %s" (MatrixToString (Minor (m0, - 1, 0)))

printfn "Minor (m0, 0, 1) = %s" (MatrixToString (Minor (m0, 0, 1)))

printfn "Minor (m0, 0, - 1) = %s" (MatrixToString (Minor (m0, 0, - 1)))

printfn "Det m0 = %f" (Det m0)

let m7 = ColConcat (RowConcat (m0, m1), RowConcat (m2, m3))
printfn "m7 = ColConcat (RowConcat (m0, m1), RowConcat (m2, m3)) = %s" (MatrixToString m7)
printfn "Det m7 = %f" (Det m7)

let v10 = Cramer (m0,Vector( [1.0; 2.0] ))
printfn "v10 = Cramer (m0,Vector( [1, 2] )) = %s" (VectorToString v10)
printfn "VectorToMatrix v10 = %s" (MatrixToString (VectorToMatrix v10))
printfn "m0*v10 = %s" (MatrixToString (m0*v10))

printfn "VectorOne 3 = %s" (VectorToString (VectorOne 3))

let m8 = Kronecker (m0,m0)
printfn "m8 = Kronecker (m0,m0) = %s" (MatrixToString m8)

printfn "trace (m8) = %f" (trace m8)

printfn "id 4 = %s"  (MatrixToString (id 4))

printfn "MatrixRandom (4,3) = %s"  (MatrixToString (MatrixRandom (4, 3)))

printfn "Inverse m0 = %s"  (MatrixToString (Inverse m0))
printfn "(Inverse m0)*m0 = %s"  (MatrixToString ((Inverse m0)*m0))

let m = 8;
printf "Testing timing of generating and inverting a %dx%d random matrix:" m m
let stopWatch = System.Diagnostics.Stopwatch.StartNew()
let A = MatrixRandom (m,m)
let B = Inverse A
stopWatch.Stop()
printfn " %f ms" stopWatch.Elapsed.TotalMilliseconds
