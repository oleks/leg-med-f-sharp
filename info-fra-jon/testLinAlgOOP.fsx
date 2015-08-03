open LinAlgOOP

let generateTestMatrix (M: int) (N: int) (x:float) (y:float) =
  let matrix = new Matrix(M,N)
  for i in 0..matrix.rows-1 do
    for j in 0..matrix.cols-1 do
      matrix.[i, j] <- 1.0+float(i) * x - float(j) * y
  matrix
/// This is a comment by Jon! 3

let print (s:string) (a:Matrix) =
  printfn "%s: %d x %d =\n%A" s a.rows a.cols a.ToArray

let test1 = generateTestMatrix 3 4 2.3 1.1
print "test1" test1

let submatrix = test1.[0..1, 0..1]
print "test1.[0..1, 0..1]" submatrix

let firstRow = test1.[0,*]
print "test1.[0,*]" firstRow

let secondRow = test1.[1,*]
print "test1.[1,*]" secondRow

let firstCol = test1.[*,0]
print "test1.[*,0]" firstCol

let test2 = test1.Transpose
print "test1.Transpose" test2

let I = Matrix.Id(3)
print "I" I

let J = test1.Add(test1)
print "test1+test1" J

let K = I.Mul(test1)
print "I*test1" K

let P = test1.Mul(test1.Transpose)
print "test1*(test1.Transpose)" P

let Q = test1.RowConcat(test1)
print "test1.RowConcat(test1)" Q

let R = test1.ColConcat(test1)
print "test1*(test1.ColConcat(test1)" R

print "Minors of test1" test1
for i in 0..test1.rows-1 do
  for j in 0..test1.cols-1 do
    let str = sprintf "%d, %d" i j
    print str (test1.Minor(i,j))

printfn "I.Det = %f" I.Det

let A = Matrix.Random 3 3
print "A" A
let b = Matrix.Random 3 1
print "b" b
let x = A.Cramer(b);
print "x" x
let bb = A.Mul(x);
print "Ax" bb

let B = A.Inverse();
print "B" B
let AB = A.Mul(B)
print "AB" AB
