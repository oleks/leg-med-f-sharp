module LinAlgOOP
/// A general Matrix class implemented with 2D arrays in object oriented style. Matrices are mutatable
type Matrix =
  class
    /// Create new matrix of size rows,cols
    new : int * int -> Matrix

    /// Create a new matrix as a copy of the given 2d array
    new : a2DArray:float [,] -> Matrix

    /// Return the number of columns in this
    member cols : int

    /// Return the number of rows in this
    member rows : int

    /// Return the value at index (a,b)
    member Item : a:int * b:int -> float with get

    /// Set the value of this matrix at (a,b) using "=" notation
    member Item : a:int * b:int -> float with set

    /// Return the submatrix defined by indices (a, b0..b1)
    member GetSlice : row:int * colStart:int option * colFinish:int option -> Matrix

    /// Return the submatrix defined by indices (a0..a1,b)
    member GetSlice : rowStart:int option * rowFinish:int option * col:int -> Matrix

    /// Return the submatrix defined by indices (a0..a1, b0..b1)
    member GetSlice : rowStart:int option * rowFinish:int option * colStart:int option * colFinish:int option -> Matrix

    /// Return a copy of the elements of this matrix as an 2d array
    member ToArray : float [,]

    /// <summary>Concatenate to matrices along the row direction.</summary>
    /// <param name="other">A matrix, must have same numbre of columns as this.</param>
    /// <returns>A new matrix.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the two matrices have different number of columns.</exception>
    member RowConcat : other:Matrix -> Matrix

    /// <summary>Concatenate to matrices along the column direction.</summary>
    /// <param name="other">A matrix, must have same numbre of rows as this.</param>
    /// <returns>A new matrix.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the two matrices have different number of rows.</exception>
    member ColConcat : other:Matrix -> Matrix

    /// <summary>Make a new matrix whos result is the sum of this and its argument.</summary>
    /// <param name="other">A matrix, must have same size as this.</param>
    /// <returns>A new matrix of same size as this.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the two matrices are of different sizes.</exception>
    member Add : other:Matrix -> Matrix

    /// <summary>Make a new matrix whos result is product of a scalar multiplied with this.</summary>
    /// <param name="a">Any number.</param>
    /// <returns>A new matrix of same size as this.</returns>
    member Mul : a:float -> Matrix

    /// <summary>Make a new matrix whos result is the matrix product of this with other.</summary>
    /// <param name="other">A matrix, must have same size number of rows as this has columns.</param>
    /// <returns>A new matrix.</returns>
    /// <exception cref="System.ArgumentException">Thrown when the number of columns in this is different that the number of rows in other.</exception>
    member Mul : other:Matrix -> Matrix

    /// Return a new matrix as a copy of this except row i and column j
    member Minor : i:int * j:int -> Matrix

    /// Calculate the determinant of this
    member Det : float

    /// Return a new matrix as a copy of this transposed
    member Transpose : Matrix

    /// Solve for x using Cramer's rule in this*x = b
    member Cramer : b:Matrix -> Matrix

    /// Solve for the invers of this using successive applications of Cramer's rule
    member Inverse : unit -> Matrix

    /// Create a new identity matrix of size rows*rows
    static member Id : rows:int -> Matrix

    /// Create a new matrix of size rows cols, where each entry is drawn from a uniformly distributed stochastic variable in the interval [0,1]
    static member Random : rows:int -> cols:int -> Matrix
  end
