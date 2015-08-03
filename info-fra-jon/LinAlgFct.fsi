module LinAlgFct
[<Sealed>]
/// A general immutable Vector type implemented in functional programming style
type Vector =

  /// Add 2 vectors elementwise, e.g., let u = v + w, assuming that bindings to v and w exist and are of same length.
  static member ( + ) : Vector * Vector -> Vector

  /// Multiply a the elements of a vector with a float, e.g., let u = 3.0 * v, assuming that binding to v exists.
  static member ( * ) : a:float * Vector -> Vector

  /// Vector dot product also known as the inner product, e.g., let a = v * w, assuming that bindings to v and w exist and are of same length.
  static member ( * ) : Vector * Vector -> float

  /// Subtract 2 vectors elementwise, e.g., let u = v - w, assuming that bindings to v and w exist and are of same length.
  static member ( - ) : Vector * Vector -> Vector

  /// Negate a vector elementwise, e.g., let u = -v, assuming that binding to v exists.
  static member ( ~- ) : Vector -> Vector

/// Create a vector from a list's elements preserving the order
val Vector : float list -> Vector

/// Calculate the length of a vector, i.e., its number of elements
val length : Vector -> int

/// Create a new vector as the slice for an existing, equivalent to v(rowStart..rowFinish)
val VectorGetSlice : Vector * rowStart:int * rowFinish:int -> Vector

/// Calculate the vector 2-norm
val VectorNorm : Vector -> float

/// Create a List of the vector's elements
val VectorToList : Vector -> float list

/// Create a string of the vector's elements
val VectorToString : Vector -> string

/// Create a vector of length n whos elements are all 1
val VectorOne : int -> Vector

/// Create a vector of length n where each element is drawn from a uniformly distributed stochastic variable in the interval [0,1]
val VectorRandom : int -> Vector
    
[<Sealed>]
/// A general immutable Matrix type implemented in functional programming style
type Matrix =
  static member ( + ) : Matrix * Matrix -> Matrix
  static member ( * ) : a:float * Matrix -> Matrix
  static member ( * ) : Matrix * Matrix -> Matrix
  static member ( * ) : Matrix * Vector -> Matrix
  static member ( * ) : Vector * Matrix -> Matrix
  static member ( - ) : Matrix * Matrix -> Matrix
  static member ( ~- ) : Matrix -> Matrix
val Matrix: float list list -> Matrix
val rows : Matrix -> int
val cols : Matrix -> int
val transpose : Matrix -> Matrix
val VectorToMatrix : Vector -> Matrix
val MatrixToVector : Matrix -> Vector
val MatrixGetSlice : Matrix * int * int * int * int -> Matrix
val RowConcat : Matrix * Matrix -> Matrix
val ColConcat : Matrix * Matrix -> Matrix
val Minor : Matrix * int * int -> Matrix
val Det : Matrix -> float
val Cramer : Matrix * Vector -> Vector
val Inverse : Matrix -> Matrix
val Kronecker : Matrix * Matrix -> Matrix
val MatrixToListList : Matrix -> float list list
val MatrixToString : Matrix -> string
val trace : Matrix -> float
val id : int -> Matrix
val MatrixRandom : int * int -> Matrix
