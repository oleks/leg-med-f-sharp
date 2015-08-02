  //module Exam2011

  open Operators.Checked // changes definitions of basic arithmetic operators to catch overflow.

  exception Domain of string // does not inherit any type/class

  // OPGAVE 1

  let rec stirling (n, k) =
    try
      if (n < 0 || k < 0) then raise (Domain("Both arguments must be non-negative."))
      elif (n < k) then 0
      elif (n = 0 && k = 0) then 1
      elif n = 0 then 0
      elif k = 0 then 0
      else k*stirling ((n-1), k) + stirling ((n-1), (k-1))
    with
      | Domain(str) -> (printfn "Domain: %s" str; 0)
      | :? System.OverflowException -> (printfn "Overflow"; System.Int32.MinValue) // :? checks type

  (*
     stirling (4,2) -> (goes to "else")
     2*stirling(3,2) + stirling(3,1) -> (both go to "else")
     2*(2*stirling(2,2) + stirling(2,1)) + 1*stirling(2,1) +stirling(2,0) ->
     2*(2*(2*stirling(1,2)+stirling(1,1))+1*stirling(1,1)+stirling(1,0))+1*(1*stirling(1,1)+stirling(1,0)+0) ->
     2*(2*(2*0+stirling(0,1)+stirling(0,0))+1*stirling(0,1)+stirling(0,0)+0)+1*(1*stirling(0,1)+stirling(0,0)+0) ->
     2*(2*(0+0+1)+0+1+0)+1*(1*0+1+0) ->
     2*3+1 ->
     7
  Once upon a midnight dreary as I pondered weak and weary,
     *)

  // OPGAVE 2

  let rec length_of_list x =
    match x with
      | x :: xs ->
        1+length_of_list(xs)
      | [] -> 0

  let rec erNedreTrekantRest xl =
    match xl with
      | x :: y :: xs ->
        (length_of_list(x) = length_of_list(y) - 1) && erNedreTrekantRest(y::xs)
      | x :: xs ->
          true //this lenght should be the same as the length of the original list.
      | [] -> true 

  let erNedreTrekant xl =
    match xl with
      | x :: xs ->
        length_of_list(x) = 1 && erNedreTrekantRest (x :: xs)
      | [] ->
        true

  let rec getn n xs =
    match n, xs with
      | 0, (x::_) -> x
      | _, (_::xs') -> getn (n - 1) xs'
      | _, [] -> invalidArg "n" "n is too large"

  let rec diagonalRest n xl =
    match n, xl with
      | n, (x::xs) -> getn n x :: diagonalRest (n+1) xs
      | n, [] -> []


  let rec diagonal xl =
    if erNedreTrekant xl then
      diagonalRest 0 xl
    else
      printfn "Incorrect input"; []

  let rec vend xl =
    if erNedreTrekant xl then List.rev xl
    else
        printfn "Incorrect input"; []

  let rec h_old xl =
    match xl with
      | [] -> printfn "Invalid input"; (0.0,0.0)
      | [x : float] -> (x, x)
      | x :: xs ->
        let (frst,sdst) = h_old xs
        if x < frst then (x, sdst)
        else if x > sdst then (frst,x) else (frst,sdst)


  // OPGAVE 3

  (*
  f:
    - f: 'a list -> int list -> 'a list
         Tager en liste af indexer (1. argument) og en 'a-liste, og returnerer de elementer i 'a-listen, som har index fra index-listen.
    - g: Udvælger alle de elementer fra xs, som opfylder returnerer true for samtlige funktion i listen af funktioner, f. f: 'a -> bool.
    - h: Returnerer det mindste og største element i en listte.
     *)


  let f xl nl =
    match xl, nl with
      | _, [] -> []
      | xs, ns ->
        let rec kth xs n =
          match xs, n with
            | (x::_), 1 -> x
            | (_::xs'), _ -> kth xs' (n - 1)
            | [], _ -> invalidArg "n" "n is too large"
        List.map (kth xs) ns

  // Denne bør omskrives til noget med map.
  let rec g xl fl =
    match xl, fl with
      | xl, []        -> xl
      | xl, (f :: fs) ->
        let loop yl =
          List.filter f yl
        g (loop xl) fs

  let h xl =
    let a x y = if x < y then y else x
    let b x y = if x > y then y else x
    (List.fold b System.Int32.MaxValue xl, List.fold a System.Int32.MinValue xl)


  type matrix<'a> = 'a list list

  let rec isMatrix (A:matrix<'a>) =
    match A with
      | [] -> true
      | x::y::xs -> length_of_list(x) = length_of_list(y) && isMatrix(y::xs)
      | x::xs -> true

  let dim (A:matrix<'a>) =
    if (isMatrix A) then
      ((length_of_list A), (length_of_list(List.head(A))))
    else
      printfn "This is not a matrix"; (0,0)

let transpose xl =
  let rec transpose_help xl1 n =
    if (length_of_list xl1 = n) then
      []
    else
      List.map (getn n) xl1 :: (transpose_help xl1 (n+1))
  in
    transpose_help xl 0

// let getElement1 xl i j = getn j (getn i xl)

// let getElement2 xl i j = getn i (getn j xl)

let getRow (A:matrix<int>) n = getn (n-1) A

let getColumn (A:matrix<int>) n = List.map (getn (n-1)) A

let rec multiplyElements xl yl =
  if (length_of_list(xl) = length_of_list(yl)) then
    match xl, yl with
      | x :: xs, y :: ys -> x*y :: (multiplyElements xs ys)
      | [], _ -> []
      | _, [] -> []
  else
    printfn "Lists have wrong length"; []
      

let getMultiIndex (A:matrix<int>) (B:matrix<int>) i j =
  List.fold (fun x y -> x+y) 0 (multiplyElements (getRow A i) (getColumn B j))

// Get 1 row of a matrix C = A*B
let multiply1 (A:matrix<int>) (B:matrix<int>) n =
  List.map (getMultiIndex A B n) [1..(length_of_list (getColumn A 1))];;

let multiply (A:matrix<int>) (B:matrix<int>) = List.map (multiply1 A B) [1..(length_of_list (getColumn A 1))]

let A:matrix<int> = [[4;1;3];[2;0;5]]
let B:matrix<int> = [[4;2];[1;0];[3;5]]


  
