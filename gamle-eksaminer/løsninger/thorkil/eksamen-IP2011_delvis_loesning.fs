module Exam2011

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

// OPGAVE 3

      
