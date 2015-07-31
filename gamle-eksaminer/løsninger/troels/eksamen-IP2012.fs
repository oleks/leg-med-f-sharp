module Eksamen

open System

let product = List.fold ( *) 1
let sum = List.fold (+) 0
let concatMap f l = List.map f l |> List.concat
let rec range f t = if f > t then []
                    else f :: range (f+1) t

let fjern y l =
  let rec fjern' xs l = match xs with
                          | [] -> None
                          | (x::xs') -> if x = y then Some (List.rev l @ xs')
                                        else fjern' xs' (x::l)
  in fjern' l []

let rec insertWith f l k v =
  match l with
    | [] -> [(k,v)]
    | (k',v')::xs -> if k' = k then (k,f v v') :: xs
                     else (k',v') :: insertWith f xs k v

(* Opgave 1 *)

let reverse (s : string) = String.mapi (fun i _ -> s.[s.Length - 1 - i]) s

let erPalindrom (s : string) = s = reverse s

let erUdvidetPalindrom (s : string) =
  let interesting c = not (String.exists (fun x -> x = c) ".,?!:;" || Char.IsWhiteSpace c)
  in Seq.map Char.ToUpper s
     |> Seq.filter interesting
     |> String.Concat
     |> erPalindrom

(* Opgave 2 *)

type rute = Stop | Frem of int * rute | Drej of int * rute

let rec korrekt = function
  | Stop        -> true
  | Frem (d, r) -> d >= 0 && korrekt r
  | Drej (_, r) -> korrekt r

let rec laengde = function
  | Stop        -> 0
  | Frem (d, r) -> d + laengde r
  | Drej (_, r) -> laengde r

let rec erNormaliseret = function
  | Stop        -> true
  | Frem (0, _) -> false
  | Drej (0, _) -> false
  | Drej (_, Drej (_, _)) -> false
  | Drej (g, r) -> not (g <= -180 || g > 180) && erNormaliseret r
  | Frem (_, Frem (_, _)) -> false
  | Frem (_, r)           -> erNormaliseret r

let rec normaliserRute = function
  | Stop        -> Stop
  | Frem (0, r) -> normaliserRute r
  | Drej (0, r) -> normaliserRute r
  | Drej (g1, Drej (g2, r)) -> normaliserRute (Drej (g1 + g2, r))
  | Drej (g, r) -> Drej (g % 360, r)
  | Frem (d1, Frem (d2, r)) -> normaliserRute (Frem (d1 + d2, r))
  | Frem (_, r)             -> normaliserRute r

(* Opgave 3 *)

let kvadratfrit n =
  let rec tjek n m =
    match n with
    | 1 -> true
    | _ -> if double m > sqrt(double n) then true
           elif n % (m*m) <> 0 then tjek (if (n % m) = 0 then n / m else n) (m+1)
           else false
  in if n <= 0 then invalidArg "n" "must be positive" else tjek n 2

let maksKvadratfrit n =
    let rec soeg m = if n % m = 0 && kvadratfrit m
                     then m else soeg (m-1)
    in if n <= 0 then invalidArg "n" "must be positive" else soeg n

(* Opgave 4 *)

let erPermutationAf (l1, l2) =
  let komb x l =
    match x with
      | Some xs -> fjern l xs
      | None   -> None
  in List.fold komb (Some l1) l2 = Some []

let rec sub l1 = function
  | [] -> l1
  | x::xs -> match fjern x l1 with
               | None     -> sub l1 xs
               | Some l1' -> sub l1' xs

let rec next m = function
  | 1 -> [1]
  | n -> if n % m = 0 then m :: next m (n / m)
         else next (m+1) n
let divisors = next 2


let antalPermutationer l =
  let xs = List.fold (fun l v -> insertWith (fun x y -> x + y) l v 1) [] l
  let n = List.length l
  product (sub (concatMap divisors (range 2 n))
               (concatMap divisors (concatMap (snd >> range 2) xs)))

let antalPermutationerNy = antalPermutationer

(* Opgave 5 *)

let rec grupper p = function
  | [] -> []
  | (x::xs) -> let x' = p x
               let comp y = x' = p y
               let (l1,l2) = List.partition comp xs
               (x::l1) :: grupper p l2

let rec gentag f x = try gentag f (f x) with _ -> x

let gcd =
  let gcd' (a,b) = (b, a % b)
  gentag gcd' >> fst

(* Opgave 6 *)

type 'a trae = K of 'a * ('a trae list)

let t7 = K (3, [K (4, [K (7, []); K (1, []); K (5, [K (6, []); K (7, [])])])])

let rec praeorden (K (x, ts)) = x :: concatMap praeorden ts

let rec erstat (K (x, ts), l) = match l with
  | y::ys ->
    let rec erstat' (ts, l) = match ts with
      | [] -> ([], l)
      | t::ts -> let (t', l') = erstat (t, l)
                 let (ts', l'') = erstat' (ts, l')
                 (t'::ts', l'')
    let (ts', ys') = erstat' (ts, ys)
    (K (y, ts'), ys')
  | [] -> invalidArg "l" "is empty"

(* Opgave 7 *)

(* TODO: Jeg gider ikke hitte ud af fil-I/O. *)

(* Opgave 8 *)

(* Lader sig ikke gøre i F#. *)
