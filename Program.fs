// vim: ft=fsharp

(*
    Functional Programming - Assignment 1
    Sam Al-Sapti (sals@itu.dk)
    February 7th, 2022
*)


(*
    GREEN
*)

// Exercise 1.1
let sqr x = x * x

// Exercise 1.2
let pow x n = x ** n

// Exercise 1.3
let rec sum =
    function
    | 0 -> 0
    | n -> n + sum (n - 1)

// Exercise 1.4
let rec fib =
    function
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n - 1) + fib (n - 2)

// Exercise 1.5
let dup : string -> string = fun s -> s + s

// Exercise 1.6
let rec dupn : string -> int -> string = fun s n ->
    match n with
    | 0 -> ""
    | 1 -> s
    | n -> s + dupn s (n - 1)

// Exercise 1.7
let rec bin (n, k) =
    if k = 0 || k = n then
        1
    elif n = 0 || k = 0 || n <= k then
        0
    else
        bin (n - 1, k - 1) + bin (n - 1, k)


(*
    YELLOW
*)

// Exercise 1.8
let timediff (h1, m1) (h2, m2) = (m2 - m1) + (h2 - h1) * 60

// Exercise 1.9
let minutes = timediff (0, 0)

// Exercise 1.10
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

// Exercise 1.11
let empty def = fun pos -> def


(*
    RED
*)

// Exercise 1.12
let add newPos cv word = fun pos ->
    if newPos = pos then
        cv
    else
        word pos

// Exercise 1.13
let hello : int -> char * int =
    empty (char 0, 0)
    |> add 0 ('H', 4)
    |> add 1 ('E', 1)
    |> add 2 ('L', 1)
    |> add 3 ('L', 1)
    |> add 4 ('O', 1)

// Exercise 1.14
let singleLetterScore (word : int -> char * int) = fun pos -> word pos |> fun (c, p) -> p
let doubleLetterScore (word : int -> char * int) = fun pos -> word pos |> fun (c, p) -> p * 2
let tripleLetterScore (word : int -> char * int) = fun pos -> word pos |> fun (c, p) -> p * 3
