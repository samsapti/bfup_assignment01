﻿// vim: ft=fsharp

(*
    Functional Programming - Assignment 1
    Sam Al-Sapti (sals@itu.dk)
    February 4th, 2022
*)

(*
    GREEN
*)

// Exercise 1.1
let sqr x = x * 2

// Exercise 1.2
let pow x n = x ** n

// Exercise 1.3
let rec sum = function
    | 0 -> 0
    | n -> n + sum (n - 1)

// Exercise 1.4
let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n - 1) + fib (n - 2)

// Exercise 1.5
let dup s: string = s + s

// Exercise 1.6
let rec dupn (s: string) = function
    | 1 -> s
    | n -> s + dupn s (n - 1)

// Exercise 1.7
let rec bin (n, k) =
    if k = 0 || k = n then 1
    else bin (n - 1, k - 1) + bin (n - 1, k)

(*
    YELLOW
*)

// Exercise 1.8
let timediff (h1, m1) (h2, m2) = (m2 - m1) + 60 * (h2 - h1)

// Exercise 1.9
let minutes = timediff (0, 0)

// Exercise 1.10
let curry f x y = f (x, y)
let uncurry f (x, y) = f x y

// Exercise 1.11
let empty (pair : char * int) = fun (pos : int) -> pair