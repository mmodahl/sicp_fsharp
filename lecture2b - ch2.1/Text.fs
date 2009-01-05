#light


/// 2  Building Abstractions with Data

/// 2.1  Introduction to Data Abstraction

/// 2.1.1  Example: Arithmetic Operations for Rational Numbers

// Using the synthesis of wishful thinking, if someone has implemented ...
//(make-rat <n> <d>)
//(numer <x>)
//(denom <x>)

// With built-in Lists
let make_rat n d = [n; d;]
let numer x = (List.nth x 0)
let denom x = (List.nth x 1)


// With Tuples
let make_rat (n:int) (d:int) = (n,d)
let numer (n,d) = n
let denom (n,d) = d

// As a Closure
let make_rat n d = function
    | 1 -> n
    | 0 -> d
let numer p = p 1
let denom p = p 0


// we can express rational math...
//(define (add-rat x y)
//  (make-rat (+ (* (numer x) (denom y))
//               (* (numer y) (denom x)))
//            (* (denom x) (denom y))))
let add_rat x y = 
    (make_rat ((numer x) * (denom y) + (numer y) * (denom x)) ((denom x) * (denom y)))

//(define (sub-rat x y)
//  (make-rat (- (* (numer x) (denom y))
//               (* (numer y) (denom x)))
//            (* (denom x) (denom y))))
let sub_rat x y = 
    (make_rat ((numer x) * (denom y) - (numer y) * (denom x)) ((denom x) * (denom y)))

//(define (mul-rat x y)
//  (make-rat (* (numer x) (numer y))
//            (* (denom x) (denom y))))
let mul_rat x y = 
    (make_rat ((numer x) * (numer y)) ((denom x) * (denom y)))

//(define (div-rat x y)
//  (make-rat (* (numer x) (denom y))
//            (* (denom x) (numer y))))
let div_rat x y = 
    (make_rat ((numer x) * (denom y)) ((denom x) * (numer y)))

//(define (equal-rat? x y)
//  (= (* (numer x) (denom y))
//     (* (numer y) (denom x))))
let eql_rat x y = 
    ((numer x) * (denom y)) =  ((denom x) * (numer y))


// Generally for Lists...
//(define x (cons 1 2))
//(car x)
//(cdr x)

let mutable x = [2; ]   // List Creation
x <- List.Cons(1, x)    // Cons'ing on to the front
List.hd x               // car
List.nth x 0            // also car
List.tl x               // cdr

//(define (print-rat x)
//  (newline)
//  (display (numer x))
//  (display "/")
//  (display (denom x)))
let print_rat x = 
    printfn "%i / %i" (numer x) (denom x)

//(define one-half (make-rat 1 2))
let one_half = make_rat 1 2
//(define one-third (make-rat 1 3))
let one_third = make_rat 1 3


//(define (make-rat n d)
//  (let ((g (gcd n d)))
//    (cons (/ n g) (/ d g))))

let rec gcd (a:int) (b:int) = 
    if (b = 0) then
        a
    else
        (gcd b (a % b))

let make_rat n d = 
    let g = (gcd n d)
    [ (n/g); (d/g); ]

/// 2.1.2  Abstraction Barriers

/// 2.1.3  What Is Meant by Data?

/// 2.1.4  Extended Exercise: Interval Arithmetic