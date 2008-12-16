#light


/// 1.1.1 Expressions

// Representing numbers in F# should be identical to Lisp
486

// here I receive an IDE warning
// "This expression should have type 'unit' but has type 'int.'"

// Select the Expression and right click -> send to F# Interactive


// Simple Arithmetic Expressions are written with traditional infix notation
(137 + 349)

// and arithmetic works as expected
1000 - 334
5 + 4 * 3 - 2 / 1

// Without prefix notation, we have to spell out each infix operator
// (+ 21 35 12 7)
21 + 35 + 12 + 7

//(* 25 4 12)
25 * 4 * 12

// Nesting is done based on the expected arithmetic rules.
//(+ (* 3 5) (- 10 6))
(3 * 5) + (10 - 6)

// Without the parenthesis based function calls, there is no such thing as pretty-printing,
//(+ (* 3
//      (+ (* 2 4)
//         (+ 3 5)))
//   (+ (- 10 7)
//      6))

(((3 + 5) + (2 * 4)) * 3) + ((10 - 7) + 6)



/// 1.1.2  Naming and the Environment
// Lisp's define look about equivalent of F#'s let
// (define size 2)
let size = 2

size

(size * 5)

//(define pi 3.14159)
//(define radius 10)
//(* pi (* radius radius))

let pi = 3.14159
let radius = 10
pi * (radius * radius)
// Here I finally have an error and not simply a warning:
// The type 'int' does not match the type 'float'.

let radius = 10.0
pi * (radius * radius)


//(define circumference (* 2 pi radius))
//circumference
let circumference = 2.0 * pi * radius
circumference



///1.1.3  Evaluating Combinations

// (* (+ 2 (* 4 6))
//   (+ 3 5 7))

((4 * 6) + 2) * (3 + (5 + 7))



/// 1.1.4  Compound Procedures

// Creating new procedures that take parameters 
// (define (square x) (* x x))
let square x = x * x

//(square 21)
square 21

// (square (+ 2 5))
square (2+5)
square 2+5 // evaluates (square 2) + 5

//(define (sum-of-squares x y)
//  (+ (square x) (square y)))
let sum_of_squares x y = square x + square y

//(define (f a)
//  (sum-of-squares (+ a 1) (* a 2)))
let f a = sum_of_squares (a+1) (a*2)

//(f 5)
f 5



/// 1.1.5  The Substitution Model for Procedure Application



/// 1.1.6  Conditional Expressions and Predicates

// Conditionals in F# can be evaulated matches against patterns

//(define (abs x)
//  (cond ((> x 0) x)
//        ((= x 0) 0)
//        ((< x 0) (- x))))

let abs x = match (x > 0) with 
            | true -> x
            | false -> (x * -1)
            
// but I can't imagine that this is the correct method for doing this.



// if then else logic in F# follows a traditional syntax:
// "if <predicate> then <consequent> else <alternative>"

// (define (abs x)
//   (if (< x 0)
//       (- x)
//       x))

let abs x = if (x < 0) then (x * -1) else x

// Predicate Operators are about as expected

true && false
true || false
true <> false
true = false


// (define (>= x y)
//   (or (> x y) (= x y)))
let gte x y = ((x > y) || (x = y))

// Operator Overloading doesn't appear to be so simple.


// 1.1.7  Example: Square Roots by Newton's Method

//(define (sqrt x)
//  (sqrt-iter 1.0 x))

//(define (sqrt-iter guess x)
//  (if (good-enough? guess x)
//      guess
//      (sqrt-iter (improve guess x)
//                 x)))
//                 
//(define (improve guess x)
//  (average guess (/ x guess)))
//
//(define (average x y)
//  (/ (+ x y) 2))
//  
//(define (good-enough? guess x)
//  (< (abs (- (square guess) x)) 0.001))


let abs x = if (x > 0.0) then x else (x * -1.0)
let square (x:float) = x * x
let average x y = (x + y) / 2.0
let improve guess x = average guess (x / guess)
let good_enough guess x =  0.001 > (abs((square guess) - x))

// rec here signifies a recursive function
let rec sqrt_iter guess x =
    if (good_enough guess x) then 
        guess 
    else
        sqrt_iter (improve guess x) x
        
let sqrt x = sqrt_iter 1.0 x

sqrt 9.0

(sqrt (sqrt 2.0) + (sqrt 3.0))


// 1.1.8  Procedures as Black-Box Abstractions
// Internal definitions and block structure
// F# does do block structure.  And I love that White-Space matters.

let self_contained_sqrt (x:float) = 
    let rec sqrt_iter (guess:float) (x:float) =
        let abs (x:float) = if (x > 0.0) then x else (x * -1.0)
        let square (x:float) = x * x
        let average (x:float) (y:float) = (x + y) / 2.0
        let improve (guess:float) (x:float) = average guess (x / guess)
        let good_enough (guess:float) (x:float) =  0.001 > (abs((square guess) - x))
        
        if (good_enough guess x) then 
            guess 
        else
            sqrt_iter (improve guess x) x
    sqrt_iter 1.0 x