#light

///Exercise 1.1. 
// Below is a sequence of expressions. What is the result 
// printed by the interpreter in response to each expression? 
// Assume that the sequence is to be evaluated in the order 
// in which it is presented.

//10
10                  // --> val it : 10

//(+ 5 3 4)
5 + 3 + 4           // --> val it : 12

//(- 9 1)
9 - 1               // --> val it : 8

//(/ 6 2)
6 / 2               // --> val it : int = 3

//(+ (* 2 4) (- 4 6))
(2 * 4) + (4 - 6)   // --> val it : 6

//(define a 3)
let a = 3           // --> val a : int

//(define b (+ a 1))
let b = (a + 1)     // --> val b : int

//(+ a b (* a b))
(a + b + (a * b))   // --> val it : int = 19

//(= a b)
(a = b)             // --> val it : bool = false


//(if (and (> b a) (< b (* a b)))
//    b
//    a)
if ((b>a) && (b > (a*b))) then b
else a
                    // --> val it : int = 3


//(cond ((= a 4) 6)
//      ((= b 4) (+ 6 7 a))
//      (else 25))
if (a = 4) then 6 else
if (b = 4) then (6 + 7 + a) else 25
                    // --> val it : int = 16

//(+ 2 (if (> b a) b a))
( 2 + (if (b > a) then b else a))
                    // --> val it : int = 6
//(* (cond ((> a b) a)
//         ((< a b) b)
//         (else -1))
//   (+ a 1))
if (a > b) then a else
    if (a < b) then b else -1 
* 
(a + 1)             // --> val it : int = 16




///Exercise 1.2. 


///Exercise 1.3.   
// Define a procedure that takes three numbers as arguments 
// and returns the sum of the squares of the two larger numbers. 
let largest_squares a b c =
    let square x = x * x
    let sum_of_squares x y = square x + square y
    let first_largest a b c = if ( a>b && a>c) then a else if (b>c) then b else c
    let second_largest a b c = if ((a>b && a<c) || (a<b && a>c)) then a else if ((a>b)&& (b>c)) || ((a<b)&& (b<c)) then b else c
    sum_of_squares (first_largest a b c) (second_largest a b c)
    
    
///Exercise 1.4. 
// Observe that our model of evaluation allows for combinations whose 
// operators are compound expressions. Use this observation to describe 
// the behavior of the following procedure:

// (define (a-plus-abs-b a b)
//   ((if (> b 0) + -) a b))

// As strictly typed as F# appears at this moment, I'm not certain
// if it is going to allow the return of a primitive operator.
let a_plus_abs_b a b = if (b > 0) then a + b else a - b


///Exercise 1.5. 
// Ben Bitdiddle has invented a test to determine whether the 
// interpreter he is faced with is using applicative-order evaluation 
// or normal-order evaluation. He defines the following two procedures:

// (define (p) (p))

// (define (test x y)
//   (if (= x 0)
//       0
//       y))

// Then he evaluates the expression
// (test 0 (p))

// What behavior will Ben observe with an interpreter that uses 
// applicative-order evaluation? What behavior will he observe 
// with an interpreter that uses normal-order evaluation? 
// Explain your answer. (Assume that the evaluation rule for the
// special form if is the same whether the interpreter is using 
// normal or applicative order: The predicate expression is evaluated
// first, and the result determines whether to evaluate the consequent
// or the alternative expression.) 


open System

let p = Console.ReadKey(true)
let test x y = if (x = 0) then 0 else p ()
test 0 p

// GAH! F# appears to be doing really deep type checking.  I haven't been
// able to find a way to create an anonymous function or delegate capable
// of testing the interpreter like in the Lisp example.

// I think that the evaluation of the Console.ReadKey (which would effectively
// test if the p got evaluated) might be a decent substitute.

// We can see here that the p is not evaluated which would lead us toward
// assuming that, if F#'s interpreter was substitution based, it was doing
// a normal order evaluation.  That being said: it would also make sense to
// allow for special cases where I/O operations were only evaluated when
// absolutely necessary and therefore, we have proven nothing.  There is 
// much to learn about the F# internals.

        
        
/// Exercise 1.6.  
// Alyssa P. Hacker doesn't see why if needs to be provided as a special form.
// ``Why can't I just define it as an ordinary procedure in terms of cond?'' she
// asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and she 
// defines a new version of if:
//
//(define (new-if predicate then-clause else-clause)
//  (cond (predicate then-clause)
//        (else else-clause)))
//
// Eva demonstrates the program for Alyssa:
//
//(new-if (= 2 3) 0 5)
//5
//
//(new-if (= 1 1) 0 5)
//0
//
// Delighted, Alyssa uses new-if to rewrite the square-root program:
//
//(define (sqrt-iter guess x)
//  (new-if (good-enough? guess x)
//          guess
//          (sqrt-iter (improve guess x)
//                     x)))
//
// What happens when Alyssa attempts to use this to compute square roots? Explain. 




///Exercise 1.7. 



///Exercise 1.8. 
// Newton's method for cube roots is based on the fact that if y is an approximation
// to the cube root of x, then a better approximation is given by the value

// (x/y*y + 2y) / 3

// Use this formula to implement a cube-root procedure analogous to the square-root
// procedure. (In section 1.3.4 we will see how to implement Newton's method in
// general as an abstraction of these square-root and cube-root procedures.) 

let cube_root (x:float) =
    let rec cube_root_iter (guess:float) (x:float) = 
        let cube (x:float) = x * x * x
        let average (x:float) (y:float) = (x + y) / 2.0
        let abs (x:float) = if (x < 0.0) then x * -1.0 else x
        let cube_good_enough (guess:float) (x:float) = (abs ((cube guess) - x)) < 0.001
        let cube_improve (guess:float) (x:float) = (x / (guess * guess) + (2.0 * guess)) / 3.0
        if (cube_good_enough guess x) then
            guess
        else
            cube_root_iter (cube_improve guess x) x 
    cube_root_iter 1.0 x

(cube_root 27.0)            // --> val it : float = 3.000000541
(cube_root 729.0)           // --> val it : float = 9.0
