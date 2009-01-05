#light

//Exercise 1.29.  Simpson's Rule is a more accurate method of numerical integration than 
//the method illustrated above. Using Simpson's Rule, the integral of a function f between 
//a and b is approximated as
//
//where h = (b - a)/n, for some even integer n, and yk = f(a + kh). (Increasing n increases 
//the accuracy of the approximation.) Define a procedure that takes as arguments f, a, b, 
//and n and returns the value of the integral, computed using Simpson's Rule. Use your 
//procedure to integrate cube between 0 and 1 (with n = 100 and n = 1000), and compare the 
//results to those of the integral procedure shown above.



//Exercise 1.30.  The sum procedure above generates a linear recursion. The procedure can 
//be rewritten so that the sum is performed iteratively. Show how to do this by filling in 
//the missing expressions in the following definition:
//
//(define (sum term a next b)
//  (define (iter a result)
//    (if <??>
//        <??>
//        (iter <??> <??>)))
//  (iter <??> <??>))

//(define (sum term a next b)
//  (define (iter a result)
//    (if a > b
//        result
//        (iter (next a) (+ result (term a)))))
//  (iter a 0))

let sum term a next b = 
    let rec iter a result = 
        if (a > b) then
            result
        else
            (iter (next a) (result + (term a)))
    iter a 0


//Exercise 1.31.  
//a.  The sum procedure is only the simplest of a vast number of similar abstractions that 
//can be captured as higher-order procedures.51 Write an analogous procedure called product 
//that returns the product of the values of a function at points over a given range. 
//Show how to define factorial in terms of product. Also use product to compute 
//approximations to using the formula52

//b.  If your product procedure generates a recursive process, write one that generates an 
//iterative process. If it generates an iterative process, write one that generates a 
//recursive process.



//Exercise 1.32.  a. Show that sum and product (exercise 1.31) are both special cases of a 
//still more general notion called accumulate that combines a collection of terms, using 
//some general accumulation function:
//
//(accumulate combiner null-value term a next b)
//
//Accumulate takes as arguments the same term and range specifications as sum and product, 
//together with a combiner procedure (of two arguments) that specifies how the current term 
//is to be combined with the accumulation of the preceding terms and a null-value that 
//specifies what base value to use when the terms run out. Write accumulate and show how 
//sum and product can both be defined as simple calls to accumulate.

//b. If your accumulate procedure generates a recursive process, write one that generates 
//an iterative process. If it generates an iterative process, write one that generates a 
//recursive process.




//Exercise 1.33.  You can obtain an even more general version of accumulate (exercise 1.32) 
//by introducing the notion of a filter on the terms to be combined. That is, combine only 
//those terms derived from values in the range that satisfy a specified condition. The 
//resulting filtered-accumulate abstraction takes the same arguments as accumulate, 
//together with an additional predicate of one argument that specifies the filter. Write 
//filtered-accumulate as a procedure. Show how to express the following using 
//filtered-accumulate:

//a. the sum of the squares of the prime numbers in the interval a to b (assuming that you 
//have a prime? predicate already written)


//b. the product of all the positive integers less than n that are relatively prime to n 
//(i.e., all positive integers i < n such that GCD(i,n) = 1). 



//Exercise 1.34.  Suppose we define the procedure
//
//(define (f g)
//  (g 2))
//
//Then we have
//
//(f square)
//4
//
//(f (lambda (z) (* z (+ z 1))))
//6
//
//What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? 
//Explain. 



//Exercise 1.35.  Show that the golden ratio  (section 1.2.2) is a fixed point of the 
//transformation x  1 + 1/x, and use this fact to compute  by means of the fixed-point 
//procedure.



//Exercise 1.36.  Modify fixed-point so that it prints the sequence of approximations it 
//generates, using the newline and display primitives shown in exercise 1.22. Then find a 
//solution to xx = 1000 by finding a fixed point of x log(1000)/log(x). 
//(Use Scheme's primitive log procedure, which computes natural logarithms.) Compare the 
//number of steps this takes with and without average damping. (Note that you cannot start 
//fixed-point with a guess of 1, as this would cause division by log(1) = 0.)



//Exercise 1.37.  a. An infinite continued fraction is an expression of the form
//
//As an example, one can show that the infinite continued fraction expansion with the 
//Ni and the Di all equal to 1 produces 1/, where is the golden ratio (described in 
//section 1.2.2). One way to approximate an infinite continued fraction is to truncate 
//the expansion after a given number of terms. Such a truncation -- a so-called k-term 
//finite continued fraction -- has the form
//
//Suppose that n and d are procedures of one argument (the term index i) that return the Ni and 
//Di of the terms of the continued fraction. Define a procedure cont-frac such that evaluating 
//(cont-frac n d k) computes the value of the k-term finite continued fraction. Check your procedure 
//by approximating 1/ using
//
//(cont-frac (lambda (i) 1.0)
//           (lambda (i) 1.0)
//           k)
//
//for successive values of k. How large must you make k in order to get an approximation that is 
//accurate to 4 decimal places?
//
//b. If your cont-frac procedure generates a recursive process, write one that generates an iterative 
//process. If it generates an iterative process, write one that generates a recursive process.
//
//Exercise 1.38.  In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus 
//Continuis, which included a continued fraction expansion for e - 2, where e is the base of the natural 
//logarithms. In this fraction, the Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... 
//Write a program that uses your cont-frac procedure from exercise 1.37 to approximate e, based on Euler's expansion.
//
//Exercise 1.39.  A continued fraction representation of the tangent function was published in 1770 by the German 
//mathematician J.H. Lambert:
//
//where x is in radians. Define a procedure (tan-cf x k) that computes an approximation to the tangent function 
//based on Lambert's formula. K specifies the number of terms to compute, as in exercise 1.37. 
//
//Exercise 1.40.  Define a procedure cubic that can be used together with the newtons-method procedure in 
//expressions of the form
//
//(newtons-method (cubic a b c) 1)
//
//to approximate zeros of the cubic x3 + ax2 + bx + c.
//
//Exercise 1.41.  Define a procedure double that takes a procedure of one argument as argument and returns a 
//procedure that applies the original procedure twice. For example, if inc is a procedure that adds 1 to its 
//argument, then (double inc) should be a procedure that adds 2. What value is returned by
//
//(((double (double double)) inc) 5)
//
//Exercise 1.42.  Let f and g be two one-argument functions. The composition f after g is defined to be the 
//function x f(g(x)). Define a procedure compose that implements composition. For example, if inc is a procedure 
//that adds 1 to its argument,
//
//((compose square inc) 6)
//49
//
//Exercise 1.43.  If f is a numerical function and n is a positive integer, then we can form the nth repeated 
//application of f, which is defined to be the function whose value at x is f(f(...(f(x))...)). For example, 
//if f is the function x x + 1, then the nth repeated application of f is the function x x + n. If f is the 
//operation of squaring a number, then the nth repeated application of f is the function that raises its argument 
//to the 2nth power. Write a procedure that takes as inputs a procedure that computes f and a positive integer n 
//and returns the procedure that computes the nth repeated application of f. Your procedure should be able to be 
//used as follows:
//
//((repeated square 2) 5)
//625
//
//Hint: You may find it convenient to use compose from exercise 1.42.
//
//Exercise 1.44.  The idea of smoothing a function is an important concept in signal processing. If f is a function 
//and dx is some small number, then the smoothed version of f is the function whose value at a point x is the average 
//of f(x - dx), f(x), and f(x + dx). Write a procedure smooth that takes as input a procedure that computes f and 
//returns a procedure that computes the smoothed f. It is sometimes valuable to repeatedly smooth a function (that 
//is, smooth the smoothed function, and so on) to obtained the n-fold smoothed function. Show how to generate the 
//n-fold smoothed function of any given function using smooth and repeated from exercise 1.43.
//
//Exercise 1.45.  We saw in section 1.3.3 that attempting to compute square roots by naively finding a fixed point 
//of y x/y does not converge, and that this can be fixed by average damping. The same method works for finding cube 
//roots as fixed points of the average-damped y x/y2. Unfortunately, the process does not work for fourth roots -- 
//a single average damp is not enough to make a fixed-point search for y x/y3 converge. On the other hand, if we 
//average damp twice (i.e., use the average damp of the average damp of y x/y3) the fixed-point search does converge. 
//Do some experiments to determine how many average damps are required to compute nth roots as a fixed-point search 
//based upon repeated average damping of y x/yn-1. Use this to implement a simple procedure for computing nth roots 
//using fixed-point, average-damp, and the repeated procedure of exercise 1.43. Assume that any arithmetic operations 
//you need are available as primitives.
//
//Exercise 1.46.  Several of the numerical methods described in this chapter are instances of an extremely general 
//computational strategy known as iterative improvement. Iterative improvement says that, to compute something, we 
//start with an initial guess for the answer, test if the guess is good enough, and otherwise improve the guess and 
//continue the process using the improved guess as the new guess. Write a procedure iterative-improve that takes two
//procedures as arguments: a method for telling whether a guess is good enough and a method for improving a guess. 
//Iterative-improve should return as its value a procedure that takes a guess as argument and keeps improving the 
//guess until it is good enough. Rewrite the sqrt procedure of section 1.1.7 and the fixed-point procedure of section 
//1.3.3 in terms of iterative-improve. 