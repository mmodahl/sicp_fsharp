#light

        
/// 1.2  Procedures and the Processes They Generate

/// 1.2.1  Linear Recursion and Iteration

//(define (factorial n)
//  (if (= n 1)
//      1
//      (* n (factorial (- n 1)))))

let rec factorial (n:int) =
    if (n = 1) then
        1
    else
        (n * (factorial (n - 1)))


//(define (factorial n)
//  (fact-iter 1 1 n))
//
//(define (fact-iter product counter max-count)
//  (if (> counter max-count)
//      product
//      (fact-iter (* counter product)
//                 (+ counter 1)
//                 max-count)))
let factorial_iter (n:int) =
    let rec fact_iter (product:int) (counter:int) (max_count:int) =
        if (counter > max_count) then
            product
        else
            (fact_iter (product * counter) (counter + 1) max_count)
    fact_iter 1 1 n
 

/// 1.2.2  Tree Recursion

//(define (fib n)
//  (cond ((= n 0) 0)
//        ((= n 1) 1)
//        (else (+ (fib (- n 1))
//                 (fib (- n 2))))))

let rec fib (n:int) = 
    if (n = 0) then
        0
    else 
        if (n = 1) then
            1
        else
            (fib (n-1)) + (fib (n-2))
      
//(define (fib n)
//  (fib-iter 1 0 n))
//
//(define (fib-iter a b count)
//  (if (= count 0)
//      b
//      (fib-iter (+ a b) a (- count 1))))

let fib (n:int) = 
    let rec fib_iter (a:int) (b:int) (count:int) =
        if (count = 0) then
            b
        else
            fib_iter (a + b) a (count - 1)
    fib_iter 1 0 n



/// 1.2.3  Orders of Growth

//(define (count-change amount)
//  (cc amount 5))
//(define (cc amount kinds-of-coins)
//  (cond ((= amount 0) 1)
//        ((or (< amount 0) (= kinds-of-coins 0)) 0)
//        (else (+ (cc amount
//                     (- kinds-of-coins 1))
//                 (cc (- amount
//                        (first-denomination kinds-of-coins))
//                     kinds-of-coins)))))
//(define (first-denomination kinds-of-coins)
//  (cond ((= kinds-of-coins 1) 1)
//        ((= kinds-of-coins 2) 5)
//        ((= kinds-of-coins 3) 10)
//        ((= kinds-of-coins 4) 25)
//        ((= kinds-of-coins 5) 50)))

let first_denomination kinds_of_coins =
    match kinds_of_coins with
    | 1 -> 1
    | 2 -> 5
    | 3 -> 10
    | 4 -> 25
    | 5 -> 50
    | n -> 0

let rec cc amount kinds_of_coins = 
    if (amount = 0) then
        1
    else
        if (amount < 0) || (kinds_of_coins = 0) then
            0
        else
            (cc amount (kinds_of_coins - 1)) + (cc (amount - (first_denomination kinds_of_coins)) kinds_of_coins)

let count_change amount = cc amount 5



/// 1.2.4  Exponentiation

//(define (expt b n)
//  (if (= n 0)
//      1
//      (* b (expt b (- n 1)))))

let rec expt (b:int) (n:int) = 
    if (n = 0) then
        1
    else 
        b * (expt b (n - 1))

//(define (expt b n)
//  (expt-iter b n 1))
//
//(define (expt-iter b counter product)
//  (if (= counter 0)
//      product
//      (expt-iter b
//                (- counter 1)
//                (* b product)))) 

let expt (b:int) (n:int) = 
    let rec expt_iter (b:int) (counter:int) (product:int) = 
        if (counter = 0) then
            product
        else
            expt_iter b (counter - 1) (b * product)
    expt_iter b n 1


//(define (fast-expt b n)
//  (cond ((= n 0) 1)
//        ((even? n) (square (fast-expt b (/ n 2))))
//        (else (* b (fast-expt b (- n 1))))))
//
//(define (even? n)
//  (= (remainder n 2) 0))


let rec fast_expt (b:int) (n:int) = 
    let square (i:int) = i * i
    let even n = if (n % 2 = 0) then true else false
    
    if (n=0) then
        1
    else
        if (even n) then
            square (fast_expt b (n/2))
        else
            b * fast_expt b (n - 1)


/// 1.2.5  Greatest Common Divisors

//(define (gcd a b)
//  (if (= b 0)
//      a
//      (gcd b (remainder a b))))

let rec gcd (a:int) (b:int) = 
    if (b = 0) then
        a
    else
        (gcd b (a % b))



/// 1.2.6  Example: Testing for Primality

//(define (smallest-divisor n)
//  (find-divisor n 2))
//(define (find-divisor n test-divisor)
//  (cond ((> (square test-divisor) n) n)
//        ((divides? test-divisor n) test-divisor)
//        (else (find-divisor n (+ test-divisor 1)))))
//(define (divides? a b)
//  (= (remainder b a) 0))

let smallest_divisor (n:int) = 
    let rec find_divisor (n:int) (test_divisor:int) = 
        let square (i:int) = i * i
        let divides (a:int) (b:int) = (b%a = 0)
        
        if ((square test_divisor) > n) then
            n
        else
            if (divides test_divisor n) then
                test_divisor
            else
                find_divisor n (test_divisor + 1)    
    (find_divisor n 2)
    
    
//(define (expmod base exp m)
//  (cond ((= exp 0) 1)
//        ((even? exp)
//         (remainder (square (expmod base (/ exp 2) m))
//                    m))
//        (else
//         (remainder (* base (expmod base (- exp 1) m))
//                    m))))        

let rec expmod (basen:int) (exp:int) (m:int) = 
    let square x = x * x
    if (exp = 0) then 
        1
    else
        if ((exp % 2) = 0) then 
            (square (expmod basen (exp / 2) m)) % m
        else
            (basen * (expmod basen (exp - 1) m)) % m
            
//(define (fermat-test n)
//  (define (try-it a)
//    (= (expmod a n n) a))
//  (try-it (+ 1 (random (- n 1)))))

open System  
let fermat_test (n:int) = 
    let try_it (a:int) = (expmod a n n) = a
    try_it (Random().Next(1, n-1))
    
    
//(define (fast-prime? n times)
//  (cond ((= times 0) true)
//        ((fermat-test n) (fast-prime? n (- times 1)))
//        (else false)))

let rec fast_prime n times = 
    if times = 0 then
        true
    else
        if (fermat_test n) then
            (fast_prime n (times - 1))
        else
            false
            