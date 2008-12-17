#light

        
/// 1.3  Procedures and the Processes They Generate
//(define (cube x) (* x x x))

let cube x = x * x * x


/// 1.3.1  Procedures as Arguments

//(define (sum-integers a b)
//  (if (> a b)
//      0
//      (+ a (sum-integers (+ a 1) b))))

let rec sum_integers a b =
    if (a > b) then
        0
    else
        (a + (sum_integers (a + 1) b))


//(define (sum-cubes a b)
//  (if (> a b)
//      0
//      (+ (cube a) (sum-cubes (+ a 1) b))))

let rec sum_cubes a b =
    if (a > b) then
        0
    else
        (cube a + (sum_cubes (a + 1) b))


//(define (pi-sum a b)
//  (if (> a b)
//      0
//      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

let rec pi_sum (a:float) (b:float) =
    if (a > b) then
        0.0
    else
         (1.0 / (a * (a + 2.0))) + (pi_sum (a + 4.0) b)


//(define (<name> a b)
//  (if (> a b)
//      0
//      (+ (<term> a)
//         (<name> (<next> a) b))))

//let <name> a b =
//    if (a > b) then
//        0
//    else
//        (<term> a) + (<name> (<next> a) b)


//(define (sum term a next b)
//  (if (> a b)
//      0
//      (+ (term a)
//         (sum term (next a) next b))))

let rec sum term a next b = 
    if (a > b) then
        0
    else
        (term a) + (sum term (next a) next b)


//(define (inc n) (+ n 1))
//(define (sum-cubes a b)
//  (sum cube a inc b))

let inc n = (n + 1)
let sum_cubes_alt a b = sum cube a inc b


//(sum-cubes 1 10)
//3025

//(define (identity x) x)
//
//(define (sum-integers a b)
//  (sum identity a inc b))

let identity x = x
let sum_integers_alt a b = sum identity a inc b

//(define (pi-sum a b)
//  (define (pi-term x)
//    (/ 1.0 (* x (+ x 2))))
//  (define (pi-next x)
//    (+ x 4))
//  (sum pi-term a pi-next b))

let rec sum_float term a next b = 
    if (a > b) then
        0.0
    else
        (term a) + (sum_float term (next a) next b)

let pi_sum_alt (a:float) (b:float) = 
    let pi_term (x:float) = 
        (x * (x + 2.0)) / 1.0
    let pi_next (x:float) = x + 4.0
    (sum_float pi_term a pi_next b)
    
    
//(define (integral f a b dx)
//  (define (add-dx x) (+ x dx))
//  (* (sum f (+ a (/ dx 2.0)) add-dx b)
//     dx))

let integral f a b dx = 
    let add_dx x = (x + dx)
    (sum_float f (a + (dx / 2.0)) add_dx b) * dx
    

//(integral cube 0 1 0.01)
//.24998750000000042
//(integral cube 0 1 0.001)
//.249999875000001

let cube_float x:float = x * x * x

printfn "%f" (integral cube_float 0.0 1.0 0.01)
//      -> val it : float = 0.2499875
printfn "%f" (integral cube_float 0.0 1.0 0.001)
//      -> val it : float = 0.249999875
//      but prints 0.250000

/// 1.3.2  Constructing Procedures Using Lambda

//(lambda (x) (+ x 4))
//(fun x -> x + 4)

//(lambda (x) (/ 1.0 (* x (+ x 2))))
//(fun x ->  1.0 / (x * (x+2.0))



//(define (pi-sum a b)
//  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
//       a
//       (lambda (x) (+ x 4))
//       b))

let pi_sum_lambda a b = 
    (sum_float (fun x -> 1.0 / (x * (x + 2.0))) a (fun x -> x + 4.0) b)
    

let sum_integers_lambda a b = 
    sum (fun x -> x) a (fun x -> x+1) b
    
//(define (integral f a b dx)
//  (* (sum f
//          (+ a (/ dx 2.0))
//          (lambda (x) (+ x dx))
//          b)
//     dx))

let integral_lambda f a b dx = 
    (sum_float f (a + (dx / 2.0)) (fun x -> x + dx) b) * dx
    
    
//(lambda (<formal-parameters>) <body>)
//(fun <formal-parameters> -> <body>)



/// Using let to create local variables
//(define (f x y)
//  (define (f-helper a b)
//    (+ (* x (square a))
//       (* y b)
//       (* a b)))
//  (f-helper (+ 1 (* x y)) 
//            (- 1 y)))

let f x y = 
    let square a = a * a
    let f_helper a b = 
       ((square a) * x) + (y * b) + (a * b)
    (f_helper (1 + (x * y)) (1 - y))

//(define (f x y)
//  ((lambda (a b)
//     (+ (* x (square a))
//        (* y b)
//        (* a b)))
//   (+ 1 (* x y))
//   (- 1 y)))


let f_alt x y = 
    let square a = a * a
    ((fun a b -> ((square a) * x) + (y * b) + (a * b)) (1 + (x * y)) (1 - y))
    
//(define (f x y)
//  (let ((a (+ 1 (* x y)))
//        (b (- 1 y)))
//    (+ (* x (square a))
//       (* y b)
//       (* a b))))

let f_let x y =
    let square j = j * j
    let a = 1 + x * y
    let b = 1 - y
    (x * square a) + (y * b) + (a * b)
    


/// 1.3.3  Procedures as General Methods


/// Finding roots of equations by the half-interval method

//(define (search f neg-point pos-point)
//  (let ((midpoint (average neg-point pos-point)))
//    (if (close-enough? neg-point pos-point)
//        midpoint
//        (let ((test-value (f midpoint)))
//          (cond ((positive? test-value)
//                 (search f neg-point midpoint))
//                ((negative? test-value)
//                 (search f midpoint pos-point))
//                (else midpoint))))))

//(define (close-enough? x y)
//  (< (abs (- x y)) 0.001))

let abs (x:float) = if x < 0.0 then x * -1.0 else x
let average (x:float) (y:float) = (x + y) / 2.0
let close_enoughQ (x:float) (y:float) = abs (x-y) < 0.001

let positiveQ (x:float) = x > 0.0
let negativeQ (x:float) = x < 0.0

let rec search f (neg_point:float) (pos_point:float) = 
    let midpoint = average neg_point pos_point
    if close_enoughQ neg_point pos_point then
        midpoint
    else
        let test_value = f midpoint
        if (positiveQ test_value) then
            search f neg_point midpoint
        else 
            if (negativeQ test_value) then
                search f midpoint pos_point
            else
                midpoint
        
//(define (half-interval-method f a b)
//  (let ((a-value (f a))
//        (b-value (f b)))
//    (cond ((and (negative? a-value) (positive? b-value))
//           (search f a b))
//          ((and (negative? b-value) (positive? a-value))
//           (search f b a))
//          (else
//           (error "Values are not of opposite sign" a b)))))        
        
let half_interval_method f (a:float) (b:float) =
    let a_value = f a
    let b_value = f b
    
    if (negativeQ a_value) && (positiveQ b_value) then
        search f a b
    else if 
        (negativeQ b_value) && (positiveQ a_value) then
        search f b a
    else
        printfn "Fail Dude\n" |> ignore
        0.0
         
half_interval_method sin 2.0 4.0

//(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
//                      1.0
//                      2.0)

half_interval_method (fun x -> x * x * x - 2.0 * x - 3.0) 1.0 2.0



//Finding fixed points of functions

//(define tolerance 0.00001)
//(define (fixed-point f first-guess)
//  (define (close-enough? v1 v2)
//    (< (abs (- v1 v2)) tolerance))
//  (define (try guess)
//    (let ((next (f guess)))
//      (if (close-enough? guess next)
//          next
//          (try next))))
//  (try first-guess))    

let tolerance = 0.000001
let fixed_point f first_guess = 
    let close_enoughQ (v1:float) (v2:float) = abs (v1-v2) < tolerance
    let rec try_ guess = 
        let next = f guess
        if close_enoughQ guess next then
            next
        else
            try_ next
    try_ first_guess
         
fixed_point cos 1.0

//(fixed-point (lambda (y) (+ (sin y) (cos y)))
//             1.0)

fixed_point (fun (y:float) -> sin y + cos y) 1.0


//(define (sqrt x)
//  (fixed-point (lambda (y) (average y (/ x y)))
//               1.0))

let sqrt x = 
    let average (f1:float) (f2:float) = (f1 + f2) / 2.0
    fixed_point (fun y -> printfn "%f/%f = %f" x y (x/y) |> ignore 
                          (average y (x/y)) ) 1.0
    


/// 1.3.4  Procedures as Returned Values

//(define (average-damp f)
//  (lambda (x) (average x (f x))))

let average_damp f =
    let average (i:float) (j:float) = (i + j) / 2.0
    (fun x -> average x (f x))
    
let square x = x * x

(average_damp square) 10

//(define (sqrt x)
//  (fixed-point (average-damp (lambda (y) (/ x y)))
//               1.0))

let sqrt x =
    fixed_point (average_damp (fun y -> x/y)) 1.0

sqrt 9.0


//(define (cube-root x)
//  (fixed-point (average-damp (lambda (y) (/ x (square y))))
//               1.0))

let cube_root x = 
    let square i:float = i * i
    fixed_point (average_damp (fun y -> (x / square y))) 1.0

    
/// Newton's method

//(define (deriv g)
//  (lambda (x)
//    (/ (- (g (+ x dx)) (g x))
//       dx)))
//(define dx 0.00001)

let dx = 0.00001
let deriv g = 
    (fun x -> (g(x + dx) - (g x)) / dx)

let cube (f:float) = f * f * f
cube 5.0
(deriv cube) 5.0
    
    
//(define (newton-transform g)
//  (lambda (x)
//    (- x (/ (g x) ((deriv g) x)))))
//(define (newtons-method g guess)
//  (fixed-point (newton-transform g) guess))

let newton_transform g = 
    (fun x -> (x - (g x) / ((deriv g) x)))
let newton_method g guess = 
    fixed_point (newton_transform g) guess
    
//(define (sqrt x)
//  (newtons-method (lambda (y) (- (square y) x))
//                  1.0))

let sqrt x = 
    let square (f:float) = f*f
    newton_method (fun y -> square y - x) 1.0



/// Abstractions and first-class procedures

//(define (fixed-point-of-transform g transform guess)
//  (fixed-point (transform g) guess))

let fixed_point_of_transform g transform guess = 
    (fixed_point (transform g) guess)

//(define (sqrt x)
//  (fixed-point-of-transform (lambda (y) (/ x y))
//                            average-damp
//                            1.0))

let sqrt x = fixed_point_of_transform (fun y -> x/y) average_damp 1.0


//(define (sqrt x)
//  (fixed-point-of-transform (lambda (y) (- (square y) x))
//                            newton-transform
//                            1.0))

let sqrt x = 
    let square (f:float) = f * f
    fixed_point_of_transform (fun y -> square y - x) newton_transform 1.0
