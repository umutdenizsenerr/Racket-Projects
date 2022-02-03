; umut deniz sener
; 2018400225
; compiling: yes
; complete: yes
#lang racket

;; given
(struct num (value grad)
    #:property prop:custom-write
    (lambda (num port write?)
        (fprintf port (if write? "(num ~s ~s)" "(num ~a ~a)")
            (num-value num) (num-grad num))))

(define mynumber (num 12.3 1.0))

(define get-value (lambda(num-list)  (if (list? num-list)
  (map (lambda (element) (num-value (eval element))) num-list )(num-value num-list)))
  )
(define get-grad (lambda(num-list)  (if (list? num-list)
  (map (lambda (element) (num-grad (eval element))) num-list )(num-grad num-list)))
  )

(define add
  (lambda (start . nums)
   (num (apply +(get-value(cons start nums)))
    (apply +(get-grad(cons start nums))))

    ))
(define mull (lambda args(
    cond [(= (length args) 2) (+ (* (get-grad (car(cdr args))) (get-value (car args))) (*(get-value (car(cdr args))) (get-grad (car args))))]
        [else
        (+ (*(get-grad (car args)) (apply * (get-value(cdr args)))) (* (get-value (car args))
         (apply mull (cdr args))))

            ]              
)))
(define mul (lambda args (num (apply *(get-value args)) (apply mull args ))))
                      
  
(define sub
   (lambda (start . nums)
   (num (apply -(get-value(cons start nums)))
    (apply -(get-grad(cons start nums))))

    ))

 (define relu (lambda (x) (if (> (num-value x) 0) x (num 0.0 0.0))))
 (define mse (lambda (x y) (mul (sub x y) (sub x y))))

  (define h (make-hash))

(define create-hash (lambda (names values var) 
                      (cond [(= (length names) 0) h]
                            [else (hash-set! h (car names) (num(car values) (cond [(eq? var (car names)) 1.0]
                                                                                  [else 0.0]


                                                                                  ))) (create-hash(cdr names)(cdr values) var)]

)))

(define parse (lambda (hash expr) (

cond [(eq? expr '()) '() ]
     [(list? expr) (cons (parse hash (car expr)) (parse hash (cdr expr)))]
     [(eq? '+ expr) 'add]
     [(eq? '* expr) 'mul]
     [(eq? '- expr) 'sub]
     [(eq? 'mse expr) 'mse]
     [(eq? 'relu expr) 'relu]
     [(number? expr) (num expr 0.0) ]
     [else (hash-ref hash expr)]
                                   )))
(define grad (lambda (names values var expr)

(num-grad (eval(parse(create-hash names values var ) expr)))
               ))
 


(define partial-grad (lambda (names values vars expr)

     (map (lambda (name) (cond [(member name vars) (grad names values name expr)]
                               [else 0.0])) names) 
     
                ))


(define gradient-descent (lambda (names values vars lr expr)
                (map (lambda (value result) (- value (* lr result))) values (partial-grad names values vars expr)
                                )
                           ))
(define optimize (lambda (names values vars lr k expr)

               (let ((k (- k 1)))
                 (cond [(> k 0)
                 (optimize names (gradient-descent names values vars lr expr) vars lr k expr)]

                 [else (gradient-descent names values vars lr expr)])
                 )



                   ))