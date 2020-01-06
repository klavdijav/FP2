#lang racket

(struct true () #:transparent)
(struct false () #:transparent)
(struct zz (n) #:transparent)     ;cela stevila
(struct qq (e1 e2) #:transparent) ;racionalna stevila
(struct cc (e1 e2) #:transparent) ;kompleksna stevila
(struct .. (e1 e2) #:transparent) ;zaporedja
(struct empty () #:transparent)   ;konec zaporedja
(struct if-then-else (condition e1 e2) #:transparent)
(struct is-zz? (e1) #:transparent)
(struct is-qq? (e1) #:transparent)
(struct is-cc? (e1) #:transparent)
(struct is-bool? (e1) #:transparent)
(struct is-seq? (e1) #:transparent)
(struct is-proper-seq? (e1) #:transparent)
(struct is-empty? (e1) #:transparent)
(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct leq? (e1 e2) #:transparent)
(struct rounding (e1) #:transparent)
(struct =? (e1 e2) #:transparent)
(struct left (e1) #:transparent)
(struct right (e1) #:transparent)
(struct ~ (e1) #:transparent)
(struct all? (e1) #:transparent)
(struct any? (e1) #:transparent)

(struct vars (s e1 e2) #:transparent)
(struct valof (s) #:transparent)

(struct fun (name farg body) #:transparent)
(struct proc (name body) #:transparent)
(struct closure (env f) #:transparent)
(struct call (e args) #:transparent)

(define (fri e env)
    (cond 
        [(true? e) e]
        [(false? e) e]
        [(zz? e) (
            if (exact-integer? (zz-n e))
                e
                (error "Error: zz must be integer.")
        )]

        [(qq? e) (
            let* (
                [e1 (fri (qq-e1 e) env)]
                [e2 (fri (qq-e2 e) env)]
                [gd (gcd (zz-n e1) (zz-n e2))]
            )
            (cond 
                [(not (and (zz? e1) (zz? e2))) (error "Error: qq must contain two zz (integers).")]
                [(= (zz-n e2) 0) (error "Error: dividing by zero not allowed.")]
                [(not (= gd 1)) (qq (zz (/ (zz-n e1) gd)) (zz (/ (zz-n e2) gd)))] ; Okrajsanje ulomka
                [#t e]
            )
        )]

        [(cc? e) (
            if (and (qq? (fri (cc-e1 e) env)) (qq? (fri (cc-e2 e) env)))
                e
                (error "Error: cc must contain two qq (racional).")
        )]

        [(..? e) (.. (fri (..-e1 e) env) (fri (..-e2 e) env))]

        [(empty? e) e]

        [(if-then-else? e) (
            let* (
                [cond (fri (if-then-else-condition e) env)]
            )
            (if (false? cond)
                (fri (if-then-else-e2 e) env)
                (fri (if-then-else-e1 e) env)
            )
        )]

        [(is-zz?? e) (
            if (zz? (fri (is-zz?-e1 e) env))
                (true)
                (false)
        )]

        [(is-qq?? e) (
            if (qq? (fri (is-qq?-e1 e) env))
                (true)
                (false)
        )]

        [(is-cc?? e) (
            if (cc? (fri (is-cc?-e1 e) env))
                (true)
                (false)
        )]

        [(is-bool?? e) (
            let ([e1 (fri (is-bool?-e1 e) env)])
            (if (or (true? e1) (false? e1))
                (true)
                (false)
            )
        )]

        [(is-seq?? e) (
            let ([e1 (fri (is-seq?-e1 e) env)])
            (if (or (..? e1) (empty? e1))
                (true)
                (false)
            )
        )]

        [(is-proper-seq?? e) (
            let ([e1 (fri (is-proper-seq?-e1 e) env)])
            (cond 
                [(empty? e1) (true)]
                [(..? e1) (fri (is-proper-seq? (..-e2 e1)) env)]
                [#t (false)]
            )
        )]

        [(is-empty?? e) (
            if (empty? (fri (is-empty?-e1 e) env))
                (true)
                (false)
        )]

        [(add? e) (
            let* (
                [e1 (fri (add-e1 e) env)]
                [e2 (fri (add-e2 e) env)]
                [add-qq (lambda (e1 e2) (
                     let (
                        [e11 (zz-n (qq-e1 e1))]
                        [e12 (zz-n (qq-e2 e1))]
                        [e21 (zz-n (qq-e1 e2))]
                        [e22 (zz-n (qq-e2 e2))]
                    )
                    (fri (qq (zz (+ (* e11 e22) (* e21 e12))) (zz (* e12 e22))) env)
                ))]
                [add-cc (lambda (e1 e2) 
                    (cc (fri (add (cc-e1 e1) (cc-e1 e2)) env) (fri (add (cc-e2 e1) (cc-e2 e2)) env))
                )]
            )
            (cond 
                [(and (true? (fri (is-bool? e1) env)) (true? (fri (is-bool? e2) env))) 
                    (if (and (false? e1) (false? e2))
                        (false)
                        (true)
                    )]
                
                [(and (zz? e1) (zz? e2)) 
                    (zz (+ (zz-n e1) (zz-n e2)))]

                [(and (qq? e1) (qq? e2)) (add-qq e1 e2)]
                [(and (qq? e1) (zz? e2)) (add-qq e1 (qq e2 (zz 1)))]
                [(and (zz? e1) (qq? e2)) (add-qq (qq e1 (zz 1)) e2)]
                
                [(and (cc? e1) (cc? e2)) (add-cc e1 e2)]
                [(and (cc? e1) (zz? e2)) (add-cc e1 (cc (qq e2 (zz 1)) (qq (zz 0) (zz 1))))]
                [(and (zz? e1) (cc? e2)) (add-cc (cc (qq e1 (zz 1)) (qq (zz 0) (zz 1))) e2)]
                [(and (cc? e1) (qq? e2)) (add-cc e1 (cc e2 (qq (zz 0) (zz 1))))]
                [(and (qq? e1) (cc? e2)) (add-cc (cc e1 (qq (zz 0) (zz 1))) e2)]

                [(and (or (..? e1) (empty? e1)) (or (..? e2) (empty? e2))) (
                    letrec (
                        [join (lambda (el)
                            (cond 
                                [(..? el) (.. (..-e1 el) (join (..-e2 el)))]
                                [(empty? el) e2]
                                [#t (.. el (.. (..-e1 e2) (..-e2 e2)))]
                            )
                        )]
                    )
                    (join e1)
                )]

                [#t (error "Error: wrong type argument ADD.")]
            )
        )]

        [(mul? e) (
            letrec (
                [e1 (fri (mul-e1 e) env)]
                [e2 (fri (mul-e2 e) env)]
                [mul-qq (lambda (e1 e2) 
                    (fri (qq (zz (* (zz-n (qq-e1 e1)) (zz-n (qq-e1 e2)))) (zz (* (zz-n (qq-e2 e1)) (zz-n (qq-e2 e2))))) env)
                )]
                [mul-cc (lambda (e1 e2) (
                    let (
                        [ac (mul-qq (cc-e1 e1) (cc-e1 e2))]
                        [ad (mul-qq (cc-e1 e1) (cc-e2 e2))]
                        [bc (mul-qq (cc-e2 e1) (cc-e1 e2))]
                        [bd (mul-qq (cc-e2 e1) (cc-e2 e2))]
                    )
                    (cc (fri (add ac (fri (~ bd) env)) env) (fri (add ad bc) env))
                ))]
            )
            (cond 
                [(and (true? (fri (is-bool? e1) env)) (true? (fri (is-bool? e2) env))) 
                    (if (and (true? e1) (true? e2))
                        (true)
                        (false)
                )]

                [(and (zz? e1) (zz? e2)) (zz (* (zz-n e1) (zz-n e2)))]

                [(and (qq? e1) (qq? e2)) (mul-qq e1 e2)]
                [(and (qq? e1) (zz? e2)) (mul-qq e1 (qq e2 (zz 1)))]
                [(and (zz? e1) (qq? e2)) (mul-qq (qq e1 (zz 1)) e2)]

                [(and (cc? e1) (cc? e2)) (mul-cc e1 e2)]
                [(and (cc? e1) (qq? e2)) (mul-cc e1 (cc e2 (qq (zz 0) (zz 1))))]
                [(and (qq? e1) (cc? e2)) (mul-cc (cc e1 (qq (zz 0) (zz 1))) e2)]
                [(and (cc? e1) (zz? e2)) (mul-cc e1 (cc (qq e2 (zz 1)) (qq (zz 0) (zz 1))))]
                [(and (zz? e1) (cc? e2)) (mul-cc (cc (qq e1 (zz 1)) (qq (zz 0) (zz 1))) e2)]
                

                [#t (error "Error: unsupported argument type.")]
            )
        )]

        [(leq?? e) (
            let (
                [e1 (fri (leq?-e1 e) env)]
                [e2 (fri (leq?-e2 e) env)]
            )
            (cond
                [(and (true? (fri (is-bool? e1) env)) (true? (fri (is-bool? e2) env))) 
                    (if (and (true? e1) (false? e2))
                        (false)
                        (true)
                )]

                [(and (or (true? (fri (is-zz? e1) env)) (true? (fri (is-qq? e1) env)))
                 (or (true? (fri (is-zz? e2) env)) (true? (fri (is-qq? e2) env))))
                    (cond
                        [(and (zz? e1) (zz? e2)) (
                            if (<= (zz-n e1) (zz-n e2))
                                (true)
                                (false)
                        )]
                        
                        [(and (zz? e1) (qq? e2)) (
                            if (<= (zz-n e1) (/ (zz-n (qq-e1 e2)) (zz-n (qq-e2 e2))))
                                (true)
                                (false)
                        )]

                        [(and (qq? e1) (zz? e2)) (
                            if (<= (/ (zz-n (qq-e1 e1)) (zz-n (qq-e2 e1))) (zz-n e2))
                                (true)
                                (false)
                        )]

                        [(and (qq? e1) (qq? e2)) (
                            if (<= (/ (zz-n (qq-e1 e1)) (zz-n (qq-e2 e1))) (/ (zz-n (qq-e1 e2)) (zz-n (qq-e2 e2))))
                                (true)
                                (false)
                        )]
                )]

                [(and (true? (fri (is-seq? e1) env)) (true? (fri (is-seq? e2) env))) (
                    letrec ([count (lambda (s1 s2) (
                        cond 
                            [(or (and (not (..? s1)) (not (..? s2))) (and (not (..? s1)) (..? s2))) (true)]
                            [(and (..? s1) (not (..? s2))) (false)]
                            [#t (count (..-e2 s1) (..-e2 s2))]
                    ))])
                    (count e1 e2)
                )]
            )
        )]

        [(rounding? e) (
            let ([e1 (fri (rounding-e1 e) env)])
            (cond 
                [(qq? e1) (zz (round (/ (zz-n (qq-e1 e1)) (zz-n (qq-e2 e1)))))]
                [(zz? e1) e1]
                [#t (error "Erorr: unsupported argument type.")]
            )
        )]

        [(=?? e) (
            if (equal? (fri (=?-e1 e) env) (fri (=?-e2 e) env))
                (true)
                (false)
        )]

        [(left? e) (
            let ([e1 (fri (left-e1 e) env)])
            (cond 
                [(qq? e1) (qq-e1 e1)]
                [(cc? e1) (cc-e1 e1)]
                [(..? e1) (..-e1 e1)]
                [#t (error "Error: unsupported argument type LEFT.")]
            )
        )]

        [(right? e) (
            let ([e1 (fri (right-e1 e) env)])
            (cond 
                [(qq? e1) (qq-e2 e1)]
                [(cc? e1) (cc-e2 e1)]
                [(..? e1) (..-e2 e1)]
                [#t (error "Error: unsupported argument type RIGHT.")]
            )
        )]

        [(~? e) (
            let ([e (fri (~-e1 e) env)])
            (cond
                [(true? e) (false)]
                [(false? e) (true)]
                [(zz? e) (zz (- (zz-n e)))]
                [(qq? e) (qq (zz (- (zz-n (qq-e1 e)))) (qq-e2 e))]
                [(cc? e) (cc (cc-e1 e) (fri (qq (zz (- (zz-n (qq-e1 (cc-e2 e))))) (qq-e2 (cc-e2 e))) env))]
                [#t (error "Error: unsupported argument type.")]
            )
        )]

        [(all?? e) (
            letrec (
                [find-all (lambda (sez) (
                    if (..? sez)
                        (let (
                            [e1 (fri (..-e1 sez) env)]
                            [e2 (fri (..-e2 sez) env)])
                        (if (or (false? e1) (false? e2))
                            (false)
                            (find-all e2)
                        ))
                        (true)
                ))]
            )
            (find-all (fri (all?-e1 e) env))
        )]

        [(any?? e) (
            letrec (
                [find-any (lambda (sez) (
                    if (..? sez)
                        (let (
                            [e1 (fri (..-e1 sez) env)]
                            [e2 (fri (..-e2 sez) env)])
                        (if (or (true? e1) (true? e2))
                            (true)
                            (find-any e2)
                        ))
                        (false)
                ))]
            )
            (find-any (fri (any?-e1 e) env))
        )]

        [(vars? e) (
            let (
                [envlist (
                    cond 
                        [(list? (vars-s e)) (
                            map cons (vars-s e) (map (lambda (exp) (fri exp env)) (vars-e1 e))
                        )]
                        [#t (list (cons (vars-s e) (fri (vars-e1 e) env)))]
                )]
            )
            (fri (vars-e2 e) (append envlist env))
        )]

        [(valof? e) (
            let (
                [val (assoc (valof-s e) env)]
            )
            (if val 
                (fri (cdr val) env)
                (error (~a "Error: variable " (valof-s e) " not found."))
            )
        )]

        [(fun? e) (
            if (check-duplicates (fun-farg e))
                (error "Error: Duplicate argument names found.")
                (closure env e)
        )]

        [(proc? e) e]

        [(call? e) (
            let* (
                [exp (fri (call-e e) env)]
                [args (map (lambda (x) (fri x env)) (call-args e))]
            )
            (cond
                [(closure? exp) 
                    (fri (fun-body (closure-f exp)) 
                        (append 
                            (map cons (fun-farg (closure-f exp)) args)
                            (list (cons (fun-name (closure-f exp)) (closure-f exp)))
                            (closure-env exp)
                        )
                )]
                [(proc? exp) (fri (proc-body exp) (append (list (cons (proc-name exp) exp)) env))]
                [#t (error "Error: Call function accepts only closure or proc argument.")]
            )
        )]

        ; CISTO NA KONCU
        [#t (error (~a "Error: Unknown FRI command." e))]
    )
)

(define (numerator e1) (left e1))
(define (denominator e1) (right e1))
(define (re e1) (left e1))
(define (im e1) (right e1))

(define (gt? e1 e2) (~(leq? e1 e2)))
(define (inv e1) (
    if-then-else (is-zz? e1)
        (qq (zz 1) e1)
        (if-then-else (is-qq? e1)
            (qq (right e1) (left e1))
            (if-then-else (is-proper-seq? e1)
                (call (fun "reverse" (list "sez") (
                    if-then-else (is-empty? (valof "sez"))
                        (empty)
                        (add (call (valof "reverse") (list (right (valof "sez")))) (.. (left (valof "sez")) (empty)))
                )) (list e1))
                (false)))
))

;(fri (call (fun "add1" (list "el") (add (valof "el") (zz 1))) (list (zz 3))) null)
;(fri (mapping (fun "add1" (list "el") (leq? (valof "el") (zz 3))) (.. (zz 2) (.. (zz 4) (.. (zz 6) (.. (zz 8) (empty)))))) null)
(define (mapping f seq) (
    call (fun "map" (list "seq") (
        if-then-else (is-empty? (valof "seq"))
            (empty)
            (add (.. (call f (list (left (valof "seq")))) (empty)) (call (valof "map") (list (right (valof "seq")))))
    )) (list seq)
))

;(fri (call (fun "add1" (list "el") (leq? (valof "el") (zz 10))) (list (zz 3))) null)
;(fri (filtering (fun "add1" (list "el") (leq? (mul (valof "el") (zz 2)) (zz 10))) (.. (zz 2) (.. (zz 10) (.. (zz 5) (empty))))) null)
(define (filtering f seq) (
    call (fun "filter" (list "seq") (
        if-then-else (is-empty? (valof "seq"))
            (empty)
            (if-then-else (call f (list (left (valof "seq"))))
                (add (.. (left (valof "seq")) (empty)) (call (valof "filter") (list (right (valof "seq")))))
                (call (valof "filter") (list (right (valof "seq"))))
            )
    )) (list seq)
))

; Tests
(require rackunit)

; Podatkovni tipi
(check-not-exn (lambda () (fri (true) 0)))
(check-not-exn (lambda () (fri (false) 0)))
(check-exn exn:fail? (lambda () (fri (zz 2.5) 0)))
(check-exn exn:fail? (lambda () (fri (zz 2.0) 0)))
(check-not-exn (lambda () (fri (zz 2) 0)))
(check-not-exn (lambda () (fri (qq (zz 0) (zz 1)) 0)))
(check-not-exn (lambda () (fri (qq (zz 7) (zz 3)) 0)))
(check-equal? (fri (qq (zz 25) (zz 50)) 0) (qq (zz 1) (zz 2)))
(check-exn exn:fail? (lambda () (fri (qq (true) (zz 0)) 0)))
(check-exn exn:fail? (lambda () (fri (qq (zz 0) (zz 0)) 0)))
(check-exn exn:fail? (lambda () (fri (qq (zz 1) (zz 0)) 0)))
(check-not-exn (lambda () (fri (cc (qq (zz 2) (zz 1)) (qq (zz 5) (zz 1))) 0)))
(check-exn exn:fail? (lambda () (fri (cc (qq (zz 2) (zz 1)) (true)) 0)))
(check-exn exn:fail? (lambda () (fri (.. 1 2) 0)))
(check-not-exn (lambda () (fri (.. (true) (false)) 0)))
(check-not-exn (lambda () (fri (.. (zz 1) (empty)) 0)))
(check-equal? (fri (.. (is-proper-seq? (.. (zz 1) (.. (zz 2) (empty))))
(is-proper-seq? (.. (zz 1) (.. (zz 2) (zz 3))))) null) (.. (true) (false)))
(check-not-exn (lambda () (fri (empty) 0)))
(check-exn exn:fail? (lambda () (fri 2 0)))

; Nadzor toka
(check-equal? (fri (if-then-else (false) (zz 1) (zz 2)) 0) (zz 2))
(check-equal? (fri (if-then-else (true) (zz 1) (zz 2)) 0) (zz 1))
(check-equal? (fri (if-then-else (.. (zz 1) (zz 2)) (zz 1) (zz 2)) 0) (zz 1))

(check-equal? (fri (is-zz? (zz 1)) 0) (true))
(check-equal? (fri (is-zz? (empty)) 0) (false))
(check-equal? (fri (is-qq? (qq (zz 1) (zz 2))) 0) (true))
(check-equal? (fri (is-qq? (zz 2)) 0) (false))
(check-equal? (fri (is-cc? (cc (qq (zz 1) (zz 2)) (qq (zz 2) (zz 3)))) 0) (true))
(check-equal? (fri (is-cc? (zz 2)) 0) (false))
(check-equal? (fri (is-bool? (true)) 0) (true))
(check-equal? (fri (is-bool? (false)) 0) (true))
(check-equal? (fri (is-bool? (.. (true) (false))) 0) (false))
(check-equal? (fri (is-seq? (.. (true) (false))) 0) (true))
(check-equal? (fri (is-seq? (empty)) 0) (true)) 
(check-equal? (fri (is-seq? (zz 2)) 0) (false)) 
(check-equal? (fri (is-proper-seq? (empty)) 0) (true))
(check-equal? (fri (is-proper-seq? (.. (zz 1) (.. (zz 2) (empty)))) 0) (true))
(check-equal? (fri (is-proper-seq? (.. (zz 1) (.. (zz 2) (zz 3)))) 0) (false))
(check-equal? (fri (is-empty? (empty)) 0) (true))
(check-equal? (fri (is-empty? (.. (empty) (true))) 0) (false))

(check-equal? (fri (add (true) (true)) 0) (true))
(check-equal? (fri (add (true) (false)) 0) (true))
(check-equal? (fri (add (false) (true)) 0) (true))
(check-equal? (fri (add (false) (false)) 0) (false))
(check-equal? (fri (add (zz 10) (zz 5)) 0) (zz 15))
(check-equal? (fri (add (qq (zz 1) (zz 3)) (zz 5)) 0) (qq (zz 16) (zz 3)))
(check-equal? (fri (add (qq (zz 1) (zz 2)) (qq (zz 3) (zz 4))) 0) (qq (zz 5) (zz 4)))
(check-equal? (fri (add (zz 5) (qq (zz 1) (zz 3))) 0) (qq (zz 16) (zz 3)))
(check-equal? (fri (add (zz 10) (qq (zz 5) (zz 5))) 0) (qq (zz 11) (zz 1)))
(check-equal? (fri (add (cc (qq (zz 1) (zz 2)) (qq (zz 3) (zz 4))) (zz 1)) 0) (cc (qq (zz 3) (zz 2)) (qq (zz 3) (zz 4))))
(check-equal? (fri (add (zz 1) (cc (qq (zz 1) (zz 2)) (qq (zz 3) (zz 4)))) 0) (cc (qq (zz 3) (zz 2)) (qq (zz 3) (zz 4))))
(check-equal? (fri (add (qq (zz 1) (zz 3)) (cc (qq (zz 1) (zz 2)) (qq (zz 3) (zz 4)))) 0) (cc (qq (zz 5) (zz 6)) (qq (zz 3) (zz 4))))
(check-equal? (fri (add (cc (qq (zz 1) (zz 2)) (qq (zz 3) (zz 4))) (qq (zz 1) (zz 3))) 0) (cc (qq (zz 5) (zz 6)) (qq (zz 3) (zz 4))))
(check-equal? (fri (add (cc (qq (zz 1) (zz 2)) (qq (zz 3) (zz 4))) (cc (qq (zz 1) (zz 3)) (qq (zz 2) (zz 7)))) 0) (cc (qq (zz 5) (zz 6)) (qq (zz 29) (zz 28))))
(check-equal? (fri (add (.. (zz 1) (.. (zz 2) (.. (zz 3) (zz 7)))) (.. (zz 4) (zz 5))) 0)
    (.. (zz 1) (.. (zz 2) (.. (zz 3) (.. (zz 7) (.. (zz 4) (zz 5)))))))

(check-equal? (fri (mul (true) (true)) 0) (true))
(check-equal? (fri (mul (true) (false)) 0) (false))
(check-equal? (fri (mul (false) (true)) 0) (false))
(check-equal? (fri (mul (false) (false)) 0) (false))
(check-equal? (fri (mul (zz 10) (zz 5)) 0) (zz 50))
(check-equal? (fri (mul (qq (zz 1) (zz 5)) (zz 5)) 0) (qq (zz 1) (zz 1)))
(check-equal? (fri (mul (zz 5) (qq (zz 3) (zz 2))) 0) (qq (zz 15) (zz 2)))
(check-equal? (fri (mul (qq (zz 1) (zz 3)) (qq (zz 4) (zz 7))) 0) (qq (zz 4) (zz 21)))
(check-equal? (fri (mul (cc (qq (zz 1) (zz 2)) (qq (zz 3) (zz 4))) (zz 1)) 0) (cc (qq (zz 1) (zz 2)) (qq (zz 3) (zz 4))))
(check-equal? (fri (mul (zz 1) (cc (qq (zz 1) (zz 2)) (qq (zz 3) (zz 4)))) 0) (cc (qq (zz 1) (zz 2)) (qq (zz 3) (zz 4))))
(check-equal? (fri (mul (qq (zz 1) (zz 2)) (cc (qq (zz 1) (zz 2)) (qq (zz 3) (zz 4)))) 0) (cc (qq (zz 1) (zz 4)) (qq (zz 3) (zz 8))))
(check-equal? (fri (mul (cc (qq (zz 1) (zz 2)) (qq (zz 3) (zz 4))) (qq (zz 1) (zz 2))) 0) (cc (qq (zz 1) (zz 4)) (qq (zz 3) (zz 8))))
(check-equal? (fri (mul (cc (qq (zz 1) (zz 2)) (qq (zz 3) (zz 4))) (cc (qq (zz 5) (zz 6)) (qq (zz 7) (zz 8)))) null) (cc (qq (zz -23) (zz 96)) (qq (zz 17) (zz 16))))
(check-equal? (fri (add (mul (true) (true)) (false)) null) (true))

(check-equal? (fri (leq? (true) (true)) 0) (true))
(check-equal? (fri (leq? (true) (false)) 0) (false))
(check-equal? (fri (leq? (false) (true)) 0) (true))
(check-equal? (fri (leq? (false) (false)) 0) (true))

(check-equal? (fri (leq? (zz 2) (zz 3)) 0) (true))
(check-equal? (fri (leq? (zz 3) (zz 3)) 0) (true))
(check-equal? (fri (leq? (zz 5) (zz 3)) 0) (false))

(check-equal? (fri (leq? (zz 1) (qq (zz 3) (zz 2))) 0) (true))
(check-equal? (fri (leq? (zz 2) (qq (zz 4) (zz 2))) 0) (true))
(check-equal? (fri (leq? (zz 2) (qq (zz 2) (zz 2))) 0) (false))

(check-equal? (fri (leq? (qq (zz 1) (zz 2)) (zz 1)) 0) (true))
(check-equal? (fri (leq? (qq (zz 4) (zz 2)) (zz 2)) 0) (true))
(check-equal? (fri (leq? (qq (zz 2) (zz 2)) (zz 2)) 0) (true))

(check-equal? (fri (leq? (qq (zz 2) (zz 2)) (qq (zz 2) (zz 2))) 0) (true))
(check-equal? (fri (leq? (qq (zz 5) (zz 10)) (qq (zz 2) (zz 2))) 0) (true))
(check-equal? (fri (leq? (qq (zz 5) (zz 1)) (qq (zz 2) (zz 3))) 0) (false))

(check-equal? (fri (leq? (empty) (empty)) 0) (true))
(check-equal? (fri (leq? (empty) (.. (zz 1) (zz 2))) 0) (true))
(check-equal? (fri (leq? (.. (zz 1) (zz 2)) (empty)) 0) (false))
(check-equal? (fri (leq? (.. (zz 1) (.. (zz 2) (empty))) 
    (.. (zz 1) (.. (zz 2) (.. (zz 3) (zz 4))))) 0) (true))

(check-equal? (fri (rounding (qq (zz 3) (zz 2))) 0) (zz 2))
(check-equal? (fri (rounding (qq (zz 1) (zz 2))) 0) (zz 0))
(check-equal? (fri (rounding (qq (zz 2) (zz 3))) 0) (zz 1))
(check-equal? (fri (rounding (zz 3)) 0) (zz 3))
(check-exn exn:fail? (lambda () (fri (rounding (empty)) 0)))

(check-equal? (fri (=? (zz 1) (zz 1)) 0) (true))
(check-equal? (fri (=? (add (zz 1) (zz 2)) (zz 3)) 0) (true))
(check-equal? (fri (=? (mul (zz 1) (zz 2)) (zz 3)) 0) (false))
(check-equal? (fri (=? (qq (zz 2) (zz 4)) (qq (zz 1) (zz 2))) 0) (true))

(check-equal? (fri (left (qq (zz 1) (zz 2))) 0) (zz 1))
(check-equal? (fri (left (cc (qq (zz 2) (zz 1)) (qq (zz 5) (zz 1)))) 0) 
    (qq (zz 2) (zz 1)))
(check-equal? (fri (left (.. (zz 1) (.. (zz 2) (empty)))) 0) (zz 1))

(check-equal? (fri (right (qq (zz 1) (zz 2))) 0) (zz 2))
(check-equal? (fri (right (cc (qq (zz 2) (zz 1)) (qq (zz 5) (zz 1)))) 0) 
    (qq (zz 5) (zz 1)))
(check-equal? (fri (right (.. (zz 1) (.. (zz 2) (empty)))) 0) (.. (zz 2) (empty)))

(check-equal? (fri (~ (true)) 0) (false))
(check-equal? (fri (~ (false)) 0) (true))
(check-equal? (fri (~ (zz 1)) 0) (zz -1))
(check-equal? (fri (~ (qq (zz 2) (zz 3))) 0) (qq (zz -2) (zz 3)))
(check-equal? (fri (~ (cc (qq (zz 2) (zz 3)) (qq (zz 9) (zz 3)))) 0) 
    (cc (qq (zz 2) (zz 3)) (qq (zz -3) (zz 1))))

(check-equal? (fri (all? (.. (true) (.. (true) (true)))) 0) (true))
(check-equal? (fri (all? (.. (true) (.. (false) (true)))) 0) (false))
(check-equal? (fri (all? (right (.. (zz 1) (.. (zz 2) (empty)))) ) 0) (true))

(check-equal? (fri (any? (.. (false) (empty))) 0) (false))
(check-equal? (fri (any? (.. (false) (.. (false) (true)))) 0) (true))
(check-equal? (fri (any? (left (.. (.. (add (true) (false)) (false)) (.. (zz 2) (empty)))) ) 0) (true))





#| DLESKdoleksdlokeldo ksledkeslkd lsekdselokd djeskidj seikd jse kijdksejdisjdkisejdkisjkijesijsiejsifjaij aki jkfijikfewjikwejik|#
(require rackunit/text-ui)

(define all-tests
  (test-suite
   "all"
 (test-suite
  "pulic"
  (test-case "add1" (check-equal?
                     (add (mul (true) (true)) (false))
                     (add (mul (true) (true)) (false))))
 
  (test-case "add2" (check-equal?
                     (fri (add (mul (true) (true)) (false)) null)
                     (true)))
 
  (test-case "proper-seq1" (check-equal?
                            (is-proper-seq? (.. (zz 1) (.. (zz 2) (empty))))
                            (is-proper-seq? (.. (zz 1) (.. (zz 2) (empty))))))
 
  (test-case "proper-seq2" (check-equal?
                            (fri (.. (is-proper-seq? (.. (zz 1) (.. (zz 2) (empty))))
                                     (is-proper-seq? (.. (zz 1) (.. (zz 2) (zz 3))))) null)
                            (.. (true) (false))))
 
  (test-case "vars-and-complex1" (check-equal?
                                  (fri (vars "a" (cc (qq (zz 1) (zz 2)) (qq (zz -3) (zz 4)))
                                             (mul (valof "a") (valof "a"))) null)
                                  (cc (qq (zz -5) (zz 16)) (qq (zz -3) (zz 4)))))
 
  (test-case "vars-and-complex2" (check-equal?
                                  (fri (vars (list "a" "b")
                                             (list (cc (qq (zz 1) (zz 2)) (qq (zz -3) (zz 4)))
                                                   (~ (cc (qq (zz 1) (zz 2)) (qq (zz -3) (zz 4)))))
                                             (add (valof "a") (valof "b"))) null)
                                  (cc (qq (zz 1) (zz 1)) (qq (zz 0) (zz 1)))))

  (test-case "fib1" (check-equal?
                     (fri (call (fun "fib" (list "n")
                                     (if-then-else (leq? (valof "n") (zz 2))
                                                   (zz 1) (add (call (valof "fib")
                                                                     (list (add (valof "n") (zz -1))))
                                                               (call (valof "fib")
                                                                     (list (add (valof "n") (zz -2)))))))
                                (list (zz 10))) null)
                     (zz 55)))
 
  (test-case "seq1" (check-equal?
                     (fri (all? (.. (true) (.. (leq? (false) (true))
                                               (.. (=? (.. (zz -19) (zz 0))
                                                       (.. (left (add (qq (zz 1) (zz 5)) (zz -4)))
                                                           (zz 0)))
                                                   (empty)))))
                          null)
                     (true)))
 
  (test-case "variables1" (check-equal?
                           (fri (vars (list "a" "b" "c")
                                      (list (zz 1) (zz 2) (zz 3))
                                      (fun "linear" (list "x1" "x2" "x3")
                                           (add (mul (valof "a") (valof "x1"))
                                                (add (mul (valof "b") (valof "x2"))
                                                     (mul (valof "c") (valof "x3")))))) null)
                           (closure (list (cons "a" (zz 1))(cons "b" (zz 2)) (cons "c" (zz 3)))
                                    (fun "linear" '("x1" "x2" "x3")
                                         (add (mul (valof "a") (valof "x1"))
                                              (add (mul (valof "b") (valof "x2"))
                                                   (mul (valof "c") (valof "x3")))))))))
 
 (test-suite
  "misc"
  (test-case "add-seq" (check-equal?
                        (fri (add (.. (false) (empty))
                                  (.. (zz 3) (empty))) null)
                        (.. (false) (.. (zz 3) (empty)))))
 
  (test-case "add-empty" (check-equal?
                          (fri (add (empty) (empty)) null)
                          (empty))))

 #| (test-case
  "long-long"
  (check-equal?
   (fri
    (vars "a" (zz 10)
          (vars (list "f" "g")
                (list (fun "" (list "a" "b")
                           (add (valof "a") (mul (zz 5) (valof "b"))))
                      (fun "" (list "c")
                           (add (valof "a") (valof "c"))))
                (vars (list "a" "d" "g" "e")
                      (list (zz 1)
                            (call (valof "g") (list (zz -9)))
                            (fun "" (list "x")
                                 (add (valof "a") (mul (valof "x")
                                                       (call (valof "f")
                                                             (list (zz 1) (valof "a"))))))
                            (fun "" (list "f" "x")
                                 (call (valof "f") (list (valof "x")))))
                      (vars (list "fib" "test" "unit-fun" "proc")
                            (list (fun "fib" (list "n")
                                       (if-then-else (leq? (valof "n") (zz 2))
                                                     (zz 1)
                                                     (add (call (valof "fib")
                                                                (list (add (valof "n")
                                                                           (zz -1))))
                                                          (call (valof "fib")
                                                                (list (add (valof "n")
                                                                           (zz -2)))))))
                                  (fun "" (list "x")
                                       (add (valof "x") (zz 2)))
                                  
                                  (fun "" null
                                       (add (inv (add (valof "a")
                                                      (valof "a")))
                                            (valof "a")))
                                  
                                  (proc ""
                                        (folding
                                         (fun "" (list "x" "acc") (mul (valof "x") (valof "acc")))
                                         (zz 1)
                                         (.. (valof "a")
                                             (.. (zz 2)
                                                 (.. (zz 3)
                                                     (.. (zz 4)
                                                         (.. (call (valof "g")
                                                                   (list (zz 5)))
                                                             (empty)))))))))
                            
                            
                            (.. (call (valof "unit-fun") null)
                                (.. (call (valof "proc") null)
                                    (add (call (valof "g")
                                               (list (add (zz 5)
                                                          (call (valof "test")
                                                                (list (zz 3))))))
                                         (add (valof "d")
                                              (add (call (valof "f")
                                                         (list (zz -1) (zz -2)))
                                                   (add (valof "a")
                                                        (add (call (valof "fib")
                                                                   (list (zz 5)))
                                                             (call (valof "e")
                                                                   (list (valof "test") (zz 3))))))))))))))
    null)
   (.. (qq (zz 3) (zz 2)) (.. (zz 6360) (zz 521))))) |#
   
   
   ))

(run-tests all-tests)