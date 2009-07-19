
(library (numero symbolic expand)

  (export expand)

  (import (rnrs) (xitomatl AS-match)
          (numero symbolic rewrite)
          )

  ;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (expand-mul expr)

    (match expr

      ;; a * (b + c)    a b + a c

      ( (or ('* a ('+ b c)) ('* ('+ b c) a)) `(+ (* ,a ,b) (* ,a ,c)) )

      ;; a * (b - c)    a b - a c

      ( (or ('* a ('- b c)) ('* ('- b c) a)) `(- (* ,a ,b) (* ,a ,c)) )

      ((? list?) (map expand-mul expr))
      
      (else expr)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (expand-pow expr)

    (match expr

      ;; a ^ 1    a
      
      (('^ a 1) a)

      ;; (a + b) ^ 5    (a + b) * (a + b) ^ 4

      (('^ ('+ a b) (? (lambda (n) (and (number? n) (>= n 2))) n))

       `(* (+ ,a ,b) (^ (+ ,a ,b) ,(- n 1))))

      ;; (a - b) ^ 5    (a - b) * (a - b) ^ 4

      (('^ ('- a b) (? (lambda (n) (and (number? n) (>= n 2))) n))

       `(* (- ,a ,b) (^ (- ,a ,b) ,(- n 1))))

      ((? list?) (map expand-pow expr))
      
      (else expr)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (expand expr)
    (rewrite (lambda (expr)
               (expand-pow (expand-mul expr)))
             expr))

  )