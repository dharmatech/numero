
(library (numero symbolic expand)

  (export expand expand-loop)

  (import (rnrs) (xitomatl AS-match))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (expand-mul expr)

    (match expr

      ( (or ('* a ('+ b c)) ('* ('+ b c) a)) `(+ (* ,a ,b) (* ,a ,c)) )

      ( (or ('* a ('- b c)) ('* ('- b c) a)) `(- (* ,a ,b) (* ,a ,c)) )

      ( ('* a b) `(* ,(expand a) ,(expand b)) )

      ))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (expand-pow expr)

    (match expr

      ( ('^ a 1) a )

      ( ('^ ('+ a b) (? (lambda (n) (and (number? n) (>= n 2))) n))

        (expand `(* (+ ,a ,b) (^ (+ ,a ,b) ,(- n 1)))) )

      ( ('^ ('- a b) (? (lambda (n) (and (number? n) (>= n 2))) n))

        (expand `(* (- ,a ,b) (^ (+ ,a ,b) ,(- n 1)))) )

      ( else expr )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (expand expr)

    (match expr

      ( ('* a b) (expand-mul expr) )

      ( ('^ a b) (expand-pow expr) )

      ( ('+ a b) `(+ ,(expand a) ,(expand b)) )

      ( ('- a b) `(- ,(expand a) ,(expand b)) )

      ( ('/ a b) `(/ ,(expand a) ,(expand b)) )

      ( else expr )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (expand-loop old)
    (let ((new (expand old)))
      (if (equal? old new)
          old
          (expand-loop new))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )