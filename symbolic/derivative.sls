
(library (numero symbolic derivative)

  (export D)

  (import (rnrs) (xitomatl AS-match))

  (define (D expr var)

    (define (Dv expr)
      (D expr var))

    (match expr

      ( (? number?) 0 )

      ( (? (lambda (x) (eq? expr var))) 1 )

      ( (? symbol?) 0 )

      ( ('+ a b) `(+ ,(Dv a) ,(Dv b)) )

      ( ('- a b) `(- ,(Dv a) ,(Dv b)) )

      ( ('* a b) `(+ (* ,(Dv a) ,b) (* ,a ,(Dv b))) )

      ( ('/ a b) `(/ (- (* ,(Dv a) ,b) (* ,a ,(Dv b))) (* ,b ,b)) )

      ( ('^ 'e x) `(* (^ e ,x) ,(Dv x)) )

      ( ('^ f n) `(* (* ,n (^ ,f (- ,n 1))) ,(Dv f)) )

      ( ('- x)   `(- ,(Dv x)) )

      ( ('sin x) `(* (cos ,x) ,(Dv x)) )

      ( ('cos x) `(* (- (sin ,x)) ,(Dv x)) )

      ( ('= a b) `(= ,(Dv a) ,(Dv b)) )

      ( (and (f x)

             (?   (lambda (_)   (eq? x var)   )))

        `(D (,f ,x) ,var) )

      ))

  )
