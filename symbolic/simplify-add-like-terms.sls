
(library (numero symbolic simplify-add-like-terms)

  (export simplify-add-like-terms)

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic simplify-util))

  (define (simplify-add-like-terms expr)
    (match expr

      ;; (a + b)   where a and b are numbers
      ( (and ('+ a b)
             (? (lambda (_) (and (number? a) (number? b)))))
        `(+ ,a ,b) )
      
      ;; (a + b)   where a and b are like terms
      ( (and ('+ a b)
             (? (lambda (_) (like-terms? a b))))
        `(* (+ ,(coefficient a)
               ,(coefficient b))
            ,(strip-coefficient a)) )

      ;; (a + b) + c
      ;; (b + a) + c
      ;; where a contains term like c    ((a + c) + b)

      ( (or (and ('+ ('+ a b) c)
                 (? (lambda (_) (contains-like-term? a c))))
            (and ('+ ('+ b a) c)
                 (? (lambda (_) (contains-like-term? a c)))))
        
        `(+ ,(simplify-add-like-terms `(+ ,a ,c)) ,b) )

      ;; (a - b) + c    where a contains term like c    ((a + c) - b)

      ( (and ('+ ('- a b) c)
             (? (lambda (_) (contains-like-term? a c))))
        
        `(- ,(simplify-add-like-terms `(+ ,a ,c)) ,b) )

      ;; (b - a) + c    where a contains term like c    (c - a) + b

      ( (and ('+ ('- b a) c)
             (? (lambda (_) (contains-like-term? a c))))
        
        `(+ (- ,c ,a) ,b) )

      (else expr)))
  )
