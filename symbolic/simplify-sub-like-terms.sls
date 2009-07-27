
(library (numero symbolic simplify-sub-like-terms)

  (export simplify-sub-like-terms)

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic simplify-util))

  (define (simplify-sub-like-terms expr)
    (match expr
      ;; (a - b)   where a and b are like terms
      ( (and ('- a b)
             (? (lambda (_) (like-terms? a b))))
        `(* (- ,(coefficient a)
               ,(coefficient b))
            ,(strip-coefficient a)) )

      ;; (a + b) - c
      ;; (b + a) - c
      ;; where a contains term like c    (a - c) + b
      
      ( (or (and ('- ('+ a b) c)
                 (? (lambda (_) (contains-like-term? a c))))
            (and ('- ('+ b a) c)
                 (? (lambda (_) (contains-like-term? a c)))))
        
        `(+ ,(simplify-sub-like-terms `(- ,a ,c)) ,b) )


      ;; (a - b) - c    where a contains term like c    (a - c) - b

      ( (and ('- ('- a b) c)
             (? (lambda (_) (contains-like-term? a c))))
        
        `(- ,(simplify-sub-like-terms `(- ,a ,c)) ,b) )

      ;; (b - a) - c    where a contains term like c    (- a - c) + b

      ( (and ('- ('- b a) c)
             (? (lambda (_) (contains-like-term? a c))))
        
        `(+ ,(simplify-sub-like-terms `(- (- ,a) ,c)) ,b) )

      (else expr)))
  
  )
