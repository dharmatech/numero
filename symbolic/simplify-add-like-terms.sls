
(library (numero symbolic simplify-add-like-terms)

  (export simplify-add-like-terms)

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic simplify-util))

  (define (simplify-add-like-terms expr)
    (match expr
      ;; (a + b)   where a and b are like terms
      ((and ('+ a b) (? (lambda (expr) (like-terms? a b))))
       `(* (+ ,(coefficient a)
              ,(coefficient b))
           ,(strip-coefficient a)))
      ;; (a + b) + c    where a contains term like c    ((a + c) + b)
      ((and ('+ ('+ a b) c) (? (lambda (expr) (contains-like-term? a c))))
       `(+ ,(simplify-add-like-terms `(+ ,a ,c)) ,b))
      ;; (a + b) + c    where b contains term like c    ((c + b) + a)
      ((and ('+ ('+ a b) c) (? (lambda (expr) (contains-like-term? b c))))
       `(+ ,(simplify-add-like-terms `(+ ,c ,b)) ,a))
      ((? list?) (map simplify-add-like-terms expr))
      (else expr)))
  )
