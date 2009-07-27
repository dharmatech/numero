
(library (numero symbolic simplify-div)

  (export simplify-div

          simplify-from-div
          simplify-to-div
          
          )

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic simplify-parameters))

  ;; (define (simplify-div expr)
  ;;   (match expr
  ;;     ( ('/ (? number? a) (? number? b))
  ;;       (let ((val (/ a b)))
  ;;         (if (simplify-exact?)
  ;;             (if (exact? val) val expr)
  ;;             val)) )
  ;;     ( ('/ 0 x) 0 )
  ;;     ( ('/ x 1) x )

  ;;     ( ('/ a b)

  ;;       `(* ,a (^ ,b -1)) )
      
  ;;     ( else expr )))

  (define (simplify-div expr)
    (match expr
      ( ('/ (? number? a) (? number? b))
        (let ((val (/ a b)))
          (if (simplify-exact?)
              (if (exact? val) val expr)
              val)) )
      ( ('/ 0 x) 0 )
      ( ('/ x 1) x )

      ( else expr )))

  (define (negative-number? n)
    (and (number? n)
         (negative? n)))

  (define (simplify-from-div expr)
    (match expr
      ( ('/ a b)
        `(* ,a (^ ,b -1)) )
      (else expr)))

  (define (simplify-to-div expr)
    (match expr
      ( ('^ x (? negative-number? n))
        `(/ 1 (^ ,x ,(- n))) )
      (else expr)))

  )