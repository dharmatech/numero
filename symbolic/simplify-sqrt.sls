
(library (numero symbolic simplify-sqrt)

  (export simplify-sqrt)

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic simplify-parameters))

  (define (integer-with-square-factor? a)

    (and (> a 2)

         (let loop ((i 2))

           (cond ((> i (/ a 2)) #f)

                 ((= (mod a (* i i)) 0) i)

                 (else (loop (+ i 1)))))))

  (define (simplify-sqrt expr)

    (match expr

      ;; sqrt( a )    where x has a square factor

      ( ('sqrt (? integer-with-square-factor? a))

        (let ((b (integer-with-square-factor? a)))

          `(* ,b (sqrt ,(/ a (* b b))))) )

      ( ('sqrt (? number? n))
        (let ((val (sqrt n)))
          (if (simplify-exact?)
              (if (exact? val) val expr)
              val)) )

      ( else expr )))

  )