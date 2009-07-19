
(library (numero symbolic solve-for)

  (export solve-for solve-for-step)

  (import (rnrs)
          (only (srfi :1) any)
          (xitomatl AS-match)
          (numero symbolic rewrite)
          )

  (define (contains? f x)
    (or (equal? f x)
        (and (list? f)
             (any (lambda (elt) (contains? elt x)) f))))

  (define (solve-for-step eq x)

    (match eq

      ;; x = b    x = b

      ( (and ('= a b)

             (?   (lambda (_)   (equal? a x)   )))

        eq )

      ;; b = a    a = b

      ( (and ('= b a)

             (?   (lambda (_)   (contains? a x)   )))

        `(= ,a ,b) )

      ;; a + b = z    a = z - b

      ( (and ('= ('+ a b) c)
             
             (?   (lambda (_)   (contains? a x)   )))

        `(= ,a (- ,c ,b)) )

      ;; b + a = z    a = z - b

      ( (and ('= ('+ b a) c)
             
             (?   (lambda (_)   (contains? a x)   )))

        `(= ,a (- ,c ,b)) )

      ;; a * b = c    a = c / b

      ( (and ('= ('* a b) c)

             (?   (lambda (_)   (contains? a x)   )))

        `(= ,a (/ ,c ,b)) )

      ;; b * a = c    a = c / b

      ( (and ('= ('* b a) c)

             (?   (lambda (_)   (contains? a x)   )))

        `(= ,a (/ ,c ,b)) )

      ;; a / b = c    a = c b

      ( (and ('= ('/ a b) c)

             (?   (lambda (_)   (contains? a x)   )))

        `(= ,a (* ,b ,c)) )

      ;; b / a = c    a = b / c

      ( (and ('= ('/ b a) c)

             (?   (lambda (_)   (contains? a x)   )))

        `(= ,a (/ ,b ,c)) )

      ;; a ^ b = c    where 'b' is an integer    a = c^(1/b)

      ( (and ('= ('^ a b) c)

             (?   (lambda (_)   (contains? a x)   ))
             (?   (lambda (_)   (integer?  b)     )))

        `(= ,a (^ ,c (/ 1 ,b))) )

      ))

  (define (solve-for eq x)
    (rewrite (lambda (eq) (solve-for-step eq x)) eq))

  )