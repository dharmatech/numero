
(library (numero symbolic simplify-mul-vector)

  (export simplify-mul-vector)

  (import (rnrs)
          (only (srfi :1) every)
          (xitomatl AS-match)
          (numero symbolic simplify-parameters))

  (define (numeric? x)
    (or (number? x)
        (and (vector? x)
             (every number? (vector->list x)))))

  (define (simplify-mul-vector expr)

    (match expr

      (('* (? vector? a) (? vector? b))

       (vector-map (lambda (elt-a elt-b) `(* ,elt-a ,elt-b))
                   a
                   b))

      ((or ('* (? vector? a) (? number? n))
           ('* (? number? n) (? vector? a)))

       (vector-map (lambda (elt-a) `(* ,elt-a ,n)) a))

      ;; a * (b * c)

      ((and ('* a ('* b c))
            (? (lambda (_) (and (numeric? a)
                                (numeric? b)))))
       `(* ,(simplify-mul-vector `(* ,a ,b)) ,c))

      (else expr)))

  )