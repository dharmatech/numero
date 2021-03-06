
(library (numero symbolic to-alg)

  (export to-alg)

  (import (rnrs)
          (only (srfi :13) string-join)
          (xitomatl AS-match)
          )

  (define precedence-table
    '((sentinel . 0)
      (=        . 1)
      (+        . 2)
      (-        . 2)
      (*        . 3)
      (/        . 3)
      (^        . 4)))

  (define (precedence item)
    (cdr (assq item precedence-table)))

  (define (wrap-parens str)
    (string-append "(" str ")"))

  (define (alg expr m)

    (match expr

      ( (? number?) (number->string expr) )

      ( ('= a b)
        (let ((n (precedence '=)))
          (let ((str (string-append (alg a n) " = " (alg b n))))
            (if (< n m) (wrap-parens str) str))) )

      ( ('+ a b)
        (let ((n (precedence '+)))
          (let ((str (string-append (alg a n) " + " (alg b n))))
            (if (< n m) (wrap-parens str) str))) )

      ( ('- a b)
        (let ((n (precedence '-)))
          (let ((str (string-append (alg a n) " - " (alg b n))))
            (if (< n m) (wrap-parens str) str))) )

      ( ('* a b)
        (let ((n (precedence '*)))
          (let ((str (string-append (alg a n) " * " (alg b n))))
            (if (< n m) (wrap-parens str) str))) )

      ( ('/ a b)
        (let ((n (precedence '/)))
          (let ((str (string-append (alg a n) " / " (alg b n))))
            (if (< n m) (wrap-parens str) str))) )

      ( ('^ a b)
        (let ((n (precedence '^)))
          (let ((str (string-append (alg a n) " ^ " (alg b n))))
            (if (< n m) (wrap-parens str) str))) )

      ;; ( ('sin x)
      ;;   (let ((n 100))
      ;;     (let ((str (string-append "sin " (alg x n))))
      ;;       (if (< n m) (wrap-parens str) str))) )

      ;; ( ('cos x)
      ;;   (let ((n 100))
      ;;     (let ((str (string-append "cos " (alg x n))))
      ;;       (if (< n m) (wrap-parens str) str))) )

      ( ('sin x)
        (string-append "sin(" (alg x -10) ")") )

      ( ('cos x)
        (string-append "cos(" (alg x -10) ")") )

      ( (? vector?)

        (string-append "<"
                       (string-join
                        (vector->list (vector-map to-alg expr))
                        " , ")
                       ">")

        )

      ( else

        (call-with-string-output-port
         (lambda (port)
           (display expr port))) )))

  (define (to-alg expr)
    (alg expr 0))

  )
