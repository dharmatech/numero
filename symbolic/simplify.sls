
(library (numero symbolic simplify)

  (export simplify)

  (import (rnrs)
          (xitomatl AS-match)
          (numero symbolic rewrite)
          (numero symbolic simplify-remove-sub)
          (numero symbolic simplify-add-canonical)
          (numero symbolic simplify-mul-canonical)
          (numero symbolic simplify-div-canonical)
          (numero symbolic simplify-add-constants)
          (numero symbolic simplify-add-like-terms)
          (numero symbolic simplify-mul-constants)
          (numero symbolic simplify-mul-factors)
          (numero symbolic simplify-pow)
          (numero symbolic simplify-pretty)
          )

  (define-syntax compose-in-order
    (syntax-rules ()
      ( (compose g) g )
      ( (compose g f ...)
        (lambda (x)
          ((compose-in-order f ...) (g x))))))

  ;; (define (simplify expr)

  ;;   (let ((expr (rewrite simplify-remove-sub expr)))

  ;;     (let ((expr (rewrite simplify-mul-canonical expr)))

  ;;       (let ((expr (rewrite simplify-add-constants expr)))

  ;;         (let ((expr (rewrite simplify-add-constants expr)))

  ;;           (let ((expr (rewrite simplify-add-like-terms expr)))

  ;;             (let ((expr (rewrite simplify-mul-constants expr)))

  ;;               (let ((expr (rewrite simplify-mul-factors expr)))

  ;;                 expr))))))))

  (define (simplify expr)

    (let ((expr (rewrite (compose-in-order simplify-add-canonical
                                           simplify-remove-sub
                                           simplify-mul-canonical
                                           simplify-div-canonical
                                           simplify-add-constants
                                           simplify-add-like-terms
                                           simplify-mul-constants
                                           simplify-mul-factors
                                           simplify-pow)
                         expr)))



      (rewrite (compose-in-order simplify-pretty
                                 simplify-mul-constants)
               expr)))

  )