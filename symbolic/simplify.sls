
(library (numero symbolic simplify)

  (export simplify simplify-step)

  (import (rnrs)
          (xitomatl AS-match)

          (numero symbolic rewrite)

          (numero symbolic simplify-add-canonical)
          (numero symbolic simplify-sub-canonical)
          (numero symbolic simplify-mul-canonical)
          
          (numero symbolic simplify-add)
          (numero symbolic simplify-sub)
          (numero symbolic simplify-mul)
          (numero symbolic simplify-div)

          (numero symbolic simplify-neg)

          (numero symbolic simplify-add-like-terms)
          (numero symbolic simplify-sub-like-terms)

          (numero symbolic simplify-mul-factors)

          (numero symbolic simplify-remove-sub)

          (numero symbolic simplify-pow)
          
          ;; (numero symbolic simplify-div-canonical)
          ;; (numero symbolic simplify-add-constants)
          
          ;; (numero symbolic simplify-sqrt)
          ;; (numero symbolic simplify-dot)
          ;; (numero symbolic simplify-pretty)
          ;; (numero symbolic simplify-abs)

          )

  (define-syntax compose-in-order
    (syntax-rules ()
      ( (compose g) g )
      ( (compose g f ...)
        (lambda (x)
          ((compose-in-order f ...) (g x))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (define (simplify expr)

  ;;   (if (vector? expr)

  ;;       (vector-map simplify expr)

  ;;       (let ((expr (rewrite simplify-add-canonical expr)))

  ;;         (let ((expr (rewrite (compose-in-order simplify-remove-sub
  ;;                                                simplify-mul-canonical
  ;;                                                simplify-div
  ;;                                                simplify-sqrt
  ;;                                                simplify-div-canonical
  ;;                                                simplify-add-constants
  ;;                                                simplify-add-like-terms
  ;;                                                simplify-mul-constants
  ;;                                                simplify-mul-factors
  ;;                                                simplify-pow
  ;;                                                simplify-dot
  ;;                                                simplify-abs)
  ;;                              expr)))

  ;;           (rewrite (compose-in-order simplify-pretty
  ;;                                      simplify-mul-constants
  ;;                                      simplify-pow
  ;;                                      )
  ;;                    expr)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define simplify-step

    (compose-in-order simplify-add-canonical
                      simplify-sub-canonical
                      simplify-mul-canonical

                      simplify-add
                      simplify-sub
                      simplify-mul
                      simplify-div

                      simplify-neg

                      simplify-add-like-terms
                      simplify-sub-like-terms

                      simplify-mul-factors

                      simplify-pow

                      ))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-basic expr)

    (let ((expr (cond ((list?   expr) (map        simplify-basic expr))
                      ((vector? expr) (vector-map simplify-basic expr))
                      (else expr))))

      (simplify-step expr)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify expr) (rewrite simplify-basic expr))

  )