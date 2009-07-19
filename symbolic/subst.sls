
(library (numero symbolic subst)

  (export subst)
  
  (import (rnrs))

  (define (identity x) x)

  (define (list->alist lis)
    (if (null? lis)
        '()
        (cons (cons (list-ref lis 0)
                    (list-ref lis 1))
              (list->alist (cdr (cdr lis))))))

  (define (lookup alist key)

    (let ((result (assoc key alist)))

      (if result
          (cdr result)
          #f)))

  (define (substitute-alist expr alist)

    (define (subst elt)
      (substitute-alist elt alist))

    (cond ((lookup alist expr) => identity)

          ((list? expr) (map subst expr))

          (else expr)))

  (define (substitute expr . rest)
    (substitute-alist expr (list->alist rest)))

  (define-syntax subst
    (syntax-rules ()
      ((subst expr (key val) ...)
       (substitute-alist expr (list (cons 'key val) ...)))))

  )