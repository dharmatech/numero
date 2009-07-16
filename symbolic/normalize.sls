
(library (numero symbolic normalize)

  (export normalize-loop normalize)

  (import (rnrs) (xitomatl AS-match))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (symbolic-equal? a b)
    (equal? a b))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (coefficient expr)

    (match expr

      ( ('* (? number? a) b) a )

      ( else 1 )))

  (define (strip-coefficient expr)

    (match expr

      ( ('* (? number?) b) b )

      ( else expr )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (like-terms? a b)
    (or (symbolic-equal? (strip-coefficient a)
                         (strip-coefficient b))
        (and (number? a)
             (number? b))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (contains-like-term? expr term)

    (or (like-terms? expr term)

        (match expr

          ( ((or '+ '-) a b)

            (or (contains-like-term? a term)
                (contains-like-term? b term)) )

          ( else #f ))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (normalize-add expr)

    (match expr

      ;; (a + (b + c))	((a + b) + c)

      ( ('+ a ('+ b c)) `(+ (+ ,a ,b) ,c) )

      ;; (a + (b - c))	((a + b) - c)

      ( ('+ a ('- b c)) `(- (+ ,a ,b) ,c) )

      ;; a + b    where a and b are numbers

      ( ('+ (? number? a) (? number? b)) (+ a b))

      ;; x + 0    x

      ;; 0 + x    x
      
      ( (or ('+ x 0) ('+ 0 x)) x )

      ;; (a + b)   where a and b are like terms

      ( (and ('+ a b) (? (lambda (expr) (like-terms? a b))))

        `(* (+ ,(coefficient a)
               ,(coefficient b))
            ,(strip-coefficient a)) )

      ;; (a + b) + c    where a contains term like c    ((a + c) + b)
      
      ( (and ('+ ('+ a b) c)

             (? (lambda (expr)

                  (contains-like-term? a c))))

        `(+ ,(normalize `(+ ,a ,c)) ,b) )

      ;; (a + b) + c    where b contains term like c    ((c + b) + a)

      ( (and ('+ ('+ a b) c)

             (? (lambda (expr)

                  (contains-like-term? b c))))

        `(+ ,(normalize `(+ ,c ,b)) ,a) )

      ( ('+ a b) `(+ ,(normalize a) ,(normalize b)) )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (define (normalize-sub expr)

  ;;   (match expr

  ;;          ;; (a - (b + c))		((a - b) - c)

  ;;          ( ('- a ('+ b c)) `(- (- ,a ,b) ,c) )

  ;;          ;; (a - (b - c))		((a - b) + c)

  ;;          ( ('- a ('- b c)) `(+ (- ,a ,b) ,c) )

  ;;          ;; (a + b) where a and b are numbers	a+b

  ;;          ( ('- (? number? a) (? number? b)) (- a b) )

  ;;          ;; (a - 0)

  ;;          ( ('- a 0) a )

  ;;          ;; (0 - b)

  ;;          ( ('- 0 b) `(- ,b) )

  ;;          ( ('- a b) `(- ,(normalize a) ,(normalize b)) )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (normalize-sub expr)

    (match expr

      ;; a - b	a + -b

      ( ('- a b) `(+ ,a (- ,b)) )

      ;; -a		-1 * a

      ( ('- a) `(* -1 ,a) )

      ( else expr )

      ))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (base expr)
    (match expr
      ( ('^ a b) (base a) )
      ( else expr )))

  (define (exponent expr)
    (match expr
      ( ('^ a b) b )
      ( else 1 )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (var-term? expr)
    (symbol? (base expr)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (symbol<? a b)
    (string<? (symbol->string a)
              (symbol->string b)))

  (define (var<? a b)
    (symbol<? a b))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (not-number? obj)
    (not (number? obj)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (fun-name? expr)
    (and (symbol? expr)
         (char-alphabetic?
          (string-ref (symbol->string expr) 0))))

  (define (fun-term? expr)
    (let ((expr (base expr)))
      (and (list? expr)
           (>= (length expr) 1)
           (fun-name? (car expr)))))

  (define (fun-name expr)
    (car (base expr)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Factors are arranged in order:
  ;;
  ;;    numbers variables functions
  ;;
  ;; and then alphabetically

  (define (normalize-mul expr)

    (match expr

      ;; ((* a b) c)    (* a (* b c))

      ( ('* ('* a b) c) `(* ,a (* ,b ,c)) )

      ;; a * 5    5 * a

      ( ('* (? not-number? a) (? number? b)) `(* ,b ,a) )

      ;; a * (5 * c)    5 * (a * c)

      ( ('* (? not-number? a) ('* (? number? b) c)) `(* ,b (* ,a ,c)) )

      ;; (* a b)    where (var<? b a)    (* b a)

      ( (and ('* (? var-term? a) (? var-term? b))
             (? (lambda (expr)
                  (var<? (base b)
                         (base a)))))

        `(* ,b ,a) )

      ;; (* a (* b c))   where (var<? b a)   (* b (* a c))

      ( (and ('* (? var-term? a) ('* (? var-term? b) c))

             (? (lambda (expr)
                  (var<? (base b)
                         (base a)))))

        `(* ,b (* ,a ,c)) )

      ;; Sort function call factors alphabetically
      ;;
      ;; b(x) * a(x)    a(x) * b(x)

      ( (and ('* (? fun-term? a)
                 (? fun-term? b))

             (? (lambda (expr)
                  (symbol<? (fun-name b)
                            (fun-name a)))))

        `(* ,b ,a) )

      ;; b(x) * ( a(x) * c(x) )    a(x) * ( b(x) * c(x) )

      ( (and ('* (? fun-term? a)
                 ('* (? fun-term? b)
                     c))

             (? (lambda (expr)
                  (symbol<? (fun-name b)
                            (fun-name a)))))

        `(* ,b (* ,a ,c)) )

      ;; (* (+ a b) c)	(* c (+ a b))

      ;;  ('* ('+ a b) c) `(* ,c (+ ,a ,b)) )

      ;; -1 * (a + b)	-a + -b

      ( ('* -1 ('+ a b))	`(+ (- ,a) (- ,b)) )

      ( ('* 0 b) 0 )

      ( ('* 1 b) b )

      ( ('* (? number? a) (? number? b)) (* a b) )

      ( ('* (? number? a) ('* (? number? b) c)) `(* ,(* a b) ,c) )

      ;; combine like factors (a^2 * a^3) => a^5

      ( (and ('* a b)
             (? (lambda (expr)
                  (symbolic-equal? (base a)
                                   (base b)))))

        `(^ ,(base a)
            (+ ,(exponent a)
               ,(exponent b))) )

      ;; (a * (a * b))	a ^ 2 * b

      ( (and ('* a ('* b c))
             (? (lambda (expr)
                  (symbolic-equal? (base a)
                                   (base b)))))

        `(* (^ ,(base a)
               (+ ,(exponent a)
                  ,(exponent b)))
            ,c) )

      ;; a * (b + c)    where a and c are numbers

      ( ('* (? number? a) ('+ b (? number? c))) `(+ (* ,a ,b) ,(* a c)) )

      ( ('* a b) `(* ,(normalize a) ,(normalize b)) )

      ))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (normalize-div expr)
    (match expr
      ( ('/ a b) `(/ ,(normalize a) ,(normalize b)) )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (normalize-pow expr)
    (match expr
      ( ('^ a 0) 1 )
      ( ('^ a 1) a )
      ( ('^ a b) `(^ ,(normalize a) ,(normalize b)) )))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (normalize expr)

    (match expr

      ( ('+ a b) (normalize-add expr) )
      ( ('- a b) (normalize-sub expr) )
      ( ('* a b) (normalize-mul expr) )
      ( ('/ a b) (normalize-div expr) )
      ( ('^ a b) (normalize-pow expr) )

      ( ('= a b) `(= ,(normalize a) ,(normalize b)) )

      ( ('- a) (normalize-sub expr) )

      ( else expr )

      ))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (normalize-loop old)
    (let ((new (normalize old)))
      (if (equal? new old)
          old
          (normalize-loop new))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )