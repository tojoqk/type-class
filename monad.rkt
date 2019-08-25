#lang typed/racket
(require "type-class.rkt")
(provide define-monad
         with-monad
         define-monad-library
         monad-do)

(module+ test
  (require typed/rackunit))

(define-type-class monad M
  [return : (All (A) (-> A (M A)))]
  [bind : (All (A B) (-> (M A) (-> A (M B)) (M B)))])

(define-syntax (monad-do stx)
  (syntax-case stx ()
    [(k expr ...)
     (with-syntax ([bind (datum->syntax #'k 'bind)])
       #'(monad-do* bind expr ...))]))

(define-syntax (monad-do* stx)
  (syntax-case stx (<- let =>)
    [(_ bind (let var : T expr) rest ...)
     #'(let ([var : T expr])
         (monad-do* bind rest ...))]
    [(_ bind (<- var : T expr) rest ...)
     #'(bind expr
             (lambda ([var : T])
               (monad-do* bind rest ...)))]
    [(_ bind [=> expr] rest rest* ...)
     #'(bind expr
             (lambda ([_ : Any])
               (monad-do* bind rest rest* ...)))]
    [(_ bind [=> expr]) #'expr]))

(module+ test
  (define-monad list-monad (Listof A)
    (define (return x) (list x))
    (define (bind m f) (append-map f m)))

  (define-monad-library monad-stdlib (M A)
    (export ap join)

    (: ap (All (A B) (-> (M (-> A B)) (M A) (M B))))
    (define (ap mf m)
      (monad-do (<- f : (-> A B) mf)
                (<- x : A m)
                (=> (return (f x)))))
    
    (: join (All (A) (-> (M (M A)) (M A))))
    (define (join mm)
      (monad-do (<- m : (M A) mm)
                (=> m))))

  (with-monad list-monad (Listof A)
    (import monad-stdlib)

    (check-equal? (join (list (list 'a 'b 'c) (list 'd 'e 'f)))
                  (list 'a 'b 'c 'd 'e 'f))

    (check-equal? (ap (list (lambda ([x : Natural]) (sqr x))
                            (lambda ([x : Natural]) (* x 2)))
                      (list 1 2 3))
                  (list 1 4 9 2 4 6)))

  (with-monad list-monad (Listof A)
    (import)

    (check-equal? (monad-do (<- x : Natural (list 1 2))
                            (<- y : Symbol (list 'a 'b))
                            (let l : (List Natural Symbol) (list x y))
                            (=> (return l)))
                  (list (list 1 'a)
                        (list 1 'b)
                        (list 2 'a)
                        (list 2 'b)))))
