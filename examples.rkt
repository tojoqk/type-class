#lang typed/racket
(require "type-class.rkt")

(define-type-class monad M
  ([return : (All (A) (-> A (M A)))]
   [bind : (All (A B) (-> (M A) (-> A (M B)) (M B)))]))

(define-type-class monad2 M
  ([return : (All (A B) (-> A (M A B)))]
   [bind : (All (A B C) (-> (M A C) (-> A (M B C)) (M B C)))]))

(struct (A) just ([value : A]) #:prefab)
(struct nothing () #:prefab)

(define-type (Maybe A) (U (just A) nothing))

(define-monad maybe-monad (Maybe A)
  (: return (All (A) (-> A (Maybe A))))
  (define (return x)  (just x))

  (: bind (All (A B) (-> (Maybe A) (-> A (Maybe B)) (Maybe B))))
  (define (bind m f)
    (cond
      [(just? m) (f (just-value m))]
      [(nothing? m) (nothing)])))

(define-monad-lambda monad-join (M A)
  (: join (All (A) (-> (M (M A)) (M A))))
  (define (join mm)
    (bind mm (lambda ([m : (M A)]) m)))
  join)

(with-monad (Maybe A) maybe-monad
  (define join (monad-join Maybe maybe-monad))
  (join (join (return (return (return 42))))))

(define-monad list-monad (Listof A)
  (: return (All (A) (-> A (Listof A))))
  (define (return x) (list x))
  (: bind (All (A B) (-> (Listof A) (-> A (Listof B)) (Listof B))))
  (define (bind m f)
    (append-map f m)))

(with-monad (Listof A) list-monad
  (define join (monad-join Listof list-monad))
  (join (join (return (return (return 42))))))

(struct (A) right ([value : A]) #:prefab)
(struct (A) left ([value : A]) #:prefab)
(define-type (Either A B) (U (right A) (left B)))

(define-monad2 either-monad (Either A B)
  (: return (All (A B) (-> A (Either A B))))
  (define (return x) (right x))
  (: bind (All (A B C) (-> (Either A C) (-> A (Either B C)) (Either B C))))
  (define (bind m f)
    (cond
      [(right? m) (f (right-value m))]
      [(left? m) m])))
