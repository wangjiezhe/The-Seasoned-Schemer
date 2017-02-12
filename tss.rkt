#lang tss

;;; 11. Welcome Back to the Show

#;(define member
    (lambda (a lat)
      (cond
        [(null? lat) #f]
        [else (or (eq? a (car lat))
                  (member a (cdr lat)))])))

(define is-first?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (eq? (car lat) a)])))

#;(define two-in-a-row?
    (lambda (lat)
      (cond
        [(null? lat) #f]
        [else
         (or (is-first? (car lat) (cdr lat))
             (two-in-a-row? (cdr lat)))])))

#;(define two-in-a-row?
    (lambda (lat)
      (cond
        [(null? lat) #f]
        [else
         (is-first-b? (car lat) (cdr lat))])))

(define is-first-b?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) a)
                (two-in-a-row? lat))])))

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) preceding)
                (two-in-a-row-b? (car lat) (cdr lat)))])))

#;(define two-in-a-row?
    (lambda (lat)
      (cond
        [(null? lat) #f]
        [else (two-in-a-row-b? (car lat) (cdr lat))])))

; sonssf: sum of numbers seen so far
(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
      [(null? tup) '()]
      [else (cons (+ sonssf (car tup))
                  (sum-of-prefixes-b (+ sonssf (car tup)) (cdr tup)))])))

#;(define sum-of-prefixes
    (lambda (tup)
      (sum-of-prefixes-b 0 tup)))

(define pick
  (lambda (n lat)
    (cond
      [(one? n) (car lat)]
      [else (pick (sub1 n) (cdr lat))])))

; rev-pre: reversed prefix
(define scramble-b
  (lambda (tup rev-pre)
    (cond
      [(null? tup) '()]
      [else
       (cons (pick (car tup)
                   (cons (car tup) rev-pre))
             (scramble-b (cdr tup)
                         (cons (car tup) rev-pre)))])))

#;(define scramble
    (lambda (tup)
      (scramble-b tup '())))



;;; 12. Take Cover

#;(define multirember
    (lambda (a lat)
      ((Y (lambda (mr)
            (lambda (lat)
              (cond
                [(null? lat) '()]
                [(eq? a (car lat))
                 (mr (cdr lat))]
                [else (cons (car lat)
                            (mr (cdr lat)))]))))
       lat)))

(define length
  (Y (lambda (length)
       (lambda (l)
         (cond
           [(null? l) 0]
           [else
            (add1 (length (cdr l)))])))))

#;(define multirember
    (lambda (a lat)
      ((letrec
           ([mr (lambda (lat)
                  (cond
                    [(null? lat) '()]
                    [(eq? a (car lat))
                     (mr (cdr lat))]
                    [else (cons (car lat)
                                (mr (cdr lat)))]))])
         mr)
       lat)))

(define multirember
  (lambda (a lat)
    (letrec
        ([mr (lambda (lat)
               (cond
                 [(null? lat) '()]
                 [(eq? a (car lat))
                  (mr (cdr lat))]
                 [else
                  (cons (car lat)
                        (mr (cdr lat)))]))])
      (mr lat))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        [(null? l) '()]
        [(test? (car l) a) (cdr l)]
        [else (cons (car l)
                    ((rember-f test?) a (cdr l)))]))))

(define rember-eq? (rember-f eq?))

#;(define multirember-f
    (lambda (test?)
      (lambda (a lat)
        (cond
          [(null? lat) '()]
          [(test? (car lat) a)
           ((multirember-f test?) a (cdr lat))]
          [else (cons (car lat)
                      ((multirember-f test?) a (cdr lat)))]))))

(define multirember-f
  (lambda (test?)
    (letrec
        ([m-f (lambda (a lat)
                (cond
                  [(null? lat) '()]
                  [(test? (car lat) a)
                   (m-f a (cdr lat))]
                  [else
                   (cons (car lat)
                         (m-f a (cdr lat)))]))])
      m-f)))

#;(define multirember
    (letrec
        ([mr (lambda (a lat)
               (cond
                 [(null? lat) '()]
                 [(eq? (car lat) a)
                  (mr a (cdr lat))]
                 [else
                  (cons (car lat)
                        (mr a (cdr lat)))]))])
      mr))

#;(define member?
    (lambda (a lat)
      ((letrec
           ([yes? (lambda (l)
                    (cond
                      [(null? l) #f]
                      [(eq? (car l) a) #t]
                      [else (yes? (cdr l))]))])
         yes?)
       lat)))

(define member?
  (lambda (a lat)
    (letrec
        ([yes? (lambda (l)
                 (cond
                   [(null? l) #f]
                   [(eq? (car l) a) #t]
                   [else (yes? (cdr l))]))])
      (yes? lat))))

#;(define union
    (lambda (set1 set2)
      (cond
        [(null? set1) set2]
        [(member? (car set1) set2)
         (union (cdr set1) set2)]
        [else (cons (car set1)
                    (union (cdr set1) set2))])))

#;(define union
    (lambda (set1 set2)
      (letrec
          ([U (lambda (set)
                (cond
                  [(null? set) set2]
                  [(member? (car set) set2)
                   (U (cdr set))]
                  [else (cons (car set)
                              (U (cdr set)))]))])
        (U set1))))

#;(define union
    (lambda (set1 set2)
      (letrec
          ([U (lambda (set)
                (cond
                  [(null? set) set2]
                  [(M? (car set) set2)
                   (U (cdr set))]
                  [else (cons (car set)
                              (U (cdr set)))]))]
           [M? (lambda (a lat)
                 (cond
                   [(null? lat) #f]
                   [(eq? (car lat) a) #t]
                   [else (M? a (cdr lat))]))])
        (U set1))))

(define union
  (lambda (set1 set2)
    (letrec
        ([U (lambda (set)
              (cond
                [(null? set) set2]
                [(M? (car set) set2)
                 (U (cdr set))]
                [else (cons (car set)
                            (U (cdr set)))]))]
         [M? (lambda (a lat)
               (letrec
                   ([N? (lambda (lat)
                          (cond
                            [(null? lat) #f]
                            [(eq? (car lat) a) #t]
                            [else (N? a (cdr lat))]))])
                 (N? lat)))])
      (U set1))))

(define two-in-a-row?
  (lambda (lat)
    (letrec
        ([W (lambda (a lat)
              (cond
                [(null? lat) #f]
                [else (or (eq? (car lat) a)
                          (W (car lat) (cdr lat)))]))])
      (cond
        [(null? lat) #f]
        [else (W (car lat) (cdr lat))]))))

#;(define two-in-a-row?
    (letrec
        ([W (lambda (a lat)
              (cond
                [(null? lat) #f]
                [else (or (eq? (car lat) a)
                          (W (car lat) (cdr lat)))]))])
      (lambda (lat)
        (cond
          [(null? lat) #f]
          [else (W (car lat) (cdr lat))]))))

(define sum-of-prefixes
  (lambda (tup)
    (letrec
        ([S (lambda (sss tup)
              (cond
                [(null? tup) '()]
                [else
                 (cons (+ sss (car tup))
                       (S (+ sss (car tup))
                          (cdr tup)))]))])
      (S 0 tup))))

#;(define scramble
    (lambda (tup)
      (letrec
          ([P (lambda (tup rp)
                (cond
                  [(null? tup) '()]
                  [else (cons (pick (car tup)
                                    (cons (car tup) rp))
                              (P (cdr tup)
                                 (cons (car tup) rp)))]))])
        (P tup '()))))



;;; 13. Hop, Skip, and Jump

#;(define intersect
    (lambda (set1 set2)
      (cond
        [(null? set1) '()]
        [(member? (car set1) set2)
         (cons (car set1)
               (intersect (cdr set1) set2))]
        [else (intersect (cdr set1) set2)])))

#;(define intersect
    (lambda (set1 set2)
      (letrec
          ([I (lambda (set)
                (cond
                  [(null? set) '()]
                  [(member? (car set) set2)
                   (cons (car set)
                         (I (cdr set)))]
                  [else (I (cdr set))]))])
        (I set1))))

#;(define intersectall
    (lambda (lset)
      (cond
        ; [(null? lset) '()]
        [(null? (cdr lset)) (car lset)]
        [else (intersect (cat lset)
                         (intersectall (cdr lset)))])))

#;(define intersectall
    (lambda (lset)
      (letrec
          ([A (lambda (lset)
                (cond
                  [(null? (cdr lset))
                   (car lset)]
                  [else (intersect (car lset)
                                   (A (cdr lset)))]))])
        (cond
          [(null? lset) '()]
          [else (A lset)]))))

#;(define intersectall
    (lambda (lset)
      (letcc hop
        (letrec
            ([A (lambda (lset)
                  (cond
                    [(null? (car lset))
                     (hop '())]
                    [(null? (cdr lset))
                     (car lset)]
                    [else (intersect (car lset)
                                     (A (cdr lset)))]))])
          (cond
            [(null? lset) '()]
            [else (A lset)])))))

(define intersect
  (lambda (set1 set2)
    (letrec
        ([I (lambda (set1)
              (cond
                [(null? set1) '()]
                [(member? (car set1) set2)
                 (cons (car set1)
                       (I (cdr set1)))]
                [else (I (cdr set1))]))])
      (cond
        [(null? set2) '()]
        [else (I set1)]))))

(define intersectall
  (lambda (lset)
    (letcc hop
      (letrec
          ([A (lambda (lset)
                (cond
                  [(null? (car lset))
                   (hop '())]
                  [(null? (cdr lset))
                   (car lset)]
                  [else (I (car lset)
                           (A (cdr lset)))]))]
           [I (lambda (s1 s2)
                (letrec
                    ([J (lambda (s1)
                          (cond
                            [(null? s1) '()]
                            [(member? (car s1) s2)
                             (cons (car s1)
                                   (J (cdr s1)))]
                            [else (J (cdr s1))]))])
                  (cond
                    [(null? s2) (hop '())]
                    [else (J s1)])))])
        (cond
          [(null? lset) '()]
          [else (A lset)])))))

(define rember
  (lambda (a lat)
    (letrec
        ([R (lambda (lat)
              (cond
                [(null? lat) '()]
                [(eq? (car lat) a) (cdr lat)]
                [else (cons (car lat)
                            (R (cdr lat)))]))])
      (R lat))))

(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ([R (lambda (lat)
              (cond
                [(null? lat) '()]
                [(eq? (car lat) a) '()]
                [else (cons (car lat)
                            (R (cdr lat)))]))])
      (R lat))))

(define rember-upto-last
  (lambda (a lat)
    (letcc skip
      (letrec
          ([R (lambda (lat)
                (cond
                  [(null? lat) '()]
                  [(eq? (car lat) a)
                   (skip (R (cdr lat)))]
                  [else
                   (cons (car lat)
                         (R (cdr lat)))]))])
        (R lat)))))



;;; 14. Let There Be Names

#;(define leftmost
    (lambda (l)
      (cond
        [(atom? (car l)) (car l)]
        [else (leftmost (car l))])))

#;(define leftmost
    (lambda (l)
      (cond
        [(null? l) '()]
        [(atom? (car l)) (car l)]
        [else (cond
                [(atom? (leftmost (car l)))
                 (leftmost (car l))]
                [else (leftmost (cdr l))])])))

#;(define leftmost
    (lambda (l)
      (cond
        [(null? l) '()]
        [(atom? (car l)) (car l)]
        [else (let ([a (leftmost (car l))])
                (cond
                  [(atom? a) a]
                  [else (leftmost (cdr l))]))])))

#;(define rember1*
    (lambda (a l)
      (cond
        [(null? l) '()]
        [(atom? (car l))
         (cond
           [(eq? (car l) a) (cdr l)]
           [else (cons (car l)
                       (rember1* a (cdr l)))])]
        [else
         (cond
           [(eqlist?
             (rember1* a (car l))
             (car l))
            (cons (car l)
                  (rember1* a (cdr l)))]
           [else (cons (rember1* a (car l))
                       (cdr l))])])))

#;(define rember1*
    (lambda (a l)
      (letrec
          ([R (lambda (l)
                (cond
                  [(null? '())]
                  [(atom? (car l))
                   (cond
                     [(eq? (car l) a) (cdr l)]
                     [else (cons (car l)
                                 (R (cdr l)))])]
                  [else
                   (cond
                     [(eqlist?
                       (R (car l))
                       (car l))
                      (cons (car l)
                            (R (cdr l)))]
                     [else (cons (R (car l))
                                 (cdr l))])]))])
        (R l))))

#;(define rember1*
    (lambda (a l)
      (letrec
          ([R (lambda (l)
                (cond
                  [(null? '())]
                  [(atom? (car l))
                   (cond
                     [(eq? (car l) a) (cdr l)]
                     [else (cons (car l)
                                 (R (cdr l)))])]
                  [else
                   (let ([av (R (car l))])
                     (cond
                       [(eqlist? (car l) av)
                        (cons (car l) (R (cdr l)))]
                       [else (cons av (cdr l))]))]))])
        (R l))))

#;(define depth*
    (lambda (l)
      (cond
        [(null? l) 1]
        [(atom? (car l))
         (depth* (cdr l))]
        [else
         (cond
           [(> (depth* (cdr l))
               (add1 (depth* (car l))))
            (depth* (cdr l))]
           [else (depth* (car l))])])))

#;(define depth*
    (lambda (l)
      (let ([a (add1 (depth* (car l)))]
            [d (depth* (cdr l))])
        (cond
          [(null? l) 1]
          [(atom? (car l)) d]
          [else (cond
                  [(> d a) d]
                  [else a])]))))

#;(define depth*
    (lambda (l)
      (cond
        [(null? l) 1]
        [(atom? (car l) )
         (depth* (cdr l))]
        [else
         (let ([a (add1 (depth* (car l)))]
               [d (depth* (cdr l))])
           (cond
             [(> d a) d]
             [else a]))])))

#;(define depth*
    (lambda (l)
      (cond
        [(null? l) 1]
        [else
         (let ([d (depth* (cdr l))])
           (cond
             [(atom? (car l)) d]
             [else
              (let ([a (add1 (depth* (car l)))])
                (cond
                  [(> d a) d]
                  [else a]))]))])))

#;(define depth*
    (lambda (l)
      (cond
        [(null? l) 1]
        [(atom? (car l))
         (depth* (cdr l))]
        [else
         (let ([a (add1 (depth* (car l)))]
               [d (depth* (cdr l))])
           (if (> d a) d a))])))

(define max
  (lambda (m n)
    (if (> m n) m n)))

(define depth*
  (lambda (l)
    (cond
      [(null? l) 1]
      [(atom? (car l))
       (depth* (cdr l))]
      [else (max
             (add1 (depth* (car l)))
             (depth* (cdr l)))])))

(define scramble
  (lambda (tup)
    (letrec
        ([P (lambda (tup rp)
              (cond
                [(null? tup) '()]
                [else (let ([rp (cons (car tup) rp)])
                        (cons (pick (car tup) rp)
                              (P (cdr rp))))]))])
      (P tup '()))))

#;(define leftmost
    (lambda (l)
      (letcc skip
        (lm l skip))))

(define lm
  (lambda (l out)
    (cond
      [(null? l) '()]
      [(atom? (car l)) (out (car l))]
      [else
       (lm (car l) out)
       (lm (cdr l) out)])))

(define leftmost
  (lambda (l)
    (letcc skip
      (letrec
          ([lm (lambda (l)
                 (cond
                   [(null? l) '()]
                   [(atom? (car l))
                    (skip (car l))]
                   [else
                    (lm (car l))
                    (lm (cdr l))]))])
        (lm l)))))

#;(define rm
    (lambda (a l oh)
      (cond
        [(null? l) (oh 'no)]
        [(atom? (car l))
         (if (eq? (car l) a)
             (cdr l)
             (cons (car l)
                   (rm a (cdr l) oh)))]
        [else
         (if (atom?
              (letcc oh
                (rm a (car l) oh)))
             (cons (car l)
                   (rm a (cdr l) oh))
             (cons (rm a (car l) 0)
                   (cdr l)))])))

#;(define rember1*
    (lambda (a l)
      (if (atom? (letcc oh (rm a l oh)))
          l
          (rm a l '()))))

#;(define rember1*
    (lambda (a l)
      (let ([new-l (letcc oh (rm a l oh))])
        (if (atom? new-l)
            l
            new-l))))

#;(define rm
    (lambda (a l oh)
      (cond
        [(null? l) (oh 'no)]
        [(atom? (car l))
         (if (eq? (car l) a)
             (cdr l)
             (cons (car l)
                   (rm a (cdr l) oh)))]
        [else
         (let ([new-car
                (letcc oh
                  (rm a (car l) oh))])
           (if (atom? new-car)
               (cons (car l)
                     (rm a (cdr oh)))
               (cons new-car (cdr l))))])))

(define rember1*
  (lambda (a l)
    (try oh (rm a l oh) l)))

(define rm
  (lambda (a l oh)
    (cond
      [(null? l) (oh 'no)]
      [(atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l)
                 (rm a (cdr l) oh)))]
      [else
       (try oh2
            (cons (rm a (car l) oh2)
                  (cdr l))
            (cons (car l)
                  (rm a (cdr l) oh)))])))



;;; 15. The Difference Between Men and Boys

(define x
  (cons 'chicago
        (cons 'pizza
              '())))

(set! x 'gone)
(set! x 'skin)

(define gourmet
  (lambda (food)
    (cons food
          (cons x '()))))

(set! x 'rings)

(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
          (cons x
                '()))))

(define dinner
  (lambda (food)
    (cons 'milkshake
          (cons food
                '()))))

(define dinnerR
  (lambda (food)
    (set! x food)
    (cons 'milkshake
          (cons food
                '()))))

(define omnivore
  (let ([x 'minestrone])
    (lambda (food)
      (set! x food)
      (cons food
            (cons x
                  '())))))

(define gobbler
  (let ([x 'minestrone])
    (lambda (food)
      (set! x food)
      (cons food
            (cons x
                  '())))))

(define nibbler
  (lambda (food)
    (let ([x 'donut])
      (set! x food)
      (cons food
            (cons x
                  '())))))

(define food 'none)

(define glutton
  (lambda (x)
    (set! food x)
    (cons 'more
          (cons x
                (cons 'more
                      (cons x
                            '()))))))

#;(define chez-nous
    (lambda ()
      (set! food x)
      (set! x food)))

(define chez-nous
  (lambda ()
    (let ([a food])
      (set! food x)
      (set! x a))))



;;; 16. Ready, Set, Bang!

