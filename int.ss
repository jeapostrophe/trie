(module int mzscheme
  (require (lib "plt-match.ss")
           (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "list.ss"))
  (require (planet "maybe.ss" ("jaymccarthy" "mmss.plt" 1)))
  (require "fmap.ss")
  (provide little-endian-fmap@
           big-endian-fmap@
           positive-big-endian-fmap@)
  
  ;; HELPERS
  (define (zero-bit? k m)
    (zero? (bitwise-and k m)))
    
  ;; INTs
  (define-signature int^ (mask branching-bit))
  
  (define little-endian-int@
    (unit/sig int^ (import)
      (define (mask k m)
        (bitwise-and k (sub1 m)))
      
      (define (lowest-bit x)
        (bitwise-and x (* -1 x)))
      
      (define (branching-bit p0 p1)
        (lowest-bit (bitwise-xor p0 p1)))))
  
  (define big-endian-int@
    (unit/sig int^ (import)
      (define (mask k m)
        (bitwise-and (bitwise-ior k (sub1 m)) 
                     (bitwise-not m)))
      
      (define (naive-highest-bit x)
        (unless (< x 256)
          (error 'highest-bit))
        (let loop ([i 7])
          (cond
            [(zero? i) 1]
            [(equal? (arithmetic-shift x (* -1 i)) 1)
             (arithmetic-shift 1 i)]
            [else
             (loop (sub1 i))])))
      
      (define hbit
        (apply vector (map naive-highest-bit (build-list 256 identity))))
      
      (define (highest-bit32 x)
        (let ([n (arithmetic-shift x -24)])
          (if (not (zero? n))
              (arithmetic-shift (vector-ref hbit n) 24)
              (let ([n (arithmetic-shift x -16)])
                (if (not (zero? n))
                    (arithmetic-shift (vector-ref hbit n) 16)
                    (let ([n (arithmetic-shift x -8)])
                      (if (not (zero? n))
                          (arithmetic-shift (vector-ref hbit n) 8)
                          (vector-ref hbit x))))))))
      
      (define highest-bit highest-bit32)
      
      (define (branching-bit p0 p1)
        (highest-bit (bitwise-xor p0 p1)))))
  
  ;; FMAP    
  (define non-binary-fmap@
    (unit/sig fmap^ (import (orig : fmap-base^))
      (define fmap? orig:fmap?)
      (define is-equal? orig:is-equal?)
      (define elt-equal? orig:elt-equal?)
      (define empty orig:empty)
      (define empty? orig:empty?)
      (define singleton orig:singleton)
      (define lookup orig:lookup)
      (define insert orig:insert)
      (define remove orig:remove)
      (define merge orig:merge)
      (define foldr orig:foldr)
      (define subset? orig:subset?)
      (define intersect orig:intersect)
      (define difference orig:difference)
      (define minimum orig:minimum)
      (define maximum orig:maximum)
      (define (member? . args)
        (just? (apply orig:lookup args)))))
  
  (define binary-fmap@
    (unit/sig fmap^ (import (orig : fmap-base^))
      (define fmap? orig:fmap?)
      (define is-equal? orig:is-equal?)
      (define elt-equal? orig:elt-equal?)
      (define empty orig:empty)
      (define empty? orig:empty?)
      (define singleton orig:singleton)
      (define lookup orig:binary-lookup)
      (define insert orig:insert)
      (define remove orig:remove)
      (define merge orig:merge)
      (define foldr orig:foldr)
      (define subset? orig:subset?)
      (define intersect orig:intersect)
      (define difference orig:difference)
      (define minimum orig:binary-minimum)
      (define maximum orig:binary-maximum)
      (define (member? . args)
        (just? (apply orig:lookup args)))))
  
  ;; FMAP-BASE
  (define-signature fmap-base^ (fmap?
                                is-equal? elt-equal?
                                empty empty? singleton 
                                lookup binary-lookup
                                insert remove
                                merge foldr
                                subset?
                                intersect difference
                                minimum maximum
                                binary-minimum binary-maximum))
  
  (define fmap@
    (unit/sig fmap-base^ (import int^)      
      ; Types
      (define-struct trie () (make-inspector))
      (define-struct (trie:empty trie) () (make-inspector))
      (define-struct (trie:leaf trie) (k a) (make-inspector))
      (define-struct (trie:branch trie) (p b l r) (make-inspector))
      
      ; Helpers
      (define (prefix-match? k p m)
        (equal? p (mask k m)))
      
      (define ((swap f) x y)
        (f y x))
      
      (define (join p0 t0
                    p1 t1)
        (let ([m (branching-bit p0 p1)])
          (if (zero-bit? p0 m)
              (make-trie:branch (mask p0 m) m t0 t1)
              (make-trie:branch (mask p0 m) m t1 t0))))
      
      (define make-branch
        (match-lambda*
          [(list p m (struct trie:empty ()) (struct trie:empty ()))
           (make-trie:empty)]
          [(list p m (and t (struct trie:leaf (j a))) (struct trie:empty ()))
           t]
          [(list p m (struct trie:empty ()) (and t (struct trie:leaf (j a))))
           t]
          [(list p m a b)
           (make-trie:branch p m a b)]))
      
      ; Exports
      (define fmap? trie?)
      (define is-equal? 
        (match-lambda*
          [(list (struct trie:empty ()) (struct trie:empty ()))
           #t]
          [(list (struct trie:empty ()) t2)
           #f]
          [(list t1 (struct trie:empty ()))
           #f]
          [(list (struct trie:leaf (k a)) (struct trie:leaf (j b)))
           (and (equal? k j) (equal? a b))]
          [(list (struct trie:leaf (k a)) t2)
           #f]
          [(list t1 (struct trie:leaf (j b)))
           #f]
          [(list (struct trie:branch (p1 m1 l1 r1)) (struct trie:branch (p2 m2 l2 r2)))
           (and (equal? p1 p2)
                (equal? m1 m2)
                (is-equal? l1 l2)
                (is-equal? r1 r2))]))
      (define elt-equal? =)
      (define empty (make-trie:empty))
      (define empty? trie:empty?)
      
      (define (singleton k v)
        (make-trie:leaf k v))
      
      (define lookup
        (match-lambda*
          [(list k (struct trie:empty ()))
           (make-nothing)]
          [(list k (struct trie:leaf (j a)))
           (if (elt-equal? k j)
               (make-just a)
               (make-nothing))]
          [(list k (struct trie:branch (_ m l r)))
           (lookup k (if (zero-bit? k m) l r))]))
      
      (define binary-lookup
        (match-lambda*
          [(list k (struct trie:empty ()))
           (make-nothing)]
          [(list k (struct trie:leaf (j a)))
           (if (elt-equal? k j)
               (make-just a)
               (make-nothing))]
          [(list k (struct trie:branch (p m l r)))
           (binary-lookup k (if (<= k p) l r))]))
      
      (define (insert c k x t)
        (letrec ([ins
                  (match-lambda
                    [(struct trie:empty ())
                     (make-trie:leaf k x)]
                    [(and t (struct trie:leaf (j y)))
                     (if (elt-equal? j k)
                         (make-trie:leaf k (c x y))
                         (join k (make-trie:leaf k x)
                               j t))]
                    [(and t (struct trie:branch (p m t0 t1)))
                     (if (prefix-match? k p m)
                         (if (zero-bit? k m)
                             (make-trie:branch p m (ins t0) t1)
                             (make-trie:branch p m t0 (ins t1)))
                         (join k (make-trie:leaf k x)
                               p t))])])
          (ins t)))
      
      (define (remove k t)
        (letrec ([rem
                  (match-lambda
                    [(struct trie:empty ())
                     (make-trie:empty)]
                    [(and t (struct trie:leaf (j y)))
                     (if (elt-equal? j k)
                         (make-trie:empty)
                         t)]
                    [(and t (struct trie:branch (p m t0 t1)))
                     (if (prefix-match? k p m)
                         (if (zero-bit? k m)
                             (make-branch p m (rem t0) t1)
                             (make-branch p m t0 (rem t1)))
                         t)])])
          (rem t)))
      
      (define (merge c s t)
        (letrec ([mrg 
                  (match-lambda*
                    [(list (struct trie:empty ()) t)
                     t]
                    [(list t (struct trie:empty ()))
                     t]
                    [(list (struct trie:leaf (k x)) t)
                     (insert c k x t)]
                    [(list t (struct trie:leaf (k x)))
                     (insert (swap c) k x t)]
                    [(list (and s (struct trie:branch (p m s0 s1)))
                           (and t (struct trie:branch (q n t0 t1))))
                     (cond
                       [(and (equal? m n) (equal? p q))
                        (make-branch p m (mrg s0 t0) (mrg s1 t1))]
                       [(and (< m n)
                             (prefix-match? q p m))
                        (if (zero-bit? q m)
                            (make-branch p m (mrg s0 t) s1)
                            (make-branch p m s0 (mrg s1 t)))]
                       [(and (> m n)
                             (prefix-match? p q n))
                        (if (zero-bit? p n)
                            (make-branch q n (mrg s t0) t1)
                            (make-branch q n t0 (mrg s t1)))]
                       [else
                        (join p s
                              q t)])])])
          (mrg s t)))
      
      (define (foldr f i t)
        (match t
          [(struct trie:empty ())
           i]
          [(struct trie:leaf (k v))
           (f k v i)]
          [(struct trie:branch (p m l r))
           (foldr f (foldr f i l) r)]))
      
      (define subset?
        (match-lambda*
          [(list (struct trie:empty ()) _)
           #t]
          [(list _ (struct trie:empty ()))
           #f]
          [(list (struct trie:leaf (k1 v1)) s2)
           (just? (lookup k1 s2))]
          [(list (struct trie:branch (p1 m1 l1 r1))
                 (struct trie:leaf (k2 v2)))
           #f]
          [(list (struct trie:branch (p1 m1 l1 r1))
                 (struct trie:branch (p2 m2 l2 r2)))
           (cond
             [(and (equal? m1 m2)
                   (equal? p1 p2))
              (and (subset? l1 l2)
                   (subset? r1 r2))]
             [(and (> m1 m2)
                   (prefix-match? p1 p2 m2))
              (if (zero-bit? p1 m2)
                  (and (subset? l1 l2)
                       (subset? r1 l2))
                  (and (subset? l1 r2)
                       (subset? r1 r2)))]
             [else
              #f])]))
      
      (define (intersect c s0 s1)
        (letrec ([intersect 
                  (match-lambda*
                    [(list (struct trie:empty ()) t)
                     (make-trie:empty)]
                    [(list t (struct trie:empty ()))
                     (make-trie:empty)]
                    [(list (and s1 (struct trie:leaf (k x))) t)
                     (if (just? (lookup k t))
                         s1
                         (make-trie:empty))]
                    [(list t (and s2 (struct trie:leaf (k x))))
                     (if (just? (lookup k t))
                         s2
                         (make-trie:empty))]
                    [(list (and s (struct trie:branch (p m s0 s1)))
                           (and t (struct trie:branch (q n t0 t1))))
                     (cond
                       [(and (equal? m n) (equal? p q))
                        (merge c 
                               (intersect s0 t0)
                               (intersect s1 t1))]
                       [(and (< m n)
                             (prefix-match? q p m))
                        (intersect (if (zero-bit? q m) s0 s1)
                                   t)]
                       [(and (> m n)
                             (prefix-match? p q n))
                        (intersect s
                                   (if (zero-bit? p n) t0 t1))]
                       [else
                        (make-trie:empty)])])])
          (intersect s0 s1)))
      
      (define (difference c s0 s1)
        (letrec ([difference
                  (match-lambda*
                    [(list (struct trie:empty ()) t)
                     (make-trie:empty)]
                    [(list t (struct trie:empty ()))
                     t]
                    [(list (and s1 (struct trie:leaf (k x))) s2)
                     (if (just? (lookup k s2))
                         (make-trie:empty)
                         s1)]
                    [(list s1 (and s2 (struct trie:leaf (k x))))
                     (remove k s1)]
                    [(list (and s1 (struct trie:branch (p1 m1 l1 r1)))
                           (and s2 (struct trie:branch (p2 m2 l2 r2))))
                     (cond
                       [(and (equal? m1 m2) (equal? p1 p2))
                        (merge c
                               (difference l1 l2)
                               (difference r1 r2))]
                       [(and (< m1 m2)
                             (prefix-match? p2 p1 m1))
                        (if (zero-bit? p2 m1)
                            (merge c (difference l1 s2) r1)
                            (merge c l1 (difference r1 s2)))]
                       [(and (> m1 m2)
                             (prefix-match? p1 p2 m2))
                        (if (zero-bit? p1 m2)
                            (difference s1 l2)
                            (difference s1 r2))]
                       [else
                        s1])])])
          (difference s0 s1)))
      
      (define minimum
        (match-lambda
          [(struct trie:empty ())
           (make-nothing)]
          [(struct trie:leaf (j a))
           (make-just (cons j a))]
          [(struct trie:branch (_ _ s t))
           (min (minimum s) (minimum t))]))
      
      (define maximum
        (match-lambda
          [(struct trie:leaf (j a))
           j]
          [(struct trie:branch (_ _ s t))
           (max (maximum s) (maximum t))]))
      
      (define binary-minimum
        (match-lambda
          [(struct trie:leaf (j a))
           j]
          [(struct trie:branch (_ _ s _))
           (binary-minimum s)]))
      
      (define binary-maximum
        (match-lambda
          [(struct trie:leaf (j a))
           j]
          [(struct trie:branch (_ _ _ t))
           (binary-maximum t)]))))
  
  (define little-endian-fmap@
    (compound-unit/sig (import)
                       (link [ENDIAN      : int^ (little-endian-int@)]
                             [FMAP-BASE   : fmap-base^ (fmap@ ENDIAN)]
                             [FMAP        : fmap^ (non-binary-fmap@ FMAP-BASE)])
                       (export (open FMAP))))
  
  (define big-endian-fmap@
    (compound-unit/sig (import)
                       (link [ENDIAN : int^ (big-endian-int@)]
                             [FMAP-BASE   : fmap-base^ (fmap@ ENDIAN)]
                             [FMAP        : fmap^ (non-binary-fmap@ FMAP-BASE)])
                       (export (open FMAP))))
  
  (define positive-fmap@
    (unit/sig fmap-base^ (import (orig : fmap-base^))
      (define fmap? orig:fmap?)
      (define is-equal? orig:is-equal?)
      (define elt-equal? orig:elt-equal?)
      (define empty orig:empty)
      (define empty? orig:empty?)
      (define (singleton k v)
        (if (positive? k)
            (orig:singleton k v)
            (error 'positive)))
      (define (insert c k v t)
        (if (positive? k)
            (orig:insert c k v t)
            (error 'positive)))  
      (define lookup orig:binary-lookup)
      (define binary-lookup orig:binary-lookup)
      (define remove orig:remove)
      (define merge orig:merge)
      (define foldr orig:foldr)
      (define subset? orig:subset?)
      (define intersect orig:intersect)
      (define difference orig:difference)
      (define minimum orig:binary-minimum)
      (define binary-minimum orig:binary-minimum)
      (define maximum orig:binary-maximum)
      (define binary-maximum orig:binary-maximum)))
  
  (define positive-big-endian-fmap@
    (compound-unit/sig (import)
                       (link [ENDIAN : int^ (big-endian-int@)]
                             [BASE   : fmap-base^ (fmap@ ENDIAN)]
                             [FMAP-BASE   : fmap-base^ (positive-fmap@ BASE)]
                             [FMAP        : fmap^ (binary-fmap@ FMAP-BASE)])
                       (export (open FMAP)))))

