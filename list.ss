(module list mzscheme
  (require (lib "plt-match.ss")
           (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "list.ss"))
  (require (planet "maybe.ss" ("jaymccarthy" "mmss.plt" 1)))
  (require "fmap.ss")
  (provide (all-defined))

  (define list:empty empty)
  
  (define (specialize-list-fmap@ elt@)
    (compound-unit/sig (import)
                       (link [ELT : fmap^ (elt@)]
                             [FMAP : fmap^ (list-fmap@ ELT)])
                       (export (open FMAP))))
    
  ; List a = Empty + Cons a (List a)
  ; MapL ma v = TrieL (Maybe v) (ma (MapL ma v))
  ; MapL ma v = Spot | TrieL (Maybe v) (ma (MapL ma v))
  ; MapL ma v = Empty | OnlyL (Maybe v) | OnlyR (ma (MapL ma v)) | Both (Maybe v) (ma (MapL ma v))
  (define list-fmap@
    (unit/sig fmap^ (import (ma : fmap^))
      (define-struct MapL () (make-inspector))
      (define-struct (Empty MapL) () (make-inspector))
      (define-struct (OnlyL MapL) (Empty) (make-inspector))
      (define-struct (OnlyR MapL) (Cons) (make-inspector))
      (define-struct (Both MapL) (Empty Cons) (make-inspector))
      
      (define make-both
        (match-lambda*
          [(list (struct nothing ()) (? ma:empty?))
           (make-Empty)]
          [(list mE (? ma:empty?))
           (make-OnlyL mE)]
          [(list (struct nothing ()) mC)
           (make-OnlyR mC)]
          [(list mE mC)
           (make-Both mE mC)]))
      
      (define fmap? MapL?)
      (define is-equal? #f)
      (define elt-equal? ma:is-equal?)
      (define empty (make-Empty))
      (define empty? Empty?)
      
      (define (singleton k v)
        (letrec ([single
                  (match-lambda 
                    [(list)
                     (make-OnlyL (make-just v))]
                    [(list-rest x xs)
                     (make-OnlyR (ma:singleton x (single xs)))])])
          (single k)))

      (define lookup
        (match-lambda*
          [(list lst (struct Empty ()))
           (make-nothing)]
          [(list (list) (struct OnlyL (E)))
           E]
          [(list (list) (struct OnlyR (C)))
           (make-nothing)]
          [(list (list) (struct Both (E C)))
           E]
          [(list (list-rest x xs) (struct OnlyL (E)))
           (make-nothing)]
          [(list (list-rest x xs) (struct OnlyR (C)))
           (maybe-bind (ma:lookup x C) (lambda (a) (lookup xs a)))]
          [(list (list-rest x xs) (struct Both (E C)))
           (maybe-bind (ma:lookup x C) (lambda (a) (lookup xs a)))]))
      
      (define (insert c k v t)
        (merge c (singleton k v) t))
      
      (define (rmv x xs C)
        (match (ma:lookup x C)
          [(struct nothing ())
           C]
          [(struct just (Cx))
           (let* ([Cxp (remove xs Cx)]
                  ; we remove the binding of Cx to cx, rendering Cx'
                  [Cp (ma:insert fmap-replace x Cxp C)])
             Cp)]))
      
      (define remove
        (match-lambda*
          [(list k (struct Empty ()))
           (make-Empty)]
          [(list (list) (struct OnlyL (E)))
           (make-Empty)]
          [(list (list-rest x xs) (and t (struct OnlyL (E))))
           t]
          [(list (list) (and t (struct OnlyR (C))))
           t]
          [(list (list-rest x xs) (struct OnlyR (C)))
           (make-OnlyR (rmv x xs C))]
          [(list (list) (struct Both (E C)))
           (make-OnlyR C)]
          [(list (list-rest x xs) (struct Both (E C)))
           (make-both E (rmv x xs C))]))      
      
      (define (merge c t1 t2)
        (letrec ([mrg
                  (match-lambda*
                    [(list (struct Empty ()) t2)
                     t2]
                    [(list t1 (struct Empty ()))
                     t1]
                    [(list (struct OnlyL (E1)) (struct OnlyL (E2)))
                     (make-OnlyL (combine c E1 E2))]
                    [(list (struct OnlyL (E1)) (struct OnlyR (C2)))
                     (make-Both E1 C2)]
                    [(list (struct OnlyL (E1)) (struct Both (E2 C2)))
                     (make-Both (combine c E1 E2) C2)]
                    [(list (struct OnlyR (C1)) (struct OnlyL (E2)))
                     (make-Both E2 C1)]
                    [(list (struct OnlyR (C1)) (struct OnlyR (C2)))
                     (make-OnlyR (ma:merge mrg C1 C2))]
                    [(list (struct OnlyR (C1)) (struct Both (E2 C2)))
                     (make-Both E2 (ma:merge mrg C1 C2))]
                    [(list (struct Both (E1 C1)) (struct OnlyL (E2)))
                     (make-Both (combine c E1 E2) C1)]
                    [(list (struct Both (E1 C1)) (struct OnlyR (C2)))
                     (make-Both E1 (ma:merge mrg C1 C2))]
                    [(list (struct Both (E1 C1)) (struct Both (E2 C2)))
                     (make-Both (combine c E1 E2)
                                (ma:merge mrg C1 C2))])])
          (mrg t1 t2)))
      
      (define (foldr f i t)
        (define (help lst i t)
          (match t
            [(struct Empty ())
             i]
            [(struct OnlyL (E))
             (match E
               [(struct nothing ())
                i]
               [(struct just (v))
                (f (reverse lst) v i)])]
            [(struct OnlyR (C))
             (ma:foldr (lambda (elem elem-map acc)
                         (help (list* elem lst) acc elem-map))
                       i
                       C)]
            [(struct Both (E C))
             (help lst 
                   (help lst i (make-OnlyL E))
                   (make-OnlyR C))]))
        (help list:empty i t))

      (define (member? k t)
        (just? (lookup k t))))))
      