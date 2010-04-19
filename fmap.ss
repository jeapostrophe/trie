(module fmap mzscheme
  (require (lib "unitsig.ss"))
  (provide (all-defined))
  
  (define (fmap-do-not-replace x y) y)
  (define (fmap-replace x y) x)  

  (define-signature fmap^ (fmap? 
                           is-equal? elt-equal?
                           empty empty? singleton 
                           lookup 
                           insert remove
                           merge
                           foldr
                           ; subset?
                           ; intersect difference
                           ; minimum maximum
                           member?)))