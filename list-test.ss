(module list-test mzscheme
  (require (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "list.ss"))
  (require (planet "maybe.ss" ("jaymccarthy" "mmss.plt" 1)))
  (require "fmap.ss"
           "int.ss"
           "list.ss")
  
  (print-struct #t)
  
  (define-values/invoke-unit/sig fmap^
    (specialize-list-fmap@ positive-big-endian-fmap@)
    fmap)
  
  (define (test)
    (foldl (lambda (e a)
             (fmap:insert fmap-replace e (gensym) a))
           fmap:empty
           (list (list 1 2 3 4)
                 (list 4 3 45 6)
                 (list 1 2)
                 (list)
                 (list 4 3 45 7)
                 (list 1 2 3 4 5)))))