(module int-test mzscheme
  (require (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "list.ss"))
  (require (planet "maybe.ss" ("jaymccarthy" "mmss.plt" 1)))
  (require "int.ss"
           "fmap.ss")
  
  (print-struct #t)
  
  (define-values/invoke-unit/sig fmap^
    positive-big-endian-fmap@
    fmap)
  
  (define (oka)
    (foldl (lambda (k v a)
             (fmap:insert fmap-replace k v a))
           fmap:empty
           (list 1 4 5)
           (list 'x 'y 'z)))
  
  (define (test)
    (foldl (lambda (e a)
             (fmap:insert fmap-replace e e a))
           fmap:empty
           (build-list 10 add1))))