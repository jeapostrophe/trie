(module symbol-test mzscheme
  (require (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "list.ss"))
  (require (planet "maybe.ss" ("jaymccarthy" "mmss.plt" 1)))
  (require "fmap.ss"
           "symbol.ss")
  
  (print-struct #t)
  
  (define-values/invoke-unit/sig fmap^
    symbol-fmap@
    fmap)
  
  (define (test)
    (foldl (lambda (e a)
             (fmap:insert fmap-replace e (gensym) a))
           fmap:empty
           `(foo bar zog zig zah zag bog foz))))