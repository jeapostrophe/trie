(module string-test mzscheme
  (require (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "list.ss"))
  (require (planet "maybe.ss" ("jaymccarthy" "mmss.plt" 1)))
  (require "fmap.ss"
           "string.ss")
  
  (print-struct #t)
  
  (define-values/invoke-unit/sig fmap^
    string-fmap@
    fmap)
  
  (define (test)
    (foldl (lambda (e a)
             (fmap:insert fmap-replace e (gensym) a))
           fmap:empty
           (list "foo"
                 "bar"
                 "zog"
                 "zig"
                 "zah"
                 "zag"
                 "bog"
                 "foz"))))