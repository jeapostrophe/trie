(module string mzscheme
  (require (lib "plt-match.ss")
           (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "list.ss"))
  (require (planet "maybe.ss" ("jaymccarthy" "mmss.plt" 1)))
  (require "fmap.ss"
           "int.ss"
           "list.ss"
           "proj.ss")
  (provide (all-defined))
  
  (define string-fmap@
    (proj-fmap@ (lambda (str)
                  (map char->integer (string->list str)))
                (lambda (iclst)
                  (list->string
                   (map integer->char iclst)))
                (specialize-list-fmap@ positive-big-endian-fmap@))))