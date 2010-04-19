(module symbol mzscheme
  (require (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "list.ss"))
  (require "fmap.ss"
           "proj.ss"
           "string.ss")
  (provide (all-defined))
  
  (define symbol-fmap@
    (proj-fmap@ symbol->string
                string->symbol
                string-fmap@)))