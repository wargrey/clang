#lang typed/racket/base

(provide (all-defined-out))

(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [header-read read]))
  (provide (rename-out [header-read-syntax read-syntax]))
  (provide (rename-out [header-info get-info]))
  
  (require stdc/village/clang/reader))
