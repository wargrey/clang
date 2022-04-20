#lang typed/racket/base

(provide (all-defined-out))

(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [cpp-read read]))
  (provide (rename-out [cpp-read-syntax read-syntax]))
  (provide (rename-out [cpp-info get-info]))
  
  (require stdc/village/clang/reader))
