#lang typed/racket/base

(provide (all-defined-out) Syn-Token-Stdin)

(require digimon/token)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cpp-open-input-port : (->* (Syn-Token-Stdin) (Boolean (U String Symbol False)) Input-Port)
  (lambda [/dev/stdin [enable-line-counting? #false] [port-name #false]]
    (define /dev/rawin : Input-Port (syn-token-stdin->port /dev/stdin #px"\\.c(pp|xx)?$" 'cppin port-name))

    (when (and enable-line-counting? (not (port-counts-lines? /dev/rawin)))
      (port-count-lines! /dev/rawin))
    
    (syn-token-port-skip-lang-line /dev/rawin)
    
    /dev/rawin))

(define header-open-input-port : (->* (Syn-Token-Stdin) (Boolean (U String Symbol False)) Input-Port)
  (lambda [/dev/stdin [enable-line-counting? #false] [port-name #false]]
    (define /dev/rawin : Input-Port (syn-token-stdin->port /dev/stdin #px"\\.h(pp|xx)?$" 'cppin port-name))

    (when (and enable-line-counting? (not (port-counts-lines? /dev/rawin)))
      (port-count-lines! /dev/rawin))
    
    (syn-token-port-skip-lang-line /dev/rawin)
    
    /dev/rawin))
