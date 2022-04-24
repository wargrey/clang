#lang typed/racket/base

(require digimon/spec)

(require stdc/digitama/syntax/digicore)
(require stdc/digitama/syntax/tokenizer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-tamer-input : (-> (U String (Listof Char)) Input-Port)
  (lambda [stream.c]
    (define /dev/cin : Input-Port
      (open-input-string
       (cond [(string? stream.c) stream.c]
             [else (apply string stream.c)])))

    (port-count-lines! /dev/cin)
    /dev/cin))

(define tamer-tokens : (-> String (Listof C-Token))
  (lambda [src]
    (c-consume-tokens (open-input-string src) '/dev/cin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-behavior (it-check-tokens src.c expected-values)
  (let-values ([(tokens) (tamer-tokens src.c)])
    #:it
    ["should be parsed into ~s, when fed with ~s" expected-values src.c]
    #:do
    (for ([t (in-list tokens)]
          [v (in-list expected-values)])
      (cond [(procedure? v) (expect-satisfy (cast v (-> Any Boolean)) t)]
            [else (expect-equal (c-token->datum t) v)]))
    (expect-equal (length tokens) (length expected-values))))

#;(define-behavior (it-check-parser stream.c logsrc <rng> expected-values)
  (let ([rng-object? (or (preamble? expected-values)
                         (pattern? expected-values)
                         (name-class? expected-values)
                         (grammar-content? expected-values)
                         (annotation? expected-values)
                         (annotation-element? expected-values))])
    #:it
    ["should be parsed into ~s, when fed with ~s" expected-values stream.c] #:when rng-object?
    ["should report error due to `~a`, when fed with ~s" (object-name expected-values) stream.c] #:when (procedure? expected-values)
    ["shouldn't report error, when fed with ~s" stream.c] #:when (and (vector? expected-values) (= (vector-length expected-values) 0))
    ["should report error as in ~a, when fed with ~s" expected-values stream.c]
    #:do
    (if (not rng-object?)

        (expect-log-message logsrc expected-values
                            (λ [] (tamer-parser stream.c <rng>)))
        
        (let-values ([(es rest) (tamer-parser stream.c <rng>)])
          (expect-satisfy null? rest)
          (expect-satisfy list? es)
          (expect-equal (car (assert es list?)) expected-values)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require digimon/dtrace)
  
  (define logsrc : Log-Receiver (make-log-receiver /dev/dtrace 'warning 'exn:c:syntax))

  (current-logger /dev/dtrace)

  (spec-begin Tokenizer #:do
              (describe "Identifier" #:do
                (it-check-tokens "int" (list '#:int))
                (it-check-tokens "main" (list 'main)))
              (describe "String" #:do)))
