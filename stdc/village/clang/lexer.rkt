#lang racket/base

(provide c-lexer)

(require stdc/digitama/syntax/digicore)
(require stdc/digitama/syntax/tokenizer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-lexer ;: (-> Input-Port Natural Any (Values (U String EOF) Symbol (Option Symbol) (Option Integer) (Option Integer) Natural Any))
  (lambda [/dev/drin offset mode]
    (define t #|: (U EOF C-Token) |# (c-consume-token /dev/drin '/dev/drin #true))
    (cond [(eof-object? t) (values eof 'eof #false #false #false 0 (not mode))]
          [(c:whitespace? t) (c-hlvalues t (if (string? (c:whitespace-datum t)) 'comment 'white-space) #false mode)]
          [(c:keyword? t) (c-hlvalues t (c-keyword->drtype (c:keyword-datum t) #false) #false mode)]
          [(c:identifier? t) (c-hlvalues t (c-id->drtype (c:identifier-datum t) #false) #false mode)]
          [(c:open? t) (c-hlvalues t 'parenthesis (string->symbol (string (c:operator-datum t))) mode)]
          [(c:close? t) (c-hlvalues t 'parenthesis (string->symbol (string (c:close-datum t))) mode)]
          [(c:operator? t) (c-hlvalues t (c-char->drtype (c:operator-datum t)) #false mode)]
          [(c-numeric? t) (c-hlvalues t 'constant #false mode)]
          [else (c-hlvalues t (c-other->drtype t) #false mode)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-char->drtype ;: (-> Char Symbol)
  (lambda [delim]
    (case delim
      [(#\:) 'sexp-comment]
      [(#\, #\;) 'parenthesis]
      [(#\+ #\- #\* #\/) 'symbol]
      [else 'constant])))

(define c-keyword->drtype ;: (-> Symbol Boolean Symbol)
  (lambda [kw func?]
    (case kw
      [(#:true #:false) 'constant]
      [else 'keyword])))
  
(define c-id->drtype ;: (-> Symbol Boolean Symbol)
  (lambda [id func?]
    (case id
      [else 'symbol])))
  
(define c-other->drtype ;: (-> C-Token Symbol)
  (lambda [token]
    (cond [(c:string? token) 'string]
          ;[(c:hash? token) 'hash-colon-keyword]
          [(c:bad? token) 'error]
          [else 'other])))

(define c-hlvalues ;: (-> C-Token Symbol (Option Symbol) Any (Values String Symbol (Option Symbol) (Option Integer) (Option Integer) Natural Any))
  (lambda [t type subtype mode]
    (values "" type subtype (syn-token-start t) (syn-token-end t) 0 (not mode))))
