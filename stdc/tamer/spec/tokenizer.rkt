#lang typed/racket/base

(require digimon/spec)

(require racket/symbol)

(require stdc/digitama/syntax/digicore)
(require stdc/digitama/syntax/tokenizer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-tamer-input : (-> (U String Symbol (Listof Char)) Input-Port)
  (lambda [stream.c]
    (define /dev/cin : Input-Port
      (open-input-string
       (cond [(string? stream.c) stream.c]
             [(symbol? stream.c) (symbol->immutable-string stream.c)]
             [else (apply string stream.c)])))

    (port-count-lines! /dev/cin)
    /dev/cin))

(define tamer-tokens : (-> (U String Symbol) Any (Listof C-Token))
  (lambda [src cpp?]
    (c-consume-tokens (open-tamer-input src) '/dev/cin (and cpp? #true))))

(define tamer-datum : (-> Any Any)
  (lambda [c]
    (cond [(procedure? c) (object-name c)]
          [(string? c) (string-append "`" c "`")]
          [else c])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-behavior (it-check-tokens src.c expected-values cpp?)
  (let-values ([(tokens) (tamer-tokens src.c cpp?)])
    #:it
    ["should be parsed into ~a, when fed with (~a) [C++]" (map tamer-datum expected-values) src.c] #:when cpp?
    ["should be parsed into ~a, when fed with (~a)" (map tamer-datum expected-values) src.c]
    #:do
    (expect-equal (length tokens) (length expected-values))
    (for ([t (in-list tokens)]
          [e (in-list expected-values)])
      (cond [(procedure? e) (expect-satisfy (cast e (-> Any Boolean)) t)]
            [else (expect-equal (c-token->datum t) e)]))))

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
                (it-check-tokens "_" (list '_) #false)
                (it-check-tokens "true" (list 'true) #false)
                (it-check-tokens "true false" (list '#:true c:whitespace? '#:false) #true)
                (it-check-tokens "c-cpp" (list 'c c:operator? 'cpp) #false)
                (it-check-tokens "_\\u597d_" (list '_好_) #false)
                (it-check-tokens "テスト" (list 'テスト) #false)
                (it-check-tokens "\\u30D1\\u30F3" (list 'パン) #false)
                (it-check-tokens "0range" (list c:bad:range?) #false)

                (context "Universal Character" #:do
                  (it-check-tokens "\\uA8" (list c:bad:char?) #false)
                  (it-check-tokens "\\U2054" (list c:bad:char?) #false)
                  (it-check-tokens "\\u0022" (list c:bad:range?) #false)
                  (it-check-tokens "\\U0000036F" (list c:bad:range?) #false)
                  (it-check-tokens "\\u4f60\\u597d\\U0000FE2F" (list (string->symbol "你好\uFE2F")) #false)))

              (describe "String" #:do
                (it-check-tokens "\"abcd\"" (list "abcd") #false)
                (it-check-tokens "\"yes\\\\no\"" (list "yes\\no") #false)
                (it-check-tokens "\"\\1011\"" (list "A1") #false)
                (it-check-tokens "\"\\x41\"" (list "A") #false)
                (it-check-tokens "\"\\377\"" (list "\377") #false)
                (it-check-tokens "\"failed" (list c:bad:eof?) #false)
                (it-check-tokens "\"\\xstring\"" (list c:bad:char?) #false)
                (it-check-tokens "\"\\400\"" (list c:bad:range?) #false)

                (context "Escaped Hexadecimal Character" #:do
                  (it-check-tokens "U\"\\xDEADC0DE0\"" (list c:bad:range?) #false)
                  (it-check-tokens "\"\\xdead\"" (list c:bad:range?) #false)
                  (it-check-tokens "u\"\\xCafe\\x0000Beef\"" (list "\uCafe\uBeef") #false))

                (context "Raw String" #:do
                  (it-check-tokens "R\"()\")\"" (list "" #\) c:bad:eof?) #true)
                  (it-check-tokens "R\"xyz()\")xyz\"" (list ")\"") #true)
                  (it-check-tokens "R\"abc(Hello\"\\()abc\"" (list "Hello\"\\(") #true)
                  (it-check-tokens "R\"delimiter((a|b))delimiter\"" (list "(a|b)") #true)
                  (it-check-tokens "R\"longer-then-16-chars()longer-then-16-chars\"" (list c:bad:raw?) #true)
                  (it-check-tokens "R\"(hello\ngoodbye)\"" (list "hello\ngoodbye") #true)
                  (it-check-tokens "R\"(escaped\\ncharacters)\"" (list "escaped\\ncharacters") #true)
                  (it-check-tokens "R\"a(\n)\\\na\"\n)a\"" (list "\n)\\\na\"\n") #true)
                  (it-check-tokens "R\"(x = \"\\\"y\\\"\")\"" (list "x = \"\\\"y\\\"\"") #true))

                (context "Universal Characters" #:do
                  (it-check-tokens "\":-)\"" (list ":-)") #false)
                  (it-check-tokens "L\"\\U0001F609 is ;-)\"" (list "😉 is ;-)") #false)
                  (it-check-tokens "u8\"\\U0001F607 is O:-)\"" (list "😇 is O:-)") #false)
                  (it-check-tokens "u\"\\U0001F603 is :-D\"" (list "😃 is :-D") #false)
                  (it-check-tokens "U\"\\U0001F60E is B-)\"" (list "😎 is B-)") #false)))))
