#lang typed/racket/base

(provide (all-defined-out))

(require "digicore.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct c-srcloc
  ([in : Input-Port]
   [source : (U String Symbol)]
   [line : Positive-Integer]
   [column : Natural]
   [position : Positive-Integer])
  #:type-name C-Srcloc)

(define-syntax (c-make-token stx)
  (syntax-case stx []
    [(_ src make-c:token datum ...)
     (syntax/loc stx
       (let-values ([(line column here-position) (syn-token-port-location (c-srcloc-in src))])
         (make-c:token (c-srcloc-source src) (c-srcloc-line src) (c-srcloc-column src)
                         (c-srcloc-position src) here-position datum ...)))]))
  
(define-syntax (c-make-bad-token stx)
  (syntax-case stx []
    [(_ src c:bad:sub token datum)
     (syntax/loc stx
       (let ([bad (c-make-token src c:bad:sub (~s (cons (object-name token) datum)))])
         (c-log-read-error (c-token->string bad))
         bad))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-consume-token : (-> Input-Port (U String Symbol) (U C-Token EOF))
  (lambda [/dev/cin source]
    (define-values (line column position) (syn-token-port-location /dev/cin))
    (define srcloc (c-srcloc /dev/cin source line column position))
    (define ch (read-char /dev/cin))
    (cond [(eof-object? ch) eof]
          [(char-whitespace? ch) (c-consume-whitespace-token srcloc)]
          ;[(char-numeric? ch) (c-consume-numeric-token srcloc ch #false)]
          ;[(c-char-name-prefix? ch) (c-consume-ident-token srcloc ch)]
          [else (case ch
                  [(#\( #\[ #\{) (c-make-token srcloc c:open ch)]
                  [(#\) #\] #\}) (c-make-token srcloc c:close ch)]
                  ;[(#\") (c-consume-string-token srcloc ch)]
                  ;[(#\+) (c-consume-numeric-token srcloc ch #true)]
                  ;[(#\.) (c-consume-numeric-token srcloc ch #false)]
                  ;[(#\^ #\$ #\| #\~ #\*) (c-consume-match-token srcloc ch)]
                  ;[(#\#) (c-consume-hash-token srcloc)]
                  ;[(#\@) (c-consume-@keyword-token srcloc)]
                  [(#\/) (c-consume-comment-token srcloc)]
                  ;[(#\< #\-) (c-consume-cd-token srcloc ch)]
                  ;[(#\;) (c-make-token srcloc c:semicolon #\;)]
                  ;[(#\,) (c-make-token srcloc c:comma #\,)]
                  ;[(#\:) (c-make-token srcloc c:colon #\:)]
                  ;[(#\\) (c-consume-escaped-ident-token srcloc)]
                  ;[(#\null) (c-make-token srcloc c:delim #\uFFFD)]
                  [else (c-make-bad-token srcloc c:bad:char struct:c:punctuator ch)])])))

#;(define c-consume-cd-token : (-> C-Srcloc Char C-Token)
  ;;; https://drafts.csswg.org/c-syntax/#CDO-token-diagram
  ;;; https://drafts.csswg.org/c-syntax/#CDC-token-diagram
  (lambda [srcloc open/close]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (if (char=? open/close #\<)
        (let ([cdo : (U EOF String) (peek-string 3 0 /dev/cin)])
          (cond [(and (string? cdo) (string=? cdo "!--")) (read-string 3 /dev/cin) (c-make-token srcloc c:cdo '<!--)]
                [else (c-make-token srcloc c:delim #\<)]))
        (let ([cdc : (U EOF String) (peek-string 2 0 /dev/cin)])
          (cond [(eof-object? cdc) (c-make-token srcloc c:delim #\-)]
                [(string=? cdc "->") (read-string 2 /dev/cin) (c-make-token srcloc c:cdc '-->)]
                [(c-identifier-prefix? #\- (string-ref cdc 0) (string-ref cdc 1)) (c-consume-ident-token srcloc #\-)]
                [else (c-consume-numeric-token srcloc #\- #true)])))))

(define c-consume-comment-token : (-> C-Srcloc (U C:WhiteSpace C:Operator C:Bad))
  (lambda [srcloc]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define ch1 : (U EOF Char) (peek-char /dev/cin 0))
    (cond [(eof-object? ch1) (c-make-token srcloc c:slash #\/)]
          [(char=? ch1 #\/) (read-char /dev/cin) (let ([c (read-line /dev/cin)]) (c-make-token srcloc c:whitespace (if (string? c) c "")))]
          [(not (char=? ch1 #\*)) (c-make-token srcloc c:slash #\/)]
          [(regexp-match #px".*?((\\*/)|$)" /dev/cin) => (λ [**/] (c-make-token srcloc c:whitespace (format "/~a" (car **/))))]
          [else (c-make-bad-token srcloc c:bad:eof struct:c:whitespace "/*")])))

(define c-consume-whitespace-token : (-> C-Srcloc C:WhiteSpace)
  (lambda [srcloc]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (c-consume-whitespace /dev/cin)
    (c-make-token srcloc c:whitespace #\space)))
  
#;(define c-consume-ident-token : (-> C-Srcloc Char (U C:Ident C:Function C:URL C:URange C:Bad))
  ;;; https://drafts.csswg.org/c-syntax/#consume-an-ident-like-token
  ;;; https://drafts.csswg.org/c-syntax/#urange-syntax
  ;;; https://drafts.csswg.org/c-values/#urls
  ;;; https://drafts.csswg.org/c-variables/#defining-variables
  (lambda [srcloc id0]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define ch1 : (U EOF Char) (peek-char /dev/cin 0))
    (define ch2 : (U EOF Char) (peek-char /dev/cin 1))
    (cond [(and (char-ci=? id0 #\u) (char? ch1) (char=? ch1 #\+)
                (or (char-hexdigit? ch2) (and (char? ch2) (char=? ch2 #\?))))
           (read-char /dev/cin) (c-consume-unicode-range-token srcloc)]
          [else (let ([name (c-consume-name /dev/cin id0)])
                  (define ch : (U EOF Char) (peek-char /dev/cin))
                  (cond [(or (eof-object? ch) (not (char=? ch #\()))
                         (if (and (char=? id0 #\-) (eqv? id0 ch1))
                             (let ([--id (string->unreadable-symbol name)]) (c-make-token srcloc c:ident --id --id))
                             (c-make-token srcloc c:ident (string->symbol name) (string->symbol (string-downcase name))))]
                        [(and (or (not (string-ci=? name "url")) (regexp-match-peek #px"^.\\s*[\"']" /dev/cin)) (read-char /dev/cin))
                         (define fnorm : Symbol (string->symbol (string-downcase name)))
                         (c-make-token srcloc c:function (string->unreadable-symbol name) fnorm null #false)]
                        [else (read-char /dev/cin) (c-consume-url-token srcloc)]))])))

#;(define c-consume-escaped-ident-token : (-> C-Srcloc (U C:Ident C:Delim C:Function C:URL C:URange C:Bad))
  ;;; https://drafts.csswg.org/c-syntax/#consume-token (when #\\ is found at the beginning of a non-whitespace token)
  (lambda [srcloc]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (if (c-valid-escape? #\\ (peek-char /dev/cin 1))
        (c-consume-ident-token srcloc (c-consume-escaped-char /dev/cin))
        (syn-remake-token (c-make-bad-token srcloc c:bad:char struct:c:delim #\\) c:delim #\\))))

#;(define c-consume-string-token : (-> C-Srcloc Char (U C:String C:Bad))
  (lambda [srcloc quotation]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (let consume-string-token : (U C:String C:Bad) ([chars : (Listof Char) null])
      (define ch : (U EOF Char) (read-char /dev/cin))
      (cond [(or (eof-object? ch) (char=? ch quotation))
             (when (eof-object? ch) (c-make-bad-token srcloc c:bad:eof struct:c:string (list->string (reverse chars))))
             (c-make-token srcloc c:string (list->string (reverse chars)))]
            [(char=? ch #\newline) (c-make-bad-token srcloc c:bad:eol struct:c:string (list->string (reverse chars)))]
            [(not (char=? ch #\\)) (consume-string-token (cons ch chars))]
            [else (let ([next (peek-char /dev/cin)])
                    (cond [(eof-object? next) (consume-string-token chars)]
                          [(and (char=? next #\newline) (read-char /dev/cin)) (consume-string-token (cons ch chars))]
                          [else (consume-string-token (cons (c-consume-escaped-char /dev/cin) chars))]))]))))

#;(define c-consume-numeric-token : (-> C-Srcloc Char Boolean (U C-Numeric C:Delim C:Bad))
  ;;; https://drafts.csswg.org/c-syntax/#consume-a-number
  ;;; https://drafts.csswg.org/c-values/#numeric-types
  (lambda [srcloc sign/digit signed?]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define ch1 : (U EOF Char) (peek-char /dev/cin 0))
    (define ch2 : (U EOF Char) (peek-char /dev/cin 1))
    (cond [(not (c-number-prefix? sign/digit ch1 ch2)) (c-make-token srcloc c:delim sign/digit)]
          [else (let-values ([(n representation) (c-consume-number /dev/cin sign/digit)])
                  (let ([ch1 : (U EOF Char) (peek-char /dev/cin 0)]
                        [ch2 : (U EOF Char) (peek-char /dev/cin 1)]
                        [ch3 : (U EOF Char) (peek-char /dev/cin 2)])
                    (cond [(c-identifier-prefix? ch1 ch2 ch3)
                           (define unit : Symbol (string->symbol (string-downcase (c-consume-name /dev/cin #false))))
                           (define value : Flonum (real->double-flonum n))
                           (define rep+unit : String (~a representation unit))
                           (case unit
                             [(em ex ch ic rem)       (c-make-token srcloc c:length:font     rep+unit signed? value unit)]
                             [(cap lh rlh)            (c-make-token srcloc c:length:font     rep+unit signed? value unit)]
                             [(vw vh vi vb vmin vmax) (c-make-token srcloc c:length:viewport rep+unit signed? value unit)]
                             [(px cm mm q in pc pt)   (c-make-token srcloc c:length          rep+unit signed? value unit)]
                             [(apc pls ls)            (c-make-token srcloc c:length          rep+unit signed? value unit)]
                             [(deg grad rad turn)     (c-make-token srcloc c:angle           rep+unit signed? value unit)]
                             [(s ms min h mtn tn)     (c-make-token srcloc c:time            rep+unit signed? value unit)]
                             [(hz khz)                (c-make-token srcloc c:frequency       rep+unit signed? value unit)]
                             [(dpi dpcm dppx x)       (c-make-token srcloc c:resolution      rep+unit signed? value unit)]
                             [else                    (c-make-token srcloc c:dimension       rep+unit signed? value unit)])]
                          [(and (char? ch1) (char=? ch1 #\%) (read-char /dev/cin))
                           (define n% : Flonum (real->double-flonum (* n 0.01)))
                           (c-make-token srcloc c:percentage (string-append representation "%") signed? n%)]
                          [(flonum? n)
                           (cond [(zero? n) (c-make-token srcloc c:flzero representation signed? n)]
                                 [(fl= n 1.0) (c-make-token srcloc c:flone representation signed? n)]
                                 [else (c-make-token srcloc c:flonum representation signed? n)])]
                          [(zero? n) (c-make-token srcloc c:zero representation signed? n)]
                          [(= n 1) (c-make-token srcloc c:one representation signed? n)]
                          [else (c-make-token srcloc c:integer representation signed? n)])))])))

#;(define c-consume-url-token : (-> C-Srcloc (U C:URL C:Bad))
  ;;; https://drafts.csswg.org/c-syntax/#consume-a-url-token
  ;;; https://drafts.csswg.org/c-values/#urls
  ;;; https://drafts.csswg.org/c-values/#url-empty
  ;;; https://drafts.csswg.org/c-values/#about-invalid
  (lambda [srcloc]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (c-consume-whitespace /dev/cin)
    (let consume-url-token ([srahc : (Listof Char) null])
      (define ch : (U EOF Char) (read-char /dev/cin))
      (cond [(or (eof-object? ch) (char=? ch #\)))
             (define uri : String (list->string (reverse srahc)))
             (when (eof-object? ch) (c-make-bad-token srcloc c:bad:eof struct:c:url uri))
             (c-make-token srcloc c:url uri null #false)]
            [(and (char-whitespace? ch) (c-consume-whitespace /dev/cin))
             (define end : (U EOF Char) (read-char /dev/cin))
             (define uri : String (list->string (reverse srahc)))
             (cond [(or (eof-object? end) (char=? end #\))) (c-make-token srcloc c:url uri null #false)]
                   [else (c-consume-bad-url-remnants /dev/cin (c-make-bad-token srcloc c:bad:blank struct:c:url uri))])]
            [(c-valid-escape? ch (peek-char /dev/cin)) (consume-url-token (cons (c-consume-escaped-char /dev/cin) srahc))]
            [(or (memq ch '(#\\ #\" #\' #\()) (c-char-non-printable? ch))
             (c-consume-bad-url-remnants /dev/cin (c-make-bad-token srcloc c:bad:char struct:c:url ch))]
            [else (consume-url-token (cons ch srahc))]))))

#;(define c-consume-unicode-range-token : (-> C-Srcloc (U C:URange C:Bad))
  ;;; https://drafts.csswg.org/c-syntax/#urange-syntax
  (lambda [srcloc]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define-values (n rest) (c-consume-hexadecimal (c-srcloc-in srcloc) 6))
    (define-values (start0 end0)
      (let consume-? : (Values Fixnum Fixnum) ([s : Fixnum n] [e : Fixnum n] [? : Fixnum rest])
        (cond [(zero? ?) (values s e)]
              [else (let ([ch : (U EOF Char) (peek-char /dev/cin)])
                      (cond [(or (eof-object? ch) (not (char=? ch #\?))) (values s e)]
                            [else (read-char /dev/cin) (consume-? (fxlshift s 4)
                                                                    (fxior (fxlshift e 4) #xF)
                                                                    (fx- ? 1))]))])))
    (define-values (start end)
      (cond [(not (fx= start0 end0)) (values start0 end0)]
            [else (let ([ch1 (peek-char /dev/cin 0)]
                        [ch2 (peek-char /dev/cin 1)])
                    (cond [(and (char? ch1) (char=? ch1 #\-) (char-hexdigit? ch2) (read-char /dev/cin))
                           (define-values (end _) (c-consume-hexadecimal (c-srcloc-in srcloc) 6))
                           (values start0 end)]
                          [else (values start0 start0)]))]))
    (cond [(and (index? start) (index? end) (<= start end #x10FFFF)) (c-make-token srcloc c:urange (cons start end))]
          [(> end #x10FFFF) (c-make-bad-token srcloc c:bad:range struct:c:urange end)]
          [else (c-make-bad-token srcloc c:bad:range struct:c:urange (cons start end))])))

#;(define c-consume-hash-token : (-> C-Srcloc (U C:Hash C:Delim))
  ;;; https://drafts.csswg.org/c-syntax/#hash-token-diagram
  (lambda [srcloc]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define ch1 : (U EOF Char) (peek-char /dev/cin 0))
    (define ch2 : (U EOF Char) (peek-char /dev/cin 1))
    (define ch3 : (U EOF Char) (peek-char /dev/cin 2))
    (if (or (c-char-name? ch1) (c-valid-escape? ch1 ch2))
        (let ([name (c-consume-name (c-srcloc-in srcloc) #false)])
          (c-make-token srcloc c:hash (string->keyword name) #|(string->keyword (string-downcase name))|#))
        (c-make-token srcloc c:delim #\#))))

#;(define c-consume-@keyword-token : (-> C-Srcloc (U C:@Keyword C:Delim))
  ;;; https://drafts.csswg.org/c-syntax/#at-keyword-token-diagram
  (lambda [srcloc]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define ch1 : (U EOF Char) (peek-char /dev/cin 0))
    (define ch2 : (U EOF Char) (peek-char /dev/cin 1))
    (define ch3 : (U EOF Char) (peek-char /dev/cin 2))
    (if (c-identifier-prefix? ch1 ch2 ch3)
        (let ([name (c-consume-name (c-srcloc-in srcloc) #\@)])
          (c-make-token srcloc c:@keyword (string->keyword name) (string->keyword (string-downcase name))))
        (c-make-token srcloc c:delim #\@))))

#;(define c-consume-match-token : (-> C-Srcloc Char (U C:Match C:Delim))
  ;;; https://drafts.csswg.org/c-syntax/#include-match-token-diagram
  ;;; https://drafts.csswg.org/c-syntax/#column-token-diagram
  (lambda [srcloc prefix]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define ch : (U EOF Char) (peek-char /dev/cin))
    (cond [(and (eq? prefix #\|) (eq? ch #\|) (read-char /dev/cin)) (c-make-token srcloc c:delim #\tab)]
          [(and (eq? ch #\=) (read-char /dev/cin)) (c-make-token srcloc c:match prefix)]
          [(eq? prefix #\|) (c-make-token srcloc c:vbar prefix)]
          [else (c-make-token srcloc c:delim prefix)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-consume-whitespace : (-> Input-Port Void)
  (lambda [/dev/cin]
    (regexp-match #px"\\s*" /dev/cin)
    (void)))
  
#;(define c-consume-name : (-> Input-Port (Option Char) String)
  ;;; https://drafts.csswg.org/c-syntax/#consume-a-name
  (lambda [/dev/cin ?head]
    (let consume-name ([srahc : (Listof Char) (if ?head (list ?head) null)])
      (define ch : (U EOF Char) (peek-char /dev/cin))
      (cond [(and (c-char-name? ch) (read-char /dev/cin)) (consume-name (cons ch srahc))]
            [(and (c-valid-escape? ch (peek-char /dev/cin 1)) (read-char /dev/cin))
             (consume-name (cons (c-consume-escaped-char /dev/cin) srahc))]
            [else (list->string (reverse srahc))]))))

#;(define c-consume-number : (-> Input-Port Char (Values (U Flonum Integer) String))
  ;;; https://drafts.csswg.org/c-syntax/#consume-a-number
  (lambda [/dev/cin sign/digit]
    (let consume-number ([chars (list sign/digit)])
      (define ch : (U EOF Char) (peek-char /dev/cin))
      (cond [(and (char? ch)
                  (or (char-numeric? ch)
                      (char=? ch #\+) (char=? ch #\-)
                      (c-decimal-point? ch (peek-char /dev/cin 1))
                      (c-scientific-notation? ch (peek-char /dev/cin 1) (peek-char /dev/cin 2)))
                  (read-char /dev/cin))
             (consume-number (cons ch chars))]
            [else (let* ([representation : String (list->string (reverse chars))]
                         [?number : (Option Complex) (string->number representation)])
                    (cond [(exact-integer? ?number) (values ?number representation)]
                          [(flonum? ?number) (values ?number representation)]
                          [else (values +nan.0 representation)]))]))))

#;(define c-consume-hexadecimal : (->* (Input-Port Byte) (Fixnum #:\s?$? Boolean) (Values Fixnum Byte))
  (lambda [/dev/cin --count [result 0] #:\s?$? [eat-last-whitespace? #false]]
    (define hex : (U EOF Char) (peek-char /dev/cin))
    (cond [(or (eof-object? hex) (not (char-hexdigit? hex)) (zero? --count))
           (when (and eat-last-whitespace? (char? hex) (char-whitespace? hex)) (read-char /dev/cin))
           (values result --count)]
          [else (read-char /dev/cin) (c-consume-hexadecimal #:\s?$? eat-last-whitespace?
                                                         /dev/cin (fx- --count 1)
                                                         (fx+ (fxlshift result 4)
                                                              (char->hexadecimal hex)))])))

#;(define c-consume-escaped-char : (-> Input-Port Char)
  ;;; https://drafts.csswg.org/c-syntax/#consume-an-escaped-code-point
  (lambda [/dev/cin]
    (define esc : (U EOF Char) (read-char /dev/cin))
    (cond [(eof-object? esc) #\uFFFD]
          [(not (char-hexdigit? esc)) esc]
          [else (let-values ([(hex _) (c-consume-hexadecimal /dev/cin (sub1 6) (char->hexadecimal esc) #:\s?$? #true)])
                  (cond [(or (fx<= hex 0) (fx> hex #x10FFFF)) #\uFFFD] ; #\nul and max unicode
                        [(<= #xD800 hex #xDFFF) #\uFFFD] ; surrogate
                        [else (integer->char hex)]))])))

#;(define c-consume-bad-url-remnants : (-> Input-Port C:Bad C:Bad)
  ;;; https://drafts.csswg.org/c-syntax/#consume-the-remnants-of-a-bad-url
  (lambda [/dev/cin bad-url-token]
    (define ch : (U EOF Char) (read-char /dev/cin))
    (cond [(or (eof-object? ch) (char=? ch #\))) bad-url-token]
          [(and (char=? ch #\\) (read-char /dev/cin)) (c-consume-bad-url-remnants /dev/cin bad-url-token)]
          [else (c-consume-bad-url-remnants /dev/cin bad-url-token)])))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define char-hexdigit? : (-> (U EOF Char) Boolean : #:+ Char)
  (lambda [ch]
    (and (char? ch)
         (or (char-numeric? ch)
             (char-ci<=? #\a ch #\f)))))

(define char->hexadecimal : (-> Char Fixnum)
  (lambda [hexch]
    (cond [(char<=? #\a hexch) (fx- (char->integer hexch) #x57)]
          [(char<=? #\A hexch) (fx- (char->integer hexch) #x37)]
          [else (fx- (char->integer hexch) #x30)])))

#;(define c-char-non-printable? : (-> (U EOF Char) Boolean : #:+ Char)
  (lambda [ch]
    (and (char? ch)
         (or (char<=? #\null ch #\backspace)
             (char=? ch #\vtab)
             (char<=? #\u000E ch #\u001F)
             (char=? ch #\rubout)))))

#;(define c-char-name-prefix? : (-> (U EOF Char) Boolean : #:+ Char)
  (lambda [ch]
    (and (char? ch)
         (or (char-lower-case? ch)
             (char-upper-case? ch)
             (char=? #\_ ch)
             (char>=? ch #\u0080)))))

#;(define c-char-name? : (-> (U EOF Char) Boolean : #:+ Char)
  (lambda [ch]
    (and (char? ch)
         (or (c-char-name-prefix? ch)
             (char-numeric? ch)
             (char=? #\- ch)))))
  
#;(define c-valid-escape? : (-> (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.csswg.org/c-syntax/#escaping
  ;;; https://drafts.csswg.org/c-syntax/#starts-with-a-valid-escape
  (lambda [ch1 ch2]
    (and (char? ch1)
         (char=? ch1 #\\)
         (or (eof-object? ch2)
             (not (char=? ch2 #\newline))))))

#;(define c-identifier-prefix? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.csswg.org/c-syntax/#would-start-an-identifier
  (lambda [ch1 ch2 ch3]
    (or (c-char-name-prefix? ch1)
        (c-valid-escape? ch1 ch2)
        (and (char? ch1) (char=? ch1 #\-)
             (or (c-char-name-prefix? ch2)
                 (and (char? ch2) (char=? ch2 #\-))
                 (c-valid-escape? ch2 ch3))))))

#;(define c-number-prefix? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.csswg.org/c-syntax/#starts-with-a-number
  (lambda [ch1 ch2 ch3]
    (or (and (char? ch1) (char<=? #\0 ch1 #\9))
        (and (char? ch1) (char? ch2)
             (char=? ch1 #\.) (char<=? #\0 ch2 #\9))
        (and (char? ch1) (char? ch2)
             (or (char=? ch1 #\+) (char=? ch1 #\-))
             (or (char<=? #\0 ch2 #\9)
                 (and (char=? ch2 #\.)
                      (char? ch3) (char<=? #\0 ch3 #\9)))))))

#;(define c-scientific-notation? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.csswg.org/c-syntax/#consume-a-number
  (lambda [ch1 ch2 ch3]
    (and (char? ch1) (char? ch2)
         (char-ci=? ch1 #\e)
         (or (char<=? #\0 ch2 #\9)
             (and (or (char=? ch2 #\+) (char=? ch2 #\-))
                  (char? ch3)
                  (char<=? #\0 ch3 #\9))))))

#;(define c-decimal-point? : (-> (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.csswg.org/c-syntax/#consume-a-number
  (lambda [ch1 ch2]
    (and (char? ch1) (char? ch2)
         (char=? ch1 #\.)
         (char<=? #\0 ch2 #\9))))
