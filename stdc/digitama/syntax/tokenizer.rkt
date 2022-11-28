#lang typed/racket/base

(provide (all-defined-out))

(require "digicore.rkt")
(require "tokenizer/keyword.rkt")

(require digimon/character)
(require digimon/stdio)

(require racket/string)
(require racket/symbol)
(require racket/unsafe/ops)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type C-Bad-Constructor
  (-> (U String Symbol)
      Positive-Integer Nonnegative-Integer Positive-Integer Positive-Integer
      (Pairof Symbol String)
      C:Bad))

(define-type C-Bad-Datum (Pairof C-Bad-Constructor (Listof Char)))

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
    [(_ src c:bad:sub struct:c:token chars)
     (syntax/loc stx
       (let ([bad (c-make-token src c:bad:sub
                                (cons (assert (object-name struct:c:token) symbol?)
                                      (cond [(list? chars) (list->string chars)]
                                            [(char? chars) (string chars)]
                                            [(string? chars) chars]
                                            [else (~a chars)])))])
         (c-log-read-error (c-token->string bad))
         bad))]
    [(_ src struct:c:token bad-datum)
     (syntax/loc stx
       (c-make-bad-token src (car bad-datum) struct:c:token (cdr bad-datum)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-consume-tokens : (->* (Input-Port (U String Symbol False)) (Boolean) (Listof C-Token))
  (lambda [/dev/cin maybe-source [cpp? #false]]
    (define source : (U String Symbol) (or maybe-source (syn-token-port-name /dev/cin)))
    
    (let read-c-token ([snekot : (Listof C-Token) null])
      (define t (c-consume-token /dev/cin source cpp?))
      
      (cond [(eof-object? t) (reverse snekot)]
            [else (read-c-token (cons t snekot))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-consume-token : (-> Input-Port (U String Symbol) Boolean (U C-Token EOF))
  (lambda [/dev/cin source cpp?]
    (define-values (line column position) (syn-token-port-location /dev/cin))
    (define srcloc (c-srcloc /dev/cin source line column position))
    (define ch (read-char /dev/cin))
    (cond [(eof-object? ch) eof]
          [(char-whitespace? ch) (c-consume-whitespace-token srcloc)]
          ;[(char-numeric? ch) (c-consume-numeric-token srcloc ch #false)]
          [(c-identifier-initial-char? ch) (c-consume-identifier-token srcloc ch cpp?)]
          [else (case ch
                  [(#\( #\[ #\{ #\<) (c-make-token srcloc c:open ch)]
                  [(#\) #\] #\} #\>) (c-make-token srcloc c:close ch)]
                  [(#\") (c-consume-string-token srcloc ch cpp? #xFF)]
                  ;[(#\+) (c-consume-numeric-token srcloc ch #true)]
                  ;[(#\.) (c-consume-numeric-token srcloc ch #false)]
                  ;[(#\^ #\$ #\| #\~ #\*) (c-consume-match-token srcloc ch)]
                  ;[(#\#) (c-consume-hash-token srcloc)]
                  [(#\/) (c-consume-comment-token srcloc)]
                  [(#\;) (c-make-token srcloc c:semicolon #\;)]
                  [(#\,) (c-make-token srcloc c:comma #\,)]
                  [(#\=) (c-make-token srcloc c:eq #\=)]
                  [(#\:) (c-make-token srcloc c:colon #\:)]
                  [(#\\) (c-consume-universal-identifier-token srcloc cpp?)]
                  [else (c-make-bad-token srcloc c:bad:char struct:c:punctuator ch)])])))

(define c-consume-comment-token : (-> C-Srcloc (U C:WhiteSpace C:Operator C:Bad))
  (lambda [srcloc]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define ch1 : (U EOF Char) (peek-char /dev/cin 0))
    (cond [(eof-object? ch1) (c-make-token srcloc c:slash #\/)]
          [(eq? ch1 #\/) (read-char /dev/cin) (let ([c (read-line /dev/cin)]) (c-make-token srcloc c:whitespace (if (string? c) c "")))]
          [(not (eq? ch1 #\*)) (c-make-token srcloc c:slash #\/)]
          [(regexp-match #px".*?((\\*/)|$)" /dev/cin) => (λ [**/] (c-make-token srcloc c:whitespace (format "/~a" (car **/))))]
          [else (c-make-bad-token srcloc c:bad:eof struct:c:whitespace "/*")])))

(define c-consume-whitespace-token : (-> C-Srcloc C:WhiteSpace)
  (lambda [srcloc]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (c-consume-whitespace /dev/cin)
    (c-make-token srcloc c:whitespace #\space)))
  
(define c-consume-identifier-token : (-> C-Srcloc Char Boolean (U C:Identifier C:Keyword C:String C:Bad))
  (lambda [srcloc leader cpp?]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define maybe-id : (U String C-Bad-Datum) (c-consume-identifier /dev/cin leader))

    (cond [(pair? maybe-id) (c-make-bad-token srcloc struct:c:identifier maybe-id)]
          [else (let ([keyword-map (if (not cpp?) c-keyword-map cpp-keyword-map)]
                      [sym (string->symbol maybe-id)])
                  (cond [(hash-has-key? keyword-map sym) (c-make-token srcloc c:keyword (hash-ref keyword-map sym))]
                        [else (let ([?quotation (peek-char /dev/cin 0)])
                                (if (or (eq? ?quotation #\") (eq? ?quotation #\'))
                                    (let* ([prefix-len (string-length maybe-id)]
                                           [raw? (and (> prefix-len 0) #| <- redundant |# (eq? (string-ref maybe-id (sub1 prefix-len)) #\R))])
                                      (c-consume-special-string-token srcloc sym ?quotation cpp? raw?))
                                    (c-make-token srcloc c:identifier sym)))]))])))
  
(define c-consume-universal-identifier-token : (-> C-Srcloc Boolean (U C:Identifier C:Keyword C:String C:Bad))
  (lambda [srcloc cpp?]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define maybe-head : (U Char C-Bad-Datum) (c-consume-universal-char /dev/cin (list #\\) #false #false 0))

    (cond [(pair? maybe-head) (c-make-bad-token srcloc struct:c:identifier maybe-head)]
          [else (c-consume-identifier-token srcloc maybe-head cpp?)])))

(define c-consume-string-token : (->* (C-Srcloc Char Boolean Nonnegative-Fixnum) ((Option Symbol)) (U C:String C:Bad))
  (lambda [srcloc quotation cpp? x.ceiling [tag #false]]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))
    (define-values (maybe-string maybe-suffix)
      (if (not cpp?)
          (c-consume-native-char-sequence /dev/cin quotation x.ceiling)
          (cpp-consume-native-char-sequence /dev/cin quotation x.ceiling)))

    (cond [(string? maybe-string) (c-make-token srcloc c:string maybe-string tag maybe-suffix)]
          [else (c-make-bad-token srcloc (car maybe-string) struct:c:string
                                  (append (if (not tag) null (string->list (symbol->immutable-string tag)))
                                          (cond [(not maybe-suffix) (cdr maybe-string)]
                                                [else (append (cdr maybe-string)
                                                              (string->list (symbol->immutable-string maybe-suffix)))])))])))

(define c-consume-cpp-raw-string-token : (-> C-Srcloc Char Symbol (U C:String C:Bad))
  (lambda [srcloc quotation encoding-prefix]
    (define /dev/cin : Input-Port (c-srcloc-in srcloc))

    (define maybe-edelim : (U String C-Bad-Datum)
      (let read-delimiter ([span : Nonnegative-Fixnum 0]
                           [skip : Nonnegative-Fixnum 0])
        (define ch : (U EOF Char) (peek-char /dev/cin skip))
        (cond [(eof-object? ch) (c-consume-error-chars /dev/cin null (read-tail-string /dev/cin span #false) quotation c:bad:eof)]
              [(eq? ch quotation) (c-consume-error-chars /dev/cin null (read-tail-string /dev/cin span #false) quotation c:bad:raw)]
              [(> span 16) (c-consume-error-chars /dev/cin null (read-tail-string /dev/cin span #false) quotation c:bad:raw)]
              [(not (c-raw-string-nondelimiter-char? ch)) (read-delimiter (+ span 1) (unsafe-fx+ skip (char-utf-8-length ch)))]
              [(eq? ch #\() (begin0 (string-append (read-tail-string /dev/cin span #\)) (string quotation)) (read-char /dev/cin))]
              [else (c-consume-error-chars /dev/cin null (read-tail-string /dev/cin span #false) quotation c:bad:raw)])))

    (define maybe-string : (U String C:Bad C-Bad-Datum)
      (if (string? maybe-edelim)
          (let ([dsize : Index (string-length maybe-edelim)])
            (let read-raw-string ([span : Nonnegative-Fixnum 0]
                                  [skip : Nonnegative-Fixnum 0])
              (define ch : (U EOF Char) (peek-char /dev/cin skip))
              (cond [(eof-object? ch) (c-consume-error-chars /dev/cin null (read-tail-string /dev/cin span #false) quotation c:bad:eof)]
                    [(not (eq? ch #\))) (read-raw-string (unsafe-fx+ span 1) (unsafe-fx+ skip (char-utf-8-length ch)))]
                    [(not (equal? maybe-edelim (peek-string dsize skip /dev/cin))) (read-raw-string (unsafe-fx+ span 1) (unsafe-fx+ skip 1))]
                    [else (begin0 (read-tail-string /dev/cin span #false) (read-string! maybe-edelim /dev/cin))])))
          (c-make-bad-token srcloc (car maybe-edelim) struct:c:string
                            (append (string->list (symbol->immutable-string encoding-prefix))
                                    (cdr maybe-edelim)))))

    (cond [(string? maybe-string) (c-make-token srcloc c:string maybe-string encoding-prefix #false)]
          [(pair? maybe-string) (c-make-bad-token srcloc (car maybe-string) struct:c:string (cdr maybe-string))]
          [else maybe-string])))

(define c-consume-special-string-token : (-> C-Srcloc Symbol Char Boolean Boolean (U C:Identifier C:String C:Bad))
  (lambda [srcloc tag quotation cpp? raw?]
    (define encoding-tags : (Listof Symbol) (if (or (not cpp?) (eq? quotation #\')) c-encoding-tags cpp-encoding-tags))
    
    (cond [(not (memq tag encoding-tags)) (c-make-token srcloc c:identifier tag)]
          [else (let ([x.ceiling (cpp-encoding-tag->x.ceiling tag)])
                  (read-char (c-srcloc-in srcloc))
                  (cond [(not raw?) (c-consume-string-token srcloc quotation cpp? x.ceiling tag)]
                        [else (c-consume-cpp-raw-string-token srcloc quotation tag)]))])))

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
                          [(and (char? ch1) (eq? ch1 #\%) (read-char /dev/cin))
                           (define n% : Flonum (real->double-flonum (* n 0.01)))
                           (c-make-token srcloc c:percentage (string-append representation "%") signed? n%)]
                          [(flonum? n)
                           (cond [(zero? n) (c-make-token srcloc c:flzero representation signed? n)]
                                 [(fl= n 1.0) (c-make-token srcloc c:flone representation signed? n)]
                                 [else (c-make-token srcloc c:flonum representation signed? n)])]
                          [(zero? n) (c-make-token srcloc c:zero representation signed? n)]
                          [(= n 1) (c-make-token srcloc c:one representation signed? n)]
                          [else (c-make-token srcloc c:integer representation signed? n)])))])))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-consume-whitespace : (-> Input-Port Void)
  (lambda [/dev/cin]
    (regexp-match #px"\\s*" /dev/cin)
    (void)))
  
(define c-consume-identifier : (-> Input-Port (Option Char) (U String C-Bad-Datum))
  (lambda [/dev/cin ?leader]
    (let consume-id ([span : Nonnegative-Fixnum 0]
                     [skip : Nonnegative-Fixnum 0])
      (define ch : (U EOF Char) (peek-char /dev/cin skip))
      (cond [(eof-object? ch) (read-tail-string /dev/cin span ?leader)]
            [(c-identifier-char? ch) (consume-id (unsafe-fx+ span 1) (unsafe-fx+ skip (char-utf-8-length ch)))]
            [(not (eq? ch #\\)) (read-tail-string /dev/cin span ?leader)]
            [else (let consume-universal-id ([head : String (read-tail-string /dev/cin span ?leader)]
                                             [uspan : Nonnegative-Fixnum 0]
                                             [uskip : Nonnegative-Fixnum 0])
                    (define ch : (U EOF Char) (peek-char /dev/cin uskip))
                    (cond [(eof-object? ch) (read-tail-string /dev/cin uspan head)]
                          [(c-identifier-char? ch) (consume-universal-id head (unsafe-fx+ uspan 1) (unsafe-fx+ uskip (char-utf-8-length ch)))]
                          [(not (eq? ch #\\)) (read-tail-string /dev/cin uspan head)]
                          [else (let* ([head++ (read-tail-string /dev/cin uspan head)]
                                       [maybe-unicode (c-consume-universal-char /dev/cin null head++ #false 1)])
                                  (cond [(char? maybe-unicode) (consume-universal-id (string-append head++ (string maybe-unicode)) 0 0)]
                                        [else maybe-unicode]))]))]))))

(define c-consume-native-char-sequence : (-> Input-Port Char Nonnegative-Fixnum (Values (U String C-Bad-Datum) False))
  (lambda [/dev/cin quotation x.ceiling]
    (values
     (let consume-string ([span : Nonnegative-Fixnum 0]
                          [skip : Nonnegative-Fixnum 0])
       (define ch : (U EOF Char) (peek-char /dev/cin skip))
       (cond [(eof-object? ch) (cons c:bad:eof (string->list (read-tail-string /dev/cin span #false)))]
             [(eq? ch quotation) (begin0 (read-tail-string /dev/cin span #false) (read-char /dev/cin))]
             [(not (eq? ch #\\)) (consume-string (unsafe-fx+ span 1) (unsafe-fx+ skip (char-utf-8-length ch)))]
             [else (let consume-escaped-string ([head (read-tail-string /dev/cin span #false)]
                                                [espan : Nonnegative-Fixnum 0]
                                                [eskip : Nonnegative-Fixnum 0])
                     (define ch : (U EOF Char) (peek-char /dev/cin eskip))
                     (cond [(eof-object? ch) (cons c:bad:eof (string->list (read-tail-string /dev/cin espan head)))]
                           [(eq? ch quotation) (begin0 (read-tail-string /dev/cin espan head) (read-char /dev/cin))]
                           [(not (eq? ch #\\)) (consume-escaped-string head (unsafe-fx+ espan 1) (unsafe-fx+ eskip (char-utf-8-length ch)))]
                           [else (let* ([head++ (begin0 (read-tail-string /dev/cin espan head) (read-char /dev/cin))]
                                        [maybe-char (c-consume-escaped-char /dev/cin head++ quotation x.ceiling)])
                                   (cond [(char? maybe-char) (consume-escaped-string (string-append head++ (string maybe-char)) 0 0)]
                                         [(eof-object? maybe-char) head++]
                                         [else maybe-char]))]))]))
     #false)))

(define c-consume-string-suffix : (-> Input-Port (U String C-Bad-Datum) (Values (U String C-Bad-Datum) (Option Symbol)))
  (lambda [/dev/cin target-string]
    ; check for standard or user-defined suffix
    (cond [(not (c-identifier-char? (peek-char /dev/cin 0))) (values target-string #false)]
          [else (let ([maybe-suffix (c-consume-identifier /dev/cin #false)])
                  (cond [(string? maybe-suffix) (values target-string (string->symbol maybe-suffix))]
                        [(string? target-string) (values (cons c:bad:suffix (append (string->list target-string) (cdr maybe-suffix))) #false)]
                        [else (values (cons (car target-string) (append (cdr target-string) (cdr maybe-suffix))) #false)]))])))

(define cpp-consume-native-char-sequence : (-> Input-Port Char Nonnegative-Fixnum (Values (U String C-Bad-Datum) (Option Symbol)))
  (lambda [/dev/cin quotation x.ceiling]
    (define-values (maybe-string _) (c-consume-native-char-sequence /dev/cin quotation x.ceiling))
    (c-consume-string-suffix /dev/cin maybe-string)))

#;(define c-consume-number : (-> Input-Port Char (Values (U Flonum Integer) String))
  ;;; https://drafts.csswg.org/c-syntax/#consume-a-number
  (lambda [/dev/cin sign/digit]
    (let consume-number ([chars (list sign/digit)])
      (define ch : (U EOF Char) (peek-char /dev/cin))
      (cond [(and (char? ch)
                  (or (char-numeric? ch)
                      (eq? ch #\+) (eq? ch #\-)
                      (c-decimal-point? ch (peek-char /dev/cin 1))
                      (c-scientific-notation? ch (peek-char /dev/cin 1) (peek-char /dev/cin 2)))
                  (read-char /dev/cin))
             (consume-number (cons ch chars))]
            [else (let* ([representation : String (list->string (reverse chars))]
                         [?number : (Option Complex) (string->number representation)])
                    (cond [(exact-integer? ?number) (values ?number representation)]
                          [(flonum? ?number) (values ?number representation)]
                          [else (values +nan.0 representation)]))]))))

(define c-consume-escaped-char : (-> Input-Port (Option String) (Option Char) Nonnegative-Fixnum (U EOF Char C-Bad-Datum))
  (lambda [/dev/cin ?head ?quotation x.ceiling]
    (define next-ch : (U EOF Char) (read-char /dev/cin))
    (cond [(eof-object? next-ch) next-ch]
          [(hash-has-key? c-escape-sequences next-ch) (hash-ref c-escape-sequences next-ch)]
          [(eq? next-ch #\x)
           (let-values ([(unicode count) (peek-flexible-hexadecimal /dev/cin 0 0 0 #:ceiling (assert #xFFFFFFFFF fixnum?))])
             (cond [(= count 0) (c-consume-error-chars /dev/cin (list #\\ #\x) ?head ?quotation c:bad:char)]
                   [(and (<= unicode x.ceiling) (char-integer? unicode)) (drop-bytes /dev/cin count) (integer->char unicode)]
                   [else (c-consume-error-chars /dev/cin (list #\\ #\x) ?head ?quotation c:bad:range)]))]
          [(char-octdigit? next-ch)
           (let-values ([(unicode count) (peek-flexible-octadecimal /dev/cin 0 (char->octadecimal next-ch) #:ceiling #o777)])
             (cond [(byte? unicode) (drop-bytes /dev/cin count) (integer->char unicode)]
                   [else (c-consume-error-chars /dev/cin (list #\\) ?head ?quotation c:bad:range)]))]
          [(eq? next-ch #\u) (c-consume-universal-char /dev/cin 4 #false (list #\\ #\u) ?head ?quotation 0)]
          [(eq? next-ch #\U) (c-consume-universal-char /dev/cin 8 #false (list #\\ #\U) ?head ?quotation 0)]
          [else next-ch #| should warn "unknown escape sequence" for chars other than #\\, #\', #\", and #\? |#])))

(define c-consume-universal-char : (case-> [Input-Port (Listof Char) (Option String) (Option Char) (U One Zero) -> (U Char C-Bad-Datum)]
                                           [Input-Port Byte Boolean (Listof Char) (Option String) (Option Char) Index -> (U Char C-Bad-Datum)])
  (case-lambda
    [(/dev/cin leading-chars ?head ?quotation skip0)
     (let ([initial? (and (not ?quotation) (not (non-empty-string? ?head)))]
           [shortform-size (case (peek-char /dev/cin skip0) [(#\u) 4] [(#\U) 8] [else #false])])
       (cond [(not shortform-size) (c-consume-error-chars /dev/cin leading-chars ?head ?quotation c:bad:char)]
             [else (c-consume-universal-char /dev/cin shortform-size initial? leading-chars ?head ?quotation (+ skip0 1))]))]
    [(/dev/cin shortform-size initial? leading-chars ?head ?quotation skip0)
     (let-values ([(unicode count) (peek-unicode-from-hexadecimal /dev/cin skip0 0 0)])
       (cond [(not (= count shortform-size)) (c-consume-error-chars /dev/cin leading-chars ?head ?quotation c:bad:char)]
             [(not (c-identifier-universal-char? unicode)) (c-consume-error-chars /dev/cin leading-chars ?head ?quotation c:bad:range)]
             [(and initial? (c-identifier-non-initial-char? unicode)) (c-consume-error-chars /dev/cin leading-chars ?head ?quotation c:bad:range)]
             [else (drop-bytes /dev/cin (+ skip0 count)) unicode]))]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-identifier-char? : (-> (U EOF Char) Boolean : #:+ Char)
  (lambda [ch]
    (and (char? ch)
         (or (char-alphabetic? ch)
             (char-numeric? ch)
             (eq? #\_ ch)
             (c-identifier-implementation-defined-char? ch)))))

(define c-identifier-initial-char? : (-> (U EOF Char) Boolean : #:+ Char)
  (lambda [ch]
    (and (char? ch)
         (or (char-alphabetic? ch)
             (eq? #\_ ch)
             (c-identifier-implementation-defined-char? ch)))))

(define c-identifier-implementation-defined-char? : (-> Char Boolean)
  ; these chars as identifiers' prefixes are borrowed from CSS
  (lambda [ch]
    (or (eq? #\u00B7 ch)
        (char<=? #\u00C0 ch #\u00D6)
        (char<=? #\u00D8 ch #\u00F6)
        (char<=? #\u00F8 ch #\u037D)
        (char<=? #\u037F ch #\u1FFF)
        (eq? #\u200C ch)
        (eq? #\u200D ch)
        (eq? #\u203F ch)
        (eq? #\u2040 ch)
        (char<=? #\u2070 ch #\u218F)
        (char<=? #\u2C00 ch #\u2FEF)
        (char<=? #\u3001 ch #\uD7FF)
        (char<=? #\uF900 ch #\uFDCF)
        (char<=? #\uFDF0 ch #\uFFFD)
        (char>=? ch #\U10000))))

(define c-identifier-universal-char? : (-> Char Boolean)
  (lambda [ch]
    (or (eq? #\u00A8 ch)
        (eq? #\u00AA ch)
        (eq? #\u00AD ch)
        (eq? #\u00AF ch)
        (char<=? #\u00B2 ch #\u00B5)
        (char<=? #\u00B7 ch #\u00BA)
        (char<=? #\u00BC ch #\u00BE)
        (char<=? #\u00C0 ch #\u00D6)
        (char<=? #\u00D8 ch #\u00F6)
        (char<=? #\u00F8 ch #\u00FF)
        (char<=? #\u0100 ch #\u167F)
        (char<=? #\u1681 ch #\u180D)
        (char<=? #\u180F ch #\u1FFF)
        (char<=? #\u200B ch #\u200D)
        (char<=? #\u202A ch #\u202E)
        (char<=? #\u203F ch #\u2040)
        (eq? #\u2054 ch)
        (char<=? #\u2060 ch #\u206F)
        (char<=? #\u2070 ch #\u218F)
        (char<=? #\u2460 ch #\u24FF)
        (char<=? #\u2776 ch #\u2793)
        (char<=? #\u2E80 ch #\u2FFF)
        (char<=? #\u3004 ch #\u3007)
        (char<=? #\u3021 ch #\u302F)
        (char<=? #\u3031 ch #\uD7FF)
        (char<=? #\uF900 ch #\uFD3D)
        (char<=? #\uFD40 ch #\uFDCF)
        (char<=? #\uFDF0 ch #\uFE44)
        (char<=? #\uFE47 ch #\uFFFD)
        (char<=? #\U10000 ch #\U1FFFD)
        (char<=? #\U20000 ch #\U2FFFD)
        (char<=? #\U30000 ch #\U3FFFD)
        (char<=? #\U40000 ch #\U4FFFD)
        (char<=? #\U50000 ch #\U5FFFD)
        (char<=? #\U60000 ch #\U6FFFD)
        (char<=? #\U70000 ch #\U7FFFD)
        (char<=? #\U80000 ch #\U8FFFD)
        (char<=? #\U90000 ch #\U9FFFD)
        (char<=? #\UA0000 ch #\UAFFFD)
        (char<=? #\UB0000 ch #\UBFFFD)
        (char<=? #\UC0000 ch #\UCFFFD)
        (char<=? #\UD0000 ch #\UDFFFD)
        (char<=? #\UE0000 ch #\UEFFFD))))

(define c-identifier-non-initial-char? : (-> Char Boolean)
  (lambda [ch]
    (or (char<=? #\u0300 ch #\u036F)
        (char<=? #\u1DC0 ch #\u1DFF)
        (char<=? #\u20D0 ch #\u20FF)
        (char<=? #\uFE20 ch #\uFE2F))))

(define c-raw-string-nondelimiter-char? : (-> Char Boolean)
  (lambda [ch]
    (or (char-whitespace? ch)
        (eq? ch #\\)
        (eq? ch #\))
        (eq? ch #\())))

#;(define c-number-prefix? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.csswg.org/c-syntax/#starts-with-a-number
  (lambda [ch1 ch2 ch3]
    (or (and (char? ch1) (char<=? #\0 ch1 #\9))
        (and (char? ch1) (char? ch2)
             (eq? ch1 #\.) (char<=? #\0 ch2 #\9))
        (and (char? ch1) (char? ch2)
             (or (eq? ch1 #\+) (eq? ch1 #\-))
             (or (char<=? #\0 ch2 #\9)
                 (and (eq? ch2 #\.)
                      (char? ch3) (char<=? #\0 ch3 #\9)))))))

#;(define c-scientific-notation? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.csswg.org/c-syntax/#consume-a-number
  (lambda [ch1 ch2 ch3]
    (and (char? ch1) (char? ch2)
         (char-ci=? ch1 #\e)
         (or (char<=? #\0 ch2 #\9)
             (and (or (eq? ch2 #\+) (eq? ch2 #\-))
                  (char? ch3)
                  (char<=? #\0 ch3 #\9))))))

#;(define c-decimal-point? : (-> (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.csswg.org/c-syntax/#consume-a-number
  (lambda [ch1 ch2]
    (and (char? ch1) (char? ch2)
         (eq? ch1 #\.)
         (char<=? #\0 ch2 #\9))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-consume-error-chars : (-> Input-Port (Listof Char) (Option String) (Option Char) C-Bad-Constructor C-Bad-Datum)
  (lambda [/dev/cin tail-leading-chars ?head ?quotation c:bad:sub]
    (define head-chars : (Listof Char)
      (append (if (not ?quotation) null (list ?quotation))
              (if (not ?head) null (string->list ?head))
              tail-leading-chars))
    
    (define stop-char? : (-> Char Boolean)
      (if (not ?quotation)
          (λ [[ch : Char]] (or (char-whitespace? ch) (eq? ch #\;)))
          (λ [[ch : Char]] (eq? ch ?quotation))))
    
    (let consume-error ([span : Nonnegative-Fixnum 0]
                        [skip : Nonnegative-Fixnum 0])
      (define ch : (U EOF Char) (peek-char /dev/cin skip))
      (cond [(eof-object? ch) (cons c:bad:sub (append head-chars (string->list (read-tail-string /dev/cin span #false))))]
            [(stop-char? ch) (cons c:bad:sub (append head-chars (string->list (read-tail-string /dev/cin (unsafe-fx+ span (if ?quotation 1 0)) #false))))]
            [(not (eq? ch #\\)) (consume-error (unsafe-fx+ span 1) (unsafe-fx+ skip (char-utf-8-length ch)))]
            [else (let* ([skip++ (unsafe-fx+ skip 1)]
                         [nch (peek-char /dev/cin skip++)])
                    (consume-error (unsafe-fx+ span 2)
                                   (unsafe-fx+ skip++ (if (char? nch) (char-utf-8-length nch) 1))))]))))
