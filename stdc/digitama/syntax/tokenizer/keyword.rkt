#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-c-keywords stx)
  (syntax-case stx [:]
    [(_ id : [kw ...])
     (with-syntax ([id-map (datum->syntax #'id (string->symbol (format "~a-map" (syntax-e #'id))))]
                   [(keyword ...) (for/list ([<kw> (in-list (syntax->list #'(kw ...)))])
                                         (datum->syntax <kw> (string->keyword (symbol->string (syntax-e <kw>)))))])
       (syntax/loc stx
         (define id-map : (HashTable Symbol Keyword)
           (make-hasheq (list (cons 'kw 'keyword) ...)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-c-keywords c-keyword :
  [auto extern short while break float signed case for sizeof char goto
        static const if struct continue inline switch default int typedef
        _Generic do long union _Imaginary double register unsigned _Noreturn
        else restrict void _Static_assert enum return volatile _Thread_local])

(define-c-keywords cpp-keyword :
  [alignas alignof asm auto bool break case catch char char8_t char16_t char32_t class concept const consteval constexpr
           constinit const_cast continue co_await co_return co_yield decltype default delete do double dynamic_cast else
           enum explicit export extern false float for friend goto if inline int long mutable namespace new noexcept nullptr
           operator private protected public register reinterpret_cast requires return short signed sizeof static
           static_assert static_cast struct switch template this thread_local throw true try typedef typeid typename
           union unsigned using virtual void volatile wchar_t while])

(define c-escape-sequences : (HashTable Any Char)
  (make-hasheq (list (cons #\n #\newline)
                     (cons #\t #\tab)
                     (cons #\v #\vtab)
                     (cons #\b #\backspace)
                     (cons #\r #\return)
                     (cons #\n #\newline)
                     (cons #\f #\page)
                     (cons #\a #\u07)
                     (cons #\0 #\null))))

; C17 doesn't mention 'u8' for char
(define c-encoding-tags : (Listof Symbol) (list 'u8 'u 'U 'L))
(define cpp-encoding-tags : (Listof Symbol) (list 'u8 'u 'U 'L 'R 'u8R 'uR 'UR 'LR))

(define cpp-encoding-tag->x.ceiling : (-> Symbol Nonnegative-Fixnum)
  (lambda [encoding-tag]
    (case encoding-tag
      [(u8 u8R) #xFF]
      [(U UR) (assert #xFFFFFFFF fixnum?)]
      [else #xFFFF])))
