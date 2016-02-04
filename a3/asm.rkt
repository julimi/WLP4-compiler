#lang racket

;; scan is the main function provided, which uses the data definitions
;; and helper functions that follow. Sample tests are at the bottom of the file.
(define (scan str)
  (scan-func str asmtrlst 'start asmfinal))

;; scan-func: (listof char) trans-table symbol (listof symbol) -> (listof token)

(define (scan-func str trans start final)
  (scan-acc (string->list str) trans start final empty empty))

;; Next we specify the data definitions for tokens and the various components
;; of an FSM.

(define-struct token (kind lexeme) #:transparent)

;; A token is a (make-token k l), where k is a symbol
;;  and l is (union (list char) int).

(define-struct transition (state charset next) #:transparent)

;; A transition table is a list of transitions.
;; A transition is a (make-transition s cs ns), where s and ns are symbols,
;;  and cs is a function char->boolean that tests whether the transition applies.

;; The sample FSM provided is defined by (asmtrlst, 'start, asmfinal).
;; Definitions of asmtrlst and asmfinal follow.

;; functions used in defining sample transition table

(define (one-to-nine? ch)
  (and (char<=? #\1 ch) (char<=? ch #\9)))

(define (hex-digit? ch)
  (or
   (char-numeric? ch)
   (and (char<=? #\a ch) (char<=? ch #\f))
   (and (char<=? #\A ch) (char<=? ch #\F))))

(define (chartest ch)
  (lambda (x) (char=? x ch)))

;; sample transition table

(define asmtrlst
  (list
   (make-transition 'start char-whitespace? 'whitespace)
   (make-transition 'start char-alphabetic? 'id)
   (make-transition 'id char-alphabetic? 'id)
   (make-transition 'id char-numeric? 'id)
   (make-transition 'start one-to-nine? 'int)
   (make-transition 'int char-numeric? 'int)
   (make-transition 'start (chartest #\-) 'minus)
   (make-transition 'minus char-numeric? 'int)
   (make-transition 'start (chartest #\,) 'comma)
   (make-transition 'start (chartest #\() 'lparen)
   (make-transition 'start (chartest #\)) 'rparen)
   (make-transition 'start (chartest #\$) 'dollar)
   (make-transition 'dollar char-numeric? 'register)
   (make-transition 'register char-numeric? 'register)
   (make-transition 'start (chartest #\0) 'zero)
   (make-transition 'zero (chartest #\x) 'zerox)
   (make-transition 'zero char-numeric? 'int)
   (make-transition 'zerox hex-digit? 'hexint)
   (make-transition 'hexint hex-digit? 'hexint)
   (make-transition 'id (chartest #\:) 'label)
   (make-transition 'start (chartest #\;) 'comment)
   (make-transition 'comment (lambda (x) true) 'comment)
   (make-transition 'start (chartest #\.) 'dot)
   (make-transition 'dot (chartest #\w) 'dotw)
   (make-transition 'dotw (chartest #\o) 'dotwo)
   (make-transition 'dotwo (chartest #\r) 'dotwor)
   (make-transition 'dotwor (chartest #\d) 'dotword)
   ))

;; sample list of final states

(define asmfinal
  (list
    'register
    'int
    'id
    'label
    'comma
    'lparen
    'rparen
    'zero
    'hexint
    'comment
    'dotword
    'whitespace
    ))

;; scan-acc is the main workhorse of the lexer. It uses accumulative recursion
;; to run the FSM specified by (trans, state, final) on the list of characters cl.
;; acc accumulates the characters of the current token in reverse order, and
;; tacc accumulates the token list in reverse order.

;; scan-acc: (listof char) trans-table symbol (listof symbol) (listof char) (listof token) -> (listof token)

(define (scan-acc cl trans state final acc tacc)
  (cond
    [(empty? cl)
       (if (member state final)
           (if (or (symbol=? state 'whitespace) (symbol=? state 'comment))
               (reverse tacc)
               (reverse (cons (finalize-token state (reverse acc)) tacc)))
           (error 'ERROR "unexpected end of string\n"))]
    [else
      (let ([trl (memf (lambda (x) (found-trans? state (first cl) x)) trans)])
        (cond
          [(and (boolean? trl) (member state final))
             (if (symbol=? state 'whitespace)
                 (scan-acc cl trans 'start final empty tacc)
                 (scan-acc cl trans 'start final empty (cons (finalize-token state (reverse acc)) tacc)))]
          [(boolean? trl)
             (error 'ERROR "left to parse:~a ~a\n" state (list->string cl))]
          [(symbol=? state 'comment)
             (reverse tacc)]
          [else
             (scan-acc (rest cl) trans (transition-next (first trl)) final (cons (first cl) acc) tacc)]))]))

;; helper functions for scan-acc

(define (found-trans? state ch tr)
  (and (symbol=? state (transition-state tr))
       ((transition-charset tr) ch)))

;; finalize-token symbol (listof char) -> token
(define (finalize-token state l)
  (cond
    [(symbol=? state 'int) (make-token 'int (check-int-range (list->number l)))]
    [(symbol=? state 'zero) (make-token 'int 0)]
    [(symbol=? state 'hexint) (make-token 'hexint (check-hexint-range (list->hexint (rest (rest l)))))]
    [(symbol=? state 'register) (make-token 'register (check-reg-range (list->number (rest l))))]
    [else (make-token state l)]))

;; helper functions for finalize-token

(define (list->number lst) (string->number (list->string lst)))

(define (list->hexint lst) (string->number (list->string lst) 16))

;; Scheme supports unbounded integers but MIPS doesn't
(define (check-int-range n)
  (cond
    [(<= -2147483648 n 4294967295) n]
    [else (error 'ERROR "integer out of range: ~a" n)]))

(define (check-hexint-range n)
  (cond
    [(<= 0 n 4294967295) n]
    [else (error 'ERROR "integer out of range: ~a" n)]))

(define (check-reg-range n)
  (cond
    [(<= 0 n 31) n]
    [else (error 'ERROR "register out of range: ~a" n)]))

;; Some very basic tests
;(scan "01")
;(scan "0xabcd ; should be ignored")
;(scan ".word 01234")
;(scan "0add")
;(scan "foo:     add $1, $2, $3   ; A comment.")


; This file just uses scan to tokenize each line of the input
(define (scan-input)
  (define line (read-line))
  (cond
    [(eof-object? line) empty] ;(void)]
    [(> (string-length line) 0) ; Ignore blank lines
        ; Ignore comment-only lines as well
        ; When a comment-only line is scanned, an empty struct is returned
        (define scanned (scan line))
        (cond
          [(empty? scanned) (scan-input)]
          [else ;(printf "~a~n" scanned)
                ; make it a list
                (cons scanned (scan-input))])]
    [else (scan-input)]))

(struct label (name val) #:transparent)
(define lol '())
(define loc 0)

; helper func for label
(define (nlabel in bool ct)
  (cond
    [(equal? bool true) 
     (cond
       [(empty? in) empty]
       [(not (empty? (filter (lambda (lab) (equal? (label-name lab) (list->string (reverse (rest (reverse (token-lexeme (first in)))))))) lol)))
        (error 'ERROR "Oops! repeated use of labels!")]
       [else 
        (set! lol (cons (label (list->string (reverse (rest (reverse (token-lexeme (first in)))))) loc) lol))
        (cond
          [(empty? (rest in)) (nlabel (rest in) true ct)]
          [(equal? (token-kind (second in)) 'label) (nlabel (rest in) true 0)]
          [else (nlabel (rest in) false 1)])])]
    [else 
     (cond
       [(empty? in) empty]
       [(= ct 0) 
        (cond
          [(not (empty? (filter (lambda (lab) (equal? (token-kind lab) 'label)) in)))
           (error 'ERROR "Oops! label is not at the leftmost!")]
          [else in])]
       [else 
        (cond
          [(empty? (rest (rest in)))
           (set! loc (+ 4 loc))
           (nlabel in false 0)]
          [else in])])]))

; print lol
(define (printout lo)
  (cond
    [(empty? lo) (printf "")]
    [else 
     ;print it to standerr
     (fprintf (current-error-port) "~a ~a\n" (label-name (first lo)) (label-val (first lo)))
     (printout (rest lo))]))
; in: most outside list (whole)
(define (readforl in) 
    (cond
      [(empty? in) empty]
      [(equal? (token-kind (first (first in))) 'label)
       (cons (nlabel (first in) true 0) (readforl (rest in)))]
      [else 
       (set! loc (+ loc 4))
       (cons (nlabel (first in) false 0) (readforl (rest in)))]))

; in: each list in the list (a line)
(define (readin in)
    (cond
      [(empty? in) empty]
      [else 
       (cond
         [(equal? (token-kind (first in)) 'dotword) 
          (cond
            [(not (equal? (length in) 2)) (error 'ERROR "Oops! # of arguments for ,word is not 1.")]
            [(or (equal? (token-kind (second in)) 'hexint) (equal? (token-kind (second in)) 'int)) 
             ; integer below 2^32-1, bytes below 256^4-1, 8 bits
             (list->bytes (rest (cdddr (reverse (bytes->list (integer->integer-bytes (token-lexeme (second in)) 8 true))))))]
            [(equal? (token-kind (second in)) 'id) 
             (cond
               [(empty? (filter (lambda (ln) (equal? (list->string (token-lexeme (second in))) (label-name ln))) lol)) 
                (error 'ERROR "Oops! no such that label!")]
               [else (list->bytes (rest (cdddr (reverse 
                                                (bytes->list (integer->integer-bytes 
                                                                      (label-val (first 
                                                                                  (filter 
                                                                                   (lambda (ln) 
                                                                                     (equal? (list->string (token-lexeme (second in))) 
                                                                                             (label-name ln))) lol))) 8 true))))))])]
            [else (error 'ERROR "Oops! replace it with int, hexint or label.")])]
         [else (error 'ERROR "other instructions put here!")])]))

; output the bytes to standard output
(define (stdout in) 
  (cond
    [(empty? in) (printf "")]
    [else (write-bytes (first in))
          (stdout (rest in))]))

;(scan-input)

(stdout (map readin (filter (lambda (lst) (not (empty? lst))) (readforl (scan-input)))))
;(printout lol)