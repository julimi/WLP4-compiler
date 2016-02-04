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
(define nloc 12)

; helper func for label
(define (nlabel in bool ct)
  (cond
    [(equal? bool true) 
     (cond
       [(empty? in) empty]
       [(not (empty? (filter (lambda (lab) (equal? (label-name lab) (list->string (reverse (rest (reverse (token-lexeme (first in)))))))) lol)))
        (error 'ERROR "Oops! repeated use of labels!")]
       [else 
        (set! lol (cons (label (list->string (reverse (rest (reverse (token-lexeme (first in)))))) nloc) lol))
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
          [(or (empty? (rest (rest in))) (empty? (cdddr (cdddr in))))
           (set! nloc (+ 4 nloc))
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

;list of id
;(define lstid '("jr" "jalr" "beq" "bne" "sub" "add" "slt" "sltu" "lis" "mflo" "mfhi" "sw" "lw"))

; decimal to binary:(updated)
(define (d->b n len)
  (local 
    ((define (change n)
       (cond 
       [(< n 2) (number->string n)]
       [else (string-append (change (quotient n 2)) (number->string (remainder n 2)))]))
     (define (add0 n len)
  (cond
    [(zero? len) n]
    [else
     (set! n (string-append "0" n))
     (add0 n (sub1 len))]))
     (define (addn1 n len)
       (change2 n (sub1 len)))
     ;negate
     (define (change2 n l)
       (local (
        (define nl (add1 l))
        (define (use1 n l r nl)
         (cond
          [(= r 0) n]
          [else
           (cond
            [(equal? "1" (substring n l r)) 
             (use1 (string-append (substring n 0 l) "0" (substring n r nl)) (sub1 l) (sub1 r) nl)]
            [else
             (use1 (string-append (substring n 0 l) "1" (substring n r nl)) (sub1 l) (sub1 r) nl)])]))
        (define (use2 n l r nl jin)
          (cond
            [(= r 0) n]
            [else
             (cond
               [(not jin) n]
               [else
                (cond
                  [(equal? "1" (substring n l r))
                   (use2 (string-append (substring n 0 l) "0" (substring n r nl)) (sub1 l) (sub1 r) nl true)]
                  [else
                   (use2 (string-append (substring n 0 l) "1" (substring n r nl)) (sub1 l) (sub1 r) nl false)])])])))
         (use2 (use1 n l (add1 l) nl) l (add1 l) nl true))))
  (cond
    [(= n -32768) "1000000000000000"]
    [(>= n 0) (add0 (change n) (- len (string-length (change n))))]
    [else
     (addn1 (add0 (change (abs n)) (- len (string-length (change (abs n))))) len)])))

(define (check-range16 n)
  (cond
    [(and (>= n -32768) (<= n 32767)) n]
    [else (error 'ERROR "Out-of-range for 16 bits!")]))
;(define (add0 n len)
;  (cond
;   [(zero? len) n]
;    [else
;     (set! n (string-append "0" n))
;     (add0 n (sub1 len))]))
;binary to decimal:
(define (b->d n)
  (local ((define c 1)
    (define (helper n c) 
      (cond
        [(zero? n) 0]
        [else (+ (* c (remainder n 10)) (helper (quotient n 10) (* c 2)))])))
    (helper n c)))
; in: most outside list (whole)
(define lenlst 0)
(define (readforl in) 
    (cond
      [(empty? in) 
       (set! lenlst nloc)
       (set! nloc 12)
       empty]
      [(equal? (token-kind (first (first in))) 'label)
       (cons (nlabel (first in) true 0) (readforl (rest in)))]
      [else 
       (set! nloc (+ nloc 4))
       (cons (nlabel (first in) false 0) (readforl (rest in)))]))

;helper for changing into bytes
(define (toBytes in)
  (list->bytes (rest (cdddr 
                      (reverse 
                       (bytes->list 
                        (integer->integer-bytes in 8 true)))))))

; relocation
(define rel '())

(define loc 12)

(define (frel in)
  (cond
    [(empty? in) empty]
    [else
     (cond
       [(and (equal? (token-kind (first in)) 'dotword)
             (equal? (token-kind (second in)) 'id))
        (set! rel (cons nloc rel))
        (set! nloc (+ 4 nloc))
        in]
       [else (set! nloc (+ 4 nloc))
             in])]))

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
             (set! loc (+ loc 4))
             (list->bytes (rest (cdddr (reverse (bytes->list (integer->integer-bytes (token-lexeme (second in)) 8 true))))))]
            [(equal? (token-kind (second in)) 'id) 
             (cond
               [(empty? (filter (lambda (ln) (equal? (list->string (token-lexeme (second in))) (label-name ln))) lol)) 
                (error 'ERROR "Oops! no such that label!")]
               [else 
                (set! loc (+ loc 4))
                (list->bytes (rest (cdddr (reverse 
                                                (bytes->list (integer->integer-bytes 
                                                                      (label-val (first 
                                                                                  (filter 
                                                                                   (lambda (ln) 
                                                                                     (equal? (list->string (token-lexeme (second in))) 
                                                                                             (label-name ln))) lol))) 8 true))))))])]
            [else (error 'ERROR "Oops! replace it with int, hexint or label.")])]
         [(equal? (token-kind (first in)) 'id)
          (cond
            ; jr and jalr 
            [(or (equal? (list->string (token-lexeme (first in))) "jr")
                 (equal? (list->string (token-lexeme (first in))) "jalr"))
             (cond
               [(not (equal? (length in) 2)) (error 'ERROR "Oops! # of arguments is not 1!")]
               [(not (equal? (token-kind (second in)) 'register)) (error 'ERROR "Oops! argument is not valid!")]
               [else 
                (cond
                  [(equal? (list->string (token-lexeme (first in))) "jr")
                   (set! loc (+ loc 4))
                   (toBytes (b->d (string->number (string-append "000000" 
                                           (d->b (token-lexeme (second in)) 5) "000000000000000001000"))))]
                  [else 
                   (set! loc (+ loc 4))
                   (toBytes (b->d (string->number (string-append "000000" 
                                           (d->b (token-lexeme (second in)) 5) "000000000000000001001"))))])])]
            
            ; add, sub, slt and sltu
            [(or (equal? (list->string (token-lexeme (first in))) "add")
                 (equal? (list->string (token-lexeme (first in))) "sub")
                 (equal? (list->string (token-lexeme (first in))) "slt")
                 (equal? (list->string (token-lexeme (first in))) "sltu")) 
             (cond
               [(not (equal? (length in) 6)) (error 'ERROR "Oops! # of arguments is not 5!")]
               [(not (and (equal? (token-kind (second in)) 'register) (equal? (token-kind (third in)) 'comma)
                          (equal? (token-kind (list-ref in 3)) 'register) (equal? (token-kind (list-ref in 4)) 'comma)
                          (equal? (token-kind (list-ref in 5)) 'register))) 
                (error 'ERROR "Oops! arguments are not valid!")]
               [else
                (cond
                  [(equal? (list->string (token-lexeme (first in))) "add")
                   (set! loc (+ loc 4))
                   (toBytes (b->d (string->number (string-append "000000" 
                                           (d->b (token-lexeme (list-ref in 3)) 5)
                                           (d->b (token-lexeme (list-ref in 5)) 5) 
                                           (d->b (token-lexeme (second in)) 5) "00000100000"))))]
                  [(equal? (list->string (token-lexeme (first in))) "sub")
                   (set! loc (+ loc 4))
                   (toBytes (b->d (string->number (string-append "000000" 
                                           (d->b (token-lexeme (list-ref in 3)) 5)
                                           (d->b (token-lexeme (list-ref in 5)) 5) 
                                           (d->b (token-lexeme (second in)) 5) "00000100010"))))]
                  [(equal? (list->string (token-lexeme (first in))) "slt")
                   (set! loc (+ loc 4))
                   (toBytes (b->d (string->number (string-append "000000" 
                                           (d->b (token-lexeme (list-ref in 3)) 5)
                                           (d->b (token-lexeme (list-ref in 5)) 5) 
                                           (d->b (token-lexeme (second in)) 5) "00000101010"))))]
                  [else
                   (set! loc (+ loc 4))
                   (toBytes (b->d (string->number (string-append "000000" 
                                           (d->b (token-lexeme (list-ref in 3)) 5)
                                           (d->b (token-lexeme (list-ref in 5)) 5) 
                                           (d->b (token-lexeme (second in)) 5) "00000101011"))))])])]
            ; beq and bne
            [(or (equal? (list->string (token-lexeme (first in))) "beq")
                 (equal? (list->string (token-lexeme (first in))) "bne")) 
             (cond
               [(not (equal? (length in) 6)) (error 'ERROR "Oops! # of arguments is not 5!")]
               [(not (and (equal? (token-kind (second in)) 'register) (equal? (token-kind (third in)) 'comma)
                          (equal? (token-kind (list-ref in 3)) 'register) (equal? (token-kind (list-ref in 4)) 'comma)
                          (or (equal? (token-kind (list-ref in 5)) 'id)
                              (equal? (token-kind (list-ref in 5)) 'int)
                              (equal? (token-kind (list-ref in 5)) 'hexint)))) 
                (error 'ERROR "Oops! arguments are not valid!")]
               [(and (equal? (token-kind (list-ref in 5)) 'id)
                     (empty? (filter (lambda (lb) (equal? (list->string (token-lexeme (list-ref in 5))) (label-name lb))) lol)))
                (error 'ERROR "Oops! no this label!")]
               [(or 
                 (and (equal? (token-kind (list-ref in 5)) 'int)
                 (or  (< (token-lexeme (list-ref in 5)) -32768)
                      (> (token-lexeme (list-ref in 5)) 32767))) 
                 (and (equal? (token-kind (list-ref in 5)) 'hexint)
                 (or  (> (token-lexeme (list-ref in 5)) 65535)
                      (< (token-lexeme (list-ref in 5)) 0))))
                (error 'ERROR "Oops! offset out-of-range!")]
               [else
                (cond
                  [(equal? (list->string (token-lexeme (first in))) "beq")
                   (set! loc (+ loc 4))
                   (cond
                    [(equal? (token-kind (list-ref in 5)) 'id)
                     (toBytes (b->d (string->number (string-append "000100" 
                                            (d->b (token-lexeme (second in)) 5)
                                            (d->b (token-lexeme (list-ref in 3)) 5)
                                            (d->b (check-range16 (/ (- (label-val (first 
                                                   (filter (lambda (lb) (equal? (list->string (token-lexeme (list-ref in 5))) (label-name lb))) lol))) loc) 4)) 16)))))]
                    [else
                     (toBytes (b->d (string->number (string-append "000100" 
                                            (d->b (token-lexeme (second in)) 5)
                                            (d->b (token-lexeme (list-ref in 3)) 5)
                                            (d->b (token-lexeme (list-ref in 5)) 16)))))])]
                  [else
                   (set! loc (+ loc 4))
                   (cond
                    [(equal? (token-kind (list-ref in 5)) 'id)
                     (toBytes (b->d (string->number (string-append "000101" 
                                            (d->b (token-lexeme (second in)) 5)
                                            (d->b (token-lexeme (list-ref in 3)) 5)
                                            (d->b (check-range16 (/ (- (label-val (first 
                                                   (filter (lambda (lb) (equal? (list->string (token-lexeme (list-ref in 5))) (label-name lb))) lol))) loc) 4)) 16)))))]
                    [else
                     (toBytes (b->d (string->number (string-append "000101" 
                                            (d->b (token-lexeme (second in)) 5)
                                            (d->b (token-lexeme (list-ref in 3)) 5)
                                            (d->b (token-lexeme (list-ref in 5)) 16)))))])])])]
            
            ; lis, mflo and mfhi
            [(or (equal? (list->string (token-lexeme (first in))) "lis")
                 (equal? (list->string (token-lexeme (first in))) "mflo")
                 (equal? (list->string (token-lexeme (first in))) "mfhi"))
             (cond
               [(not (equal? (length in) 2)) (error 'ERROR "Oops! # of arguments is not 1!")]
               [(not (equal? (token-kind (second in)) 'register)) (error 'ERROR "Oops! argument is not valid!")]
               [else 
                (cond
                  [(equal? (list->string (token-lexeme (first in))) "lis")
                   (set! loc (+ loc 4))
                   (toBytes (b->d (string->number (string-append "0000000000000000" 
                                           (d->b (token-lexeme (second in)) 5) "00000010100"))))]
                  [(equal? (list->string (token-lexeme (first in))) "mflo")
                   (set! loc (+ loc 4))
                   (toBytes (b->d (string->number (string-append "0000000000000000" 
                                           (d->b (token-lexeme (second in)) 5) "00000010010"))))]
                  [else
                   (set! loc (+ loc 4))
                   (toBytes (b->d (string->number (string-append "0000000000000000" 
                                           (d->b (token-lexeme (second in)) 5) "00000010000"))))])])]
            
            ; mult, multu, div and divu
            [(or (equal? (list->string (token-lexeme (first in))) "mult")
                 (equal? (list->string (token-lexeme (first in))) "multu")
                 (equal? (list->string (token-lexeme (first in))) "div")
                 (equal? (list->string (token-lexeme (first in))) "divu")) 
             (cond
               [(not (equal? (length in) 4)) (error 'ERROR "Oops! # of arguments is not 3!")]
               [(not (and (equal? (token-kind (second in)) 'register) (equal? (token-kind (third in)) 'comma)
                          (equal? (token-kind (list-ref in 3)) 'register))) 
                (error 'ERROR "Oops! arguments are not valid!")]
               [else
                (cond
                  [(equal? (list->string (token-lexeme (first in))) "mult")
                   (set! loc (+ loc 4))
                   (toBytes (b->d (string->number (string-append "000000" 
                                           (d->b (token-lexeme (second in)) 5)
                                           (d->b (token-lexeme (list-ref in 3)) 5) "0000000000011000"))))]
                  [(equal? (list->string (token-lexeme (first in))) "multu")
                   (set! loc (+ loc 4))
                   (toBytes (b->d (string->number (string-append "000000" 
                                           (d->b (token-lexeme (second in)) 5)
                                           (d->b (token-lexeme (list-ref in 3)) 5) "0000000000011001"))))]
                  [(equal? (list->string (token-lexeme (first in))) "div")
                   (set! loc (+ loc 4))
                   (toBytes (b->d (string->number (string-append "000000" 
                                           (d->b (token-lexeme (second in)) 5)
                                           (d->b (token-lexeme (list-ref in 3)) 5) "0000000000011010"))))]
                  [else
                   (set! loc (+ loc 4))
                   (toBytes (b->d (string->number (string-append "000000" 
                                           (d->b (token-lexeme (second in)) 5)
                                           (d->b (token-lexeme (list-ref in 3)) 5) "0000000000011011"))))])])]
            
            ; sw and lw
            [(or (equal? (list->string (token-lexeme (first in))) "sw")
                 (equal? (list->string (token-lexeme (first in))) "lw")) 
             (cond
               [(not (equal? (length in) 7)) (error 'ERROR "Oops! # of arguments is not 6!")]
               [(not (and (equal? (token-kind (second in)) 'register) (equal? (token-kind (third in)) 'comma)
                          (equal? (token-kind (list-ref in 5)) 'register) (equal? (token-kind (list-ref in 4)) 'lparen)
                          (equal? (token-kind (list-ref in 6)) 'rparen)
                          (or (equal? (token-kind (list-ref in 3)) 'int)
                              (equal? (token-kind (list-ref in 3)) 'hexint)))) 
                (error 'ERROR "Oops! arguments are not valid!")]
               [(or 
                 (and (equal? (token-kind (list-ref in 3)) 'int)
                 (or (< (token-lexeme (list-ref in 3)) -32768)
                     (> (token-lexeme (list-ref in 3)) 32767))) 
                 (and (equal? (token-kind (list-ref in 3)) 'hexint)
                 (or (> (token-lexeme (list-ref in 3)) 65535)
                     (< (token-lexeme (list-ref in 3)) 0))))
                (error 'ERROR "Oops! offset out-of-range!")]
               [else
                (cond
                  [(equal? (list->string (token-lexeme (first in))) "sw")
                   (set! loc (+ loc 4))
                   (toBytes (b->d (string->number (string-append "101011" 
                                          (d->b (token-lexeme (list-ref in 5)) 5)
                                          (d->b (token-lexeme (second in)) 5)
                                          (d->b (token-lexeme (list-ref in 3)) 16)))))]
                  [else
                   (set! loc (+ loc 4))
                   (toBytes (b->d (string->number (string-append "100011" 
                                          (d->b (token-lexeme (list-ref in 5)) 5)
                                          (d->b (token-lexeme (second in)) 5)
                                          (d->b (token-lexeme (list-ref in 3)) 16)))))])])]
            [else (error 'ERROR "no such that instruction!")])]
         [else (error 'ERROR "invalid instruction!")])]))

; output the bytes to standard output
(define (stdout in) 
  (cond
    [(empty? in) (printf "")]
    [else (write-bytes (first in))
          (stdout (rest in))]))

; output the bytes to note
(define (onote lst)
  (cond
    [(empty? lst) (printf "")]
    [else (write-bytes (toBytes 1))
          (write-bytes (toBytes (first lst)))
          (onote (rest lst))]))
;(scan-input)
(define (fout in)
  (write-bytes (toBytes 268435458))
  (write-bytes (toBytes (+ lenlst (* 8 (length rel)))))
  (write-bytes (toBytes lenlst))
  (stdout in)
  (onote rel)
  )

;(stdout (map readin (filter (lambda (lst) (not (empty? lst))) (readforl (scan-input)))))
(fout (map readin (map frel (filter (lambda (lst) (not (empty? lst))) (readforl (scan-input))))))
;(fout (map readin (map frel (readforl (scan-input)))))
;(printout lol)