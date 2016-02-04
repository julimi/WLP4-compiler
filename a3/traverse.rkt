#lang racket

; input each pairs
(define (read-all-input)
(local ((define item (read)))
(if (eof-object? item) 
    empty
    (cons (cons item (cons (read) empty)) (read-all-input)))))

; structure for tree
(struct tree (val noc c))

; create a tree
(define (get-tree in tr)
  (local (
   ;(define val (first (first in)))
   ;(define noc (second (first in)))
   ; helper functions:
   ;(1) list all this node's children
   (define (l-c tr noc)
     (cond
       [(zero? noc) empty]
       [else (cons (first tr) (l-c (rest tr) (sub1 noc)))]))
   ;(2) remove the children we get form (1)
   (define (r-c tr noc)
     (cond
       [(zero? noc) tr]
       [else (r-c (rest tr) (sub1 noc))])))
  (cond
   [(empty? in) tr]
   [(zero? (second (first in)))
     (get-tree (rest in) (cons (tree (first (first in)) (second (first in)) empty) tr))]
   [else (get-tree (rest in) 
                   (cons (tree (first (first in)) (second (first in)) (l-c tr (second (first in))))
                         (r-c tr (second (first in)))))])))

; print the tree
(define (p-tree tr)
  (cond
    [(zero? (tree-noc tr)) (printf "~a ~a\n" (tree-val tr) (tree-noc tr))]
    [else 
      (map p-tree (tree-c tr))
      (printf "~a ~a\n" (tree-val tr) (tree-noc tr))]))

(p-tree (first (get-tree (reverse (read-all-input)) empty)))