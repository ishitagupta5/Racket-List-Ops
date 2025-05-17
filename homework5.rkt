#lang racket

(require racket/list)
{provide asum find-indices remove square}


(define (square lst)
  (map (lambda (x) (* x x)) lst)
)  

(define (remove n lst)
  (filter (lambda (x) (not (equal? x n))) lst)
)  


(define (asum lst)
  (foldr - 0 lst)
)  

(define (find-indices n lst)
  (define (helper xs idx)
    (cond
      [(empty? xs) '()]
      [(equal? (car xs) n)
       (cons idx (helper (cdr xs) (add1 idx)))]
      [else
       (helper (cdr xs) (add1 idx))]))
  (helper lst 0)
)

