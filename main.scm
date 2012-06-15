#!/usr/local/bin/gosh

(define count 0)

(define (solve? num ls)
  (let loop ((ls ls)(n 1))
	(cond
	  ((null? ls)#t)
	  ((or (= (- num (car ls)) n)
		   (= (- (car ls) num) n))
	   #f)
	  (else (loop (cdr ls)(+ n 1))))))

(define (check? lst)
  (let loop ((lst lst))
	(cond
	  ((null? (cdr lst))#t)
	  ((solve? (car lst) (cdr lst))
	   (loop (cdr lst)))
	  (else #f))))

(define (queen lst)
  (let loop ((lst lst)(result '()))
	(if (null? lst)
	  (and (check? (reverse result))
		   (set! count (+ count 1))
		   (display (string-append (x->string count)"回目: "))
		   (print (reverse result)))
	  (for-each (lambda(x)(loop (delete x lst) (cons x result))) lst))))

(define (main args)
  (display ">> ")(flush)
  (let ((n (read)))
	(queen (iota n 0))))
