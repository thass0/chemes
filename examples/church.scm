(define (zero f)
  (lambda (x) x))

(define (succ n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(succ zero)
(succ (succ zero))
