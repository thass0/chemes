(define lang (string->list "scheme"))
(define name
  (list->string
   (append (cdr lang) (list (car lang)))))
name  ; chemes
