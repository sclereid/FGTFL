#lang racket

(define (shows . s)
  (cond ((null? s)
         (newline))
        (else (display (car s))
              (apply shows (cdr s)))))

(define (id x) x)

(define-syntax cond+
  (syntax-rules (else)
    ((cond+) (cond))
    ((cond+ (else x ...))
     (begin x ...))
    ((cond+ (p k x ...) s ...)
     (let ((k p))
       (if k
           (begin x ...)
           (cond+ s ...))))))

(define operators-list
  `((* / %)
    (+ -)
    (== != > < >= <=)
    (&& ,(string->symbol "||"))))

(define operator-string-list
  (map (lambda (x) (list (symbol->string x) x))
       (flatten operators-list)))

(define (to-substrings str)
  (string-split
   (let ((brakets (string->list "()[]"))
         (syms (string->list "!@#$%^&*+-=:/~\\<>|")))
     (let loop ((left (string->list "  ")) (right (string->list str)))
       (if (null? right)
           (list->string (reverse left))
           (let ((x (car right)))
             (cond
               ((member x brakets)
                (loop (cons #\space (cons x (cons #\space left))) (cdr right)))
               ((member x syms)
                (if (member (car left) syms)
                    (loop (cons x left) (cdr right))
                    (loop (cons x (cons #\space left)) (cdr right))))
               (else
                (if (member (car left) syms)
                    (loop (cons x (cons #\space left)) (cdr right))
                    (loop (cons x left) (cdr right)))))))))))

(define (handle-words w)
  (cond+
    ((string->number w) n n)
    ((assoc w operator-string-list) pair (cadr pair))
    (else w)))

(define (map* f l)
  (cond
    ((null? l) l)
    ((pair? l)
     (cons (map* f (car l))
           (map* f (cdr l))))
    (else (f l))))

(define (handle-parenthesis ls)
  (car
   (let loop ((left '()) (right ls))
     (cond
       ((null? right)
        (cons (reverse left) '()))
       ((equal? (car right) ")")
        (cons (reverse left) (cdr right)))
       ((equal? (car right) "(")
        (let ((res (loop '() (cdr right))))
          (loop (cons (car res) left) (cdr res))))
       (else
        (loop (cons (car right) left) (cdr right)))))))

(define (split-at-first syms ls)
  (let loop ((left '()) (right ls))
    (if (null? right)
        #f
        (let ((x (car right)))
          (cond
            ((member x syms)
             (list x (reverse left) (cdr right)))
            (else
             (loop (cons x left) (cdr right))))))))

(define rev-operator-list (reverse operators-list))

(define (polish-style ls)
  (let loop ((l ls) (remaining-ops rev-operator-list))
    (cond ((or (not (pair? l)) (null? remaining-ops)) l)
          ((and (pair? l) (null? (cdr l))) (loop (car l) remaining-ops))
          (else
           (let* ((current-ops (car remaining-ops))
                  (s (split-at-first current-ops l)))
             (if s
                 (let ((p1 (car s))
                       (p2 (loop (cadr s) rev-operator-list))
                       (p3 (loop (caddr s) rev-operator-list)))
                   (if (null? p2)
                       (list p1 p3)
                       (list p1 p2 p3)))
                 (loop l (cdr remaining-ops))))))))

(define (lexical-parse s)
  (polish-style (map* handle-words (handle-parenthesis (to-substrings s)))))

(define operators-procedure
  (list (list '+ +) (list '- -) (list '* *) (list '/ /) (list '% remainder)
        (list '== (lambda s (if (apply = s) 1 0)))
        (list '!= (lambda s (if (apply = s) 0 1)))
        (list '>  (lambda s (if (apply > s) 1 0)))
        (list '<  (lambda s (if (apply < s) 1 0)))
        (list '>= (lambda s (if (apply >= s) 1 0)))
        (list '<= (lambda s (if (apply <= s) 1 0)))
        (list '&& (lambda (x y) (if (not (or (= x 0) (= y 0))) 1 0)))
        (list (string->symbol "||") (lambda (x y) (if (not (and (= x 0) (= y 0))) 1 0)))))

(define (calculate m)
  (cond
    ((number? m) m)
    ((not (pair? m))
     (shows m)
     (error "^unexpected expression"))
    (else
     (let* ((info (assq (car m) operators-procedure))
            (operator (cadr (cond (info info) (else (shows (car m)) (error "^unkonwn operator")))))
            (operands (map calculate (cdr m))))
       (apply operator operands)))))

