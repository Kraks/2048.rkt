#lang racket
;; Guannan Wei <kiss.kraks@gmail.com>

;; only for 4x4 matrix, need fix
(define transpose
  (lambda (a)
    (list (map first a)
          (map second a)
          (map third a)
          (map fourth a))))

(define mergeAux
  (lambda (y acc)
    (cond
      [(empty? acc) (cons y '())]
      [(and (eq? (last acc) y)
            (not (eq? y 0)))
       (append (reverse (cdr (reverse acc))) `(,(* y 2) 0))]
      [(not (eq? y 0)) (append acc `(,y))]
      [else acc])))
  
(define mergeLineLeft
  (lambda (xs)
    (let ([ys (filter-not zero? (foldl mergeAux '() xs))])
      (append ys (make-list (- (length xs) (length ys)) 0)))))

(define mergeLineRight
  (lambda (xs)
    (reverse (mergeLineLeft (reverse xs)))))

(define mergeLeft
  (lambda (a)
    (map mergeLineLeft a)))

(define mergeRight
  (lambda (a)
    (map mergeLineRight a)))

(define mergeUp
  (lambda (a)
    (transpose (mergeLeft (transpose a)))))

(define mergeDown
  (lambda (a)
    (transpose (mergeRight (transpose a)))))

(define traverse
  (lambda (a f)
    (ormap (lambda (line) (ormap f line))
           a)))

(define isWin
  (lambda (a)
    (traverse a (lambda (x) (eq? x 2048)))))

(define isFail
  (lambda (a)
    (and (andmap (lambda (line) (isLineFail line)) a)
         (andmap (lambda (line) (isLineFail line)) (transpose a)))))

(define isLineFail
  (lambda (line)
    (let ([ziped (zip line)])
      (andmap (lambda (p) (not (or (eq? (car p) (cdr p))
                              (zero? (car p))
                              (zero? (cdr p)))))
              ziped))))

(define zip
  (lambda (lst)
    (cond
      [(>= (length lst) 2)
       (cons (cons (car lst) (cadr lst))
             (zip (cdr lst)))]
      [else '()])))
