#lang racket
;; A text-based 2048 game
;; Guannan Wei <kiss.kraks@gmail.com>

(require racket/format)

(define transpose
  (lambda (a)
    (map (lambda (i) (map (lambda (line) (list-ref line i)) a)) 
         (range (length a)))))

(define mergeAux
  (lambda (y acc)
    (cond
      [(empty? acc) (cons y '())]
      [(eq? (last acc) y) (append (reverse (cdr (reverse acc))) `(,(* y 2) 0))]
      [else (append acc `(,y))])))

(define mergeLineLeft
  (lambda (xs)
    (let ([ys (filter-not zero? (foldl mergeAux '() (filter-not zero? xs)))])
      (append ys (make-list (- (length xs) (length ys)) 0)))))

(define mergeLineRight
  (lambda (xs) (reverse (mergeLineLeft (reverse xs)))))

(define mergeLeft
  (lambda (a) (map mergeLineLeft a)))

(define mergeRight
  (lambda (a) (map mergeLineRight a)))

(define mergeUp
  (lambda (a) (transpose (mergeLeft (transpose a)))))

(define mergeDown
  (lambda (a) (transpose (mergeRight (transpose a)))))

(define isWin
  (lambda (a)
    (ormap (lambda (line) (ormap (lambda (x) (eq? x 2048)) line)) a)))

(define isFail
  (lambda (a)
    (and (andmap (lambda (line) (isLineFail line)) a)
         (andmap (lambda (line) (isLineFail line)) (transpose a)))))

(define isLineFail
  (lambda (line)
    (let ([ziped (zip line)])
      (andmap (lambda (p)
                (not (or (eq? (car p) (cdr p))
                         (zero? (car p))
                         (zero? (cdr p))))) ziped))))

(define zip
  (lambda (lst)
    (if (>= (length lst) 2)
        (cons (cons (car lst) (cadr lst))
              (zip (cdr lst)))
        '())))

(define prettyPrint
  (lambda (a)
    (for-each (lambda (x)
                (cond [(list? x) (begin (prettyPrint x) (printf "\n"))]
                      [else (printf (~a x #:min-width 5 #:align 'right))])) a)))

(define newRandomNumber
  (lambda ()
    (let* ([seed '(2 2 2 2 4)]
           [pos (random (length seed))])
      (list-ref seed pos))))

(define setRandomNumber
  (lambda (a)
    (let* ([x (random (length a))]
           [y (random (length a))]
           [r (newRandomNumber)]
           [v (list-ref (list-ref a x) y)])
      (cond [(not (zero? v)) (setRandomNumber a)]
            [else (replace a x (replace (list-ref a x) y r))]))))

(define replace
  (lambda (lst pos val)
    (append (take lst pos) `(,val) (list-tail lst (+ pos 1)))))

(define newEmpty
  (lambda (size)
    (build-list size (lambda (x) (build-list size (lambda (y) 0))))))

(define newGame
  (lambda (a)
    (begin
      (prettyPrint a)
      (cond [(isFail a) (printf "You failed\n")]
            [(isWin a) (printf "You won\n")]
            [else (let* ([key (read)]
                         [b (match key
                              ['a (mergeLeft a)]
                              ['w (mergeUp a)]
                              ['s (mergeDown a)]
                              ['d (mergeRight a)]
                              ['q (begin (printf "quit\n") (exit))]
                              [_  (begin (printf "Invalid input\n") a)])])
                    (cond [(equal? a b) (newGame b)]
                          [else (newGame (setRandomNumber b))]))]))))

(define A (setRandomNumber (setRandomNumber (newEmpty 4))))

(newGame A)
