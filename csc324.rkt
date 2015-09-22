; 4: determine the length of a list
(define (len-lst lst)
  (if (empty? lst)
      0
      (+ 1 (len-lst(rest lst)))))


; 5: determine if given item appears in list
(define (contains lst x)
  (if (empty? lst)
      #f
      (or (equal? x (first lst)) (contains (rest lst) x))))


; 6: determine the number of duplicates in a list
(define (count-dupl lst)
  (if (empty? lst)
      0
      (if (contains (rest lst) (first lst)) 
          (+ 1 (count-dupl(rest lst)))
          (count-dupl(rest lst))
          )))


; 7: remove all duplicates from a list
(define (rem-dupl lst)
  (if (empty? lst)
      '()
      (if (contains (rest lst) (first lst))
          (rem-dupl (rest lst))
          (cons (first lst) (rem-dupl (rest lst))))))


; 8: given two lists, output the intersection and then the union
(define (intersection a b)
  (if (empty? a)
      '()
      (if (contains b (first a))
          (cons (first a) (intersection (rest a) b))
          (intersection (rest a) b))))

(define (union a b)
  (rem-dupl (append a b)))


;10: take list of lists and insert item at the front of every list
(define (append-first lst item)
  (if (empty? lst)
      '()
      (cons (cons item (first lst)) (append-first (rest lst) item))))

(define (append-first lst item)
  (map (lambda(x) (cons item x))
         lst))
  
  

; 11: find powerset of list set (all subsets)
(define (powerset lst)
  (if (empty? lst)
      '(())
      (let ([rst (powerset (rest lst))])
        (append (append-first rst (first lst))
                rst))))

; 12: give subsets of size k of list set


; 16a: determine the length of a list
(define (len-lst lst)
  (len-lst-helper lst 0))

(define (len-lst-helper lst acc)
  (if (empty? lst)
      acc
      (len-lst-helper (rest lst) (+ acc 1))))

; 16b: determine if a given item appears in a list
; already tail recursive?

; 16c: determine number of duplicates in list
(define (count-duplicates lst)
  (count-dupl lst 0))

(define (count-dupl lst acc)
  (if (empty? lst)
      acc
      (if (contains (rest lst) (first lst)) 
          (count-dupl (rest lst) (+ acc 1))
          (count-dupl (rest lst) acc )
          )))


; 17: take a predicate and lst, and return the number of items that satisfy the predicate 
(define (count-bool predicate lst)
  (if (empty? lst)
      0
      (+ (predicate (first lst)) (count-bool predicate (rest lst)))))


; 18a: #17 with filter
(define (count-bool predicate lst)
  (length (filter (lambda (x) (predicate x)) lst)))
 
; 19: take a list of unary functions and a value arg, and then return a list of the results of applying each function to arg
(define (apply-func-list func-lst arg)
  (map (lambda (x) (x arg)) func-lst))

; 20: is foldl tail-recursive?
; foldl is tail recursive: there is no work to be done after the foldl recursive call (in tail call position)

; 21a: implement map using foldl
(define (foldl-map func lst)
  (foldl (lambda (x acc)
           (cons (func x) acc))
         '()
         lst))
; 21b: implement reduce using foldl
(define (foldl-filter func lst)
  (foldl(lambda (x acc)
      (if (func x)
        (append acc (list x))
        acc))
    empty
    lst))