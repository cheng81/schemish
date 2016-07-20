;; we need the standard library
;;  - for-each
;;  - unfold
(load "stdlib.scm")

;; checks whether the given element signals the end of the stream
(define stream:end? (lambda (arg) (eq? arg 'you-fell-off-the-end)))
(define (check_stream_end el return)
  (if (stream:end? el)
    (begin (return 'you-fell-off-the-end) #f)
    #t))

;; custom generator function
(define (stream:generator func)
  (define (control return)
    (define (yield v)
      (set! return (callCC
          (lambda (next)
            (set! control next)
            (return v)))))
    (func yield)
    (return 'you-fell-off-the-end))
  (lambda ()
    (callCC control)))

;; call a function for each element in a stream
(define (stream:for-each func stream)
  (stream:generator (lambda (yield)
    (define (loop)
      (define el (stream))
      (if (stream:end? el) #u
        (begin
          (func yield el)
          (loop))))
    (loop))))

(define (stream:for-each-curried func)
  (lambda (stream)
    (stream:for-each func stream)))

;; turns a list into a stream
(define (stream:of_list lst)
  (stream:generator (lambda (yield)
    (for-each yield lst))))

;; turns a list into an infinite stream by cycling through the values
(define (stream:cycle lst)
  (define (loop yield)
    (for-each yield lst)
    (loop yield))
  (stream:generator loop))

;; infinite streams of integer
(define (stream:int)
  (stream:generator (lambda (yield)
    (define (loop n)
      (yield n)
      (loop (+ n 1)))
    (loop 0))))

;; given a predicate and a stream, returns the stream of items that pass the predicate
(define (stream:filter pred)
  (stream:for-each-curried (lambda (yield elm) (if (pred elm) (yield elm) #u))))

;; given a mapping function and a stream, returns the streams of mapped values
(define (stream:map fn)
  (stream:for-each-curried (lambda (yield elm) (yield (fn elm)))))

;; given a list of streams, returns a stream of items
;; obtained by cycling through the given list of streams
(define (stream:round_robin streams)
  (define (main yield)
    (define (loop lst saved)
      (case (list (null? lst) (null? saved))
        (((list #t #t)) #u)           ;; all streams ended, break from the loop
        (((list #t #f))               ;; cycle ended, loop again
          (loop (reverse saved) lst))
        (else                         ;; mid-iteration
          (define cur (car lst))
          (define el (cur))
          (if (stream:end? el)
            (loop (cdr lst) saved)    ;; stream ended, recur without it
            (begin                    ;; yield element, continue
              (yield el)
              (loop (cdr lst) (cons cur saved)))))))
    (loop streams (list)))
  (stream:generator main))


;; given a value n and a stream, extract n items into a list
(define (stream:take n stream)
  (if (= n 0)
    '()
    (begin
      (define el (stream))
      (if (stream:end? el)
        '()
        (cons el (stream:take (- n 1) stream))))))

;; same as take, but using the unfold standard lib function
(define (stream:take2 n stream)
  (define el (stream)) ;; set up first element
  (define (consume_stream _) ;; check if stream has ended, otherwise return the extracted element
    (set! n (- n 1))
    (if (stream:end? el)
      '()
      el))
  (define (test_n _) ;; end the unfold if the stream ended or we have enough values
    (set! el (stream))
    (or (= n 0) (stream:end? el)))
  (unfold consume_stream (consume_stream '()) test_n))

;(define (stream:of_list lst)
;  (define (control-state return)
;    (for-each
;     (lambda (element)
;               (set! return (call-with-current-continuation
;                              (lambda (resume-here)
;                               (set! control-state resume-here)
;                               (return element)))))
;     lst)
;    (return 'you-fell-off-the-end))
;  (define (generator)
;    (call-with-current-continuation control-state))
;  generator)

;(define (stream:cycle lst)
;  (define (control-state return)
;    (define (_loop _lst)
;      (set! return (call-with-current-continuation
;                    (lambda (resume-here)
;                      (set! control-state resume-here)
;                      (return (car _lst)))))
;      (if (null? (cdr _lst))
;        (_loop lst)
;        (_loop (cdr _lst))))
;    (_loop lst))
;  (define (generator)
;    (call-with-current-continuation control-state))
;  generator)

;(define (stream:int)
;  (define (control-state return)
;    (define (_loop cur)
;      (set! return (call-with-current-continuation
;                      (lambda (resume-here)
;                        (set! control-state resume-here)
;                        (return cur))))
;      (_loop (+ cur 1)))
;    (_loop 0))
;  (define (generator)
;    (call-with-current-continuation control-state))
;  generator)

;(define (stream:filter pred)
;  (lambda (stream)
;    (define (control-state return)
;      (define (_loop)
;        (define el (stream))
;        (if (eq? el 'you-fell-off-the-end)
;          (return 'you-fell-off-the-end)
;          (if (pred el)
;            (begin
;              (set! return (call-with-current-continuation
;                (lambda (resume-here)
;                  (set! control-state resume-here)
;                  (return el))))
;              (_loop))
;            (_loop))))
;      (_loop))
;    (define (generator)
;      (call-with-current-continuation control-state))
;    generator))

;(define (stream:map fn)
;  (lambda (stream)
;    (define (control-state return)
;      (define (_loop)
;        (define el (stream))
;        (if (eq? el 'you-fell-off-the-end)
;          (return 'you-fell-off-the-end)
;          (begin
;            (define mel (fn el))
;            (set! return (call-with-current-continuation
;              (lambda (resume-here)
;                (set! control-state resume-here)
;                (return mel))))
;            (_loop))))
;      (_loop))
;    (define (generator)
;      (call-with-current-continuation control-state))
;    generator))

;(define (stream:round_robin streams)
;  (define (control-state return)
;    (define front (list))
;    (define rear streams)
;    (define (pick)
;      (case (list (null? front) (null? rear))
;        (((list #t #t))
;          'you-fell-off-the-end)
;        (((list #f #t))
;          (set! rear (reverse front))
;          (set! front (list))
;          (pick))
;        (else
;          (define s (car rear))
;          (set! rear (cdr rear))
;          (define el (s))
;          (if (eq? el 'you-fell-off-the-end)
;            (pick)
;            (begin
;              (set! front (cons s front))
;              el)))))
;    (define (_loop)
;      (set! return (call-with-current-continuation
;        (lambda (resume-here)
;          (set! control-state resume-here)
;          (return (pick)))))
;      (_loop))
;    (_loop))
;  (define (generator)
;    (call-with-current-continuation control-state))
;  generator)
