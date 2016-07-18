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

;; turns a list into a stream
(define (stream:of_list lst)
  (define (control-state return)
    (for-each
     (lambda (element)
               (set! return (call-with-current-continuation
                              (lambda (resume-here)
                               (set! control-state resume-here)
                               (return element)))))
     lst)
    (return 'you-fell-off-the-end))
  (define (generator)
    (call-with-current-continuation control-state))
  generator)

;; turns a list into an infinite stream by cycling through the values
(define (stream:cycle lst)
  (define (control-state return)
    (define (_loop _lst)
      (set! return (call-with-current-continuation
                    (lambda (resume-here)
                      (set! control-state resume-here)
                      (return (car _lst)))))
      (if (null? (cdr _lst))
        (_loop lst)
        (_loop (cdr _lst))))
    (_loop lst))
  (define (generator)
    (call-with-current-continuation control-state))
  generator)

;; infinite streams of integer
(define (stream:int)
  (define (control-state return)
    (define (_loop cur)
      (set! return (call-with-current-continuation
                      (lambda (resume-here)
                        (set! control-state resume-here)
                        (return cur))))
      (_loop (+ cur 1)))
    (_loop 0))
  (define (generator)
    (call-with-current-continuation control-state))
  generator)

;; given a predicate and a stream, returns the stream of items that pass the predicate
(define (stream:filter pred)
  (lambda (stream)
    (define (control-state return)
      (define (_loop)
        (define el (stream))
        (if (eq? el 'you-fell-off-the-end)
          (return 'you-fell-off-the-end)
          (if (pred el)
            (begin
              (set! return (call-with-current-continuation
                (lambda (resume-here)
                  (set! control-state resume-here)
                  (return el))))
              (_loop))
            (_loop))))
      (_loop))
    (define (generator)
      (call-with-current-continuation control-state))
    generator))

;; given a mapping function and a stream, returns the streams of mapped values
(define (stream:map fn)
  (lambda (stream)
    (define (control-state return)
      (define (_loop)
        (define el (stream))
        (if (eq? el 'you-fell-off-the-end)
          (return 'you-fell-off-the-end)
          (begin
            (define mel (fn el))
            (set! return (call-with-current-continuation
              (lambda (resume-here)
                (set! control-state resume-here)
                (return mel))))
            (_loop))))
      (_loop))
    (define (generator)
      (call-with-current-continuation control-state))
    generator))

;; given a list of streams, returns a stream of items
;; obtained by cycling through the given list of streams
(define (stream:round_robin streams)
  (define (control-state return)
    (define front (list))
    (define rear streams)
    (define (pick)
      (case (list (null? front) (null? rear))
        (((list #t #t))
          'you-fell-off-the-end)
        (((list #f #t))
          (set! rear (reverse front))
          (set! front (list))
          (pick))
        (else
          (define s (car rear))
          (set! rear (cdr rear))
          (define el (s))
          (if (eq? el 'you-fell-off-the-end)
            (pick)
            (begin
              (set! front (cons s front))
              el)))))
    (define (_loop)
      (set! return (call-with-current-continuation
        (lambda (resume-here)
          (set! control-state resume-here)
          (return (pick)))))
      (_loop))
    (_loop))
  (define (generator)
    (call-with-current-continuation control-state))
  generator)

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
