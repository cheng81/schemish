(load "stdlib.scm")

(define stream:end? (lambda (arg) (eq? arg 'you-fell-off-the-end)))
(define (check_stream_end el return)
  (if (stream:end? el)
    (begin (return 'you-fell-off-the-end) #f)
    #t))

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

(define (stream:take n stream)
  (if (= n 0)
    '()
    (begin
      (define el (stream))
      (if (stream:end? el)
        '()
        (cons el (stream:take (- n 1) stream))))))

(define (stream:take2 n stream)
  (define el (stream))
  (define (consume_stream _)
    (set! n (- n 1))
    (if (stream:end? el)
      '()
      el))
  (define (test_n _)
    (set! el (stream))
    (or (= n 0) (stream:end? el)))
  (unfold consume_stream (consume_stream '()) test_n))