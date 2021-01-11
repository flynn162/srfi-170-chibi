;; Section 3.1

(define-record-type PosixError
  (%make-posix-error number name message)
  posix-error?
  (number posix-error-number)
  (name posix-error-name)
  (message posix-error-message)
  )

(define (posix-error errno)
  (define (cmp a b)
    (cond ((= a b) 0)
          ((< a b) -1)
          (#t 1)))

  (let* ((idx (vector-binary-search errno-values errno cmp))
         (name (if (not idx) 'EUNKNOWN (vector-ref errno-names idx)))
         )
    (%make-posix-error errno name (pa-strerror errno))
    ))

;; Helper functions for posix-error

(define (wrap-errno-procedure proc)
  (lambda (arg)
    (let ((result (proc arg)))
      (if (equal? #f result)
          (raise (posix-error (pa-errno)))
          result)
      )))

;; Section 3.2

;; Section 3.3

;; Section 3.5

(define current-directory cs:current-directory)

(define set-current-directory!
  (wrap-errno-procedure cs:change-directory))

(define pid cs:current-process-id)

(define (%nice delta)
  (let* ((return-value (pa-nice delta))
         (errno (pa-errno)))
    (if (= 0 errno) return-value
        (raise (posix-error errno))
        )))

(define nice
  (case-lambda
   (() (%nice 1))
   ((delta) (cond
             ((not (integer? delta)) (error "Delta must be an integer"))
             ((< delta -100) (%nice -100))
             ((> delta 100) (%nice 100))
             (#t (%nice delta)))
    )))

;; Section 3.6

(define (double-check-chibi-info proc getter:id getter:name)
  (lambda (uid/name)
    (let ((info (proc uid/name)))
      (cond
       ((and (integer? uid/name) (= (getter:id info) uid/name))
       info)
       ((and (string? uid/name) (string=? (getter:name info) uid/name))
        info)
       (#t #f)
       ))))

(define user-info
  (double-check-chibi-info cs:user-information cs:user-id cs:user-name))

(define user-info? cs:user?)
(define user-info:name cs:user-name)
(define user-info:uid cs:user-id)
(define user-info:gid cs:user-group-id)
(define user-info:home-dir cs:user-home)
(define user-info:shell cs:user-shell)

;;* TODO: user-info:full-name user-info:parsed-full-name

(define group-info
  (double-check-chibi-info cs:group-information cs:group-id cs:group-name))

(define group-info? cs:group?)
(define group-info:name cs:group-name)
(define group-info:gid cs:group-id)

;; Section 3.10

;; Section 3.11

(define get-environment-variables r7rs:get-environment-variables)
(define get-environment-variable r7rs:get-environment-variable)

(define (set-environment-variable! name value)
  (let ((result (pa-setenv name value)))
    (cond
     ((= result 0) #t)
     ((= result -1) (raise (posix-error (pa-errno))))
     ((= result -2) (error "Environment variable name"))
     ((= result -3) (error "Environment variable value"))
     (#t (error "Unknown error"))
     )))

(define (delete-environment-variable! name)
  (let ((result (pa-unsetenv name)))
    (cond
     ((= result 0) #t)
     ((= result -1) (raise (posix-error (pa-errno))))
     ((= result -2) (error "Parameter"))
     (#t (error "Unknown error"))
     )))

;; Section 3.12

(define (terminal? port)
  (cond
   ((port? port) (cs:is-a-tty? port))
   (#t (error "Expected a port"))
   ))
