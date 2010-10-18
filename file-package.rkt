#lang racket/base

(require racket/contract
         racket/file
         racket/match
         racket/path
         file/gzip
         file/gunzip)


;; pack-current-directory: -> bytes
;; Produces bytes representing the content of the current
;; directory.
(define (pack-current-directory)
  (let ([bytes-output-port (open-output-bytes)])
    (pack-current-directory-to-port bytes-output-port)
    (get-output-bytes bytes-output-port)))
    

;; pack-current-directory-to-port: output-port -> void
;; Writes out representation of current directory to output-port.
(define (pack-current-directory-to-port an-output-port)
  (let-values ([(ip op) (make-pipe)])
    (write  (for/list ([a-path (pathlist-closure 
                                (list (build-path 'same)))])
              (cond [(directory-exists? a-path)
                     (list (munge-path a-path))]
                    [(file-exists? a-path)
                     (list (munge-path a-path)
                           (file->bytes a-path))]
                    [else
                     (error 'pack-current-directory)]))
            op)
    (close-output-port op)
    (gzip-through-ports ip an-output-port #f (current-seconds))))


  
;; unpack-into-current-directory: bytes -> void
(define (unpack-into-current-directory directory-bytes 
                                       #:exists (exists-flag 'error))
  (unpack-port-into-current-directory (open-input-bytes directory-bytes)
                                      #:exists exists-flag))
  

;; unpack-port-into-current-directory: input-port -> void
(define (unpack-port-into-current-directory an-input-port
                                            #:exists (exists-flag 'error))
  (let-values ([(inp outp) (make-pipe)])
    (gunzip-through-ports an-input-port outp)
    (let ([s-exp (read inp)])
      (for ([elt s-exp])
        (match elt
          [(list a-munged-path)
           (make-directory* (unmunge-path a-munged-path))]
          [(list a-munged-path 
                 (and some-bytes (? bytes?)))
           (call-with-output-file (unmunge-path a-munged-path)
             (lambda (op)
               (write-bytes some-bytes op))
             #:exists exists-flag)])))))

  
  

;; munge-path: path -> (listof (or/c 'same string))
(define (munge-path a-path)
  (map (lambda (component)
         (cond
           [(eq? component 'same) 'same]
           [(eq? component 'up)
            (error 'munge-path)]
           [else
            (path->string component)]))
       (explode-path a-path)))

;; unmunge-path: (listof (or/c 'same string)) -> path
(define (unmunge-path x)
  (apply build-path x))


(provide/contract 
 [pack-current-directory (-> bytes?)]
 [pack-current-directory-to-port (output-port? . -> . any)]
 [unpack-into-current-directory (bytes? . -> . any)]
 [unpack-port-into-current-directory ((input-port?) 
                                      (#:exists (or/c 'error
                                                      'append
                                                      'update
                                                      'replace
                                                      'truncate
                                                      'truncate/replace)) 
                                      . ->* . any)])