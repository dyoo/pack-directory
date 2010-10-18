#lang racket/base

(require racket/contract
         racket/file
         racket/match
         racket/path
         file/gzip
         file/gunzip)


;; pack-current-directory: -> bytes
;; Produces a bytes representing the content of the current
;; directory.
(define (pack-current-directory)
  (let-values ([(ip op) (make-pipe)]
               [(bytes-output-port) (open-output-bytes)])
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
    (gzip-through-ports ip bytes-output-port #f (current-seconds))
    (get-output-bytes bytes-output-port)))
    
  
;; unpack-into-current-directory: bytes -> void
(define (unpack-into-current-directory directory-bytes)
  (let-values ([(inp outp) (make-pipe)])
    (gunzip-through-ports (open-input-bytes directory-bytes) outp)
    (let ([s-exp (read inp)])
      (for ([elt s-exp])
        (match elt
          [(list a-munged-path)
           (make-directory* (unmunge-path a-munged-path))]
          [(list a-munged-path 
                 (and some-bytes (? bytes?)))
           (call-with-output-file (unmunge-path a-munged-path)
             (lambda (op)
               (write-bytes some-bytes op)))])))))
  
           
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
 [unpack-into-current-directory (bytes? . -> . any)])