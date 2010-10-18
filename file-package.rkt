#lang racket/base

(require racket/contract
         racket/file
         racket/match
         file/gzip)


;; pack-current-directory: -> bytes
;; Produces a bytes representing the content of the current
;; directory.
(define (pack-current-directory)
  (let-values ([(ip op) (make-pipe)]
               [(bytes-output-port) (open-output-bytes)])
    (write  (for/list ([a-path (pathlist-closure 
                                (list (build-path 'same)))])
              (cond [(directory-exists? a-path)
                     (list a-path)]
                    [(file-exists? a-path)
                     (list a-path (file->bytes a-path))]
                    [else
                     (error 'pack-current-directory)]))
            op)
    (gzip-through-ports ip bytes-output-port #f (current-seconds))
    (get-output-bytes)))
    
  
;; unpack-into-current-directory: bytes -> void
(define (unpack-into-current-directory directory-bytes)
  (let ([s-exp 
         (read (open-input-bytes directory-bytes))])
    (for ([elt s-exp])
      (match elt
        [(list a-path)
         (make-directory a-path)]
        [(list a-path 
               (and some-bytes (? bytes?)))
         (call-with-output-file a-path
           (lambda (op)
             (write-bytes some-bytes op)))]))))
       
           


(provide/contract 
 [pack-current-directory (-> bytes?)]
 [unpack-into-current-directory (bytes? . -> . any)])