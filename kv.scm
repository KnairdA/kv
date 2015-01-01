(include "src/store.scm")

(define base "~/.kv/")

(define (file-in-base file)
  (conc base file))

(define (read-all-stores)
  (map (lambda (file) (path->store (file-in-base file)))
       (remove (lambda (x) (directory? (file-in-base file)))
               (directory base))))

(define (perform-operation arguments)
  (let ((count (length arguments)))
    (cond ((= 0 count) (print (stores->print (read-all-stores base))))
          ((= 1 count) (print (store->print  (path->store (file-in-base (first arguments))))))
          ((= 2 count) (print (entry->print  (path->store (file-in-base (first arguments))) (second arguments))))
          (else        (write-entry          (path->store (file-in-base (first arguments))) (second arguments) (drop arguments 2))))))

(perform-operation (command-line-arguments))
