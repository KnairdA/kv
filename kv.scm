(include "src/store.scm")

(define (perform-operation arguments)
  (let ((count (length arguments)))
    (cond ((= 0 count) (print-all-stores))
          ((= 1 count) (print-store (read-store (first arguments))))
          ((= 2 count) (print-value (read-store (first arguments)) (second arguments)))
          (else        (write-entry (read-store (first arguments)) (first arguments) (second arguments) (drop arguments 2))))))

(perform-operation (command-line-arguments))
