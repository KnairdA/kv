(use srfi-1)
(use posix)
(use csv)
(use csv-string)
(use fmt)

(define base "~/.kv/")

(define-values (format-cell format-record format-csv) (make-format))

(define (expand-store repo)
  (conc base repo))

(define (list-files dir)
  (remove (lambda (x) (directory? (expand-store x)))
          (directory dir)))

(define (read-store store)
  (map csv-record->list
       ((csv-parser) (read-all (expand-store store)))))

(define (read-key store key)
  (find (lambda (entry) (string=? key (car entry)))
        (read-store store)))

(define (print-store store)
  (print (read-store store)))

(define (print-key store key)
  (print (second (read-key store key))))

(define (print-all-stores)
  (if (directory? (create-directory base))
    (for-each print (list-files base))))

(define (write-store store content)
  (call-with-output-file (expand-store store)
                         (lambda (output)
                           (fmt output (dsp (format-csv (map list->csv-record content)))))))

(define (add-unique store key value)
  (append (filter
            (lambda (entry) (not (string=? key (car entry))))
            (read-store store))
          (list (list key value))))

(define (write-key store key value)
    (write-store store (add-unique store key value)))

(define (perform-operation arguments)
  (let ((count (length arguments)))
    (cond ((= 0 count) (print-all-stores))
          ((= 1 count) (print-store (first arguments)))
          ((= 2 count) (print-key   (first arguments) (second arguments)))
          (else        (write-key   (first arguments) (second arguments) (drop arguments 2))))))

(perform-operation (command-line-arguments))
