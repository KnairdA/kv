(use srfi-1)
(use posix)
(use csv)
(use csv-string)
(use fmt)

(define base "~/.kv/")

(define-values (format-cell format-record format-csv) (make-format))

(define (expand-store repo)
  (conc base repo))

(define (valid-entry? entry)
  (if (list? entry)
    (= 2 (length entry))
    #f))

(define (key-of-entry entry)
  (car entry))

(define (value-of-entry entry)
  (if (valid-entry? entry)
    (car (cdr entry))
    "invalid entry"))

(define (is-entry-of-key? key)
  (lambda (entry)
    (string=? key (key-of-entry entry))))

(define (flatten-value value)
  (string-intersperse value " "))

(define (list-files dir)
  (remove (lambda (x) (directory? (expand-store x)))
          (directory dir)))

(define (read-store store)
  (let ((store (expand-store store)))
    (if (boolean? (file-exists? store))
      (list)
      (filter valid-entry?
              (map csv-record->list
                   ((csv-parser) (read-all store)))))))

(define (read-key store key)
  (find (is-entry-of-key? key)
        (read-store store)))

(define (print-store store)
  (for-each (lambda (entry) (print (key-of-entry entry)))
            (read-store store)))

(define (print-key store key)
  (print (value-of-entry (read-key store key))))

(define (print-all-stores)
  (if (directory? (create-directory base))
    (for-each print (list-files base))))

(define (format-store store)
  (format-csv (map list->csv-record store)))

(define (change-key-value store key value)
  (append (filter
            (is-entry-of-key? key)
            (read-store store))
          (list (list key value))))

(define (write-store store content)
  (call-with-output-file (expand-store store)
                         (lambda (output)
                           (fmt output (dsp (format-store content))))))

(define (write-key store key value)
    (write-store store (change-key-value store key (flatten-value value))))

(define (perform-operation arguments)
  (let ((count (length arguments)))
    (cond ((= 0 count) (print-all-stores))
          ((= 1 count) (print-store (first arguments)))
          ((= 2 count) (print-key   (first arguments) (second arguments)))
          (else        (write-key   (first arguments) (second arguments) (drop arguments 2))))))

(perform-operation (command-line-arguments))
