(use srfi-1)
(use posix)
(use csv)
(use csv-string)
(use fmt)

(define base "~/.kv/")

(define-values (format-cell format-record format-csv) (make-format))

(define-record entry key value)

(define (expand-store repo)
  (conc base repo))

(define (is-entry-of-key? key)
  (lambda (entry)
    (string=? key (entry-key entry))))

(define (list->entry-value value)
  (string-intersperse value " "))

(define (list->entry raw)
  (if (and (list? raw) (= 2 (length raw)))
    (make-entry (first raw) (second raw))
    #f))

(define (entry->record entry)
  (list->csv-record
    (list (entry-key entry) (entry-value entry))))

(define (store->csv store)
  (format-csv (map entry->record store)))

(define (entry->print store key)
  (if (equal? #f store)
    #f
    (let ((entry (read-key store key)))
      (if (entry? entry)
        (entry-value entry)
        #f))))

(define (store->print store)
  (if (equal? #f store)
    #f
    (foldr (lambda (entry accumulated)
             (if (string-null?  accumulated)
               (entry-key entry)
               (conc (entry-key entry) #\newline accumulated)))
           ""
           store)))

(define (list-files dir)
  (remove (lambda (x) (directory? (expand-store x)))
          (directory dir)))

(define (read-all-stores)
  (if (directory? (create-directory base))
    (list-files base)))

(define (read-store store)
  (let ((store (expand-store store)))
    (if (equal? #f (file-exists? store))
      #f
      (map list->entry
           (map csv-record->list
                ((csv-parser) (read-all store)))))))

(define (read-key store key)
  (find (is-entry-of-key? key) store))

(define (print-all-stores)
  (for-each print (read-all-stores)))

(define (delete-entry store key)
  (remove (is-entry-of-key? key) store))

(define (change-entry store key value)
  (append (delete-entry store key)
          (list (make-entry key value))))

(define (write-store store content)
  (call-with-output-file (expand-store store)
                         (lambda (output)
                           (fmt output (dsp (store->csv content))))))

(define (write-entry source target key value)
  (let ((source (if (equal? #f source)
                  (list)
                  source)))
    (write-store target
                 (change-entry source key (list->entry-value value)))))
