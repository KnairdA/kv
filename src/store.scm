(use srfi-1)
(use posix)
(use csv)
(use csv-string)
(use fmt)
(use filepath)

(define-values (format-cell format-record format-csv) (make-format))

(define-record entry key value)
(define-record store name path content)

;; entry functions

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

(define (entry->print store key)
  (if (null-list? (store-content store))
    #f
    (let ((entry (read-key store key)))
      (if (entry? entry)
        (entry-value entry)
        #f))))

;; store functions

(define (path->store-content path)
  (if (equal? #f (file-exists? path))
    (list)
    (map list->entry
         (map csv-record->list
              ((csv-parser) (read-all path))))))

(define (path->store path)
  (make-store
    (filepath:take-file-name path)
    path
    (path->store-content path)))

(define (stores->print stores)
  (foldr (lambda (store accumulated)
           (if (string-null? accumulated)
             (store-name store)
             (conc (store-name store) #\newline accumulated)))
           ""
           stores))

(define (store->print store)
  (if (null-list? (store-content store))
    #f
    (foldr (lambda (entry accumulated)
             (if (string-null? accumulated)
               (entry-key entry)
               (conc (entry-key entry) #\newline accumulated)))
           ""
           (store-content store))))

(define (store->csv store)
  (format-csv (map entry->record (store-content store))))

(define (write-store store)
  (call-with-output-file (store-path store)
                         (lambda (output)
                           (fmt output (dsp (store->csv store))))))

;; store entry access and manipulation

(define (read-key store key)
  (find (is-entry-of-key? key) (store-content store)))

(define (delete-entry store key)
  (remove (is-entry-of-key? key) (store-content store)))

(define (change-entry store key value)
  (append (delete-entry store key)
          (list (make-entry key value))))

(define (write-entry store key value)
  (write-store (make-store (store-name store)
                           (store-path store)
                           (change-entry store key (list->entry-value value)))))
