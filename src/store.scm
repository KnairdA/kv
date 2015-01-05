(use srfi-1)
(use posix)
(use csv)
(use csv-string)
(use fmt)
(use format)
(use filepath)

(define-values (format-cell format-record format-csv) (make-format))

(define-record entry key value)
(define-record store name path content)

;; store and entry printing

(define-record-printer (entry x output)
  (fmt output (dsp (entry-key x))))

(define-record-printer (store x output)
  (if (null-list? (store-content x))
    (fmt output (dsp #f))
    (format output "~{~&~A~}" (store-content x))))

(define (stores->print stores)
  (format #f "~{~&~A~}" (map store-name stores)))

;; entry conversion

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

;; store conversion and serialization

(define (path->store-content path)
  (if (equal? #f (file-exists? path))
    (list)
    (map list->entry
         (map csv-record->list
              ((csv-parser) (read-all path))))))

(define (path->store path #!optional do-not-read)
  (make-store
    (filepath:take-file-name path)
    path
    (if (equal? #f do-not-read)
      (path->store-content path)
      (list))))

(define (store->csv store)
  (format-csv (map entry->record (store-content store))))

(define (write-store store)
  (call-with-output-file (store-path store)
                         (lambda (output)
                           (fmt output (dsp (store->csv store))))))

;; entry list query and manipulation

(define (find-entry entries key)
  (find (is-entry-of-key? key) entries))

(define (remove-entry entries key)
  (remove (is-entry-of-key? key) entries))

(define (append-unique-entry entries entry)
  (append (remove-entry entries (entry-key entry))
          (list entry)))

;; high level interface to read, change and commit manipulations in one call

(define (read-value store key)
  (let ((entry (find-entry (store-content store) key)))
    (if (entry? entry)
      (entry-value entry)
      #f)))

(define (write-entry store key value)
  (write-store (make-store (store-name store)
                           (store-path store)
                           (append-unique-entry (store-content store)
                                                (make-entry key (list->entry-value value))))))

(define (delete-entry store key)
  (write-store (make-store (store-name store)
                           (store-path store)
                           (remove-entry (store-content store) key))))

(define (rename-entry store old-key new-key)
  (write-store (make-store (store-name store)
                           (store-path store)
                           (append-unique-entry (remove-entry (store-content store) old-key)
                                                (make-entry new-key (read-value store old-key))))))
