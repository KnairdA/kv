(use format)

(define (entry->pretty-print entry offset)
  (let ((key (entry-key entry)))
    (conc key
          (make-string (- offset (string-length key)))
          ": "
          (entry-value entry))))

(define (maximum-key-length entries)
  (fold (lambda (entry maximum)
          (max maximum (string-length (entry-key entry))))
        0
        entries))

(define (entries->pretty-print entries)
  (let ((offset (+ 1 (maximum-key-length entries))))
    (format #f "~{~&~A~}" (map (lambda (entry)
                                 (entry->pretty-print entry offset))
                               entries))))

(define (merge-stores stores)
  (fold (lambda (store entries)
          (append (store-content store) entries))
        (list)
        stores))
