(use format)

(define (file-in-base file)
  (conc base file))

(define (read-all-stores #!optional do-not-read)
  (map (lambda (file) (path->store (file-in-base file) do-not-read))
       (remove (lambda (x) (directory? (file-in-base x)))
               (directory base))))

(define (entry->pretty-print entry #!optional offset)
  (let ((key (entry-key entry)))
    (conc key
          (make-string (if offset
                         (- offset (string-length key))
                         1))
          ": "
          (entry-value entry))))

(define (maximum-key-length entries)
  (fold (lambda (entry maximum)
          (max maximum (string-length (entry-key entry))))
        0
        entries))

(define (entries->pretty-print entries)
  (if (null-list? entries)
    #f
    (let ((offset (+ 1 (maximum-key-length entries))))
      (format #f "~{~&~A~}" (map (lambda (entry)
                                   (entry->pretty-print entry offset))
                                 entries)))))

(define (merge-stores stores)
  (fold (lambda (store entries)
          (append (store-content store) entries))
        (list)
        stores))
