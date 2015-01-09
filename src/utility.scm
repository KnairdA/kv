(use format)

(define (file-in-base file)
  (conc base file))

(define (maximum-key-length entries)
  (fold (lambda (entry maximum)
          (max maximum (string-length (entry-key entry))))
        0
        entries))

(define (make-key-value-pretty-printer first-column second-column)
  (lambda (key value)
    (conc (make-string first-column)
          key
          (make-string (- second-column (+ first-column (string-length key))))
          value)))

(define (entry->pretty-print entry #!optional printer)
  ((if printer printer
               (make-key-value-pretty-printer 0
                                              (+ 2
                                                 (string-length (entry-key entry)))))
   (entry-key entry)
   (entry-value entry)))

(define (entries->pretty-print entries)
  (if (null-list? entries)
    #f
    (let ((printer (make-key-value-pretty-printer 0
                                                  (+ 2
                                                     (maximum-key-length entries)))))
      (format #f "~{~&~A~}" (map (lambda (entry)
                                   (entry->pretty-print entry printer))
                                 entries)))))

(define (read-all-stores #!optional do-not-read)
  (map (lambda (file) (path->store (file-in-base file) do-not-read))
       (remove (lambda (x) (directory? (file-in-base x)))
               (directory base))))

(define (merge-stores stores)
  (fold (lambda (store entries)
          (append (store-content store) entries))
        (list)
        stores))
