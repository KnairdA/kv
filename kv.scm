(include "src/store.scm")
(include "src/utility.scm")

(define base "~/.kv/")

(define-record documentation arguments description)
(define-record command       name implementation documentation)

(define (perform-help arguments)
  (let ((printer (make-key-value-pretty-printer 0 30)))
    (print (format #f "~{~&~A~}"
                   (map (lambda (command)
                          (let ((documentation (command-documentation command)))
                            (printer (documentation-arguments   documentation)
                                     (documentation-description documentation))))
                        commands)))))

(define (perform-show arguments)
  (let ((count (length arguments)))
    (cond ((= 0 count) (print (stores->print        (read-all-stores #t))))
          ((= 1 count) (print                       (path->store (file-in-base (first arguments)))))
          ((= 2 count) (print (read-entry-returning (path->store (file-in-base (first arguments))) (second arguments) entry-value)))
          (else        (print "show: storage and key required at most")))))

(define (perform-all arguments)
  (let ((count (length arguments)))
    (cond ((= 0 count) (print (entries->pretty-print (merge-stores  (read-all-stores)))))
          ((= 1 count) (print (entries->pretty-print (store-content (path->store (file-in-base (first arguments)))))))
          ((= 2 count) (print (read-entry-returning                 (path->store (file-in-base (first arguments))) (second arguments) entry->pretty-print)))
          (else        (print "all: storage and key required at most")))))

(define (perform-write arguments)
  (if (>= (length arguments) 3)
    (update-store-using (path->store (file-in-base (first arguments)))
                        (write-entry (second arguments) (drop arguments 2)))
    (print "write: storage, key and value required")))

(define (perform-delete arguments)
  (if (= (length arguments) 2)
    (update-store-using (path->store (file-in-base (first arguments)))
                        (delete-entry (second arguments)))
    (print "delete: storage and key required")))

(define (perform-rename arguments)
  (if (= (length arguments) 3)
    (update-store-using (path->store (file-in-base (first arguments)))
                        (rename-entry (second arguments) (third arguments)))
    (print "rename: storage, old-key and new-key required")))

(define commands
  (list (make-command "help"
                      perform-help
                      (make-documentation "help"
                                          "displays this message"))
        (make-command "show"
                      perform-show
                      (make-documentation "show [STORE [KEY]]"
                                          "prints stores, keys of stores or values of keys"))
        (make-command "all"
                      perform-all
                      (make-documentation "all [STORE [KEY]]"
                                          "prints key value pairs of every store, a single store or a single key"))
        (make-command "write"
                      perform-write
                      (make-documentation "write STORE KEY VALUE"
                                          "writes VALUE to KEY in STORE"))
        (make-command "delete"
                      perform-delete
                      (make-documentation "delete STORE KEY"
                                          "deletes KEY in STORE"))
        (make-command "rename"
                      perform-rename
                      (make-documentation "rename STORE OLD-KEY NEW-KEY"
                                          "renames OLD-KEY of STORE to NEW-KEY"))))

(define (name->command-implementation name)
  (let ((command (find (lambda (command)
                         (string=? name (command-name command)))
                       commands)))
    (if (equal? #f command)
      (lambda (arguments)
        ((name->command-implementation "show") (append (list name) arguments)))
      (command-implementation command))))

(define (perform-operation arguments)
  (if (null-list? arguments)
    ((name->command-implementation "show")          (list))
    ((name->command-implementation (car arguments)) (cdr arguments))))

(perform-operation (command-line-arguments))
