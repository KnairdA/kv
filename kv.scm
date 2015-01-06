(include "src/store.scm")
(include "src/utility.scm")

(define base "~/.kv/")

(define-record command name implementation)

(define (file-in-base file)
  (conc base file))

(define (read-all-stores #!optional do-not-read)
  (map (lambda (file) (path->store (file-in-base file) do-not-read))
       (remove (lambda (x) (directory? (file-in-base x)))
               (directory base))))

(define (perform-show arguments)
  (let ((count (length arguments)))
    (cond ((= 0 count) (print (stores->print (read-all-stores #t))))
          ((= 1 count) (print                (path->store (file-in-base (first arguments)))))
          ((= 2 count) (print (read-value    (path->store (file-in-base (first arguments))) (second arguments))))
          (else        (print "show: too many arguments")))))

(define (perform-all arguments)
  (let ((count (length arguments)))
    (cond ((= 0 count) (print (entries->pretty-print (merge-stores  (read-all-stores)))))
          ((= 1 count) (print (entries->pretty-print (store-content (path->store (file-in-base (first arguments)))))))
          (else        (print "all: too many arguments")))))

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

(define commands (list (make-command "show"   perform-show)
                       (make-command "all"    perform-all)
                       (make-command "write"  perform-write)
                       (make-command "delete" perform-delete)
                       (make-command "rename" perform-rename)))

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
