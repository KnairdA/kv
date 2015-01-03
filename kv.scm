(include "src/store.scm")

(define base "~/.kv/")

(define-record command name implementation)

(define (file-in-base file)
  (conc base file))

(define (read-all-stores)
  (map (lambda (file) (path->store (file-in-base file) #t))
       (remove (lambda (x) (directory? (file-in-base x)))
               (directory base))))

(define (perform-show arguments)
  (let ((count (length arguments)))
    (cond ((= 0 count) (print (stores->print (read-all-stores))))
          ((= 1 count) (print (path->store   (file-in-base   (first arguments)))))
          ((= 2 count) (print (entry-value   (entry-of-store (path->store (file-in-base (first arguments))) (second arguments)))))
          (else        (print "show: too many arguments")))))

(define (perform-write arguments)
  (if (>= (length arguments) 3)
    (write-entry (path->store (file-in-base (first arguments))) (second arguments) (drop arguments 2))
    (print "write: storage, key and value required")))

(define commands (list (make-command "show"  perform-show)
                       (make-command "write" perform-write)))

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
