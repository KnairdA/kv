(use srfi-1)

(include "src/commands.scm")

(define base "~/.kv/")

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
    (if command
      (command-implementation command)
      (lambda (arguments)
        ((name->command-implementation "show") (append (list name) arguments))))))

(define (perform-operation arguments)
  (if (null-list? arguments)
    ((name->command-implementation "show")          (list))
    ((name->command-implementation (car arguments)) (cdr arguments))))

(perform-operation (command-line-arguments))
