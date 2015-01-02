(require-extension test)

(include "src/store.scm")

(define (make-dummy-store content)
  (make-store "test" "" content))

(test-group "store manipulation"
            (test "delete-entry"
                  (list (make-entry "key2" "value2"))
                  (delete-entry (make-dummy-store (list (make-entry "key1" "value1") (make-entry "key2" "value2")))
                                "key1"))
            (test "change-entry (update)"
                  (list (make-entry "key1" "value1") (make-entry "key2" "updated-value"))
                  (change-entry (make-dummy-store (list (make-entry "key1" "value1") (make-entry "key2" "value2")))
                                "key2"
                                "updated-value"))
            (test "change-entry (create)"
                  (list (make-entry "key1" "value1") (make-entry "key2" "value2") (make-entry "key3" "value3"))
                  (change-entry (make-dummy-store (list (make-entry "key1" "value1") (make-entry "key2" "value2")))
                                "key3"
                                "value3")))

(test-group "entry printing"
            (test "entry->print"
                  "value2"
                  (entry->print (make-dummy-store (list (make-entry "key1" "value1") (make-entry "key2" "value2")))
                                "key2"))
            (test "entry->print (invalid entry)"
                  #f
                  (entry->print (make-dummy-store (list (make-entry "key1" "value1") (make-entry "key2" "value2")))
                                "key3"))
            (test "entry->print (invalid store)"
                  #f
                  (entry->print (make-dummy-store (list)) "key")))

(test-group "store printing"
            (test "print"
                  (conc "key1" #\newline "key2")
                  (fmt #f (make-dummy-store (list (make-entry "key1" "value1") (make-entry "key2" "value2")))))
            (test "print (invalid store)"
                  "#f"
                  (fmt #f (make-dummy-store (list)))))

(test-exit)
