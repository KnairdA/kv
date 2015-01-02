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

(test-group "store querying"
            (test "entry-of-store"
                  (make-entry "key2" "value2")
                  (entry-of-store (make-dummy-store (list (make-entry "key1" "value1") (make-entry "key2" "value2"))) "key2"))
            (test "entry-of-store (non-existing key)"
                  #f
                  (entry-of-store (make-dummy-store (list (make-entry "key1" "value1") (make-entry "key2" "value2"))) "key3")))

(test-group "store printing"
            (test "print entry"
                  "key"
                  (fmt #f (make-entry "key" "value")))
            (test "print store"
                  (conc "key1" #\newline "key2")
                  (fmt #f (make-dummy-store (list (make-entry "key1" "value1") (make-entry "key2" "value2")))))
            (test "print empty store"
                  "#f"
                  (fmt #f (make-dummy-store (list))))
            (test "print list of store"
                  (conc "store1" #\newline "store2")
                  (fmt #f (stores->print (list (make-store "store1" "" (list)) (make-store "store2" "" (list)))))))

(test-exit)
