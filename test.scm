(require-extension test)

(include "src/store.scm")

(define (make-dummy-store content)
  (make-store "test" "" content))

(test-group "entry list manipulation"
            (test "remove-entry"
                  (list (make-entry "key2" "value2"))
                  (remove-entry (list (make-entry "key1" "value1") (make-entry "key2" "value2"))
                                "key1"))
            (test "append-unique-entry (update)"
                  (list (make-entry "key1" "value1") (make-entry "key2" "updated-value"))
                  (append-unique-entry (list (make-entry "key1" "value1") (make-entry "key2" "value2"))
                                       (make-entry "key2" "updated-value")))
            (test "append-unique-entry (create)"
                  (list (make-entry "key1" "value1") (make-entry "key2" "value2") (make-entry "key3" "value3"))
                  (append-unique-entry (list (make-entry "key1" "value1") (make-entry "key2" "value2"))
                                       (make-entry "key3" "value3"))))

(test-group "higher order entry list transformations"
            (test "write-entry (update)"
                  (list (make-entry "key1" "value1") (make-entry "key2" "value_changed"))
                  ((write-entry "key2" (list "value_changed")) (list (make-entry "key1" "value1") (make-entry "key2" "value_changed"))))
            (test "write-entry (create)"
                  (list (make-entry "key1" "value1") (make-entry "key2" "value2"))
                  ((write-entry "key2" (list "value2")) (list (make-entry "key1" "value1"))))
            (test "delete-entry (existing)"
                  (list (make-entry "key1" "value1"))
                  ((delete-entry "key2") (list (make-entry "key1" "value1") (make-entry "key2" "value2"))))
            (test "delete-entry (non-existing)"
                  (list (make-entry "key1" "value1") (make-entry "key2" "value2"))
                  ((delete-entry "key3") (list (make-entry "key1" "value1") (make-entry "key2" "value2"))))
            (test "rename-entry (existing)"
                  (list (make-entry "key1" "value1") (make-entry "key3" "value2"))
                  ((rename-entry "key2" "key3") (list (make-entry "key1" "value1") (make-entry "key2" "value2"))))
            (test "rename-entry (non-existing)"
                  (list (make-entry "key1" "value1") (make-entry "key2" "value2"))
                  ((rename-entry "key3" "key2") (list (make-entry "key1" "value1") (make-entry "key2" "value2")))))

(test-group "entry list querying"
            (test "find-entry"
                  (make-entry "key2" "value2")
                  (find-entry (list (make-entry "key1" "value1") (make-entry "key2" "value2")) "key2"))
            (test "find-entry (non-existing key)"
                  #f
                  (find-entry (list (make-entry "key1" "value1") (make-entry "key2" "value2")) "key3")))

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
