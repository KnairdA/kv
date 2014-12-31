(require-extension test)

(include "src/store.scm")

(test-group "store-entry-manipulation"
            (test "delete-entry"
                  (list (make-entry "key2" "value2"))
                  (delete-entry (list (make-entry "key1" "value1") (make-entry "key2" "value2"))
                                "key1"))
            (test "change-entry (update)"
                  (list (make-entry "key1" "value1") (make-entry "key2" "updated-value"))
                  (change-entry (list (make-entry "key1" "value1") (make-entry "key2" "value2"))
                                "key2"
                                "updated-value"))
            (test "change-entry (create)"
                  (list (make-entry "key1" "value1") (make-entry "key2" "value2") (make-entry "key3" "value3"))
                  (change-entry (list (make-entry "key1" "value1") (make-entry "key2" "value2"))
                                "key3"
                                "value3")))

(test-exit)
