(import (scheme base)
        (chibi test)
        (170)
        )

(test-begin "Additional")

;; Unicode test
(let ((key "_CHIBI_SUSHI") (value "寄蜉蝣於天地渺滄海之一粟"))
  (set-environment-variable! key value)
  (test-equal string=? value (get-environment-variable key))
  (delete-environment-variable! key)
  )

(for-each
 (lambda (kv)
   (test-error (set-environment-variable! (car kv) (cdr kv)))
   )
 '(;; NUL character test
   ("_TEST_\x00;VARIABLE" . "value")
   ("\x00;_TEST_VARIABLE" . "value")
   ("_TEST_VARIABLE\x00;" "value")
   ("_TEST_VARIABLE" "val\x00;ue")
   ("_TEST_VARIABLE" "\x00;value")
   ("_TEST_VARIABLE" "value\x00;")
   ;; '=' in names
   ("MY=_TEST" "value")
   ("=MY_TEST" "value")
   ("MY_TEST=" "value")
   ("=" "value")
   ;; empty variable name
   ("" "value")
   ))

;; Must be able to clear the value of a variable
(set-environment-variable! "_TEST" "")
(test-equal string=? "" (get-environment-variable "_TEST"))
(delete-environment-variable! "_TEST")
(test-not (get-environment-variable "_TEST"))

(test-end "Additional")
(test-exit)
