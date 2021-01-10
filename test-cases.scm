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

(test-end "Additional")
(test-exit)
