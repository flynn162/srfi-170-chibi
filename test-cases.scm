(import (scheme base)
        (srfi 145)
        (170)
        )

;; Unicode test
(let ((key "_CHIBI_SUSHI") (value "寄蜉蝣於天地渺滄海之一粟"))
  (set-environment-variable! key value)
  (assume (string=? (get-environment-variable key) value) "Unicode!")
  (delete-environment-variable! key)
  )

