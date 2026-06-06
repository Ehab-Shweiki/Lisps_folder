(defun join-strings (lst delim / result)
  (setq result (if lst (car lst) ""))   ; start with first item or empty
  (foreach s (cdr lst)
    (setq result (strcat result delim s))
  )
  result
)
(defun join-strings2 (lst delim)
  "join in inline code"
  (apply 'strcat
    (cons (car lst2)
          (mapcar '(lambda (x) (strcat delim x))
                  (cdr lst2))))
)