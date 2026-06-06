(setq txt_math_eval1 "0.5*2")
(setq txt_math_eval2 ".5*2 + 4 - 1*(1/2+1)")
(setq txt_math_eval3 ".5*-2 + -4 - 1*(1/2+1)")

(defun is-operator (c)
  (member c '("+" "-" "*" "/"))
)

(defun precedence (op)
  (cond ((or (equal op "+") (equal op "-")) 1)
        ((or (equal op "*") (equal op "/")) 2)
        (T 0))
)
(setq s txt_math_eval3)
(defun tokenize (s / i len c tok result prev)
  (setq i 0 len (strlen s) result '() prev "")
  (while (< i len)
    (setq c (substr s (+ i 1) 1))
    (cond
      ;; skip whitespace
      ((member c '(" " "\t"))
       (setq i (1+ i)))

      ;; handle parentheses
      ((member c '("(" ")"))
       (setq result (append result (list c)))
       (setq prev c)
       (setq i (1+ i)))

      ;; handle operators
      ((is-operator c)
       ;; if '-' is start of number or after '(', treat as sign
       (if (and (equal c "-")
                (or (equal prev "") (equal prev "(") (is-operator prev)))
         (progn
           ;; read negative number
           (setq tok "-")
           (setq i (1+ i))
           (while (and (< i len)
                       (or (>= (ascii (substr s (+ i 1) 1)) 48)
                           (<= (ascii (substr s (+ i 1) 1)) 57)
                           (equal (substr s (+ i 1) 1) ".")))
             (setq tok (strcat tok (substr s (+ i 1) 1)))
             (setq i (1+ i)))
           (setq result (append result (list tok)))
           (setq prev tok))
         (progn
           (setq result (append result (list c)))
           (setq prev c)
           (setq i (1+ i)))))

      ;; handle numbers
      ((or (and (>= c "0") (<= c "9")) (equal c "."))
       (setq tok "")
       (while (and (< i len)
                   (or (and (>= (substr s (+ i 1) 1) "0")
                            (<= (substr s (+ i 1) 1) "9"))
                       (equal (substr s (+ i 1) 1) ".")))
         (setq tok (strcat tok (substr s (+ i 1) 1)))
         (setq i (1+ i)))
       (setq result (append result (list tok)))
       (setq prev tok))

      (T ; invalid char
       (princ (strcat "\nInvalid character: " c))
       (return nil))
    )
  )
  result
)

(defun infix-to-postfix (tokens / output stack token)
  (setq output '() stack '())
  (foreach token tokens
    (cond
      ((and (not (is-operator token))
            (not (equal token "("))
            (not (equal token ")")))
       (setq output (append output (list token))))
      ((is-operator token)
       (while (and stack
                   (not (equal (car (last stack)) "("))
                   (>= (precedence (car (last stack))) (precedence token)))
         (setq output (append output (list (car (reverse stack)))))
         (setq stack (reverse (cdr (reverse stack)))))
       (setq stack (append stack (list token))))
      ((equal token "(")
       (setq stack (append stack (list token))))
      ((equal token ")")
       (while (and stack (not (equal (car (last stack)) "(")))
         (setq output (append output (list (car (reverse stack)))))
         (setq stack (reverse (cdr (reverse stack)))))
       (if (and stack (equal (car (last stack)) "("))
         (setq stack (reverse (cdr (reverse stack))))
         (princ "\nMismatched parentheses")))
    )
  )
  ;; pop remaining
  (while stack
    (setq output (append output (list (car (reverse stack)))))
    (setq stack (reverse (cdr (reverse stack)))))
  output
)

(defun eval-postfix (tokens / stack token a b)
  (setq stack '())
  (foreach token tokens
    (cond
      ((not (is-operator token))
       (setq stack (append stack (list (atof token)))))
      (T
       (setq b (car (last stack)))
       (setq stack (reverse (cdr (reverse stack))))
       (setq a (car (last stack)))
       (setq stack (reverse (cdr (reverse stack))))
       (setq stack
         (append stack
           (list (cond
                   ((equal token "+") (+ a b))
                   ((equal token "-") (- a b))
                   ((equal token "*") (* a b))
                   ((equal token "/") (/ a b))))))))
  )
  (car stack)
)

(defun c:EvalLocal ( / expr tokens rpn result )
  (setq expr txt_math_eval2) ;(getstring T "\nEnter math expression: "))
  (setq tokens (tokenize expr))
  (if tokens
    (progn
      (setq rpn (infix-to-postfix tokens))
      (setq result (eval-postfix rpn))
      (princ (strcat "\nResult: " (rtos result 2 6)))
    )
  )
  (princ)
)
