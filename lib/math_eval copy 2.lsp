(defun tokenize (s / i len ch ch-next token result done)
  (setq i 0 len (strlen s) result '())

  (while (< i len)
    (setq ch (substr s (+ i 1) 1))

    (cond
      ;; Skip whitespace
      ((member ch '(" " "\t")) (setq i (1+ i)))

      ;; Variable assignment
      ((equal ch "=")
       (setq i (1+ i))
       (setq var (car (reverse result)))
       (setq result (reverse (cdr (reverse result))))
       (setq val "")
       (while (< i len)
         (setq ch (substr s (+ i 1) 1))
         (setq val (strcat val ch))
         (setq i (1+ i)))
       (set-var var (atof val))
       (princ (strcat "\n" var " = " val))
       (return nil))

      ;; Operators and parentheses
      ((member ch '("+" "-" "*" "/" "^" "%" "(" ")"))
       (setq result (append result (list ch)))
       (setq i (1+ i)))

      ;; Numbers or percentages
      ((or (and (>= ch "0") (<= ch "9"))
           (equal ch ".")
           (and (equal ch "-") (or (null result) (member (last result) '("(" "+" "-" "*" "/" "^" "%")))))
       (setq token ch i (1+ i) done nil)
       (while (and (< i len) (not done))
         (setq ch-next (substr s (+ i 1) 1))
         (if (or (and (>= ch-next "0") (<= ch-next "9")) (equal ch-next "."))
           (progn
             (setq token (strcat token ch-next))
             (setq i (1+ i)))
           (setq done T)))
       
       ;; Normalize decimals
       (cond
         ((wcmatch token ".^\\.") (setq token (strcat "0" token)))                  ; .5 → 0.5
         ((wcmatch token "-.^\\.") (setq token (strcat "-0" (substr token 2))))     ; -.5 → -0.5
         ((wcmatch token "*.^") (setq token (strcat token "0"))))                   ; 5. → 5.0

       ;; Convert trailing % to value * 0.01
       (if (and (< i len) (equal (substr s (+ i 1) 1) "%"))
         (progn
           (setq token (rtos (* (atof token) 0.01) 2 10))
           (setq i (1+ i))))
       (setq result (append result (list token))))

      ;; Function or name
      ((and (>= ch "a") (<= ch "z"))
       (setq token ch i (1+ i))
       (while (< i len)
         (setq ch-next (substr s (+ i 1) 1))
         (if (and (>= ch-next "a") (<= ch-next "z"))
           (progn (setq token (strcat token ch-next)) (setq i (1+ i)))
           (setq i len)))
       (if (or (is-function token) (assoc token *math-vars*) (equal token "pi"))
         (setq result (append result (list token)))
         (progn
           (princ (strcat "\n'" token "' is not defined."))
           (exit))))

      ;; Invalid char
      (T
       (princ (strcat "\nInvalid character: '" ch "'"))
       (exit))
    )
  )
  result)

