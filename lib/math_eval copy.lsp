;;;;;;
(setq i 0 token (nth i tokens)) ; start of loop
(setq i (+ i 1) token (nth i tokens)) ; next variable
;;;;;;
(setq txt_math_eval1 "0.5*2")
(setq txt_math_eval21 "0.5*2% + % 4 - 1*(1/2+1)")
(setq txt_math_eval2 "-.5*2 + 4 - 1*(1/2+1)")
(setq txt_math_eval3 ".5*-2 + -4 - 1*(1/2+1)")
(setq txt_math_eval4 "abs(.5*-2) + min(-4 ,- 1*(1/2+1))")

;; AutoLISP: Full math evaluator with symbolic support and validation
;; Supports: + - * / ^ ( ) sqrt abs sin cos tan asin acos atan rad deg
;; All angles are in degrees for trig, except rad/deg

(defun is-operator (tok)
  (member tok '("+" "-" "*" "/" "^")))

(defun precedence (op)
  (cond ((equal op "^") 3)
        ((member op '("*" "/")) 2)
        ((member op '("+" "-")) 1)
        (T 0)))

(defun is-function (tok)
  (member tok '("sqrt" "abs" "sin" "cos" "tan" "asin" "acos" "atan" "rad" "deg")))

(defun apply-func (func val)
  (cond ((equal func "sqrt") (sqrt val))
        ((equal func "abs") (abs val))
        ((equal func "sin") (sin (* val (/ pi 180.0))))
        ((equal func "cos") (cos (* val (/ pi 180.0))))
        ((equal func "tan") (tan (* val (/ pi 180.0))))
        ((equal func "asin") (* (atan (/ val (sqrt (- 1 (* val val))))) (/ 180.0 pi)))
        ((equal func "acos") (* (atan (/ (sqrt (- 1 (* val val))) val)) (/ 180.0 pi)))
        ((equal func "atan") (* (atan val) (/ 180.0 pi)))
        ((equal func "rad") (* val (/ pi 180.0)))
        ((equal func "deg") (* val (/ 180.0 pi)))
        (T (princ (strcat "\nFunction '" func "' not defined")) nil)))

(defun tokenize (s / i len c token result)
  (setq i 0 len (strlen s) result '())
  (while (< i len)
    (setq c (substr s (+ i 1) 1))
    (cond
      ;; skip whitespace
      ((member c '(" " "\t"))
       (setq i (1+ i)))

      ;; handle parentheses
      ((member c '("(" ")"))
       (setq result (append result (list c)))
       (setq i (1+ i)))
      
      ;; handle numbers
      ((or (and (>= c "0") (<= c "9")) (equal c ".")
           (and (equal c "-") (or (null result) (member (last result) '("(" "+" "-" "*" "/" "^")))))
       (setq token c i (1+ i))
       (while (and (< i len) (or (>= (substr s (+ i 1) 1) "0") (<= (substr s (+ i 1) 1) "9") (equal (substr s (+ i 1) 1) ".")))
         (setq token (strcat token (substr s (+ i 1) 1)))
         (setq i (1+ i)))
       (setq result (append result (list token))))
      ((or (>= c "a") (<= c "z"))
       (setq token c i (1+ i))
       (while (and (< i len) (>= (ascii (substr s (+ i 1) 1)) 97) (<= (ascii (substr s (+ i 1) 1)) 122))
         (setq token (strcat token (substr s (+ i 1) 1)))
         (setq i (1+ i)))
       (if (is-function token)
         (setq result (append result (list token)))
         (progn (princ (strcat "\n'" token "' is not defined.")) (exit))))
      (T (princ (strcat "\nInvalid character: '" c "'") ) (exit))
    )
  )
  result)

(defun infix-to-postfix (tokens / output stack token)
  (setq output '() stack '())
  (foreach token tokens
    (cond
      ((numberp (read token)) (setq output (append output (list token))))
      ((is-function token) (setq stack (append stack (list token))))
      ((equal token "(") (setq stack (append stack (list token))))
      ((equal token ")")
       (while (and stack (not (equal (last stack) "(")))
         (setq output (append output (list (car (last stack)))))
         (setq stack (reverse (cdr (reverse stack)))))
       (if (equal (last stack) "(") (setq stack (reverse (cdr (reverse stack)))))
       ;; If function on top
       (if (and stack (is-function (last stack)))
         (progn (setq output (append output (list (car (last stack)))))
                (setq stack (reverse (cdr (reverse stack)))))))
      ((is-operator token)
       (while (and stack (is-operator (last stack)) (>= (precedence (last stack)) (precedence token)))
         (setq output (append output (list (car (last stack)))))
         (setq stack (reverse (cdr (reverse stack)))))
       (setq stack (append stack (list token))))
      (T (princ (strcat "\nUnknown token in parsing: '" token "'")) (exit))
    )
  )
  ;; Flush stack
  (foreach x (reverse stack) (setq output (append output (list x))))
  output)

(defun eval-postfix (tokens / stack token a b)
  (setq stack '())
  (foreach token tokens
    (cond
      ((numberp (read token)) (setq stack (append stack (list (atof token)))))
      ((is-operator token)
       (setq b (car (last stack)) stack (reverse (cdr (reverse stack))))
       (setq a (car (last stack)) stack (reverse (cdr (reverse stack))))
       (setq stack (append stack
         (list
           (cond
             ((equal token "+") (+ a b))
             ((equal token "-") (- a b))
             ((equal token "*") (* a b))
             ((equal token "/") (/ a b))
             ((equal token "^") (expt a b))
           )
         ))))
      ((is-function token)
       (setq a (car (last stack)) stack (reverse (cdr (reverse stack))))
       (setq stack (append stack (list (apply-func token a)))))
      (T (princ (strcat "\nCannot evaluate token: '" token "'")) (exit))
    )
  )
  (car stack))

(defun c:EvalExpr ( / expr tokens postfix result )
  (setq expr (getstring T "\nEnter math expression: "))
  (setq tokens (tokenize expr))
  (if tokens
    (progn
      (setq postfix (infix-to-postfix tokens))
      (setq result (eval-postfix postfix))
      (if result (princ (strcat "\nResult: " (rtos result 2 6)))))
  )
  (princ))


;------------------------

(setq tokens (tokenize (setq expr txt_math_eval21)))
(foreach a tokens
  (princ (read a))
  (princ"\n")
  )


; (progn
;   (setq num ")" token num)
;   (is-number num)
; )
; (defun is-number (token)
;   (and
;     (or (eq (type token) 'STR) (eq (type token) 'SYM))
;     (not (wcmatch token "*(\"*\"*\"*)*")) ; crude string-literal block
;     (wcmatch token "[-+~0-9.]*") ; only digits and . or -
;     (not (wcmatch token "*[A-Za-z]*")) ; reject any with letters
;     (not (wcmatch token "*(*")) ; reject parentheses
;     (not (wcmatch token "*)*"))
;     (numberp (distof token)) ; built-in parser with units support
;   )
; )

(progn
	(setq ch "z")
    (princ (ascii ch))
    (princ "\n")
	(princ (or (>= ch "a") (<= ch "z")))
)

(cond
         ((wcmatch "0.5" "`.*") (setq token (strcat "0" token)))                  ; .5 → 0.5
  		(wcmatch ".54" "`.#")
         ((wcmatch "-.5" "-.^\\.") (setq token (strcat "-0" (substr token 2))))     ; -.5 → -0.5
         ((wcmatch "5." "*.^") (setq token (strcat token "0"))))

(progn
(setq str "-54.")
(normalize-number-string str)
)

(defun normalize-number-string (str / first-dot second-dot ch-before ch-after is-negative)
  (setq is-negative nil)

  ;; Check for leading minus sign
  (if (= (substr str 1 1) "-")
    (progn
      (setq is-negative T)
      (setq str (substr str 2)) ; Remove the negative sign for processing
    )
  )

  ;; Check dot positions
  (setq first-dot (vl-string-search "." str))
  (setq second-dot (if first-dot (vl-string-search "." str (1+ first-dot))))

  ;; Proceed only if exactly one dot
  (cond
    ;; Case: more than one dot
    (second-dot
     (princ "\nInvalid number: more than one '.' detected.")
     (if is-negative (strcat "-" str) str) ; return as-is

    )
    
    ;; Case: no dots at all → return unmodified
    ((not first-dot)
     (if is-negative (strcat "-" str) str)
    )
    
    ;; Case: exactly one dot → normalize
    (T
      ;; Get char before dot (if any)
      (if (> first-dot 0)
        (setq ch-before (substr str first-dot 1))
        (setq ch-before "")
      )
      ;; Get char after dot (if any)
      (if (< (1+ first-dot) (strlen str))
        (setq ch-after (substr str (+ first-dot 2) 1))
        (setq ch-after "")
      )

      ;; Add "0" before dot if needed
      (if (or (= ch-before "") (not (<= 48 (ascii ch-before) 57)))
        (setq str (strcat "0" str))
      )

      ;; Update dot position (in case string changed)
      (setq first-dot (vl-string-search "." str))

      ;; Add "0" after dot if needed
      (if (or (= ch-after "") (not (<= 48 (ascii ch-after) 57)))
        (setq str (strcat (substr str 1 (1+ first-dot)) "0"))
      )
      
      ;; Reattach minus sign if needed
     (if is-negative (setq str (strcat "-" str)))

     str
    )
  )
)

(defun normalize-number-string (str / first-dot second-dot ch-before ch-after sign)

  ;; --- Step 1: Handle sign (+ or -) ---
  (cond
    ((= (substr str 1 1) "-")
     (setq sign "-")
     (setq str (substr str 2))
    )
    ((= (substr str 1 1) "+")
     (setq sign "+")
     (setq str (substr str 2))
    )
    (T (setq sign "")) ; no sign
  )

  ;; --- Step 2: Count dots ---
  (setq first-dot (vl-string-search "." str))
  (setq second-dot (if first-dot (vl-string-search "." str (1+ first-dot))))

  ;; --- Step 3: Decision logic ---
  (cond
    ;; Too many dots — invalid number
    (second-dot
     (princ "\nInvalid number: more than one '.' detected.")
     (strcat sign str) ; return original
    )

    ;; No dot — valid integer, no change
    ((not first-dot)
     (strcat sign str)
    )

    ;; Exactly one dot — normalize
    (T
     ;; Get char before and after dot
     (setq ch-before (if (> first-dot 0) (substr str first-dot 1) ""))
     (setq ch-after  (if (< (1+ first-dot) (strlen str)) (substr str (+ first-dot 2) 1) ""))

     ;; Add 0 before dot if needed
     (if (or (= ch-before "") (not (<= 48 (ascii ch-before) 57)))
       (setq str (strcat "0" str))
     )

     ;; Update dot pos in case changed
     (setq first-dot (vl-string-search "." str))

     ;; Add 0 after dot if needed
     (if (or (= ch-after "") (not (<= 48 (ascii ch-after) 57)))
       (setq str (strcat (substr str 1 (1+ first-dot)) "0"))
     )

     ;; Reattach sign
     (strcat sign str)
    )
  )
)
