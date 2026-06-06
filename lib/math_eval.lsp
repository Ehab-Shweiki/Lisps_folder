(setq txt_math_eval1 "0.5*2")
(setq txt_math_eval21 "0.5*2% + % 4 - 1*(1/2+1)")
(setq txt_math_eval2 ".5*2 + 4 - 1*(1/2+1)")
(setq txt_math_eval3 "-.5*-2 + -4 - 1*(1/2+1)")
(setq txt_math_eval4 "abs(.5*-2) + min(-4 ,- 1*(1/2+1))")

;; AutoLISP: Full math evaluator with symbolic support and validation
;; Supports: + - * / ^ % ( ) sqrt abs sin cos tan asin acos atan rad deg
;; All angles are in degrees for trig, except rad/deg
;; Also supports % as percent value: e.g., 5%*100 -> 0.05*100
;; Supports variable assignment (e.g., x=5) and constants like pi

(setq *math-vars* '(("pi" . 3.141592653589793)))

(defun set-var (key val)
  (setq *math-vars* (subst (cons key val) (assoc key *math-vars*) *math-vars*))
  (if (not (assoc key *math-vars*))
    (setq *math-vars* (append *math-vars* (list (cons key val))))))

(defun get-var (key)
  (cond
    ((assoc key *math-vars*) (cdr (assoc key *math-vars*)))
    ((equal key "pi") 3.141592653589793)
    (T (princ (strcat "\n'" key "' is not defined.")) (exit))))

(defun is-operator (tok)
  (member tok '("+" "-" "*" "/" "^" "%")))

(defun precedence (op)
  (cond ((equal op "^") 3)
        ((member op '("*" "/" "%")) 2)
        ((member op '("+" "-")) 1)
        (T 0)))

(defun is-function (tok)
  (member tok '("sqrt" "abs" "sin" "cos" "tan" "asin" "acos" "atan" "rad" "deg" "min" "max" "log" "exp")))

(defun apply-func (func val /)
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
        ((equal func "log") (log val))
        ((equal func "exp") (exp val))
        ((equal func "min") val) ;; placeholder for multi-arg
        ((equal func "max") val)
        (T (princ (strcat "\nFunction '" func "' not defined")) nil)))

;;;
(setq s expr)
;;;
(defun tokenize (s / i len ch ch-next token result done)
  (setq i 0 len (strlen s) result '())

  (while (< i len)
    (setq ch (substr s (+ i 1) 1)) ; current char
    
    (cond
      ;; Skip whitespace
      ((member ch '(" " "\t")) (setq i (1+ i)))
      
      ;; Variable assignment (e.g., x=5)
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
      
      ;; handle Operators and parentheses 
      ((member ch '("+" "-" "*" "/" "^" "%" "(" ")"))
       (setq result (append result (list ch)))
       (setq i (1+ i)))
      
  ;;;
  (setq i 0 s expr ch (substr s (+ i 1) 1))
  (setq i s expr (+ i 1) ch (substr s (+ i 1) 1))
  ;;;
      
      ;; handle numbers or percentage (starts with digit, minus, or dot)
      ((or (and (>= ch "0") (<= ch "9"))
           (equal ch ".")
           (and (equal ch "-") (or (null result) (member (last result) '("(" "+" "-" "*" "/" "^" "%")))))
       (setq token ch i (1+ i) done nil)
       ;; Continue collecting digits, decimal points
	   (while (and (< i len) (not done))
         (setq ch-next (substr s (+ i 1) 1)) ; next character
		 ;; Check if character is a digit or dot
         (if (or 
               (and (>= ch-next "0") (<= ch-next "9"))
               (equal ch-next "."))
           (progn
             (setq token (strcat token ch-next))
             (setq i (1+ i)))
           (setq done T))) ; exit loop
       
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
      
      ;; Function name or variable
      ((and (>= ch "a") (<= ch "z"))
       (setq token ch i (1+ i) done nil)
       (while (and (< i len) (not done))
         (setq ch-next (substr s (+ i 1) 1))
         (if (and (>= ch-next "a") (<= ch-next "z"))
           (progn
             (setq token (strcat token ch-next))
             (setq i (1+ i)))
           (setq done T))) ; exit loop
       (if (or (is-function token) (assoc token *math-vars*) (equal token "pi"))
         (setq result (append result (list token)))
         (progn
           (princ (strcat "\n'" token "' is not defined."))
           (exit))))
      
	  ;; Fallback: invalid character
      (T 
        (princ (strcat "\nInvalid character: '" ch "'") )
        (exit))
    )
  )
  result
)
;;;;;;
(setq i 0 token (nth i tokens)) ; start of loop
(setq i (+ i 1) token (nth i tokens)) ; next variable
;;;;;;
(defun infix-to-postfix (tokens / output stack token)
  (setq output '() stack '())
  (foreach token tokens
    (cond
      ((numberp (distof token)) (setq output (append output (list token))))
      ((or (assoc token *math-vars*) (equal token "pi")) (setq output (append output (list (rtos (get-var token) 2 12)))) )
      ((is-function token) (setq stack (append stack (list token))))
      ((equal token "(") (setq stack (append stack (list token))))
      ((equal token ")")
       (while (and stack (not (equal (last stack) "(")))
         (setq output (append output (list (car (last stack)))))
         (setq stack (reverse (cdr (reverse stack)))))
       (if (equal (last stack) "(") (setq stack (reverse (cdr (reverse stack)))))
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
  (foreach x (reverse stack) (setq output (append output (list x))))
  output
)


(defun eval-postfix (tokens / stack token a b)
  (setq stack '())
  (foreach token tokens
    (cond
      ((numberp (distof token)) (setq stack (append stack (list (atof token)))))
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
             ((equal token "%") (rem a b))
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
  (setq expr txt_math_eval3) ;(getstring T "\nEnter math expression (or assignment x=val): "))
  (setq tokens (tokenize expr))
  (if tokens
    (progn
      (setq postfix (infix-to-postfix tokens))
      (setq result (eval-postfix postfix))
      (if result (princ (strcat "\nResult: " (rtos result 2 6)))))
  )
  (princ))
