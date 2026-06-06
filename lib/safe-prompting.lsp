(vl-load-com)

;;---------------------------------------------------------------
;; Core generic safe prompt function
;;---------------------------------------------------------------
(defun safe:prompt (fn args / result)
  "Safely calls a user input function and catches cancel/errors.
   Arguments:
     fn   — symbol of the function (e.g. 'getpoint)
     args — list of arguments to pass (e.g. (list msg))
   Returns:
     value if success, nil if cancelled or error."
  (setq result (vl-catch-all-apply fn args))
  (if (vl-catch-all-error-p result)
    (progn
      (prompt (strcat "\n[SafePrompt] " (vl-catch-all-error-message result)))
      nil
    )
    result
  )
)

;;---------------------------------------------------------------
;; Wrapped ASK functions
;;---------------------------------------------------------------

(defun safe-ask:point   (msg)        (safe:prompt 'getpoint  (list msg)))
(defun safe-ask:corner  (msg basept) (safe:prompt 'getcorner (list msg basept)))
(defun safe-ask:real    (msg)        (safe:prompt 'getreal   (list msg)))
(defun safe-ask:int     (msg)        (safe:prompt 'getint    (list msg)))
(defun safe-ask:string  (msg)        (safe:prompt 'getstring (list msg)))
(defun safe-ask:angle   (msg basept) (safe:prompt 'getangle  (list msg basept)))
(defun safe-ask:dist    (msg basept) (safe:prompt 'getdist   (list msg basept)))
(defun safe-ask:orient  (msg basept) (safe:prompt 'getorient (list msg basept)))
(defun safe-ask:ent     (msg)        (safe:prompt 'entsel    (list msg)))
(defun safe-ask:nent    (msg)        (safe:prompt 'nentsel   (list msg)))
(defun safe-ask:ss      (filter)     (safe:prompt 'ssget     (if filter (list filter) (list))))
(defun safe-ask:keyword (msg opts / res)
  "Prompts for keyword using initget + getkword safely.
   opts is a space-separated string of keywords (e.g. \"Yes No\")."
  (initget opts)
  (safe:prompt 'getkword (list msg))
)


;;---------------------------------------------------------------
;; Example usage template
;;---------------------------------------------------------------
; (defun TESTSAFE (/ pt val ang ss)
;   (if (setq pt (safe-ask:point "\nPick point: "))
;     (prompt (strcat "\nYou picked " (rtos (car pt) 2 3) ", " (rtos (cadr pt) 2 3)))
;     (prompt "\nCancelled.")
;   )

;   (if (setq val (safe-ask:real "\nEnter a number: "))
;     (prompt (strcat "\nYou entered " (rtos val 2 3)))
;     (prompt "\nCancelled.")
;   )

;   (if (setq ss (safe-ask:ss nil))
;     (prompt (strcat "\nYou selected " (itoa (sslength ss)) " entities."))
;     (prompt "\nNo selection."))
;   (princ)
; )
