(defun c:SP ()
  (command "_.select" "_p")
  (princ)
)

;; Prompt for the command in the AutoCAD command line
(princ "\nType 'SP' to run the command 'Select Previous'.")
(princ)