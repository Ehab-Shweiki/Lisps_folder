(defun c:TEST-GRREAD (/ data code)
  "
  Mouse:
    Move:   Code=5 , Data: Point
    LC:     Code=3 , Data: Point
    RC:     Code=25, Data: scaler number!
  KP Enter: 
    Code: 2
    Data:
      ESC: 27 (but not written, Application ERROR: Console break)
      Enter: 13
      Space: 32
  "
  (princ "\n--- grread test started ---")
  (princ "\nMove mouse, click, press keys, or ESC to stop.")
  (while (setq data (grread T 1 0))
    (setq code (car data))
    ;; print code and data
    (princ (strcat
             "\ncode: " (vl-princ-to-string code)
             "   data: " (vl-princ-to-string (cdr data))
           )
    )
    ;; stop
    (if (= code nil)
      (progn
        (princ "\n--- test ended ---")
        (exit)
      )
    )
    ;; ESC exit immediately
  )
  (princ)
)
