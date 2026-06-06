; *** options cant be within ssget
; *** it can be within entget, getpoint


(defun c:menu+getpoint ( / pt )
  ;; Define keywords (shortcuts), e.g., S for Square, C for Circle
  (initget "S C") ; these are the allowed keyword shortcuts

  ;; Ask for a point or keyword
  (setq pt (getpoint "\nSpecify point or [S/C]: "))

  ;; Check if user entered a keyword instead of point
  (cond
    ((= pt "S")
     (princ "\nOption S (Square) selected.")
     ;; call your square logic here
    )
    ((= pt "C")
     (princ "\nOption C (Circle) selected.")
     ;; call your circle logic here
    )
    ((and pt (listp pt))
     ;; A point was picked
     (princ (strcat "\nPoint selected: " (rtos (car pt) 2 2) ", " (rtos (cadr pt) 2 2)))
     ;; default behavior with point input
    )
    (T
     (princ "\nInvalid input.")
    )
  )
  
  (princ)
)

(defun menu+entsel (/ ent )
  (setq continue T)
  ; (while (and ent (not (listp ent)))
  (while continue
    (initget "Opt1 Opt2")
    (setq ent (entsel "\nSelect object or choose option [Opt1/Opt2]: <Exit> "))
    
    (cond
      ((= ent "Opt1")
       (princ "\nOption 1 selected."))
      ((= ent "Opt2")
       (princ "\nOption 2 selected."))
      ((and ent (listp ent))
       ;; An object was selected
       (princ (strcat "\nObject selected: " (vl-princ-to-string ent)))
       (setq continue nil))
      ((null ent)
       (progn
         (princ "\nExiting.")
         (setq continue nil) ; exit the loop
       )
      )
    )
  )
)



(defun menu-keyword (/ kw continue)
  ;; ===============================
  ;;      MULTI-CHOICE MENU LOOP
  ;; ===============================
  (setq continue T)
  (while continue
    (initget "a b c All")
    (setq kw (getkword "\nSelect option [a/b/c/All] <Done>: "))
    
    ;; user pressed ENTER → stop the menu
    (if (null kw)
      (setq continue nil)
    )
    
    ;; ========== HANDLE EACH OPTION ==========
    (cond
      ((= kw "a")
       ;; call your logic for option a here
      )
      ((= kw "b")
       ;; call your logic for option b here
      )
      ((= kw "c")
       ;; call your logic for option c here
      )
      ((= kw "All")
       ;; call your logic for option a,b,c here
      )
      (T
       ;; no logic , just skip
       (princ "\nNo valid option selected.")
      )
    ) ; end cond
  ) ; end while
)

(defun Menu-Loop-EnterStops (/ kw continue)
  ; (setq kw "any")
  ; (while (not kw)
  (setq continue T)
  
  (while continue
    (initget "Opt1 Opt2 Opt3 All")
    (setq kw (getkword "\n[Opt1/Opt2/Opt3/All] or <Enter> to cancel: "))
    
    (cond
      ((= kw "Opt1") (princ "\nOption 1 done."))
      ((= kw "Opt2") (princ "\nOption 2 done."))
      ((= kw "Opt3") (princ "\nOption 3 done."))
      ((= kw "All")  (princ "\nAll Options done."))
      
      ((null kw)     (princ "\nUser cancelled with Enter.")
                     (setq continue nil) ; exit if user pressed Enter
      )
    )
  )
)

(defun Menu-Loop-kwStops (/ kw continue)
  (setq continue T)

  (while continue
    (initget "Opt1 Opt2 Opt3 Return")
    (setq kw (getkword "\n[Opt1/Opt2/Opt3/Return]: "
    ))

    (cond
      ((= kw "Opt1") (princ "\nOption 1 done."))
      ((= kw "Opt2") (princ "\nOption 2 done."))
      ((= kw "Opt3") (princ "\nOption 3 done."))

      ((= kw "Return")
        (setq continue nil) ; exit ONLY if Return selected
      )
    )
  )

  (princ "\nExited Return-loop.")
)


(defun nested-menus () (/ main-kw continue)
  ;; ===============================
  ;;      MULTI-CHOICE MENU LOOP
  ;; ===============================
  (setq continue T)
  (while continue ; as none Exit-keyword Loop
    (initget "no-loop-Menu Menu-Loop-EnterStops Menu-Loop-ReturnOnly")
    (setq main-kw (getkword "\nSelect option [no-loop-Menu/Menu-Loop-EnterStops/Menu-Loop-ReturnOnly] <Done>: "))
    
    ;; user pressed ENTER → stop the menu
    (if (null main-kw)
      (setq continue nil)
    )
    
    ;; ========== HANDLE EACH OPTION ==========
    (cond
      ((= main-kw "no-loop-Menu") ; One-Shot Menu
        ;; construct new options (no loop) here
        (initget "suboption-1 suboption-2 suboption-3")
        (setq sub-kw (getkword "\nSelect sub-option [1/2/3] <Done>: "))
        (cond
          ((= sub-kw "suboption-1")
           ;; call your logic for suboption-1 here
          )
          ((= sub-kw "suboption-2")
           ;; call your logic for suboption-2 here
          )
          ((= sub-kw "suboption-3")
           ;; call your logic for suboption-3 here
          )
          (T
           ;; no logic , just skip
           (princ "\nNo valid sub-option selected.")
          )
        ) ; end cond
      )
      ((= main-kw "Menu-Loop-EnterStops") ; menu with auto return (if none selected)
        ;; construct new options (no loop) here
        (setq sub-continue T)
        (while sub-continue
          (initget "suboption-1 suboption-2 suboption-3")
          (setq sub-kw (getkword "\nSelect sub-option [1/2/3] <Done>: "))
          ;; user pressed ENTER → stop the sub-menu
          (if (null sub-kw)
            (setq sub-continue nil)
          )
          ;; ========== HANDLE EACH SUB-OPTION ==========
          (cond
            ((= sub-kw "suboption-1")
            ;; call your logic for suboption-1 here
            )
            ((= sub-kw "suboption-2")
            ;; call your logic for suboption-2 here
            )
            ((= sub-kw "suboption-3")
            ;; call your logic for suboption-3 here
            )
            (T
            ;; no logic , just skip
            (princ "\nDone.")
            )
          ) ; end cond
        ) ; end while
      )
      ((= main-kw "Menu-Loop-ReturnOnly") ; not auto return
       ;; call your logic for option c here
       (setq sub-continue T)
       (while sub-continue
          (initget "Opt1 Opt2 Opt3 Return")
          (setq sub-kw (getkword "\n[Opt1/Opt2/Opt3/Return]: "))
          ;; user pressed ENTER → stop the sub-menu
          (if (null sub-kw)
            (setq sub-continue nil)
          )
          ;; ========== HANDLE EACH SUB-OPTION ==========
          (cond
            ((= kw "Opt1") (princ "\nOption 1 done."))
            ((= kw "Opt2") (princ "\nOption 2 done."))
            ((= kw "Opt3") (princ "\nOption 3 done."))

            ((= kw "Return")
              (setq continue nil) ; exit ONLY if Return selected
            )
            
            ;; Enter does NOT stop the loop
            ((null kw) (princ "\nEnter ignored; must type Return to exit."))
          ) ; end cond
        ) ; end while
      )

      (T
       ;; no logic , just skip
       (princ "\nNo valid option selected.")
      )
    ) ; end cond
  ) ; end while
)