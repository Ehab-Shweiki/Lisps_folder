(defun c:RevCloud_auto ( / p1 p2 oldcolor oldwidth)
  (initcommandversion 2)

  ;; Prompt user for rectangle corners
  (prompt "\nSelect first corner of rectangle: ")
  (setq p1 (getpoint))
  (prompt "\nSelect opposite corner: ")
  (setq p2 (getcorner p1 "\nSelect opposite corner: "))

  ;; Ensure both points are selected
  (if (and p1 p2)
    (progn
      ;; Start tracking last entity
      (setq ent (entlast)) ; save last entity before command

      ;; Call REVCLOUD
      (vl-cmdf "._REVCLOUD" "_R" p1 p2 "")

      ;; Find the new entity (revcloud polyline)
      (setq ent (entnext ent))

      ;; If it's a polyline, modify its properties
      (if (and ent (entget ent))
        (progn
          (entmod (subst (cons 62 1) (assoc 62 (entget ent)) (entget ent))) ; color = 1 (red)
          (entmod (subst (cons 43 3.0) (assoc 43 (entget ent)) (entget ent))) ; start width
          (entmod (subst (cons 44 3.0) (assoc 44 (entget ent)) (entget ent))) ; end width
          (entupd ent)
        )
      )
    )
    (prompt "\nInvalid corners.")
  )
  (princ)
)

(defun c:RV () (c:RevCloud_auto))
(princ "\nType 'RV' to Run RevCloud_auto command.")
(princ)