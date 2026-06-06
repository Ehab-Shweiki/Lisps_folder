(defun c:FixBlkVisualProps ( / ss i blk blkname blkdef ent ed blkcolor dxftypes)
  (prompt "\nSelect blocks to apply visual fixes...")
  ;; Supported basic geometry types
  (setq dxftypes '("LINE" "ARC" "CIRCLE" "LWPOLYLINE" "POLYLINE" "SPLINE" "ELLIPSE"))

  (if (setq ss (ssget '((0 . "INSERT"))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq blk (ssname ss i))
        (setq blkname (cdr (assoc 2 (entget blk))))
        (setq blkcolor (cdr (assoc 62 (entget blk)))) ; block ref color
        (setq blkdef (tblobjname "BLOCK" blkname))
        (setq ent (entnext blkdef))
        (while ent
          (setq ed (entget ent))
          (cond
            ;; Case 1: Dimensions
            ((wcmatch (cdr (assoc 0 ed)) "*DIMENSION")
             ;; Apply block color to DIM components
             (foreach code '(62 176 177 178)
               (if (assoc code ed)
                 (setq ed (subst (cons code blkcolor) (assoc code ed) ed))
                 (setq ed (append ed (list (cons code blkcolor))))
               )
             )
             (entmod ed)
            )

            ;; Case 2: Basic visual entities
            ((member (cdr (assoc 0 ed)) dxftypes)
             ;; Color → ByBlock
             (if (assoc 62 ed)
               (setq ed (subst (cons 62 0) (assoc 62 ed) ed))
               (setq ed (append ed (list (cons 62 0))))
             )
             ;; Linetype → ByBlock
             (if (assoc 6 ed)
               (setq ed (subst (cons 6 "ByBlock") (assoc 6 ed) ed))
               (setq ed (append ed (list (cons 6 "ByBlock"))))
             )
             ;; Lineweight → ByBlock
             (if (assoc 370 ed)
               (setq ed (subst (cons 370 -1) (assoc 370 ed) ed))
               (setq ed (append ed (list (cons 370 -1))))
             )
             ;; Transparency → ByBlock (optional)
             (if (assoc 440 ed)
               (setq ed (subst (cons 440 0) (assoc 440 ed) ed))
               (setq ed (append ed (list (cons 440 0))))
             )
             (entmod ed)
            )
          )
          (setq ent (entnext ent))
        )
        (setq i (1+ i))
      )
      (princ "\n✓ Dimensions and visual entities in selected blocks were updated.")
    )
    (prompt "\nNo blocks selected.")
  )
  (princ)
)
