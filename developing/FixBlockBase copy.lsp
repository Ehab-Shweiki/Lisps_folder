(defun c:FixBlockBase ( / sel i ename blkName blkDef basePt ent insPt delta)
  (vl-load-com)

  (prompt "\nSelect blocks to normalize base point to (0,0): ")
  (setq sel (ssget '((0 . "INSERT"))))

  (if sel
    (progn
      (repeat (setq i (sslength sel))
        (setq ename (ssname sel (setq i (1- i))))
        (setq ent (vlax-ename->vla-object ename))
        (setq blkName (vla-get-Name ent))
        (setq blkDef (vla-Item (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object))) blkName))

        ;; Skip if it's an external reference or layout block
        (if (and (not (vla-get-IsXRef blkDef)) (not (vla-get-IsLayout blkDef)))
          (progn
            ;; Get the first insertion point
            (setq insPt (vlax-get ent 'InsertionPoint))
            ;; Use it as base point to offset block content
            (setq basePt (vlax-3D-point (list (car insPt) (cadr insPt) 0.0)))
            (setq delta (mapcar '- '(0 0 0) basePt))

            ;; Move all entities in the block definition by (-insPt)
            (vlax-for obj blkDef
              (if (not (vlax-property-available-p obj 'Position)) ; skip attributes
                (vla-Move obj basePt (vlax-3D-point '(0 0 0)))
              )
            )

            ;; Move all insertions of this block in model space
            (vlax-for blk (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
              (if (and (= (vla-get-ObjectName blk) "AcDbBlockReference")
                       (= (strcase (vla-get-Name blk)) (strcase blkName)))
                (progn
                  (setq insPt (vlax-get blk 'InsertionPoint))
                  (setq newPt (mapcar '+ insPt basePt))
                  (vlax-put blk 'InsertionPoint (vlax-3D-point newPt))
                )
              )
            )
          )
        )
      )
    )
    (prompt "\nNothing selected or selection is not block references.")
  )

  (princ "\nBlock base points normalized to 0,0.")
  (princ)
)
