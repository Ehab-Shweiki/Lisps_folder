(defun c:FixBlockBase ( / doc ms sel i ename blk blkname blkdef entlist minpt delta obj newpt)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms (vla-get-ModelSpace doc))

  (prompt "\nSelect block references to reset base point to (0,0): ")
  (setq sel (ssget '((0 . "INSERT"))))

  (if sel
    (progn
      (repeat (setq i (sslength sel))
        (setq ename (ssname sel (setq i (1- i))))
        (setq blk (vlax-ename->vla-object ename))
        (setq blkname (vla-get-Name blk))
        (setq blkdef (vla-Item (vla-get-Blocks doc) blkname))

        ;; Skip xrefs or layout blocks
        (if (and (not (vla-get-IsXRef blkdef)) (not (vla-get-IsLayout blkdef)))
          (progn
            ;; Get bounding box of block definition
            (setq entlist '())
            (vlax-for obj blkdef
              (if (vlax-method-applicable-p obj 'GetBoundingBox)
                (progn
                  (setq p1 (vlax-3D-point '(0 0 0)) p2 (vlax-3D-point '(0 0 0)))
                  (vla-GetBoundingBox obj 'p1 'p2)
                  (setq p1 (vlax-safearray->list p1))
                  (setq entlist (cons p1 entlist))
                )
              )
            )
            ;; Calculate minimum point of bounding box
            (if entlist
              (progn
                (setq minpt (apply 'mapcar (cons 'min entlist))) ; minimum X,Y,Z
                (setq delta (mapcar '- '(0 0 0) minpt)) ; move vector to (0,0,0)

                ;; Move all entities inside the block definition
                (vlax-for obj blkdef
                  (vla-Move obj (vlax-3D-point minpt) (vlax-3D-point '(0 0 0)))
                )

                ;; Move all block insertions to compensate
                (vlax-for b ms
                  (if (and (= (vla-get-ObjectName b) "AcDbBlockReference")
                           (= (strcase (vla-get-Name b)) (strcase blkname)))
                    (progn
                      (setq newpt (mapcar '+ (vlax-get b 'InsertionPoint) minpt))
                      (vlax-put b 'InsertionPoint (vlax-3D-point newpt))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    (prompt "\nNothing selected.")
  )
  (princ "\nDone. Block base moved to 0,0 and insertions adjusted.")
  (princ)
)
