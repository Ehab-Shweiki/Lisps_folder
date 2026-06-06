(vl-load-com)

(defun c:HatchToPlineCheck
  ( / :area-ok-p :pt-member-p :self-int-p :ents-after :mark-bad
      ss i h last ents e bad-h totalH totalP badH badP )

  (defun :area-ok-p (h / r)
    (setq r (vl-catch-all-apply 'getpropertyvalue (list h "Area")))
    (not (vl-catch-all-error-p r))
  )

  (defun :pt-member-p (p lst / ok)
    (foreach q lst
      (if (equal p q 1e-8)
        (setq ok T)
      )
    )
    ok
  )

  (defun :self-int-p (e / obj r pts verts p)
    (setq obj (vlax-ename->vla-object e))

    (setq r
      (vl-catch-all-apply
        'vlax-invoke
        (list obj 'IntersectWith obj acExtendNone)
      )
    )

    (if (or (vl-catch-all-error-p r) (null r))
      nil
      (progn
        (setq verts
          (mapcar
            '(lambda (x) (append (cdr x) '(0.0)))
            (vl-remove-if-not
              '(lambda (x) (= 10 (car x)))
              (entget e)
            )
          )
        )

        (while r
          (setq p   (list (car r) (cadr r) (caddr r))
                pts (cons p pts)
                r   (cdddr r)
          )
        )

        (setq pts
          (vl-remove-if
            '(lambda (p) (:pt-member-p p verts))
            pts
          )
        )

        pts
      )
    )
  )

  (defun :ents-after (last / out)
    (while (setq last (entnext last))
      (setq out (cons last out))
    )
    (reverse out)
  )

  (defun :mark-bad (e / o)
    (setq o (vlax-ename->vla-object e))
    (vla-put-Color o 1)
    (vla-put-Lineweight o acLnWt050)
  )

  (setq totalH 0 totalP 0 badH 0 badP 0)

  (if (setq ss (ssget '((0 . "HATCH"))))
    (progn
      (repeat (setq i (sslength ss))
        (setq h (ssname ss (setq i (1- i)))
              totalH (1+ totalH)
              bad-h nil
              last (entlast)
        )

        (vl-cmdf "_.HATCHGENERATEBOUNDARY" h "")
        (setq ents (:ents-after last))
        (setq totalP (+ totalP (length ents)))

        (if (not (:area-ok-p h))
          (setq bad-h T)
        )

        (foreach e ents
          (if (:self-int-p e)
            (progn
              (:mark-bad e)
              (setq badP (1+ badP))
              (setq bad-h T)
            )
          )
        )

        (if bad-h
          (setq badH (1+ badH))
        )
      )

      (princ
        (strcat
          "\nHatches checked: " (itoa totalH)
          "\nPolylines generated: " (itoa totalP)
          "\nBad hatches: " (itoa badH)
          "\nBad polylines marked red: " (itoa badP)
        )
      )
    )
    (princ "\nNo hatches selected.")
  )
  (princ)
)

;; Short alias
(defun c:HTP () (c:HatchToPlineCheck))


(princ "\nType HTP or HatchToPlineCheck to run.")
(princ)