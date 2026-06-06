(defun c:RENUM_Val (/ p1 p2 ss lst order ent obj cen dx dy baseVal newVal i atts att blkpt)

  ;; --- Global holder for last value ---
  (if (not *RENUM_LastValue*)
    (setq *RENUM_LastValue* 0)
  )

  ;; --- Sorting function ---
  (defun sort1D-func (a b)
    (cond
      ((= order "X+") (< (car a) (car b))) ;; Left → Right
      ((= order "X-") (> (car a) (car b))) ;; Right → Left
      ((= order "Y+") (< (cadr a) (cadr b))) ;; Bottom → Top
      ((= order "Y-") (> (cadr a) (cadr b))) ;; Top → Bottom
    )
  )

  ;; --- Ask for base number ---
  (prompt "\nSelect a TEXT/MTEXT/block with 1 attribute to get starting number, or press Enter to type or use previous.")
  (setq ent (car (entsel "\nSelect starting object (or Enter): ")))

  (cond
    (ent
      (setq obj (vlax-ename->vla-object ent))
      (cond
        ;; TEXT or MTEXT
        ((member (vla-get-objectname obj) '("AcDbText" "AcDbMText"))
         (setq baseVal (atoi (vla-get-TextString obj)))
         (prompt (strcat "\nStarting number taken from text: " (itoa baseVal)))
        )
        ;; Block with 1 attribute
        ((and (= (vla-get-objectname obj) "AcDbBlockReference")
              (vlax-method-applicable-p obj 'GetAttributes)
              (= (length (setq atts (vlax-invoke obj 'GetAttributes))) 1))
         (setq baseVal (atoi (vla-get-TextString (car atts))))
         (prompt (strcat "\nStarting number taken from block attribute: " (itoa baseVal)))
        )
        (T
         (prompt "\nInvalid object selected. Using typed value instead.")
         (setq ent nil)
        )
      )
    )
  )

  (if (null ent)
    (progn
      (initget 128)
      (setq baseVal (getint (strcat "\nEnter starting number <" (itoa *RENUM_LastValue*) ">: ")))
      (if (null baseVal)
        (setq baseVal *RENUM_LastValue*)
      )
    )
  )

  ;; Store for reuse
  (setq *RENUM_LastValue* baseVal)

  ;; --- Loop for multiple renumbering rounds ---
  (while T
    (prompt "\nPick selection window for next group (Enter to finish)...")
    (setq p1 (getpoint "\nPick first corner of selection window (or Enter to finish): "))
    (if (null p1)
      (progn
        (prompt "\nRenumbering finished.")
        (exit) ; exit loop
      )
    )
    (setq p2 (getcorner p1 "\nPick opposite corner: "))

    ;; Determine direction
    (setq dx (abs (- (car p2) (car p1))))
    (setq dy (abs (- (cadr p2) (cadr p1))))
    (setq order
      (if (> dx dy)
        (if (> (car p2) (car p1)) "X+" "X-")
        (if (> (cadr p2) (cadr p1)) "Y+" "Y-")
      )
    )

    (prompt (strcat "\nSorting direction: "
      (cond
        ((= order "X+") "Left → Right")
        ((= order "X-") "Right → Left")
        ((= order "Y+") "Bottom → Top")
        ((= order "Y-") "Top → Bottom")
      )
    ))

    ;; Select all relevant types: TEXT, MTEXT, INSERT
    (setq ss (ssget "C" p1 p2 '((0 . "TEXT,MTEXT,INSERT"))))
    (if (not ss)
      (prompt "\nNo valid objects selected.")
      (progn
        (setq lst '())
        (repeat (setq i (sslength ss))
          (setq ent (ssname ss (setq i (1- i))))
          (setq obj (vlax-ename->vla-object ent))
          (cond
            ;; TEXT / MTEXT
            ((member (vla-get-objectname obj) '("AcDbText" "AcDbMText"))
             (setq cen (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj))))
             (setq lst (cons (list cen obj 'text) lst))
            )
            ;; Block with 1 attribute
            ((and (= (vla-get-objectname obj) "AcDbBlockReference")
                  (vlax-method-applicable-p obj 'GetAttributes)
                  (= (length (setq atts (vlax-invoke obj 'GetAttributes))) 1))
             (setq blkpt (vlax-safearray->list (vlax-variant-value (vla-get-InsertionPoint obj))))
             (setq lst (cons (list blkpt obj 'block (car atts)) lst))
            )
          )
        )

        ;; Sort by direction
        (setq lst (vl-sort lst '(lambda (a b) (sort1D-func (car a) (car b)))))

        ;; Apply renumbering
        (setq newVal (1+ *RENUM_LastValue*))
        (foreach item lst
          (setq obj (cadr item))
          (cond
            ((= (caddr item) 'text)
             (vla-put-TextString obj (itoa newVal)))
            ((= (caddr item) 'block)
             (vla-put-TextString (cadddr item) (itoa newVal)))
          )
          (setq newVal (1+ newVal))
        )

        ;; Update stored value
        (setq *RENUM_LastValue* (1- newVal))

        (prompt (strcat "\nRenumbered " (itoa (length lst)) " items. Last value now: " (itoa *RENUM_LastValue*)))
      )
    )
  )

  (princ)
)

(defun c:RN () (c:RENUM_Val))
(princ "\nType 'RN' to run the ReNumber_byValue command.")
(princ)
