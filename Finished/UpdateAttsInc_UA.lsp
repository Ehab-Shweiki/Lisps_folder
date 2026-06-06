(defun c:UA (/ ss inc i blkEnt blkObj tag val dotPos decimals newVal origDec
                   origZeroTrail trailing dotPosNew newDec pad atts)

  (vl-load-com)

  ;; Global variable to store last increment
  (if (not *lastIncValue*) (setq *lastIncValue* 0.0))

  ;; Helper: Get attribute objects from a block
  (defun get-attributes (blk)
    (vl-remove-if
     'null
     (mapcar
      (function
       (lambda (ent)
         (if (= "AcDbAttribute" (vla-get-objectname ent))
           ent
         )
       )
      )
      (vlax-invoke blk 'GetAttributes)
     )
    )
  )

  ;; Ask for increment value (with default)
  (initget "") ; allow any real number
  (setq inc (getreal (strcat "\nEnter increment value for numeric attributes <"
                             (rtos *lastIncValue* 2 4) ">: ")))
  (if (null inc) (setq inc *lastIncValue*))
  (setq *lastIncValue* inc) ;; save it globally

  ;; Select blocks
  (prompt "\nSelect blocks to update attributes: ")
  (setq ss (ssget '((0 . "INSERT"))))
  (if (null ss)
    (progn (prompt "\nNo blocks selected.") (exit))
  )

  ;; Loop through all selected blocks
  (setq i 0)
  (while (< i (sslength ss))
    (setq blkEnt (ssname ss i))
    (setq blkObj (vlax-ename->vla-object blkEnt))
    (setq atts (get-attributes blkObj))

    (foreach att atts
      (setq tag (strcase (vla-get-tagstring att)))
      (setq val (vla-get-textstring att))

      (if (/= "" val)
        (if (distof val)
          (progn
            ;; preserve decimal format
            (setq dotPos (vl-string-position (ascii ".") val))
            (setq decimals
              (if dotPos
                (strlen (substr val (+ dotPos 2)))
                0
              )
            )
            (if (> decimals 8) (setq decimals 8))

            (setq newVal (rtos (+ (distof val) inc) 2 decimals))

            ;; handle trailing zeros
            (if (and dotPos (> decimals 0))
              (progn
                (setq origDec (substr val (+ dotPos 2)))
                (setq origZeroTrail (strlen (vl-string-right-trim "0" origDec)))
                (setq trailing (- (strlen origDec) origZeroTrail))

                (setq dotPosNew (vl-string-position (ascii ".") newVal))
                (if dotPosNew
                  (progn
                    (setq newDec (substr newVal (+ dotPosNew 2)))
                    (setq pad (- (strlen origDec) (strlen newDec)))
                    (if (> pad 0)
                      (setq newVal (strcat newVal (substr "00000000" 1 pad)))
                    )
                  )
                )
              )
            )

            (vla-put-textstring att newVal)
          )
          ;; else: keep original text
          (vla-put-textstring att val)
        )
      )
    )

    (setq i (1+ i))
  )

  (prompt (strcat "\nDone. Attributes incremented by " (rtos inc 2 4) "."))
  (princ)
)

(princ "\nType 'UA' to increment attributes of selected blocks.")
(princ)
