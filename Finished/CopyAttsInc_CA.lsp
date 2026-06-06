(defun c:CA (/ src dst inc srcEnt dstEnt srcName dstName srcAtts dstAtts)
  ;; Helper: Get attribute values from block
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

  ;; Prompt user to select source and destination blocks
  
  (prompt "\nSelect source block with attributes to copy their values: ")
  (setq srcEnt (car (entsel)))
  (if (null srcEnt)
    (progn (prompt "\nNo source block selected.") (exit))
  )

  (setq src (vlax-ename->vla-object srcEnt))
  (setq srcName (vla-get-effectivename src))

  (prompt "\nSelect destination block (same name): ")
  (setq dstEnt (car (entsel)))
  (if (null dstEnt)
    (progn (prompt "\nNo destination block selected.") (exit))
  )

  (setq dst (vlax-ename->vla-object dstEnt))
  (setq dstName (vla-get-effectivename dst))

  ;; Validate block names
  (if (/= srcName dstName)
    (progn
      (prompt (strcat "\nError: Block names don't match (" srcName " vs " dstName ")."))
      (exit)
    )
  )

  ;; Get increment value
  (initget "") ; allow any real number
  (setq inc (getreal "\nEnter increment value for numeric fields <0>: "))
  (if (null inc) (setq inc 0))

  ;; Get attributes from both blocks
  (setq srcAtts (get-attributes src))
  (setq dstAtts (get-attributes dst))

  ;; Copy attribute values
  (foreach sa srcAtts
    (if (/= "" (vla-get-textstring sa)) ; non-empty value
      (progn
        (setq tag (strcase (vla-get-tagstring sa)))
        (setq val (vla-get-textstring sa))

        ;; find matching tag in destination
        (foreach da dstAtts
          (if (= tag (strcase (vla-get-tagstring da)))
            (progn
              ;; check if numeric and apply increment
              (if (distof val)
                (progn
                  ;; Detect decimal precision in original string, including trailing zeros
                  (setq dotPos (vl-string-position (ascii ".") val))
                  (setq decimals
                    (if dotPos
                      (strlen (substr val (+ dotPos 2)))
                      0
                    )
                  )
                  (if (> decimals 8) (setq decimals 8)) ; cap precision
              
                  ;; Format result with same number of decimals
                  (setq newVal (rtos (+ (distof val) inc) 2 decimals))
              
                  ;; If original had trailing zeros, re-add them manually
                  (if (and dotPos (> decimals 0))
                    (progn
                      (setq origDec (substr val (+ dotPos 2)))
                      (setq origZeroTrail (strlen (vl-string-right-trim "0" origDec)))
                      (setq trailing (- (strlen origDec) origZeroTrail))
                      
                      ;; Add missing trailing zeros only if not already there
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
              
                  (vla-put-textstring da newVal)
                )
                (vla-put-textstring da val)
              )
            )
          )
        )
      )
    )
  )
  (princ "\nAttributes values copied with increment (if numeric).")
  (princ)
)

(princ "\nType 'CA' to run the CopyAttsInc.")
(princ)
