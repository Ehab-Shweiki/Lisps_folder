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
					;; Get number of decimals in original value
					(setq dotPos (vl-string-position (ascii ".") val))
					(setq decimals
						(if dotPos
							(strlen (vl-string-right-trim "0" (substr val (+ dotPos 2))))
							0
						)
					)
					(if (> decimals 8) (setq decimals 8)) ; cap to avoid over-rounding
					(vla-put-textstring da (rtos (+ (distof val) inc) 2 decimals))
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
