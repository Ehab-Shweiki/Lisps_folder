(defun insert_table_sections (sections inspt / acObj acDoc space ht pct n tab acol flatRows i row col)
  (defun recdw-lite (val ht)
	;; estimate width ≈ 0.62 * chars * ht  + padding
	  (+ (* 0.62 (strlen (vl-princ-to-string (if val val ""))) ht)
		   (* 2.0 ht))
  )
  (defun recdims-fitcols (tab flatRows ht / i)
    (setq i 0)
    (repeat 3
      (vla-SetColumnWidth tab i
      (apply 'max
        (mapcar '(lambda (row) (recdw-lite (nth (1+ i) row) ht)) flatRows)
      )
      )
      (setq i (1+ i))
    )
  )
  
  (setq acObj (vlax-get-acad-object)
        acDoc (vla-get-ActiveDocument acObj)
        space (vlax-get acDoc (if (= 1 (getvar 'cvport)) 'PaperSpace 'ModelSpace)))
  ; ;
  ;   (setq sections (list
  ;     (list "RECTANGLES (EXACT)" listExact)
  ;     (list (strcat "RECTANGLES (NEAR)  LTOL=" (rtos *RECDIMS_LTOL* 2 3) ", ATOL=" (rtos *RECDIMS_ATOL* 2 2) "°") listNear)))
  ; ;
  ;; Base text height scaled by cannoscalevalue and user multiplier
  (setq ht (/ (* 2.5 *RECDIMS_SCALE*) (max 1e-6 (getvar 'cannoscalevalue))))
  (setq pct (trans inspt 1 0)
        n   (trans '(1 0 0) 1 0 T)
        tab (vla-AddTable space (vlax-3d-point pct)
              (+ 1 (apply '+ (mapcar '(lambda (s) (+ 1 (length (cadr s)))) sections))) ; header + each section title + rows
              3 (* 1.5 ht) ht)
        acol (vla-getinterfaceobject acObj (strcat "AutoCAD.AcCmColor." (substr (vla-get-version acObj) 1 2))))
  (vlax-put tab 'Direction n)
  
  (foreach rowType '(2 4 1) ; 2=Title, 4=Data, 1=Header
	(vla-SetTextStyle  tab rowType (getvar "TEXTSTYLE")) ; use string, not symbol
	(vla-SetTextHeight tab rowType ht)
  )

;   (mapcar '(lambda (x) (vla-SetTextStyle tab x (getvar 'textstyle)) (vla-SetTextHeight tab x ht)) '(2 4 1))
  (vla-put-HorzCellMargin tab (* 0.14 ht))
  (vla-put-VertCellMargin tab (* 0.14 ht))

  ;; Flatten rows
  (setq flatRows (list (list nil "Width" "Length" "Pcs.")))
  (foreach sec sections
    (setq flatRows (append flatRows (list (list nil (car sec) "" ""))))
    (foreach r (cadr sec) ; r = (Color W H Count)
      (setq flatRows (append flatRows (list (list (car r)                                 ; color index
                                                  (if (numberp (nth 1 r)) (rtos (nth 1 r) 2 6) (nth 1 r)) ; W
                                                  (if (numberp (nth 2 r)) (rtos (nth 2 r) 2 6) (nth 2 r)) ; H
                                                  (nth 3 r)))))                                ; Count
    )
  )
  
  ;; Set column widths
  (recdims-fitcols tab flatRows ht)
  
  ;; Fill cells
  (setq row 0)
  (foreach r flatRows
    (vla-SetRowHeight tab row (* 1.5 ht))
    (setq col 0)
    (foreach c (cdr r)
      (vla-SetText tab row col (vl-princ-to-string c))
      (if (car r)
        (progn (if (/= (vla-get-ColorIndex acol) (car r)) (vla-Put-ColorIndex acol (car r)))
               (vla-SetCellContentColor tab row col acol)))
      (setq col (1+ col)))
    (setq row (1+ row)))
  tab
)