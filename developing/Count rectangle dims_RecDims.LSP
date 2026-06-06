;;; ------------------------------------------------------------
;;; Rectangle counter (Exact + Near + NonRect) with scaled table
;;; - Simplifies polylines by removing extra collinear vertices
;;; - Modes: EXACT / NEAR / BOTH / NONRECT / ALL
;;; ------------------------------------------------------------
(vl-load-com)

;; --------- User defaults (persist across sessions) ----------
(if (not *RECDIMS_SCALE*)   (setq *RECDIMS_SCALE* 10.0))   ; table size multiplier
(if (not *RECDIMS_LTOL*)    (setq *RECDIMS_LTOL*  0.5))  ; length tolerance (drawing units) for NEAR
(if (not *RECDIMS_ATOL*)    (setq *RECDIMS_ATOL*  2.0))   ; angle tolerance (deg) for NEAR
(if (not *RECDIMS_COLTOL*)  (setq *RECDIMS_COLTOL* 1e-6)) ; collinearity tolerance (perp distance)
(if (not *RECDIMS_COLANG*)  (setq *RECDIMS_COLANG* 1.0)) ; angle tol (deg) around 180° for straightness
(if (not *RECDIMS_MODE*)    (setq *RECDIMS_MODE*  "ALL")) ; EXACT/NEAR/BOTH/NONRECT/ALL


;; --------- Math helpers ----------
(defun v- (a b) (mapcar '- a b))
(defun vdot (a b / ax ay az bx by bz)
  ;; Ensure both are 3D
  (setq ax (car a)  ay (cadr a)  az (if (caddr a) (caddr a) 0.0))
  (setq bx (car b)  by (cadr b)  bz (if (caddr b) (caddr b) 0.0))
  (+ (* ax bx) (* ay by) (* az bz))
)
(defun vlen  (v) (sqrt (max 0.0 (vdot v v))))
(defun acos (x)
  ;; Clamp x to [-1, 1] to avoid domain errors
  (setq x (max -1.0 (min 1.0 x)))
  (/ (atan (sqrt (- 1.0 (* x x))) x) 1.0) ; radians
)

(defun vang-between (a b) ; angle between vectors (deg)
  (if (or (equal (vlen a) 0.0 1e-12) (equal (vlen b) 0.0 1e-12))
    0.0
    (* 180.0 (/ 1 pi) (acos (/ (vdot a b) (max 1e-12 (* (vlen a) (vlen b))))) )
  )
)
(defun almost= (x y tol) (<= (abs (- x y)) tol))

;; ------------- Color getter (entity or layer) -------------
(defun ent-color-index (ename / edata layd c)
  (setq edata (entget ename))
  (cond
    ((setq c (cdr (assoc 62 edata))) (if (zerop c) 7 c))
    ((setq layd (tblsearch "layer" (cdr (assoc 8 edata))))
     (setq c (cdr (assoc 62 layd))) (if (zerop c) 7 c))
    (t 7)
  )
)

;; --------- Geometry: collinearity & "between" tests on XY ----------
;; A vertex is removable if the two adjacent directions are opposite (~180°), AND the vertex lies between its neighbors (triangle equality).
(defun collinear-between? (prev mid next collen angtol / v1 v2 ang lenok angok)
  (setq v1 (mapcar '- mid prev)
        v2 (mapcar '- next mid))
  (setq ang   (vang-between v1 v2))                         ; uses your existing vang-between
  (setq angok (<= (abs (- 180.0 ang)) (max 0.0 angtol)))    ; direction ≈ opposite
  (setq lenok (<= (abs (- (+ (distance prev mid) (distance mid next))
                          (distance prev next)))
                  (max 0.0 collen)))                        ; mid is between prev & next
  (and angok lenok)
)

;; --------- Get & simplify polyline vertices ----------
(defun pline-pts (edata / pts closed)
  (setq pts (mapcar 'cdr (vl-remove-if '(lambda (x) (/= (car x) 10)) edata)))
  (setq closed (or (= 1 (logand (cdr (assoc 70 edata)) 1))
                   (equal (car pts) (last pts) 1e-6)))
  ;; drop duplicate last if present
  (if (and closed (> (length pts) 0) (equal (car pts) (last pts) 1e-9))
    (setq pts (reverse (cdr (reverse pts)))))
  (list pts closed)
)

(defun simplify-pts-collinear (pts closed coltol / out n i prev mid next)
  (setq out nil n (length pts))
  (cond
    ((< n 3) pts) ; nothing to do
    (t
      (setq i 0)
      (while (< i n)
        (setq prev (nth (rem (+ n (- i 1)) n) pts)
              mid  (nth i pts)
              next (nth (rem (+ i 1) n) pts))
        (if (and closed (collinear-between? prev mid next *RECDIMS_COLTOL* *RECDIMS_COLANG*))
          (progn ; skip mid
          )
          (setq out (cons mid out))
        )
        (setq i (1+ i))
      )
      (setq out (reverse out))
      ;; If open poly, we never remove endpoints; for closed, ensure >= 3 remain
      (if (and closed (< (length out) 3)) pts out)
    )
  )
)

;; --------- EXACT rectangle test on simplified 4 points ----------
(defun rectangle_dims_exact_from-p4 (p0 p1 p2 p3 /)
  (and
    ;; diagonals equal
    (almost= (distance p0 p2) (distance p1 p3) 1e-8)
    ;; opposite sides parallel & equal (exact test)
    (equal (v- p1 p0) (v- p2 p3) 1e-8)
    (equal (v- p2 p1) (v- p3 p0) 1e-8)
  )
  ;; Return the sorted side lengths [w h]
  (vl-sort (list (distance p0 p1) (distance p1 p2)) '<)
  nil ; Else not a rectangle
)

(defun rectangle_dims_exact (edata / data pts closed)
  (setq data (pline-pts edata) pts (car data) closed (cadr data))
  (setq pts (simplify-pts-collinear pts closed *RECDIMS_COLTOL*))
  (if (and closed (= (length pts) 4))
    (rectangle_dims_exact_from-p4 (nth 0 pts) (nth 1 pts) (nth 2 pts) (nth 3 pts))
  )
)

;; --------- NEAR rectangle test on simplified 4 points ----------
(defun rectangle_dims_near (edata ltol atol / data pts closed p0 p1 p2 p3 v01 v12 v23 v30 ok)
  (setq data (pline-pts edata) pts (car data) closed (cadr data))
  (setq pts (simplify-pts-collinear pts closed *RECDIMS_COLTOL*))
  (if (and closed (= (length pts) 4))
    (progn
      (setq p0 (nth 0 pts) p1 (nth 1 pts) p2 (nth 2 pts) p3 (nth 3 pts))
      (setq v01 (v- p1 p0) v12 (v- p2 p1) v23 (v- p3 p2) v30 (v- p0 p3))
      (setq ok
        (and
          ;; near perpendicular at corners
          (almost= (vang-between v01 v12) 90.0 atol) ; interior angle at p1
          (almost= (vang-between v23 v30) 90.0 atol) ; interior angle at p3
          ;; opposite sides parallel
          (or (almost= (vang-between v01 v23) 0.0 atol) (almost= (vang-between v01 v23) 180.0 atol))
          (or (almost= (vang-between v12 v30) 0.0 atol) (almost= (vang-between v12 v30) 180.0 atol))
          ;; opposite side lengths equal within ltol
          (almost= (distance p0 p1) (distance p2 p3) ltol)
          (almost= (distance p1 p2) (distance p3 p0) ltol)
        )
      )
      (if ok
        (vl-sort (list (distance p0 p1) (distance p1 p2)) '<))
    )
  )
)

;; --------- Table (sections) ----------
;; sections: list of (rows title & the rows).  ; rows: (W H Count ColorIndex) or ('- '-' Count ColorIndex) for nonrect
(defun insert_table_sections (sections inspt / acObj acDoc space ht pct n tabl acol flatRows i row col)
  (defun recdw-lite (val ht)
	;; estimate width ≈ 0.62 * chars * ht  + padding  ; (0.62 is a rough factor for text width to height)
	  (+ (* 0.62 (strlen (vl-princ-to-string (if val val ""))) ht)
		   (* 2.0 ht))
  )
  
  ;; Text width via (textbox), compatible across DWGs.
  ;; Tries the original minimal DXF first; falls back to full DXF; then to estimate.
  (defun recdw-box (val ht / s style txb p1 p2 w)
    (setq s     (vl-princ-to-string (if val val "")))
    (setq style (getvar "TEXTSTYLE"))

    ;; 1) Try the original minimal form you mentioned
    (setq txb
      (vl-catch-all-apply
        'textbox
        (list (list (cons 1 s) (cons 7 style) (cons 40 ht)))
      )
    )

    ;; 2) If that failed or didn’t return a proper box, try a full TEXT DXF
    (if (or (vl-catch-all-error-p txb)
            (not (and (listp txb) (= (length txb) 2)
                      (listp (car txb)) (listp (cadr txb)))))
      (setq txb
        (vl-catch-all-apply
          'textbox
          (list
            (list
              (cons 0 "TEXT")
              (cons 10 '(0.0 0.0 0.0))
              (cons 40 ht)
              (cons 7 style)
              (cons 1 s)
              (cons 50 0.0) ; rotation 0
            )
          )
        )
      )
    )

    ;; 3) Compute width or fall back to a reasonable estimate
    (cond
      ((vl-catch-all-error-p txb)
      (setq w (* 0.62 (strlen s) ht)))   ; fallback estimate
      ((and (listp txb) (= (length txb) 2))
      (setq p1 (car txb)  p2 (cadr txb))
      (setq w (abs (- (car p2) (car p1)))))
      (t
      (setq w (* 0.62 (strlen s) ht)))   ; unexpected shape → estimate
    )

    ;; Add padding roughly matching cell margins
    (+ w (* 2.0 ht))
  )
  (defun recdims-fitcols (tabl flatRows ht / col widths)
    ;; tabl:     VLA table object
    ;; flatRows: list of rows shaped as (ColorOrNil Col1 Col2 Col3)
    ;; ht:       text height used for width estimation
    (if flatRows
      (foreach col '(0 1 2)   ; 0=Width, 1=Length, 2=Pcs.
        ;; Build list of widths for this column across all rows
        (setq widths
          ;
          ; from original file
          ; (mapcar 
          ;   '(lambda (x)
          ;     ((lambda (txb) (+ (abs (- (caddr txb) (car txb))) (* 2.0 ht)))
          ;       (textbox (list (cons 1 (vl-princ-to-string x)) (cons 7 (getvar 'textstyle)) (cons 40 ht)))
          ;       )
          ;     )
          ;   col
          ; )
          ;
          (mapcar
            (function (lambda (row)
              ;; Each row has a color at index 0, so table columns are at (1+ col)
              (recdw-lite 
                (if (listp row) (nth (1+ col) row) "")  ; safeguard for short rows
                ht)))
            flatRows))

        ;; Apply the maximum width (fallback to 8*ht just in case)
        (vla-SetColumnWidth tabl col
          (if widths (apply 'max widths) (* 8.0 ht)))
      )
    )
    tabl
  )
  
  (setq acObj (vlax-get-acad-object)
        acDoc (vla-get-ActiveDocument acObj)
        space (vlax-get acDoc (if (= 1 (getvar 'cvport)) 'PaperSpace 'ModelSpace)))

  ;; Base text height scaled by cannoscalevalue and user multiplier
  (setq ht (/ (* 2.5 *RECDIMS_SCALE*) (max 1e-6 (getvar 'cannoscalevalue))))
  (setq pct (trans inspt 1 0)
        n   (trans '(1 0 0) 1 0 T)
        tabl (vla-AddTable space (vlax-3d-point pct)
              (+ 1 (apply '+ (mapcar '(lambda (s) (+ 1 (length (cadr s)))) sections))) ; header + each section title + rows
              3 (* 1.5 ht) ht)
        acol (vla-getinterfaceobject acObj (strcat "AutoCAD.AcCmColor." (substr (vla-get-version acObj) 1 2))))
  (vlax-put tabl 'Direction n)
  
  (foreach rowType '(2 4 1) ; 2=Title, 4=Data, 1=Header
	(vla-SetTextStyle  tabl rowType (getvar "TEXTSTYLE")) ; use string, not symbol
	(vla-SetTextHeight tabl rowType ht)
  )

;   (mapcar '(lambda (x) (vla-SetTextStyle tabl x (getvar 'textstyle)) (vla-SetTextHeight tabl x ht)) '(2 4 1))
  (vla-put-HorzCellMargin tabl (* 0.14 ht))
  (vla-put-VertCellMargin tabl (* 0.14 ht))

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
  (recdims-fitcols tabl flatRows ht)
  
  ;; Fill cells
  (setq row 0)
  (foreach r flatRows
    (vla-SetRowHeight tabl row (* 1.5 ht))
    (setq col 0)
    (foreach c (cdr r)
      (vla-SetText tabl row col (vl-princ-to-string c))
      (if (car r)
        (progn (if (/= (vla-get-ColorIndex acol) (car r)) (vla-Put-ColorIndex acol (car r)))
               (vla-SetCellContentColor tabl row col acol)))
      (setq col (1+ col)))
    (setq row (1+ row)))
  tabl
)

;; --------- Main command ----------
(defun C:RECDIMS (/ *error* acObj acDoc space ss i ename c dims dimsNear
                     rExact rNear rOther inspt ans s m l a coltol
                     listExact listNear listOther)

  (setq acObj (vlax-get-acad-object)
        acDoc (vla-get-ActiveDocument acObj)
        space (vlax-get acDoc (if (= 1 (getvar 'cvport)) 'PaperSpace 'ModelSpace)))
  (vla-StartUndoMark acDoc)

  (defun *error* (msg)
    (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))
         (princ (strcat "\nError: " msg)))
    (vla-EndUndoMark acDoc)
    (princ))

  ;; Options
  (princ
    (strcat
      "\nCurrent — Scale=" (rtos *RECDIMS_SCALE* 2 3)
      ", Mode=" *RECDIMS_MODE*
      ", LTOL=" (rtos *RECDIMS_LTOL* 2 3)
      ", ATOL=" (rtos *RECDIMS_ATOL* 2 2) "°"
      ", COLTOL=" (rtos *RECDIMS_COLTOL* 2 6)))
  (initget "Scale Mode Tolerances Collinear Continue")
  (while (eq (setq ans (getkword "\n[Scale/Mode/Tolerances/Collinear/Continue] <Continue>: ")) "")
  )
  (cond
    ((= ans "Scale")
      (initget 6)
      (if (setq s (getreal (strcat "\nTable scale multiplier <" (rtos *RECDIMS_SCALE* 2 3) ">: ")))
        (setq *RECDIMS_SCALE* s)))
    ((= ans "Mode")
      (initget "EXACT NEAR BOTH NONRECT ALL")
      (if (setq m (getkword (strcat "\nChoose mode [EXACT/NEAR/BOTH/NONRECT/ALL] <" *RECDIMS_MODE* ">: ")))
        (setq *RECDIMS_MODE* m)))
    ((= ans "Tolerances")
      (initget 6)
      (if (setq l (getreal (strcat "\nLength tolerance (NEAR) <" (rtos *RECDIMS_LTOL* 2 3) ">: ")))
        (setq *RECDIMS_LTOL* l))
      (initget 6)
      (if (setq a (getreal (strcat "\nAngle tolerance (deg, NEAR) <" (rtos *RECDIMS_ATOL* 2 2) ">: ")))
        (setq *RECDIMS_ATOL* a)))
    ((= ans "Collinear")
		(initget 6)
		(if (setq coltol (getreal (strcat "\nCollinearity length tolerance (|pm|+|mn|-|pn|) <" (rtos *RECDIMS_COLTOL* 2 6) ">: ")))
		  (setq *RECDIMS_COLTOL* coltol))
		(initget 6)
		(if (setq ang (getreal (strcat "\nCollinearity angle tolerance around 180° <" (rtos *RECDIMS_COLANG* 2 2) ">: ")))
		  (setq *RECDIMS_COLANG* ang))
	)
  )

  ;; Selection (all LWPOLYLINEs; we will simplify and test)
  (if (setq ss (ssget '((0 . "LWPOLYLINE"))))
    ;;
          ; (setq rExact nil rNear nil rOther nil listExact nil listNear nil listOther nil) ; reset buckets
    ;;
    (progn
      (repeat (setq i (sslength ss))
        (setq ename (ssname ss (setq i (1- i))))
        (setq c (ent-color-index ename))
        (setq dims (rectangle_dims_exact (entget ename)))
        (setq dimsNear (if (not dims) (rectangle_dims_near (entget ename) *RECDIMS_LTOL* *RECDIMS_ATOL*)))
        (cond
          ;; EXACT bucket
          ((and dims (member *RECDIMS_MODE* '("EXACT" "BOTH" "ALL")))
            (if (setq old (vl-some '(lambda (d) (if (equal (cdr d) (cons c dims) 1e-8) d)) rExact))
              (setq rExact (subst (cons (1+ (car old)) (cons c dims)) old rExact))
              (setq rExact (cons (cons 1 (cons c dims)) rExact))))
          ;; NEAR bucket (only if not exact)
          ((and dimsNear (member *RECDIMS_MODE* '("NEAR" "BOTH" "ALL")))
            (if (setq oldN (vl-some '(lambda (d) (if (equal (cdr d) (cons c dimsNear) 1e-8) d)) rNear))
              (setq rNear (subst (cons (1+ (car oldN)) (cons c dimsNear)) oldN rNear))
              (setq rNear (cons (cons 1 (cons c dimsNear)) rNear))))
          ;; NONRECT bucket
          ((member *RECDIMS_MODE* '("NONRECT" "ALL"))
            (if (setq oldO (vl-some '(lambda (d) (if (= (cdr d) c) d)) rOther))
              (setq rOther (subst (cons (1+ (car oldO)) c) oldO rOther))
              (setq rOther (cons (cons 1 c) rOther))))
        )
      )

      ;; Build display lists: (W H Count Color)
      (setq listExact
        (vl-sort
          (vl-sort
            (vl-sort
              (mapcar '(lambda (a) (list (cadr a) (caddr a) (cadddr a) (car a))) rExact)
              '(lambda (a b) (< (caddr a) (caddr b))))
            '(lambda (a b) (< (cadr a) (cadr b))))
          '(lambda (a b) (< (car a) (car b)))))
      (setq listNear
        (vl-sort
          (vl-sort
            (vl-sort
              (mapcar '(lambda (a) (list (cadr a) (caddr a) (cadddr a) (car a))) rNear)
              '(lambda (a b) (< (caddr a) (caddr b))))
            '(lambda (a b) (< (cadr a) (cadr b))))
          '(lambda (a b) (< (car a) (car b)))))
      (setq listOther
        (vl-sort
          (mapcar '(lambda (a) (list (cdr a) "-" "-" (car a))) ; (Color "-" "-" Count)
            rOther)
          '(lambda (a b) (< (car a) (car b))))           ; sort by Color
      )

      ;; Insert table
      (if (setq inspt (getpoint "\nSpecify table insert point: "))
        (cond
          ((= *RECDIMS_MODE* "EXACT")
            (insert_table_sections (list (list "RECTANGLES (EXACT)" listExact)) inspt))
          ((= *RECDIMS_MODE* "NEAR")
            (insert_table_sections (list (list (strcat "RECTANGLES (NEAR)  LTOL=" (rtos *RECDIMS_LTOL* 2 3)
                                                 ", ATOL=" (rtos *RECDIMS_ATOL* 2 2) "°") listNear)) inspt))
          ((= *RECDIMS_MODE* "NONRECT")
            (insert_table_sections (list (list "NOT RECTANGLES" listOther)) inspt))
          ((= *RECDIMS_MODE* "BOTH")
            (insert_table_sections
              (list
                (list "RECTANGLES (EXACT)" listExact)
                (list (strcat "RECTANGLES (NEAR)  LTOL=" (rtos *RECDIMS_LTOL* 2 3)
                              ", ATOL=" (rtos *RECDIMS_ATOL* 2 2) "°") listNear))
              inspt))
          (t ; ALL
            (insert_table_sections
              (list
                (list "RECTANGLES (EXACT)" listExact)
                (list (strcat "RECTANGLES (NEAR)  LTOL=" (rtos *RECDIMS_LTOL* 2 3)
                              ", ATOL=" (rtos *RECDIMS_ATOL* 2 2) "°") listNear)
                (list "NOT RECTANGLES" listOther))
              inspt))
        )
      )
    )
    (princ "\nNothing selected. Select LWPOLYLINEs.")
  )

  (vla-EndUndoMark acDoc)
  (princ)
)

(princ "\nType RECDIMS+ to start (options: Scale/Mode/Tolerances/Collinear).")