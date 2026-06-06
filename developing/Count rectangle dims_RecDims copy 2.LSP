;;; ------------------------------------------------------------
;;; Rectangle counter (Exact + Near + NonRect) with scaled table
;;; - Simplifies polylines by removing extra collinear vertices
;;; - Modes: EXACT / NEAR / BOTH / NONRECT / ALL
;;; ------------------------------------------------------------
(vl-load-com)

;; --------- User defaults (persist across sessions) ----------
(if (not *RECDIMS_SCALE*)   (setq *RECDIMS_SCALE* 1.0))   ; table size multiplier
(if (not *RECDIMS_LTOL*)    (setq *RECDIMS_LTOL*  1e-2))  ; length tol (drawing units) for NEAR
(if (not *RECDIMS_ATOL*)    (setq *RECDIMS_ATOL*  2.0))   ; angle tol (deg) for NEAR
(if (not *RECDIMS_COLTOL*)  (setq *RECDIMS_COLTOL* 1e-6)) ; collinearity tol (perp distance)
(if (not *RECDIMS_MODE*)    (setq *RECDIMS_MODE*  "BOTH")) ; EXACT/NEAR/BOTH/NONRECT/ALL

;; --------- Math helpers ----------
(defun v- (a b) (mapcar '- a b))
(defun vlen (v) (sqrt (max 0.0 (+ (* (car v) (car v)) (* (cadr v) (cadr v))))))
(defun vdot (a b) (+ (* (car a) (car b)) (* (cadr a) (cadr b))))
(defun almost= (x y tol) (<= (abs (- x y)) tol))

;; acos replacement (plain AutoLISP has no built-in acos)
(defun acos (x)
  (setq x (max -1.0 (min 1.0 x)))
  (/ (atan (sqrt (- 1.0 (* x x))) x) 1.0) ; radians
)

(defun vang-between (a b) ; angle between vectors (deg)
  (if (or (equal (vlen a) 0.0 1e-12) (equal (vlen b) 0.0 1e-12))
    0.0
    (* 180.0 (/ (acos (/ (vdot a b) (max 1e-12 (* (vlen a) (vlen b))))) pi))
  )
)

;; --------- Entity color (entity ci or layer ci; map ByBlock=0 to 7) ----------
(defun ent-color-index (ename / d layd c)
  (setq d (entget ename))
  (cond
    ((setq c (cdr (assoc 62 d))) (if (zerop c) 7 c))
    ((setq layd (tblsearch "layer" (cdr (assoc 8 d))))
     (setq c (cdr (assoc 62 layd))) (if (zerop c) 7 c))
    (t 7)
  )
)

;; --------- Geometry: collinearity & "between" tests on XY ----------
(defun perp-dist-pt-line (p a b) ; perpendicular distance from p to line a-b (2D)
  (if (equal a b 1e-12)
    (distance p a)
    (abs (/ (- (* (- (car b) (car a)) (- (cadr a) (cadr p)))
               (* (- (car a) (car p)) (- (cadr b) (cadr a))))
            (max 1e-12 (distance a b))))
  )
)

(defun param-on-seg (p a b) ; returns t in R where a + t*(b-a) projects to p
  (if (equal a b 1e-12) 0.0
    (/ (+ (* (- (car p) (car a)) (- (car b) (car a)))
          (* (- (cadr p) (cadr a)) (- (cadr b) (cadr a))))
       (max 1e-12 (+ (expt (- (car b) (car a)) 2) (expt (- (cadr b) (cadr a)) 2))))
  )
)

(defun collinear-between? (prev mid next colt / t)
  (and (<= (perp-dist-pt-line mid prev next) colt)
       (setq t (param-on-seg mid prev next))
       (and (>= t -1e-9) (<= t (+ 1.0 1e-9)))))

;; --------- Get & simplify polyline vertices ----------
(defun lwpoly-pts (e / pts closed)
  (setq pts (mapcar 'cdr (vl-remove-if '(lambda (x) (/= (car x) 10)) e)))
  (setq closed (or (= 1 (logand (cdr (assoc 70 e)) 1))
                   (equal (car pts) (last pts) 1e-6)))
  ;; drop duplicate last if present
  (if (and closed (> (length pts) 0) (equal (car pts) (last pts) 1e-9))
    (setq pts (reverse (cdr (reverse pts)))))
  (list pts closed)
)

(defun simplify-pts-collinear (pts closed colt / out n i prev mid next)
  (setq out nil n (length pts))
  (cond
    ((< n 3) pts) ; nothing to do
    (t
      (setq i 0)
      (while (< i n)
        (setq prev (nth (rem (+ n (- i 1)) n) pts)
              mid  (nth i pts)
              next (nth (rem (+ i 1) n) pts))
        (if (and closed (collinear-between? prev mid next colt))
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
    (almost= (distance p0 p2) (distance p1 p3) 1e-8)
    (equal (v- p1 p0) (v- p2 p3) 1e-8)
    (equal (v- p2 p1) (v- p3 p0) 1e-8)
    (vl-sort (list (distance p0 p1) (distance p1 p2)) '<)
  )
)

(defun rectangle_dims_exact (e / pts closed)
  (setq pts (car (lwpoly-pts e)) closed (cadr (lwpoly-pts e)))
  (setq pts (simplify-pts-collinear pts closed *RECDIMS_COLTOL*))
  (if (and closed (= (length pts) 4))
    (rectangle_dims_exact_from-p4 (nth 0 pts) (nth 1 pts) (nth 2 pts) (nth 3 pts))
  )
)

;; --------- NEAR rectangle test on simplified 4 points ----------
(defun rectangle_dims_near (e ltol atol / data pts closed p0 p1 p2 p3 v01 v12 v23 v30 ok)
  (setq data (lwpoly-pts e) pts (car data) closed (cadr data))
  (setq pts (simplify-pts-collinear pts closed *RECDIMS_COLTOL*))
  (if (and closed (= (length pts) 4))
    (progn
      (setq p0 (nth 0 pts) p1 (nth 1 pts) p2 (nth 2 pts) p3 (nth 3 pts))
      (setq v01 (v- p1 p0) v12 (v- p2 p1) v23 (v- p3 p2) v30 (v- p0 p3))
      (setq ok
        (and
          (almost= (vang-between v01 v12) 90.0 atol)
          (almost= (vang-between v23 v30) 90.0 atol)
          (or (almost= (vang-between v01 v23) 0.0 atol) (almost= (vang-between v01 v23) 180.0 atol))
          (or (almost= (vang-between v12 v30) 0.0 atol) (almost= (vang-between v12 v30) 180.0 atol))
          (almost= (distance p0 p1) (distance p2 p3) ltol)
          (almost= (distance p1 p2) (distance p3 p0) ltol)
        )
      )
      (if ok (vl-sort (list (distance p0 p1) (distance p1 p2)) '<))
    )
  )
)

;; --------- Table (sections) ----------
;; sections: list of (title rows), rows: (W H Count ColorIndex) or ('- '-' Count ColorIndex) for nonrect
(defun insert_table_sections (sections inspt / acObj acDoc space ht pct n tab acol flatRows i row col)
  (setq acObj (vlax-get-acad-object)
        acDoc (vla-get-ActiveDocument acObj)
        space (vlax-get acDoc (if (= 1 (getvar 'cvport)) 'PaperSpace 'ModelSpace)))

  (setq ht (/ (* 2.5 *RECDIMS_SCALE*) (max 1e-6 (getvar 'cannoscalevalue))))
  (setq pct (trans inspt 1 0)
        n   (trans '(1 0 0) 1 0 T)
        tab (vla-AddTable space (vlax-3d-point pct)
              (+ 1 (apply '+ (mapcar '(lambda (s) (+ 1 (length (cadr s)))) sections))) ; header + each section title + rows
              3 (* 1.5 ht) ht)
        acol (vla-getinterfaceobject acObj (strcat "AutoCAD.AcCmColor." (substr (vla-get-version acObj) 1 2))))
  (vlax-put tab 'Direction n)

  (mapcar '(lambda (t) (vla-SetTextStyle tab t (getvar 'textstyle)) (vla-SetTextHeight tab t ht)) '(2 4 1))
  (vla-put-HorzCellMargin tab (* 0.14 ht))
  (vla-put-VertCellMargin tab (* 0.14 ht))

  ;; Flatten rows
  (setq flatRows (list (list nil "Width" "Length" "Pcs.")))
  (foreach sec sections
    (setq flatRows (append flatRows (list (list nil (car sec) "" ""))))
    (foreach r (cadr sec)
      (setq flatRows (append flatRows (list (list (nth 3 r)
                                                  (if (numberp (nth 0 r)) (rtos (nth 0 r) 2 6) (nth 0 r))
                                                  (if (numberp (nth 1 r)) (rtos (nth 1 r) 2 6) (nth 1 r))
                                                  (nth 2 r)))))
    )
  )

  ;; Set column widths
  (setq i 0)
  (repeat 3
    (vla-SetColumnWidth tab i
      (apply 'max
        (mapcar
          '(lambda (row)
             ((lambda (txb) (+ (abs (- (caadr txb) (caar txb))) (* 2.0 ht)))
              (textbox (list (cons 1 (vl-princ-to-string (nth (1+ i) row)))
                             (cons 7 (getvar 'textstyle)) (cons 40 ht)))))
          flatRows)))
    (setq i (1+ i))
  )

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

;; --------- Main command ----------
(defun C:RECDIMS+ (/ *error* acObj acDoc space ss i e c dims dimsNear
                     rExact rNear rOther inspt ans s m l a colt
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
      (if (setq colt (getreal (strcat "\nCollinearity tolerance (perp dist) <" (rtos *RECDIMS_COLTOL* 2 6) ">: ")))
        (setq *RECDIMS_COLTOL* colt)))
  )

  ;; Selection (all LWPOLYLINEs; we will simplify and test)
  (if (setq ss (ssget '((0 . "LWPOLYLINE"))))
    (progn
      (repeat (setq i (sslength ss))
        (setq e (ssname ss (setq i (1- i)))  c (ent-color-index e))
        (setq dims (rectangle_dims_exact (entget e)))
        (setq dimsNear (if (not dims) (rectangle_dims_near (entget e) *RECDIMS_LTOL* *RECDIMS_ATOL*)))
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
          (mapcar '(lambda (a) (list "-" "-" (car a) (cdr a))) rOther)
          '(lambda (a b) (< (nth 3 a) (nth 3 b)))))

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
