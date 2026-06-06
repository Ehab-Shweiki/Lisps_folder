;;; ------------------------------------------------------------
;;; Rectangle counter (Exact + Near) with scaled table
;;; Based on Stefan M. (2015) code, extended by ChatGPT (2025-08-13)
;;; Command: RECDIMS+
;;; ------------------------------------------------------------

(vl-load-com)

;; ---------------- User defaults (persist across sessions) ------------
(if (not *RECDIMS_SCALE*) (setq *RECDIMS_SCALE* 1.0))    ; table size multiplier on top of CANNOSCALEVALUE
(if (not *RECDIMS_LTOL*)  (setq *RECDIMS_LTOL*  0.5))   ; length tolerance (drawing units) for “near”
(if (not *RECDIMS_ATOL*)  (setq *RECDIMS_ATOL*  2.0))    ; angle tolerance (degrees) for “near”
(if (not *RECDIMS_MODE*)  (setq *RECDIMS_MODE*  "BOTH")) ; EXACT / NEAR / BOTH

;; ---------------- Small math helpers ----------------
(defun v- (a b) (mapcar '- a b))
(defun v+ (a b) (mapcar '+ a b))
(defun vdot (a b / ax ay az bx by bz)
  ;; Ensure both are 3D
  (setq ax (car a)  ay (cadr a)  az (if (caddr a) (caddr a) 0.0))
  (setq bx (car b)  by (cadr b)  bz (if (caddr b) (caddr b) 0.0))
  (+ (* ax bx) (* ay by) (* az bz))
)
(defun vlen2 (v) (vdot v v))
(defun vlen  (v) (sqrt (max 0.0 (vlen2 v))))
(defun acos (x)
  ;; Clamp x to [-1, 1] to avoid domain errors
  (setq x (max -1.0 (min 1.0 x)))
  (/ (atan (sqrt (- 1.0 (* x x))) x) 1.0) ; result in radians
)
(defun vang-between (a b)                         ; angle between vectors (deg)
  (if (or (equal (vlen a) 0.0 1e-12) (equal (vlen b) 0.0 1e-12))
    0.0
    (setq a (/ (vdot a b) (max 1e-12 (* (vlen a) (vlen b))))
          a (max -1.0 (min 1.0 a))
          a (* 180.0 (/ (acos a) pi))
    )
  )
)
(defun almost= (x y tol) (<= (abs (- x y)) tol))

;; ------------- Color getter (entity or layer) -------------
(defun ent-color-index (ename / d layd c)
  (setq d (entget ename))
  (cond
    ((setq c (cdr (assoc 62 d))) (if (zerop c) 7 c))
    ((setq layd (tblsearch "layer" (cdr (assoc 8 d))))
     (setq c (cdr (assoc 62 layd))) (if (zerop c) 7 c))
    (t 7)
  )
)

;; ------------- Exact rectangle test (original, boxed) -------------
(defun rectangle_dims_exact (edata / l)
  (setq l (mapcar 'cdr (vl-remove-if '(lambda (x) (/= (car x) 10)) edata)))
  (if
    (and
      (or
        (= 1 (logand (cdr (assoc 70 edata)) 1))
        (equal (car l) (last l) 1e-8)
      )
      (equal (distance  (car   l) (caddr l)) (distance  (cadr   l) (cadddr l)) 1e-8)
      (equal (mapcar '- (cadr  l) (car   l)) (mapcar '- (caddr  l) (cadddr l)) 1e-8)
      (equal (mapcar '- (caddr l) (cadr  l)) (mapcar '- (cadddr l) (car    l)) 1e-8)
    )
    (vl-sort (list (distance (car l) (cadr l)) (distance (cadr l) (caddr l))) '<)
  )
)

;; ------------- Near rectangle test (tolerant) -------------
;; Returns (list w h) sorted if near-rectangle, else nil.
;; Rules: closed poly (4 or 5 vertices), opposite sides ~equal within LTOL,
;; adjacent sides ~perpendicular within ATOL, opposite sides ~parallel within ATOL.
(defun rectangle_dims_near (edata ltol atol / pts closed p0 p1 p2 p3 p4 v01 v12 v23 v30 a12 a03 ok)

  (setq pts (mapcar 'cdr (vl-remove-if '(lambda (x) (/= (car x) 10)) edata)))
  (setq closed (or (= 1 (logand (cdr (assoc 70 edata)) 1))
                   (equal (car pts) (last pts) 1e-6)))
  (if (and closed (member (length pts) '(4 5)))
    (progn
      ;; normalize 4 corners p0..p3
      (if (= (length pts) 5) (setq pts (reverse (cdr (reverse pts))))) ; drop duplicate last if present
      (setq p0 (nth 0 pts) p1 (nth 1 pts) p2 (nth 2 pts) p3 (nth 3 pts))
      (setq v01 (v- p1 p0) v12 (v- p2 p1) v23 (v- p3 p2) v30 (v- p0 p3))

      (setq a12 (vang-between v01 v12)) ; interior angle at p1
      (setq a03 (vang-between v23 v30)) ; interior angle at p3

      (setq ok
        (and
          ;; near perpendicular at corners
          (almost= a12 90.0 atol)
          (almost= a03 90.0 atol)
          ;; opposite sides parallel
          (or (almost= (vang-between v01 v23) 0.0 atol) (almost= (vang-between v01 v23) 180.0 atol))
          (or (almost= (vang-between v12 v30) 0.0 atol) (almost= (vang-between v12 v30) 180.0 atol))
          ;; opposite side lengths equal within ltol

          (almost= (vlen v01) (vlen v23) ltol)
          (almost= (vlen v12) (vlen v30) ltol)
        )
      )
      (if ok
        (vl-sort (list (vlen v01) (vlen v12)) '<)
        nil
      )
    )
  )
)

;; ------------- Table builder (supports sections) -------------
;; sections is a list of (title lst) where lst rows are (W H Count ColorIndex)
(defun insert_table_sections (sections inspt / acObj acDoc space ht pct n tab acol totalRows i colWidths style txh row col colwHdr)
  (setq acObj (vlax-get-acad-object)
        acDoc (vla-get-ActiveDocument acObj)
        space (vlax-get acDoc (if (= 1 (getvar 'cvport)) 'PaperSpace 'ModelSpace)))
;;
  (setq sections (list
    (list "RECTANGLES (EXACT)" listExact)
    (list (strcat "RECTANGLES (NEAR)  LTOL=" (rtos *RECDIMS_LTOL* 2 3) ", ATOL=" (rtos *RECDIMS_ATOL* 2 2) "°") listNear)))
;;
  ;; Base text height scaled by cannoscalevalue and user multiplier
  (setq ht (/ (* 2.5 *RECDIMS_SCALE*) (max 1e-6 (getvar 'cannoscalevalue))))
  (setq pct (trans inspt 1 0)
        n   (trans '(1 0 0) 1 0 T))
  
  ;; Row count: 1 header row + for each section: 1 title row + len(lst)
  (setq totalRows 1)
  (foreach sec sections (setq totalRows (+ totalRows 1 (length (cadr sec)))))

  (setq tab (vla-AddTable space (vlax-3d-point pct) totalRows 3 (* 1.5 ht) ht))
  (setq acol (vla-getinterfaceobject acObj (strcat "AutoCAD.AcCmColor." (substr (vla-get-version acObj) 1 2))))
  (vlax-put tab 'Direction n)

  ;; edata Text styles/heights
  (mapcar
    '(lambda (rowType) (vla-SetTextStyle tab rowType (getvar 'textstyle)) (vla-SetTextHeight tab rowType ht))
    '(2 4 1)
  )
  (vla-put-HorzCellMargin tab (* 0.14 ht))
  (vla-put-VertCellMargin tab (* 0.14 ht))

  ;; Build a flattened list of rows: first the master header, then section titles, then rows
  (setq flatRows nil)
  (setq flatRows (cons (list nil "Width" "Length" "Pcs.") flatRows))
  (foreach sec sections
    (setq flatRows (append flatRows (list (list nil (car sec) "" ""))))
    (foreach r (cadr sec) ; r = (W H Pcs Color)
      (setq flatRows (append flatRows (list (list (nth 3 r) (rtos (nth 0 r) 2 6) (rtos (nth 1 r) 2 6) (nth 2 r)))))
    )
  )

  ;; Column widths to fit content
  (setq i 0)
  (repeat 3
    (vla-SetColumnWidth tab i
      (apply 'max
        (mapcar
          '(lambda (row)
             ((lambda (txb) (+ (abs (- (cadr txb) (car txb))) (* 2.0 ht)))
              (textbox (list (cons 1 (vl-princ-to-string (nth (1+ i) row))) (cons 7 (getvar 'textstyle)) (cons 40 ht)))
             )
           )
          flatRows
        )
      )
    )
    (setq i (1+ i))
  )

  ;; Fill cells
  (setq row 0)
  (foreach r flatRows
    (vla-SetRowHeight tab row (* 1.5 ht))
    (setq col 0)
    (foreach c (cdr r)
      (vla-SetText tab row col (vl-princ-to-string c))
      (if (car r) ; colorize if a ColorIndex is provided
        (progn
          (if (/= (vla-get-ColorIndex acol) (car r)) (vla-put-ColorIndex acol (car r)))
          (vla-SetCellContentColor tab row col acol)
        )
      )
      (setq col (1+ col))
    )
    (setq row (1+ row))
  )
  tab
)

;; ------------- Main command -------------
(defun C:RECDIMS+ (/ *error* acObj acDoc space ss e i dims c rExact rNear modeChoice inspt ltol atol acVer)

  (setq acObj (vlax-get-acad-object)
        acDoc (vla-get-ActiveDocument acObj)
        space (vlax-get acDoc (if (= 1 (getvar 'cvport)) 'PaperSpace 'ModelSpace)))

  (vla-StartUndoMark acDoc)

  (defun *error* (msg)
    (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))
         (princ (strcat "\nError: " msg)))
    (vla-EndUndoMark acDoc)
    (princ)
  )

  ;; --- Options: Scale, Mode, Tolerances
  (princ (strcat
    "\nCurrent: Scale=" (rtos *RECDIMS_SCALE* 2 3)
    ", Mode=" *RECDIMS_MODE*
    ", LTOL=" (rtos *RECDIMS_LTOL* 2 3)
    ", ATOL=" (rtos *RECDIMS_ATOL* 2 2) " deg"
  ))
  (initget "Scale Mode Tolerances Continue")
  (while (eq (setq ans (getkword "\n[Scale/Mode/Tolerances/Continue] <Continue>: ")) ""); do nothing
  )
  (cond
    ((= ans "Scale")
      (initget 6)
      (if (setq s (getreal (strcat "\nTable scale multiplier <" (rtos *RECDIMS_SCALE* 2 3) ">: ")))
        (setq *RECDIMS_SCALE* s)))
    ((= ans "Mode")
      (initget "EXACT NEAR BOTH")
      (if (setq m (getkword (strcat "\nChoose mode [EXACT/NEAR/BOTH] <" *RECDIMS_MODE* ">: ")))
        (setq *RECDIMS_MODE* m)
      )
    )
    ((= ans "Tolerances")
      (initget 6)
      (if (setq l (getreal (strcat "\nLength tolerance (drawing units) <" (rtos *RECDIMS_LTOL* 2 3) ">: ")))
        (setq *RECDIMS_LTOL* l)
      )
      (initget 6)
      (if (setq a (getreal (strcat "\nAngle tolerance (deg) <" (rtos *RECDIMS_ATOL* 2 2) ">: ")))
        (setq *RECDIMS_ATOL* a)
      )
    )
  )
  
  ;; Selection
  (if (setq ss (ssget '((0 . "LWPOLYLINE") (-4 . "<OR") (90 . 4) (90 . 5) (-4 . "OR>"))))
    (progn
      (repeat (setq i (sslength ss))
        (setq e (ssname ss (setq i (1- i))))
        (setq c (ent-color-index e))
        ;; EXACT
        (if (member *RECDIMS_MODE* '("EXACT" "BOTH"))
          (if (setq dims (rectangle_dims_exact (entget e)))
            (if (setq old (vl-some '(lambda (d) (if (equal (cdr d) (cons c dims) 1e-8) d)) rExact))
              (setq rExact (subst (cons (1+ (car old)) (cons c dims)) old rExact))
              (setq rExact (cons (cons 1 (cons c dims)) rExact))
            )
          )
        )
        ;; NEAR (only if not exact already counted, to avoid duplicates)
        (if (member *RECDIMS_MODE* '("NEAR" "BOTH"))
          (progn
            (setq dimsNear (rectangle_dims_near (entget e) *RECDIMS_LTOL* *RECDIMS_ATOL*))
            (if (and dimsNear
                     (or (not (member *RECDIMS_MODE* '("BOTH"))) ; if NEAR only, we take all
                         (not (rectangle_dims_exact (entget e)))  ; if BOTH, skip ones already exact
                     )
                )
              (if (setq oldN (vl-some '(lambda (d) (if (equal (cdr d) (cons c dimsNear) 1e-8) d)) rNear))
                (setq rNear (subst (cons (1+ (car oldN)) (cons c dimsNear)) oldN rNear))
                (setq rNear (cons (cons 1 (cons c dimsNear)) rNear))
              )
            )
          )
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
          '(lambda (a b) (< (car a) (car b))))
      )
      (setq listNear
        (vl-sort
          (vl-sort
            (vl-sort
              (mapcar '(lambda (a) (list (cadr a) (caddr a) (cadddr a) (car a))) rNear)
              '(lambda (a b) (< (caddr a) (caddr b))))
            '(lambda (a b) (< (cadr a) (cadr b))))
          '(lambda (a b) (< (car a) (car b))))
      )
	;;
	)
    (progn
    ;;
      ;; Insert table
      (if (setq inspt (getpoint "\nSpecify table insert point: "))
        (progn
          (cond
            ((= *RECDIMS_MODE* "EXACT")
              (insert_table_sections
                (list (list "RECTANGLES (EXACT)" listExact))
                inspt))
            ((= *RECDIMS_MODE* "NEAR")
              (insert_table_sections
                (list (list (strcat "RECTANGLES (NEAR)  LTOL=" (rtos *RECDIMS_LTOL* 2 3) ", ATOL=" (rtos *RECDIMS_ATOL* 2 2) "°") listNear))
                inspt))
            (t ; BOTH
              (insert_table_sections
                (list
                  (list "RECTANGLES (EXACT)" listExact)
                  (list (strcat "RECTANGLES (NEAR)  LTOL=" (rtos *RECDIMS_LTOL* 2 3) ", ATOL=" (rtos *RECDIMS_ATOL* 2 2) "°") listNear))
                inspt))
          )
        )
      )
    )
    (princ "\nNo polylines found (need 4-vertex LWPOLYLINEs).")
  )

  (vla-EndUndoMark acDoc)
  (princ)
)

(princ "\nType RECDIMS+ to start (options: Scale/Mode/Tolerances).")
