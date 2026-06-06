;;; ------------------------------------------------------------
;;; Contour Builder (plain AutoCAD)
;;; Author: ChatGPT (ehab-edition)
;;; Command: CONTOURBUILD
;;; ------------------------------------------------------------
;;; Features:
;;; - Inputs: closed boundary, scattered 3D points and/or 3D polylines
;;; - Grid-based IDW interpolation (k nearest, p=2)
;;; - Marching Squares for contour extraction
;;; - Segment stitching into polylines
;;; - Keeps results inside boundary
;;; ------------------------------------------------------------

(vl-load-com)

;; ---------------- Utility ----------------

(defun dist2D (p q) (distance p q))
(defun lerp (a b t) (+ a (* t (- b a))))
(defun lerp-pt (p q t) (list (lerp (car p) (car q) t) (lerp (cadr p) (cadr q) t) 0.0))

(defun bbox-of-pts (pts / xs ys)
  (setq xs (mapcar 'car pts)
        ys (mapcar 'cadr pts))
  (list (list (apply 'min xs) (apply 'min ys) 0.0)
        (list (apply 'max xs) (apply 'max ys) 0.0))
)

;; Ray casting: point in polygon (2D)
(defun point-in-poly? (pt poly / cnt i j pi pj xi yi xj yj)
  (setq cnt 0
        i 0
        j (1- (length poly)))
  (while (< i (length poly))
    (setq pi (nth i poly)
          pj (nth j poly)
          xi (car pi) yi (cadr pi)
          xj (car pj) yj (cadr pj))
    (if (and (/= yi yj)
             (<= (min yi yj) (cadr pt))
             (<  (cadr pt) (max yi yj)))
      (let ((xint (+ xi (* (- (cadr pt) yi) (/ (- xj xi) (- yj yi))))))
        (if (< (car pt) xint) (setq cnt (1+ cnt)))
      )
    )
    (setq j i i (1+ i))
  )
  (= 1 (rem cnt 2))
)

;; Ensure/Make layer
(defun ensure-layer (name color / tbl)
  (if (not (tblsearch "LAYER" name))
    (entmake
      (list (cons 0 "LAYER")
            (cons 2 name)
            (cons 70 0)
            (cons 62 color)
            (cons 6 "CONTINUOUS")))
  )
  (setvar 'CLAYER name)
)

;; Read LWPolyline/Polyline vertices (2D), returns list of (x y 0)
(defun read-poly-2d-verts (e / d v pts n bul)
  (setq d (entget e))
  (cond
    ((= (cdr (assoc 0 d)) "LWPOLYLINE")
     (setq n (cdr (assoc 90 d)))
     (setq v (vl-remove-if-not '(lambda (x) (= (car x) 10)) d))
     (setq pts (mapcar '(lambda (it) (list (cadr it) (caddr it) 0.0)) v))
    )
    ((= (cdr (assoc 0 d)) "POLYLINE")
     ;; classic 2D poly
     (setq pts '())
     (setq v (entnext e))
     (while (and v (/= (cdr (assoc 0 (entget v))) "SEQEND"))
       (setq pts (cons (list (cdr (assoc 10 (entget v)))
                             (cdr (assoc 20 (entget v)))
                             0.0) pts))
       (setq v (entnext v))
     )
     (setq pts (reverse pts))
    )
  )
  pts
)

;; Read 3D Polyline vertices (return list of 3D points)
(defun read-3dpoly-verts (e / d v pts)
  (setq d (entget e))
  (if (and (= (cdr (assoc 0 d)) "POLYLINE")
           (= (logand (cdr (assoc 70 d)) 8) 8)) ; 3D flag
    (progn
      (setq pts '())
      (setq v (entnext e))
      (while (and v (/= (cdr (assoc 0 (entget v))) "SEQEND"))
        (setq pts (cons (list (cdr (assoc 10 (entget v)))
                              (cdr (assoc 20 (entget v)))
                              (cdr (assoc 30 (entget v)))) pts))
        (setq v (entnext v))
      )
      (setq pts (reverse pts))
    )
    nil
  )
)

;; Collect survey points from selection (POINT and 3D poly vertices)
(defun collect-xyz-from-selection ( / ss i e d pts t)
  (setq pts '())
  (prompt "\nSelect survey sources: POINTs and/or 3D Polylines: ")
  (setq ss (ssget '((0 . "POINT,POLYLINE"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i)
              d (entget e))
        (cond
          ((= (cdr (assoc 0 d)) "POINT")
           (setq pts (cons (list (cdr (assoc 10 d))
                                 (cdr (assoc 20 d))
                                 (cdr (assoc 30 d))) pts)))
          ((and (= (cdr (assoc 0 d)) "POLYLINE")
                (= (logand (cdr (assoc 70 d)) 8) 8))
           (setq t (read-3dpoly-verts e))
           (if t (setq pts (append pts t))))
        )
        (setq i (1+ i))
      )
      (setq pts (reverse pts))
    )
  )
  pts
)

;; ------------- IDW Interpolation -------------

(defun k-nearest (xy points k / sorted)
  (setq sorted
        (vl-sort points
          (function
            (lambda (a b)
              (< (distance xy (list (car a) (cadr a) 0.0))
                 (distance xy (list (car b) (cadr b) 0.0)))))))
  (if (> (length sorted) k) (setq sorted (vl-remove-if 'null (subseq sorted 0 k))))
  sorted
)

;; IDW z at (x,y): k nearest, power p
(defun idw-z (xy points k p / neigh d w num den)
  (setq neigh (k-nearest xy points k))
  (setq num 0.0 den 0.0)
  (foreach P neigh
    (setq d (max 1e-9 (distance xy (list (car P) (cadr P) 0.0))))
    (setq w (/ 1.0 (expt d p)))
    (setq num (+ num (* w (caddr P))))
    (setq den (+ den w))
  )
  (if (<= den 0.0) 0.0 (/ num den))
)

;; ------------- Marching Squares -------------

;; Edge interpolation helper: returns intersection point between (p1,z1)-(p2,z2) at level L
(defun ms-edge (p1 z1 p2 z2 L / t)
  (cond
    ((= z1 z2) (list (/ (+ (car p1) (car p2)) 2.0) (/ (+ (cadr p1) (cadr p2)) 2.0) 0.0))
    (T (setq t (/ (- L z1) (- z2 z1)))
       (lerp-pt p1 p2 t))
  )
)

;; For a single cell (p00,p10,p11,p01) (clockwise), z00,z10,z11,z01 – return 0,1 or 2 segment(s)
(defun ms-cell (p00 p10 p11 p01 z00 z10 z11 z01 L / idx segs)
  ;; bit order: p00=1, p10=2, p11=4, p01=8 (inside if z>=L)
  (setq idx 0)
  (if (>= z00 L) (setq idx (+ idx 1)))
  (if (>= z10 L) (setq idx (+ idx 2)))
  (if (>= z11 L) (setq idx (+ idx 4)))
  (if (>= z01 L) (setq idx (+ idx 8)))
  (setq segs '())
  (cond
    ;; trivial
    ((or (= idx 0) (= idx 15)) segs)

    ;; 1-edge cases
    ((member idx '(1 14))
     (setq segs (list (list (ms-edge p00 z00 p10 z10 L) (ms-edge p00 z00 p01 z01 L)))))
    ((member idx '(2 13))
     (setq segs (list (list (ms-edge p10 z10 p00 z00 L) (ms-edge p10 z10 p11 z11 L)))))
    ((member idx '(4 11))
     (setq segs (list (list (ms-edge p11 z11 p10 z10 L) (ms-edge p11 z11 p01 z01 L)))))
    ((member idx '(8 7))
     (setq segs (list (list (ms-edge p01 z01 p00 z00 L) (ms-edge p01 z01 p11 z11 L)))))

    ;; ambiguous 2-edge cases
    ((member idx '(3 12))
     (setq segs (list
                 (list (ms-edge p00 z00 p01 z01 L) (ms-edge p10 z10 p11 z11 L)))))
    ((member idx '(6 9))
     (setq segs (list
                 (list (ms-edge p00 z00 p10 z10 L) (ms-edge p01 z01 p11 z11 L)))))
    ((= idx 5)
     (setq segs (list
                 (list (ms-edge p00 z00 p10 z10 L) (ms-edge p01 z01 p11 z11 L)))))
    ((= idx 10)
     (setq segs (list
                 (list (ms-edge p00 z00 p01 z01 L) (ms-edge p10 z10 p11 z11 L)))))
  )
  segs
)

;; ------------- Segment stitching -------------

(defun close-enough? (a b tol)
  (and (<= (abs (- (car a) (car b))) tol)
       (<= (abs (- (cadr a) (cadr b))) tol)))

(defun join-segments (segs tol / chains changed s a b used hit idx i)
  ;; segs: list of (p1 p2). returns list of chains [(p0 p1 ... pn) ...]
  (setq chains '())
  (foreach s segs
    (setq chains (cons (list (car s) (cadr s)) chains)))
  ;; repeatedly merge chains with touching endpoints
  (setq changed T)
  (while changed
    (setq changed nil)
    (setq i 0)
    (while (< i (length chains))
      (setq a (nth i chains))
      (setq idx (1+ i))
      (while (< idx (length chains))
        (setq b (nth idx chains))
        (setq hit nil)
        (cond
          ((close-enough? (last a) (car b) tol)
           (setq a (append a (cdr b))) (setq hit T))
          ((close-enough? (last a) (last b) tol)
           (setq a (append a (reverse (butlast b))) ) (setq hit T))
          ((close-enough? (car a) (car b) tol)
           (setq a (append (reverse (cdr a)) b)) (setq hit T))
          ((close-enough? (car a) (last b) tol)
           (setq a (append b (cdr a))) (setq hit T))
        )
        (if hit
          (progn
            (setq chains (vl-remove b chains))
            (setq chains (subst a (nth i chains) chains))
            (setq changed T idx (length chains)) ; break inner
          )
          (setq idx (1+ idx))
        )
      )
      (setq i (1+ i))
    )
  )
  chains
)

;; ------------- Draw helpers -------------

(defun make-lwpoly (pts closed? / e d)
  (if (> (length pts) 1)
    (progn
      (entmake
        (append
          (list (cons 0 "LWPOLYLINE")
                (cons 100 "AcDbEntity")
                (cons 8 (getvar 'CLAYER))
                (cons 100 "AcDbPolyline")
                (cons 90 (length pts))
                (cons 70 (if closed? 1 0)))
          (apply 'append
                 (mapcar '(lambda (p) (list (cons 10 (list (car p) (cadr p))) (cons 42 0.0)))
                         pts))
        )
      )
    )
  )
)

;; ------------- Main command -------------

(defun c:CONTOURBUILD ( / bndEnt bPts inClosed pts gridS ivl zmin zmax auto mmx mmm
                         bb xmin xmax ymin ymax x y ix iy nx ny levels
                         k p tol labelEvery labelH inside? nodes z00 z10 z11 z01
                         p00 p10 p11 p01 segs allSegs lvl chains mid)

  (princ "\n--- Contour Builder ---")

  ;; 1) Boundary
  (setq bndEnt (car (entsel "\nSelect CLOSED boundary (LWPolyline/Polyline): ")))
  (if (null bndEnt) (progn (prompt "\nNo boundary.") (exit)))
  (setq bPts (read-poly-2d-verts bndEnt))
  (if (or (null bPts) (< (length bPts) 3))
    (progn (prompt "\nInvalid boundary.") (exit))
  )
  (setq inClosed (point-in-poly? (car bPts) bPts)) ;; dummy check to force load

  ;; 2) Survey points
  (setq pts (collect-xyz-from-selection))
  (if (or (null pts) (< (length pts) 3))
    (progn (prompt "\nNeed at least 3 points.") (exit))
  )

  ;; 3) Grid + IDW settings
  (setq gridS (getreal "\nGrid size (e.g., 2.0): "))
  (if (or (null gridS) (<= gridS 0.0)) (setq gridS 2.0))
  (setq k (getint "\nIDW k-nearest (default 12): "))
  (if (or (null k) (< k 3)) (setq k 12))
  (setq p (getreal "\nIDW power p (default 2.0): "))
  (if (or (null p) (<= p 0.0)) (setq p 2.0))

  ;; 4) Contour interval/min/max
  (setq ivl (getreal "\nContour interval (e.g., 0.5): "))
  (if (or (null ivl) (<= ivl 0.0)) (setq ivl 0.5))

  (setq mmx (apply 'min (mapcar 'caddr pts)))
  (setq mmm (apply 'max (mapcar 'caddr pts)))

  (initget "Auto Manual")
  (setq auto (getkword (strcat "\nLevel range [Auto/Manual] <Auto>: ")))
  (if (or (null auto) (= auto "Auto"))
    (progn (setq zmin (fix (/ mmx ivl))) (setq zmin (* zmin ivl))
           (setq zmax (fix (/ mmm ivl))) (setq zmax (* zmax ivl)))
    (progn
      (setq zmin (getreal (strcat "\nMin level <" (rtos mmx 2 3) ">: ")))
      (if (null zmin) (setq zmin mmx))
      (setq zmax (getreal (strcat "\nMax level <" (rtos mmm 2 3) ">: ")))
      (if (null zmax) (setq zmax mmm))
    )
  )

  (if (>= zmin zmax) (progn (prompt "\nMin >= Max.") (exit)))

  ;; 5) Label options
  (setq labelEvery (getint "\nLabel every Nth vertex (0 = no labels) <0>: "))
  (if (null labelEvery) (setq labelEvery 0))
  (setq labelH (if (> labelEvery 0) (getreal "\nLabel text height <0.25>: ") 0.25))
  (if (null labelH) (setq labelH 0.25))

  ;; 6) Prepare grid over boundary bbox
  (setq bb (bbox-of-pts bPts))
  (setq xmin (car  (car bb))
        ymin (cadr (car bb))
        xmax (car  (cadr bb))
        ymax (cadr (cadr bb)))

  (setq nx (1+ (fix (/ (- xmax xmin) gridS))))
  (setq ny (1+ (fix (/ (- ymax ymin) gridS))))

  (ensure-layer "CONTOURS" 7) ;; white

  ;; Precompute node Zs (IDW) to speed ms-cell
  (prompt "\nInterpolating grid (IDW)...")
  (setq nodes (make-array (list nx ny)))
  (setq ix 0)
  (while (< ix nx)
    (setq x (+ xmin (* ix gridS)))
    (setq iy 0)
    (while (< iy ny)
      (setq y (+ ymin (* iy gridS)))
      (aset nodes (list ix iy) (idw-z (list x y 0.0) pts k p))
      (setq iy (1+ iy))
    )
    (setq ix (1+ ix))
  )

  ;; Build contour levels
  (setq levels '())
  (setq y zmin)
  (while (<= y zmax)
    (setq levels (cons y levels))
    (setq y (+ y ivl))
  )
  (setq levels (reverse levels))

  (setq tol (* 0.25 gridS)) ;; stitching tol

  ;; For each level: march all cells, gather segments, join, then draw
  (foreach L levels
    (prompt (strcat "\nLevel " (rtos L 2 3) " ..."))
    (setq allSegs '())
    (setq ix 0)
    (while (< ix (1- nx))
      (setq x (+ xmin (* ix gridS)))
      (setq iy 0)
      (while (< iy (1- ny))
        (setq y (+ ymin (* iy gridS)))
        ;; corners (clockwise)
        (setq p00 (list x        y        0.0))
        (setq p10 (list (+ x gridS) y     0.0))
        (setq p11 (list (+ x gridS) (+ y gridS) 0.0))
        (setq p01 (list x        (+ y gridS)    0.0))
        ;; z
        (setq z00 (aref nodes (list ix     iy)))
        (setq z10 (aref nodes (list (1+ ix) iy)))
        (setq z11 (aref nodes (list (1+ ix) (1+ iy))))
        (setq z01 (aref nodes (list ix     (1+ iy))))
        ;; segments for this cell
        (setq segs (ms-cell p00 p10 p11 p01 z00 z10 z11 z01 L))
        (if segs (setq allSegs (append allSegs segs)))
        (setq iy (1+ iy))
      )
      (setq ix (1+ ix))
    )

    ;; Stitch segments to chains
    (setq chains (join-segments allSegs tol))

    ;; Keep only chains whose midpoint is inside boundary; Draw
    (foreach ch chains
      (if (> (length ch) 1)
        (progn
          (setq mid (nth (fix (/ (length ch) 2)) ch))
          (if (point-in-poly? mid bPts)
            (progn
              (make-lwpoly ch nil)
              (if (> labelEvery 0)
                (progn
                  (setq i 0)
                  (while (< i (length ch))
                    (if (= 0 (rem i labelEvery))
                      (entmake (list (cons 0 "TEXT")
                                     (cons 10 (nth i ch))
                                     (cons 40 labelH)
                                     (cons 1 (rtos L 2 2))
                                     (cons 7 (getvar 'TEXTSTYLE))
                                     (cons 8 (getvar 'CLAYER))
                                     (cons 72 0) (cons 73 2))))
                    (setq i (1+ i))
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  (prompt "\nDone. Contours created on layer CONTOURS.")
  (princ)
)
