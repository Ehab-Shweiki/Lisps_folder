;; ------------------------------------------------------------
;; GRID CUT/FILL from two contour maps (Existing vs Proposed)
;; v0.1 – distance-weighted interpolation between nearest contours
;; Author: ChatGPT (for ehab)
;; ------------------------------------------------------------
;; USAGE:
;;   Command: GRIDCUT
;;     1) Select Existing contours (LWPOLYLINE or 3D POLYLINE)
;;     2) Select Proposed contours
;;     3) Pick area (two corners) or press Enter to use auto extents
;;     4) Enter grid cell size S
;;     5) Choose whether to label values (Y/N)
;;
;; OUTPUT:
;;   Draws s×s LWPOLYLINE rectangles colored by:
;;      Red   (color 1): CUT   = Existing - Proposed > +tol
;;      Blue  (color 5): FILL  = Proposed - Existing > +tol
;;      Gray  (color 8): ~Zero (|diff| <= tol)
;;   Optionally places text with value at cell center.
;;
;; NOTES & LIMITS:
;;   - Assumes each contour polyline is constant elevation.
;;   - Works with LWPOLYLINE (2D, uses DXF 38 Elevation) and
;;     3D polylines (uses any vertex/startpoint Z).
;;   - Approximation: no TIN/breaklines. Good with clean/dense contours.
;;   - Performance: For many contours / small S, it can be slow.
;;     Start with coarser S and refine.
;; ------------------------------------------------------------

(vl-load-com)

;; --------------------------
;; Utility: robust number read
(defun str->num (s)
  (cond
    ((= (type s) 'REAL) s)
    ((= (type s) 'INT)  (float s))
    ((= (type s) 'STR)  (atof s))
    (t nil)
  )
)

;; --------------------------
;; Get elevation (Z) of a contour curve (VLA object)
;; - For LWPOLYLINE: DXF group 38 (entity elevation)
;; - For 3D Polyline: use start point Z
(defun curve->elev (vla / en ed tp z st)
  (setq en  (vlax-vla-object->ename vla)
        ed  (entget en)
        tp  (cdr (assoc 0 ed)))
  (cond
    ((eq tp "LWPOLYLINE")
     (setq z (cdr (assoc 38 ed))) ; may be nil if not set
     (if z z 0.0)
    )
    ((or (eq tp "POLYLINE") (eq tp "3DPOLY"))
     (setq st (vlax-curve-getStartPoint vla))
     (if st (caddr st) 0.0)
    )
    (T ; other curves (ARC, SPLINE) fallback to start Z
     (setq st (vlax-curve-getStartPoint vla))
     (if st (caddr st) 0.0)
    )
  )
)

;; --------------------------
;; XY distance from point to curve (ignore Z)
(defun xy-distance-to-curve (vla pt / p3 cp d)
  (setq p3 (list (car pt) (cadr pt) 0.0))
  (setq cp (vlax-curve-getClosestPointTo vla p3))
  (if cp
    (distance (list (car p3) (cadr p3) 0.0)
              (list (car cp) (cadr cp) 0.0))
    1e99
  )
)

;; --------------------------
;; Build list of contours as (vla z)
(defun collect-contours (msg / ss lst i e vla z)
  (prompt msg)
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
  (if (null ss)
    nil
    (progn
      (setq lst '() i 0)
      (while (< i (sslength ss))
        (setq e   (ssname ss i)
              vla (vlax-ename->vla-object e)
              z   (curve->elev vla))
        (setq lst (cons (list vla z) lst))
        (setq i (1+ i))
      )
      lst
    )
  )
)

;; --------------------------
;; Compute elevation at XY point from contour set by
;; distance-weighted interpolation of the two nearest contours
;; with different elevations.
(defun elevation-from-contours (contours pt / best1 best2)
  (setq best1 nil best2 nil)
  (foreach c contours
    (let* ((vla (car c))
           (z   (cadr c))
           (d   (xy-distance-to-curve vla pt)))
      ;; Keep two smallest distances, prefer distinct elevations
      (cond
        ((null best1)
         (setq best1 (list d z)))
        ((< d (car best1))
         (setq best2 best1
               best1 (list d z)))
        ((null best2)
         (setq best2 (list d z)))
        ((< d (car best2))
         (setq best2 (list d z)))
      )
    )
  )
  ;; Ensure distinct elevations; if equal, just return that z
  (cond
    ((or (null best1) (null best2)) nil)
    ((equal (cadr best1) (cadr best2) 1e-9)
     (cadr best1))
    (T
     (let* ((d1 (max 1e-9 (car best1))) ; avoid div by zero
            (d2 (max 1e-9 (car best2)))
            (z1 (cadr best1))
            (z2 (cadr best2)))
       (/ (+ (* z1 d2) (* z2 d1)) (+ d1 d2))
     )
    )
  )
)

;; --------------------------
;; Get extents (minx miny maxx maxy) from a contour list
(defun contours-extents (contours / minx miny maxx maxy once vla box ll ur)
  (setq once nil)
  (foreach c contours
    (setq vla (car c))
    (if (vlax-method-applicable-p vla 'GetBoundingBox)
      (progn
        (vla-GetBoundingBox vla 'll 'ur)
        (setq ll (vlax-safearray->list ll)
              ur (vlax-safearray->list ur))
        (if (not once)
          (progn
            (setq minx (car ll) miny (cadr ll) maxx (car ur) maxy (cadr ur))
            (setq once T))
          (progn
            (setq minx (min minx (car ll))
                  miny (min miny (cadr ll))
                  maxx (max maxx (car ur))
                  maxy (max maxy (cadr ur)))
          )
        )
      )
    )
  )
  (if once (list minx miny maxx maxy) nil)
)

;; --------------------------
;; Draw a grid cell rectangle centered at pt (XY), size s
;; and color by index (ACI). Returns entity name.
(defun draw-cell (pt s colorIndex / x y h e)
  (setq x (car pt) y (cadr pt) h (/ s 2.0))
  (entmakex
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 62 colorIndex)
      (cons 100 "AcDbPolyline")
      (cons 90 4)
      (cons 70 1) ; closed
      (cons 10 (list (- x h) (- y h))) ; LL
      (cons 10 (list (+ x h) (- y h))) ; LR
      (cons 10 (list (+ x h) (+ y h))) ; UR
      (cons 10 (list (- x h) (+ y h))) ; UL
    )
  )
)

;; --------------------------
;; Optional: label text at point
(defun place-label (pt val / txt)
  (entmakex
    (list
      (cons 0 "TEXT")
      (cons 100 "AcDbEntity")
      (cons 100 "AcDbText")
      (cons 10 (list (car pt) (cadr pt) 0.0))
      (cons 40 0.7)          ; text height (will scale with S later)
      (cons 1 (rtos val 2 3)) ; 3 decimals
      (cons 7 "Standard")
      (cons 62 8)            ; gray
      (cons 72 1)
      (cons 73 2)
    )
  )
)

;; --------------------------
;; Main command
(defun c:GRIDCUT (/ ex pr bx by p1 p2 use-auto s label?
                    ext all-ext minx miny maxx maxy x y
                    tol zex zpr diff clr center)

  (setq ex (collect-contours "\nSelect EXISTING contours: "))
  (if (null ex) (progn (prompt "\nNo Existing contours selected.") (princ) (exit)))

  (setq pr (collect-contours "\nSelect PROPOSED contours: "))
  (if (null pr) (progn (prompt "\nNo Proposed contours selected.") (princ) (exit)))

  ;; Ask area: pick 2 corners or Enter for auto extents
  (initget "Auto")
  (setq p1 (getpoint "\nPick first corner of grid area, or type [Auto]: "))
  (if (and p1 (setq p2 (getcorner p1 "\nPick opposite corner: ")))
    (progn
      (setq minx (min (car p1) (car p2))
            miny (min (cadr p1) (cadr p2))
            maxx (max (car p1) (car p2))
            maxy (max (cadr p1) (cadr p2)))
    )
    (progn
      (setq all-ext (mapcar 'contours-extents (list ex pr)))
      (setq ext (car all-ext))
      (if (and ext (cadr all-ext))
        (progn
          ;; merge extents
          (setq minx (min (nth 0 ext) (nth 0 (cadr all-ext)))
                miny (min (nth 1 ext) (nth 1 (cadr all-ext)))
                maxx (max (nth 2 ext) (nth 2 (cadr all-ext)))
                maxy (max (nth 3 ext) (nth 3 (cadr all-ext))))
        )
        (if ext
          (progn
            (setq minx (nth 0 ext) miny (nth 1 ext) maxx (nth 2 ext) maxy (nth 3 ext)))
          (progn (prompt "\nCould not determine extents.") (princ) (exit))
        )
      )
      (prompt (strcat
               "\nAuto extents: ["
               (rtos minx 2 2) ", " (rtos miny 2 2) "  to  "
               (rtos maxx 2 2) ", " (rtos maxy 2 2) "]"))
    )
  )

  ;; Grid size
  (setq s (getreal "\nGrid cell size S: "))
  (if (or (null s) (<= s 0.0))
    (progn (prompt "\nInvalid S.") (princ) (exit)))

  ;; Ask labeling
  (initget "Yes No")
  (setq label? (getkword "\nPlace numeric labels at cell centers? [Yes/No] <No>: "))
  (if (null label?) (setq label? "No"))

  ;; Tolerance for near-zero (half of 0.01*S is arbitrary; user can adjust)
  (setq tol (* 0.01 s))

  (prompt "\nComputing… This can take time for fine grids.")
  (setq y (+ miny (/ s 2.0)))
  (while (<= y maxy)
    (setq x (+ minx (/ s 2.0)))
    (while (<= x maxx)
      (setq center (list x y 0.0))
      (setq zex (elevation-from-contours ex center))
      (setq zpr (elevation-from-contours pr center))
      (if (and zex zpr)
        (progn
          (setq diff (- zex zpr)) ; + = CUT, - = FILL (Existing - Proposed)
          (cond
            ((> diff tol)  (setq clr 1)) ; red cut
            ((< diff (- tol)) (setq clr 5)) ; blue fill
            (T (setq clr 8)) ; gray ~zero
          )
          (draw-cell center s clr)
          (if (eq label? "Yes")
            (progn
              ;; scale text height wrt cell size
              (entmod (subst (cons 40 (/ s 3.0)) (assoc 40 (entget (setq tx (place-label center diff)))) (entget tx)))
            )
          )
        )
        ;; else: out of range of contours — skip cell
      )
      (setq x (+ x s))
    )
    (setq y (+ y s))
  )

  (prompt "\nDone. Red: Cut, Blue: Fill, Gray: ~Zero.")
  (princ)
)
