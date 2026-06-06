;;; ───────────────────────────────────────────────
;;;  MATRIX HELPERS (list-based)
;;; ───────────────────────────────────────────────
;; --- Convert VARIANT/SAFEARRAY 4x4 -> nested list 4x4
(defun matrix-from-variant (v / flat)
  (setq flat (vlax-safearray->list (vlax-variant-value v))) ; 16 numbers
  (list (subseq flat 0 4) (subseq flat 4 8) (subseq flat 8 12) (subseq flat 12 16))
)
;; --- Convert nested list 4x4 -> VARIANT/SAFEARRAY
(defun matrix-to-variant (m)
  "Convert a 4×4 list matrix to a VARIANT (SAFEARRAY) for vla-TransformBy."
  (vlax-tmatrix m)
)
;; --- Transpose a 4x4 list matrix
(defun matrix-transpose (m)
  "Transpose a 4×4 matrix (convert rows to columns)."
  (apply 'mapcar (cons 'list m))
)
;; --- Multiply two 4x4 list matrices (no VARIANTS here)
(defun matrix-mult (matA matB / transposedB )
  "Multiply two 4×4 matrices (list form). Returns another 4×4 list matrix."
  (setq transposedB  (matrix-transpose matB))
  (mapcar
    '(lambda (rowA)
       (mapcar
         '(lambda (colB)
            (apply '+ (mapcar '* rowA colB))
          )
          transposedB 
       )
     )
    matA
  )
)

;; --- Helpers to build 4x4 list matrices
(defun matrix-translate (dx dy dz)
  "Return a 4×4 translation matrix that moves by (dx, dy, dz)."
  (list
    (list 1 0 0 dx)
    (list 0 1 0 dy)
    (list 0 0 1 dz)
    (list 0 0 0 1)
  )
)
(defun matrix-rotZ (ang) ; ang in radians
  "Return a 4×4 rotation matrix around the Z axis by angleRadians."
  (list
    (list (cos ang) (- (sin ang)) 0 0)
    (list (sin ang) (cos ang)     0 0)
    (list 0         0             1 0)
    (list 0         0             0 1)
  )
)

;;; ───────────────────────────────────────────────
;;;  MAIN FUNCTION: rotate block about bbox center
;;; ───────────────────────────────────────────────
(defun rotate-around-center (obj newAng / *doc* minpt maxpt center tx ty tz T_toOrigin T_toBack Rot M)
  "Rotate a block reference around the center of its bounding box.
   angleRadians is the rotation in radians."
  (if (and obj (= "AcDbBlockReference" (vla-get-ObjectName obj)))
    (progn
      ;; normalize newAng to [0, 2π)
      (setq newAng (rem (+ newAng (* 2 pi)) (* 2 pi)))
      
      ;; --- Get bounding box center
      (vla-GetBoundingBox obj 'minpt 'maxpt)
      (setq minpt (vlax-safearray->list minpt)
            maxpt (vlax-safearray->list maxpt))
      (setq center (mapcar '(lambda (a b) (/ (+ a b) 2.0)) minpt maxpt))
      (setq tx (car center)  ty (cadr center)  tz (caddr center))

      ;; Build component matrices
      (setq T_toOrigin (matrix-translate (- tx) (- ty) (- tz))) ; translateToOrigin 
      (setq Rot   (matrix-rotZ newAng)) ; rotationMatrix 
      (setq T_toBack (matrix-translate tx ty tz)) ; translateBack 

      ;; Combine transformations (M):  T_toBack × Rot × T_toOrigin (all as lists)
      (setq M (matrix-mult T_toBack (matrix-mult Rot T_toOrigin)))

      ;; Convert to VARIANT and apply
      (vla-StartUndoMark (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-Object))))
      (vla-TransformBy obj (matrix-to-variant M))
      (vla-EndUndoMark *doc*)
      (princ "\n✅ Rotated about bbox center.")
    )
    (princ "\n⚠️ Selected object is not a block reference.")
  )
  (princ)
)

(defun c:bulk-rotate ( / ss i obj ang angDeg)
  "Rotate multiple selected objects around their individual centers."
  (princ "\nSelect objects to rotate around their centers: ")
  (if (setq ss (ssget))
    (progn
      ;; Get rotation angle from user
      (if (setq angDeg (getreal "\nEnter rotation angle in degrees: "))
        (progn
          (setq ang (* angDeg (/ pi 180.0))) ; convert degrees to radians
          (princ (strcat "\nRotating " (itoa (sslength ss)) " objects by " (rtos angDeg 2 1) "°..."))
          
          ;; Loop through all selected objects
          (setq i 0)
          (repeat (sslength ss)
            (setq obj (vlax-ename->vla-object (ssname ss i)))
            (rotate-around-center obj ang)
            (setq i (1+ i))
          )
          (princ (strcat "\n✅ Completed rotating " (itoa (sslength ss)) " objects."))
        )
        (princ "\n⚠️ No angle specified.")
      )
    )
    (princ "\n⚠️ No objects selected.")
  )
  (princ)
)

(defun c:bulk-rotate-insPt ( / ss i obj ang angDeg)
  "Rotate multiple selected objects around their individual centers."
  (princ "\nSelect objects to rotate around their centers: ")
  (if (setq ss (ssget))
    (progn
      ;; Get rotation angle from user
      (if (setq angDeg (getreal "\nEnter rotation angle in degrees: "))
        (progn
          (setq ang (* angDeg (/ pi 180.0))) ; convert degrees to radians
          (princ (strcat "\nRotating " (itoa (sslength ss)) " objects by " (rtos angDeg 2 1) "°..."))
          
          ;; Loop through all selected objects
          (setq i 0)
          (repeat (sslength ss)
            (setq obj (vlax-ename->vla-object (ssname ss i)))
            (rotate-around-center obj ang)
            (setq i (1+ i))
          )
          (princ (strcat "\n✅ Completed rotating " (itoa (sslength ss)) " objects."))
        )
        (princ "\n⚠️ No angle specified.")
      )
    )
    (princ "\n⚠️ No objects selected.")
  )
  (princ)
)
  


;; --- Test command
; (defun TestCenterRotate ( / e obj)
;   (if (setq e (car (entsel "\nSelect block to rotate around its center: ")))
;     (progn
;       (setq obj (vlax-ename->vla-object e))
;       (setq newAng (/ pi 4)) ; 45 degrees in radians
;       (rotate-around-center obj newAng) ; rotate 45 degrees
;     )
;   )
;   (princ)
; )