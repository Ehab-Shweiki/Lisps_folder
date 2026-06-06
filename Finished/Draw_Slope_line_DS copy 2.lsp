(defun c:DSA ( / pt ptList xLen slope yLen ptNew midPt ang midPtDy slopeStr dyStr 
                offset textHeight blkName blkScale blkRef blkCopy ent attEnt attTag
                fieldStr blkID oldOsmode oldLayer newLayer *spotBlockRef*)

  ;; === Settings ===
  (setq offset 10.0)
  (setq textHeight 25.0)
  (setq blkScale 1.0)
  (setq blkName "spot_level")
  (setq newLayer "Slope_Layer")

  ;; Save current layer and OSMODE
  (setq oldLayer (getvar "CLAYER"))
  (setq oldOsmode (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  ;; Set layer
  (if (tblsearch "LAYER" newLayer)
    (setvar "CLAYER" newLayer)
    (command "_.-LAYER" "M" newLayer "")
  )

  ;; Find spot block
  (setq *spotBlockRef* (find-block-instance blkName))
  (if (null *spotBlockRef*)
    (progn
      (prompt (strcat "\nNo instance of block '" blkName "' found in the drawing."))
      (setvar "CLAYER" oldLayer)
      (setvar "OSMODE" oldOsmode)
      (exit)
    )
  )

  ;; Get start point
  (setq pt (getpoint "\nStart point: "))
  (setq ptList (list pt))

  ;; Define 3 slope segments
  (repeat 3
    (cond
      ;; 1st and 3rd fixed
      ((or (= (length ptList) 1) (= (length ptList) 3))
        (setq xLen 300.0)
        (setq slope (* 7.5 -0.01))
      )
      ;; 2nd is user input
      (T
        (setq xLen (getreal "\nSecond segment - Horizontal length (X) \"cm\": "))
        (setq slope (getreal "\nSecond segment - Slope (Y/X): "))
        (setq slope (* slope -0.01))
      )
    )

    ;; Compute values
    (setq yLen (* xLen slope))
    (setq ptNew (list (+ (car pt) xLen) (+ (cadr pt) yLen)))
    (setq ptList (append ptList (list ptNew)))

    ;; Midpoint and angle
    (setq midPt (list (/ (+ (car pt) (car ptNew)) 2.0)
                      (/ (+ (cadr pt) (cadr ptNew)) 2.0)))
    (setq ang (atan yLen xLen))
    (setq midPt (polar midPt (+ ang (/ pi 2)) offset))

    ;; Slope text
    (setq slopeStr (strcat (rtos (* slope 100.0) 2 1) "%"))
    (entmake
      (list
        '(0 . "TEXT")
        (cons 10 midPt)
        (cons 40 textHeight)
        (cons 1 slopeStr)
        (cons 7 "Standard")
        (cons 50 ang)
        (cons 62 3)
        (cons 72 1)
        (cons 73 1)
        (cons 11 midPt)
      )
    )

    ;; dy label
    (setq dyStr (strcat "dy= " (itoa (fix (+ yLen 0.5)))))
    (setq midPtDy (polar midPt (- ang (/ pi 2)) (* 2 offset)))
    (entmake
      (list
        '(0 . "TEXT")
        (cons 10 midPtDy)
        (cons 40 (/ textHeight 2.0))
        (cons 1 dyStr)
        (cons 7 "Standard")
        (cons 50 ang)
        (cons 62 3)
        (cons 72 1)
        (cons 73 3)
        (cons 11 midPtDy)
      )
    )

    ;; Copy block
    (setq blkCopy (vla-Copy *spotBlockRef*))
    (vla-Move blkCopy
      (vlax-3d-point (vlax-get *spotBlockRef* 'InsertionPoint))
      (vlax-3d-point ptNew)
    )

    ;; Move to next
    (setq pt ptNew)
  )

  ;; Draw polyline
  (if (> (length ptList) 1)
    (progn
      (command "_.PLINE")
      (foreach p ptList (command p))
      (command "")
    )
  )

  ;; Restore settings
  (setvar "OSMODE" oldOsmode)
  (setvar "CLAYER" oldLayer)
  (command "_.REGEN")
  (princ)
)

(princ "\nType 'DSA' to run the 3-slope chain command.")
(princ)
