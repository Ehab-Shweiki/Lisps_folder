;;; debug function #debug for
(defun make-pline (pts/ pt)
  (entmakex
    (append
      (list '(0 . "LWPOLYLINE")
            '(100 . "AcDbEntity")
            '(100 . "AcDbPolyline")
            (cons 90 (length pts))
            (cons 70 0))
      (mapcar '(lambda (pt) (list 10 (car pt) (cadr pt))) pts)
    )
  )
)

;;; --- Helper function: string join ---
(defun str-join (lst sep / out)
  (if lst
    (progn
      (setq out (car lst))
      (foreach x (cdr lst)
        (setq out (strcat out sep x))
      )
      out
    )
    ""
  )
)

;; --- Helper to drop the last element of a list
(defun butlast (lst /)
  (if (cdr lst)
    (reverse (cdr (reverse lst)))
    nil
  )
)

(defun get-settings ( / settings)
  ;; Default settings
  (setq settings (list
                   (cons 'text-height 2.5)  ; default MTEXT height
                   (cons 'text-width 5.0)   ; default MTEXT box width
                 )
  )
  settings
)

(defun set-text-height (height / settings)
  (setq settings (get-settings))
  (setq settings (subst (cons 'text-height height) (assoc 'text-height settings) settings))
  (print (strcat "\nText height set to: " (rtos height 2 2)))
  settings
)

;;; Intersection of two infinite lines
(defun line-intersection (p1 p2 p3 p4 / a1 b1 c1 a2 b2 c2 det x y)
  ;; Line1: p1->p2, Line2: p3->p4
  (setq a1 (- (cadr p2) (cadr p1)))
  (setq b1 (- (car p1) (car p2)))
  (setq c1 (+ (* a1 (car p1)) (* b1 (cadr p1))))

  (setq a2 (- (cadr p4) (cadr p3)))
  (setq b2 (- (car p3) (car p4)))
  (setq c2 (+ (* a2 (car p3)) (* b2 (cadr p3))))

  (setq det (- (* a1 b2) (* a2 b1)))
  (if (equal det 0.0 1e-10)
    nil  ; parallel
    (list (/ (- (* b2 c1) (* b1 c2)) det)
          (/ (- (* a1 c2) (* a2 c1)) det))
  )
)

;; Build a point list where any arc segment (bulge≠0) between two straights
;; is represented ONLY by the intersection of the two straight neighbors.
; (defun collect-straight-pts (ename / edata etype verts bulges flags closed
;                                    segCount i ptsOut prevBul currBul nextBul pInt)

;   (setq edata (entget ename)
;         etype (cdr (assoc 0 edata))
;         flags (cdr (assoc 70 edata))
;         closed (and flags (/= 0 (logand flags 1))))

;   (cond
;     ;; LINE: return start & end points
;     ((= etype "LINE")
;      (list (cdr (assoc 10 edata)) (cdr (assoc 11 edata)))
;     )

;     ;; LWPOLYLINE: parse vertices (10) & bulges (42)
;     ((= etype "LWPOLYLINE")
;      (setq verts  '()
;            bulges '())
;      (foreach d edata
;        (cond
;          ((= (car d) 10) (setq verts (append verts (list (cdr d)))))
;          ((= (car d) 42) (setq bulges (append bulges (list (cdr d))))) ) ) ;Note: bulges determine if arc is next to vertix

;      ;; Align bulges length to segment count.
;      ;; For OPEN pline: segCount = (length verts) - 1.
;      ;; (This version treats closed polylines as open for arc-replacement;
;      ;; you can extend with wrap-around if needed.)
;      (setq segCount (1- (length verts)))
;      (while (< (length bulges) segCount)
;        (setq bulges (append bulges '(0.0))))
;      (while (> (length bulges) segCount)
;        (setq bulges (butlast bulges)))

;      ;; Segment-based walk:
;      ;; start with first vertex in output, then decide what to append for each segment
;      (setq ptsOut (if verts (list (car verts)) '()))
;      (setq i 0)
;      (while (< i segCount)
;        (setq currBul (nth i bulges)
;              prevBul (if (> i 0)            (nth (1- i) bulges) nil)
;              nextBul (if (< i (1- segCount)) (nth (1+ i) bulges) nil))

;        (cond
;          ;; Straight segment: append its end vertex normally
;          ((equal currBul 0.0 1e-9)
;           (setq ptsOut (append ptsOut (list (nth (1+ i) verts))))
;          )

;          ;; Arc segment: if flanked by straights, replace last point with intersection and skip appending end
;          ((and prevBul nextBul
;                (equal prevBul 0.0 1e-9)
;                (equal nextBul 0.0 1e-9)
;                (> i 0) (< i (1- segCount))) ; need v(i-1) and v(i+2)
;           (setq pInt (line-intersection
;                        (nth (1- i) verts) (nth i verts)      ; previous straight
;                        (nth (1+ i) verts) (nth (+ i 2) verts) ; next straight
;                      ))
;           (if pInt
;             ;; Replace the last appended point (v_i) with intersection; do NOT append v_{i+1}
;             (setq ptsOut (append (butlast ptsOut) (list pInt)))
;             ;; Fallback: if lines are parallel or bad, just append end vertex
;             (setq ptsOut (append ptsOut (list (nth (1+ i) verts))))
;           )
;          )

;          ;; Arc but not between two straights (edge cases or neighboring arc):
;          ;; keep topology as-is: append the end vertex
;          (T
;           (setq ptsOut (append ptsOut (list (nth (1+ i) verts))))
;          )
;        )

;        (setq i (1+ i))
;      )
;      ptsOut
;     )

;     (T
;      (prompt (strcat "\nUnsupported entity type: " etype))
;      nil
;     )
;   )
; )


;;; ============== Main Stpes ==============

;;; --- Step 1: Select a curve (LINE or POLYLINE) ---
(defun select-curve ( / ent objName )
  ;; Prompt user to pick a curve
  (prompt "\nSelect reinforcement line or polyline: ")
  (setq ent (car (entsel)))  ; return just the ename
  (if (null ent)
    (progn
      (princ "\nNo line/polyline selected. Exiting.")
      nil
    )
    (progn
      (setq objName (cdr (assoc 0 (entget ent))))
      (princ (strcat "\nSelected entity type: " objName))
      ent   ; return the ename
    )
  )
)
;;;;;;;;;;;;;;;;

;; ---------- 1) arcs -> intersections, works for LINE & LWPOLYLINE ----------
(defun collect-straight-pts (ename / edata etype verts bulges segCount i ptsOut prevBul currBul nextBul pInt)
  ;; for LWPOLYLINE only
  (setq verts '() bulges '())
  (foreach d edata
    (cond
      ((= (car d) 10) (setq verts (append verts (list (cdr d)))))
      ((= (car d) 42) (setq bulges (append bulges (list (cdr d))))) ) )
  (setq segCount (1- (length verts)))
  (while (< (length bulges) segCount) (setq bulges (append bulges '(0.0))))
  (while (> (length bulges) segCount) (setq bulges (butlast bulges)))

  (setq ptsOut (if verts (list (car verts)) '()))
  (setq i 0)
  (while (< i segCount)
    (setq currBul (nth i bulges)
          prevBul (if (> i 0)             (nth (1- i) bulges) nil)
          nextBul (if (< i (1- segCount)) (nth (1+ i) bulges) nil))
    (cond
      ;; straight → keep end vertex
      ((equal currBul 0.0 1e-9)
      (setq ptsOut (append ptsOut (list (nth (1+ i) verts)))))
      ;; arc sandwiched by straights → replace last with intersection, skip end
      ((and prevBul nextBul
            (equal prevBul 0.0 1e-9)
            (equal nextBul 0.0 1e-9)
            (> i 0) (< i (1- segCount)))
      (setq pInt (line-intersection
                    (nth (1- i) verts) (nth i verts)
                    (nth (1+ i) verts) (nth (+ i 2) verts)))
      (if pInt
        (setq ptsOut (append (butlast ptsOut) (list pInt)))
        (setq ptsOut (append ptsOut (list (nth (1+ i) verts)))))
      )
      ;; arc touching arc or at ends → keep topology
      (T (setq ptsOut (append ptsOut (list (nth (1+ i) verts)))))
    )
    (setq i (1+ i))
  )
  ptsOut
)


;; ---------- 2) remove collinear vertices (no angles) ----------
; ;; Collinear & same direction test using cross/dot; tolRel is relative tolerance.
; (defun collinear-same-dir (a b c tolRel / v1x v1y v2x v2y cross dot n1 n2)
;   (setq v1x (- (car b) (car a))
;         v1y (- (cadr b) (cadr a))
;         v2x (- (car c) (car b))
;         v2y (- (cadr c) (cadr b))
;         cross (- (* v1x v2y) (* v1y v2x))
;         dot   (+ (* v1x v2x) (* v1y v2y))
;         n1    (sqrt (+ (* v1x v1x) (* v1y v1y)))
;         n2    (sqrt (+ (* v2x v2x) (* v2y v2y))))
;   (and (> n1 1e-12) (> n2 1e-12)
;        (<= (abs cross) (* tolRel n1 n2))  ; nearly collinear
;        (> dot 0.0))                       ; same direction (not a U-turn)
; )

; (defun simplify-collinear-pts (pts tolRel / res i)
;   (cond
;     ((< (length pts) 3) pts)
;     (T
;      (setq res (list (car pts) (cadr pts))
;            i   2)
;      (while (< i (length pts))
;        (if (collinear-same-dir
;              (nth (- (length res) 2) res)
;              (nth (- (length res) 1) res)
;              (nth i pts)
;              tolRel)
;          ;; extend straight run: replace last with current point
;          (setq res (append (butlast res) (list (nth i pts))))
;          ;; keep corner
;          (setq res (append res (list (nth i pts))))
;        )
;        (setq i (1+ i))
;      )
;      res))
; )

(defun simplify-collinear-pts (pts tolRel / res i a b c v1x v1y v2x v2y cross n1 n2)
  (cond
    ((< (length pts) 3) pts)
    (T
     (setq res (list (car pts) (cadr pts))
           i   2)
     (while (< i (length pts))
       (setq a (nth (- (length res) 2) res)
             b (nth (1- (length res)) res)
             c (nth i pts)
             v1x (- (car b) (car a))   v1y (- (cadr b) (cadr a))
             v2x (- (car c) (car b))   v2y (- (cadr c) (cadr b))
             cross (- (* v1x v2y) (* v1y v2x))
             n1    (sqrt (+ (* v1x v1x) (* v1y v1y)))
             n2    (sqrt (+ (* v2x v2x) (* v2y v2y))))
       (if (and (> n1 1e-12) (> n2 1e-12)
                (<= (abs cross) (* tolRel n1 n2))) ; nearly collinear
         ;; extend run: replace last point with c
         (setq res (append (butlast res) (list c)))
         ;; keep corner
         (setq res (append res (list c)))
       )
       (setq i (1+ i))
     )
     res))
)

;; ---------- 3) build final segments; optional angle ONLY for display ----------
(defun segments-from-pts (pts / out i n p1 p2 dx dy len ang)
  (setq out '() i 0 n (1- (length pts)))
  (while (< i n)
    (setq p1 (nth i pts) p2 (nth (1+ i) pts)
          dx (- (car p2) (car p1))
          dy (- (cadr p2) (cadr p1))
          len (distance p1 p2)
          ;; If you don't need an angle at all, set ang to nil and drop it.
          ang (* 180.0 (/ (atan dy dx) pi))  ; <- only once per merged segment
    )
    (setq out (append out (list (list len ang))))
    (setq i (1+ i))
  )
  out
)


(defun calc-curve-merged (ename / edata etype tolRel ptsA ptsB segs)
  (setq ename (select-curve)) ; #debug for
  (setq edata (entget ename)
        etype (cdr (assoc 0 edata)))
  (cond
    ;; LINE: build 2-point list and convert to ((len ang))
    ((= etype "LINE")
     (setq ptsB (list (cdr (assoc 10 edata))  ; start
                      (cdr (assoc 11 edata)))) ; end
     (segments-from-pts ptsB)
    )

    ;; LWPOLYLINE: arcs -> intersections, then collapse collinear runs
    ((= etype "LWPOLYLINE")
     (setq tolRel 1e-9)                       ; relative tolerance for collinearity
     (setq ptsA (collect-straight-pts ename)) ; arcs -> intersections
     (setq ptsB (simplify-collinear-pts ptsA tolRel)) ; collapse collinear runs
     (make-pline ptB) ;#debug for
     (segs (segments-from-pts ptsB))     ; ((len ang) ...)  -- angle optional
    )

    ;; Unsupported entity types
    (T 
     (prompt (strcat "\nUnsupported entity type: " etype)) 
     nil
    )
  )
)
(defun tst ()
  (setq curve (select-curve))
  (calc-curve-merged curve)
) ;# debug for

;;; --- Step 3: Select related text ---
(defun select-text ( / txtEnt txtStr)
  (prompt "\nSelect related text with Phi and bars info: ")
  (setq txtEnt (entsel))
  (if (not txtEnt)
    (progn
      (print "\nNo related text selected. Exiting.")
      nil
    )
    (progn
      (setq txtStr (cdr (assoc 1 (entget (car txtEnt)))))
      (print (strcat "\nSelected text: " txtStr))
      txtStr
    )
  )
)

;;; --- Step 4: Build result string ---
(defun build-result-str (txtStr totalLen segLens / resultStr)
  (if (and txtStr totalLen segLens)
    (progn
      (setq resultStr
        (strcat
          "Rebar: " txtStr
          " | Total Length = " (rtos totalLen 2 2)
          " | Segments = " (str-join segLens ", ")
        )
      )
      (print (strcat "\nFinal result string: " resultStr))
      resultStr
    )
    (progn
      (print "\nCannot build result string: missing data. Exiting.")
      nil
    )
  )
)

;;; --- Step 5: Insert MTEXT ---
(defun create-mtext (resultStr settings / insPt acadApp doc ms mtextObj textHeight textWidth)
  (if (not resultStr)
    (print "\nNo result string to create MTEXT. Exiting.")
    (progn
      ;; Get height and width from settings
      (setq textHeight (cdr (assoc 'text-height settings)))
      (setq textWidth  (cdr (assoc 'text-width  settings)))

      ;; Pick insertion point
      (setq insPt (getpoint "\nPick insertion point for rebar info text: "))
      (if (not insPt)
        (print "\nNo insertion point picked. Exiting.")
        (progn
          (print (strcat "\nInsertion point: " (rtos (car insPt) 2 2) ", " (rtos (cadr insPt) 2 2)))

          ;; Get AutoCAD objects
          (setq acadApp (vlax-get-acad-object))
          (setq doc (vla-get-ActiveDocument acadApp))
          (setq ms (vla-get-ModelSpace doc))

          ;; Create MTEXT
          (setq mtextObj
                (vla-AddMText ms
                              (vlax-3d-point (car insPt) (cadr insPt) 0)
                              textWidth
                              resultStr))
          (vla-put-Height mtextObj textHeight)
          (vla-put-AttachmentPoint mtextObj acAttachmentPointMiddleCenter)
          (print "\nMTEXT created successfully.")
        )
      )
    )
  )
)


;;; --- Main command ---
(defun c:RI ( / settings curve segData totalLen txtStr resultStr )
  ;; 1️⃣ Load default settings (text height, text width, etc.)
  (setq settings (get-settings))

  ;; 2️⃣ Select the line or polyline to measure total length
  (setq curve (select-curve))
  (if (null curve)
    (progn (princ "\n❌ No curve selected. Command canceled.") (princ) (exit))
  )

  ;; 3️⃣ Compute merged segments: length + signed angle
  (setq segData (calc-curve-merged curve))
  (if (null segData)
    (progn (princ "\n❌ No segment data. Command canceled.") (princ) (exit))
  )
  ;; total length = sum of merged segment lengths
  (setq totalLen (apply '+ (mapcar 'car segData)))

  ;; 4️⃣ Select the related text containing rebar info
  (setq txtStr (select-text))
  (if (null txtStr)
    (progn (princ "\n❌ No related text selected. Command canceled.") (princ) (exit))
  )

  ;; 5️⃣ Build final result string
  (setq resultStr
        (strcat
          "Rebar: " txtStr
          " | Total Length = " (rtos totalLen 2 2)
          " | Segments = "
          (str-join
            (mapcar
              '(lambda (s)
                 (strcat (rtos (car s) 2 2) "@" (rtos (cadr s) 2 2) "°"))
              segData)
            ", "
          )
        )
  )

  ;; 6️⃣ Create the MTEXT object at a user-specified insertion point
  (create-mtext resultStr settings)

  ;; 7️⃣ Done
  (princ "\n✅ Rebar information MTEXT created successfully.")
  (princ)
)


(defun c:RI() (c:GetRebarInfo))
(princ "\nType 'RI' to Run GetRebarInfo command.")
(princ)