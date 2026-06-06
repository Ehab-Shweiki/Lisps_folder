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
(defun collect-straight-pts (ename / edata etype verts bulges flags closed
                                   segCount i ptsOut prevBul currBul nextBul pInt)

  (setq edata (entget ename)
        etype (cdr (assoc 0 edata))
        flags (cdr (assoc 70 edata))
        closed (and flags (/= 0 (logand flags 1))))

  (cond
    ;; LINE: return start & end points
    ((= etype "LINE")
     (list (cdr (assoc 10 edata)) (cdr (assoc 11 edata)))
    )

    ;; LWPOLYLINE: parse vertices (10) & bulges (42)
    ((= etype "LWPOLYLINE")
     (setq verts  '()
           bulges '())
     (foreach d edata
       (cond
         ((= (car d) 10) (setq verts (append verts (list (cdr d)))))
         ((= (car d) 42) (setq bulges (append bulges (list (cdr d))))) ) ) ;Note: bulges determine if arc is next to vertix

     ;; Align bulges length to segment count.
     ;; For OPEN pline: segCount = (length verts) - 1.
     ;; (This version treats closed polylines as open for arc-replacement;
     ;; you can extend with wrap-around if needed.)
     (setq segCount (1- (length verts)))
     (while (< (length bulges) segCount)
       (setq bulges (append bulges '(0.0))))
     (while (> (length bulges) segCount)
       (setq bulges (butlast bulges)))

     ;; Segment-based walk:
     ;; start with first vertex in output, then decide what to append for each segment
     (setq ptsOut (if verts (list (car verts)) '()))
     (setq i 0)
     (while (< i segCount)
       (setq currBul (nth i bulges)
             prevBul (if (> i 0)            (nth (1- i) bulges) nil)
             nextBul (if (< i (1- segCount)) (nth (1+ i) bulges) nil))

       (cond
         ;; Straight segment: append its end vertex normally
         ((equal currBul 0.0 1e-9)
          (setq ptsOut (append ptsOut (list (nth (1+ i) verts))))
         )

         ;; Arc segment: if flanked by straights, replace last point with intersection and skip appending end
         ((and prevBul nextBul
               (equal prevBul 0.0 1e-9)
               (equal nextBul 0.0 1e-9)
               (> i 0) (< i (1- segCount))) ; need v(i-1) and v(i+2)
          (setq pInt (line-intersection
                       (nth (1- i) verts) (nth i verts)      ; previous straight
                       (nth (1+ i) verts) (nth (+ i 2) verts) ; next straight
                     ))
          (if pInt
            ;; Replace the last appended point (v_i) with intersection; do NOT append v_{i+1}
            (setq ptsOut (append (butlast ptsOut) (list pInt)))
            ;; Fallback: if lines are parallel or bad, just append end vertex
            (setq ptsOut (append ptsOut (list (nth (1+ i) verts))))
          )
         )

         ;; Arc but not between two straights (edge cases or neighboring arc):
         ;; keep topology as-is: append the end vertex
         (T
          (setq ptsOut (append ptsOut (list (nth (1+ i) verts))))
         )
       )

       (setq i (1+ i))
     )
     ptsOut
    )

    (T
     (prompt (strcat "\nUnsupported entity type: " etype))
     nil
    )
  )
)


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
(calc-curve-merged curve) ;# debug for
;;; --- Step 2: collect true 2D segments and merge same-angle neighbors ---
(defun calc-curve-merged (ename / pts n i pt1 pt2 dx dy ang len raw merged tol seg lastSeg)
  (setq tol 1.0) ; degrees tolerance for merging
  
  ;; 1. Collect corrected points with arc intersections handled
  (setq ename (select-curve)) ;# debug for it
  (setq pts (collect-straight-pts ename))
  (make-pline pts) ;#debug for
  (setq n (1- (length pts)))
  
  
  ;; 2. Build raw segments (length + angle)
  (setq raw '())
  (setq i 0)
  (while (< i n)
    (setq pt1 (nth i pts)
          pt2 (nth (1+ i) pts)
          dx  (- (car pt2) (car pt1))
          dy  (- (cadr pt2) (cadr pt1))
          len (distance pt1 pt2)
          ang (* 180.0 (/ (atan dy dx) pi)))
    (setq raw (append raw (list (list len ang))))
    (setq i (1+ i))
  )

  ;; 3. Merge neighbors of same angle
  (setq merged '())
  (foreach seg raw
    (if (and merged
             (< (abs (- (cadr seg) (cadr (setq lastSeg (last merged))))) tol))
      (setq merged
            (append (butlast merged)
                    (list (list (+ (car seg) (car lastSeg))
                                (cadr lastSeg)))))
      (setq merged (append merged (list seg)))
    )
  )
  merged
)



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