"Using ActiveX"
;; ===================================================
;; ╦ ╦╔═╗╦  ╔═╗╔═╗╦═╗  ╔═╗╦ ╦╔╗╔╔═╗╔╦╗╦╔═╗╔╗╔╔═╗
;; ╠═╣║╣ ║  ╠═╝║╣ ╠╦╝  ╠╣ ║ ║║║║║   ║ ║║ ║║║║╚═╗
;; ╩ ╩╚═╝╩═╝╩  ╚═╝╩╚═  ╚  ╚═╝╝╚╝╚═╝ ╩ ╩╚═╝╝╚╝╚═╝
;; ===================================================
(defun set-layer-color (obj layer color)
  (if layer (vla-put-Layer obj layer))
  (if color (vla-put-Color obj color))
)

(defun safe-put-StyleName (obj stylename)
  "Safely set StyleName if entity supports it."
  (if (style-exists-for-obj obj stylename)
    (vla-put-StyleName obj stylename)
    (prompt (strcat "\n⚠️ Style \"" stylename "\" not found for " (vla-get-ObjectName obj)))
  )
)

(defun style-exists-for-obj (obj stylename / *doc* coll dict ok) ; debug
  "Checks if the given style name exists for the object type.
   Returns T if found, NIL otherwise."

  (vl-load-com)
  (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (setq ok nil)

  ;; Identify object type by DXF/COM name
  (setq oname (vla-get-ObjectName obj))

  (cond
    ;; --- Text / MText: TextStyle
    ((wcmatch oname "AcDbText,AcDbMText")
     (setq coll (vla-get-TextStyles *doc*)))

    ;; --- Dimension: DimStyle
    ((wcmatch oname "AcDbDimension*")
     (setq coll (vla-get-DimStyles *doc*)))

    ;; --- Leader: uses DimStyle (no StyleName property)
    ((wcmatch oname "AcDbLeader")
     (setq coll (vla-get-DimStyles *doc*)))
    
    ;; --- MultiLeader: MLeaderStyle
    ((wcmatch oname "AcDbMLeader")
        (setq dict (vla-Item (vla-get-Dictionaries *doc*) "ACAD_MLEADERSTYLE"))
        (setq ok (and dict
                      (eq 'vla-object (type (vl-catch-all-apply 'vla-Item (list dict stylename))))
                  )))

    ;; --- Table: TableStyle
    ((wcmatch oname "AcDbTable")
     (cond
       ;; If TableStyles property exists, use it:
       ((vlax-property-available-p *doc* 'TableStyles)
        (setq coll (vlax-get *doc* 'TableStyles))
        (setq ok (not (vl-catch-all-error-p
                        (vl-catch-all-apply 'vla-Item (list coll stylename))))))
       ;; Fallback to NOD dictionary:
       (T
        (setq dict (vla-Item (vla-get-Dictionaries *doc*) "ACAD_TABLESTYLE"))
        (setq ok (and dict
                      (eq 'vla-object (type (vl-catch-all-apply 'vla-Item (list dict stylename)))))
        ))))

    (T
     (prompt (strcat "\n⚠️ No style type associated with " oname))
     (setq coll nil))
  )

  ;; --- Check style name in the found collection
  (if (and coll stylename)
    (or ok ; for MLeader
        (not (vl-catch-all-error-p
              (vl-catch-all-apply 'vla-Item (list coll stylename))))
    )
  )
)

(defun ensure-closed (pts / first last) ; debug
  "Ensures a list of points (2D or 3D) is closed.
   If last ≠ first, appends the first point at the end.
   Returns the possibly modified list.

   Example:
     (ensure-closed '((0 0) (10 0) (10 10)))
       → ((0 0) (10 0) (10 10) (0 0))"
  (if (and pts (> (length pts) 1))
    (progn
      (setq first (car pts)
            last  (last pts))
      ;; (last pts) returns a list containing the last element → use (car ...)
      (if (not (equal (car last) first 1e-9))
        (append pts (list first))
        pts)
    )
    pts  ; return as is for empty or single-point list
  )
)

(defun update-bulge-at-index (ename idx bulge / edata new count)
  "Updates or inserts bulge (DXF 42) values for vertices in LWPOLYLINE.
   of an LWPOLYLINE entity ENAME.
  Arguments:
  ename    → entity name of the LWPOLYLINE.
  idx      → the vertex indix
  bulge    → numeric bulge value if idx is a simple list.
             ignored if idxList is an alist (value comes from it)."

  (setq edata (entget ename)
        new '() ; we cant just add in-between the old one
        count 0
  )

  ;; rebuild DXF list inserting bulge after matching vertex indices
  (foreach pair edata
    (setq new (cons pair new))
    (if (= (car pair) 10) ; found a vertex
      (progn
        (if (= count idx)
          ;; After this vertex, insert or replace bulge
          (setq new (cons (cons 42 bulge) new))
        )
        (setq count (1+ count))
      )
    )
  )
  ;; --- Commit changes ---
  (setq new (reverse new))
  (entmod new) ; update the old DXF record for the entity 
  (entupd ename)
)

(defun update-bulges-at-indices (ename idxList bulges / pair bulge edata new count map)
  "Updates or inserts bulge (DXF 42) values for vertices in LWPOLYLINE.

  Arguments:
    ename   → entity name of LWPOLYLINE.
    idxList → list of vertex indices (e.g. '(0 1 2)).
    bulges  → either:
                - a single number (applied to all indices), or
                - a list of numbers (one per index)."

  (vl-load-com)

  ;; --- Normalize bulge data ---
  (setq map
        (cond
          ;; If bulges is a single number → duplicate for each index
          ((numberp bulges)
           (mapcar (function (lambda (i) (cons i bulges))) idxList))

          ;; If bulges is list → pair them positionally
          ((listp bulges)
           (mapcar (function (lambda (i v) (cons i v))) idxList bulges))

          (T nil))
  )

  ;; --- Process DXF structure ---
  (setq edata (entget ename)
        new '()
        count 0)
  

  (foreach pair edata
    (cond
      ;; --- Handle bulge pairs (42 . value)
      ((= (car pair) 42)
        ;; if this bulge index is listed, replace it
        (if (assoc count map)
          (progn
            (setq bulge (cdr (assoc count map)))
            (setq new (cons (cons 42 bulge) new))
            (setq count (1+ count))
          )
          (setq new (cons pair new))
        )  
      )

      ;; --- Copy all other pairs unchanged
      (T (setq new (cons pair new)))
    )
  )

  ;; --- Commit changes ---
  (setq new (reverse new))
  (entmod new) ; update the old DXF record for the entity 
  (entupd ename)
)


; (defun ensure-textstyle (name / *doc* styles st)
;   "Ensures a text style with given name exists, returns its object."
;   (vl-load-com)
;   (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-Object))
;         styles (vla-get-TextStyles *doc*))
;   (if (not (vl-catch-all-error-p (setq st (vl-catch-all-apply 'vla-Item (list styles name)))))
;     st
;     (vla-Add styles name))
; )
; (defun ensure-ltype (name / *doc*)
;   (vl-load-com)
;   (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-Object)))
;   (if (not (tblsearch "LTYPE" name))
;     (vla-Load (vla-get-Linetypes *doc*) name "acad.lin"))
; )
; (defun ensure-tablestyle (name / *doc* styles st)
;   (vl-load-com)
;   (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-Object))
;         styles (vla-get-TableStyles *doc*))
;   (if (not (vl-catch-all-error-p (setq st (vl-catch-all-apply 'vla-Item (list styles name)))))
;     st
;     (vla-Add styles name))
; )


;; ===================================================
;; ╔╦╗╔═╗╦╔╗╔  ╔╦╗╦═╗╔═╗╦ ╦  ╔═╗╦ ╦╔╗╔╔═╗╔╦╗╦╔═╗╔╗╔╔═╗
;; ║║║╠═╣║║║║   ║║╠╦╝╠═╣║║║  ╠╣ ║ ║║║║║   ║ ║║ ║║║║╚═╗
;; ╩ ╩╩ ╩╩╝╚╝  ═╩╝╩╚═╩ ╩╚╩╝  ╚  ╚═╝╝╚╝╚═╝ ╩ ╩╚═╝╝╚╝╚═╝
;; ===================================================
(defun DrawRect (args / *doc* *ms* p1 p2 x1 y1 x2 y2 pts pline base ang mirrorline newobj)
  ;;; Draws a rectangle using AutoCAD API
  ;;; Accepts no arguments, or two corner points:
  ;;; Examples:
  ;;; (DrawRect nil)
  ;;; (DrawRect p1 p2)
  
  (vl-load-com)
  ;; -- init acad objects --
  (setq *doc* (vla-get-ActiveDocument (vlax-get-acad-object))
        *ms*  (vla-get-ModelSpace *doc*))

  ;; check for args
  (if (and args (= (length args) 2))
    (progn
      (setq p1 (nth 0 args)
            p2 (nth 1 args))
    )
    ;; else prompt for corner points
    (setq p1 (getpoint "\nFirst corner: ")
          p2 (getpoint p1 "\nOpposite corner: "))
  )
  
  ;; Extract coordinates
  (setq x1 (car p1) y1 (cadr p1)
        x2 (car p2) y2 (cadr p2))

  ;; Define the rectangle vertices (closed polyline)
  (setq pts (vlax-make-safearray vlax-vbDouble '(0 . 9)))
  (vlax-safearray-fill pts (list x1 y1 x2 y1 x2 y2 x1 y2 x1 y1))
  
  ;; Add lightweight polyline
  (setq pline (vla-AddLightWeightPolyline *ms* pts))
  
  ;; Close it properly (just in case)
  (vla-put-Closed pline :vlax-true)
  (vla-update pline)
  
  (princ "\n✅ Rectangle created successfully.")
  (princ)
)
(setq args (list hatchPts T)) ; debug.
(defun draw-poly (args / pts closed color layer *doc* *ms* sa pl pts2d)
  ;;;-------------------------------------------------------------
  ;;; draw-Poly
  ;;; Draws a lightweight polyline using the AutoCAD ActiveX API.
  ;;; Arguments (in list form):
  ;;;   [0] pts    - list of 2D/3D points ((x y [z]) ...)
  ;;;   [1] closed - T/NIL  (optional)
  ;;;   [2] color  - AutoCAD color index (optional)
  ;;;   [3] layer  - layer name string (optional)
  ;;; Works for both:
  ;;;   (draw-Poly '((0 0) (10 0) (10 10)))        ; points only
  ;;;   (draw-Poly (list '((0 0) (10 0) (10 10)) T 3 "Walls")) ; full args list
  ;;; Returns:
  ;;;   VLA-OBJECT of the created polyline, or NIL if invalid input.
  ;;;-------------------------------------------------------------

  ;; -- helper function --
  (defun ensure-2d (p)
    "ensure point has 2 coords"
    (cond
      ((not (listp p)) '(0.0 0.0))
      ((= (length p) 3) (list (car p) (cadr p))) ; remove Z
      ((> (length p) 3) (list (car p) (cadr p)))
      (T (mapcar 'float p))
    )
  )
  
  ;; --- detect single vs multi-arg form ---
  (cond
    ;; case 1: points only
    ((and (and args (listp args))
          (and (car args) (listp (car args)))
          (numberp (caar args)))
     (setq pts args closed nil color nil layer nil))

    ;; case 2: packed argument list
    ((and (and args (listp args))
          (and (car args) (listp (car args)))
          (listp (caar args)))
     (setq pts    (nth 0 args)
           closed (nth 1 args)
           color  (nth 2 args)
           layer  (nth 3 args)))

    ;; anything else: invalid
    (T (prompt "\n⚠️ Invalid arguments passed to draw-Poly.") (setq pts nil))
  )
  
  ;; --- validate points ---
  (cond
    ((not (and pts (listp pts))) (prompt "\n⚠️ Points list is missing or invalid.") (exit))
    ((< (length pts) 2) (prompt "\n⚠️ Polyline needs at least 2 points.") (exit))
  )
  
  ;; -- ensure all points are 2D --
  (setq pts2d (mapcar 'ensure-2d pts))
  
  ;; --- geometric diagnostics ---
  ;; check for duplicate consecutive points
  (setq i 0)
  (repeat (- (length pts2d) 1)
    (setq p1 (nth i pts2d)
          p2 (nth (1+ i) pts2d))
    (if (equal p1 p2 1e-6)
      (prompt (strcat "\n⚠️ Points " (itoa i) " and " (itoa (1+ i))
                      " are identical — zero-length segment.")))
    (setq i (1+ i))
  )
  ;; check for collinear segments (zero turn angle)
  (if (> (length pts2d) 2)
    (progn
      (setq i 0)
      (repeat (- (length pts2d) 2)
        (setq p1 (nth i pts2d)
              p2 (nth (1+ i) pts2d)
              p3 (nth (+ i 2) pts2d))
        (setq v1 (mapcar '- p2 p1)
              v2 (mapcar '- p3 p2))
        (setq ang (- (angle '(0 0) v2) (angle '(0 0) v1)))
        (if (< (abs ang) 1e-6)
          (prompt (strcat "\n⚠️ Points " (itoa i) "," (itoa (1+ i))
                          "," (itoa (+ i 2)) " are nearly collinear.")))
        (setq i (1+ i))
      )
    )
  )
  
  
  ;; check for closing issues
  (if (and closed (not (equal (car pts2d) (last pts2d) 1e-6)))
    (prompt "\nℹ️ Polyline marked as closed but endpoints differ — AutoCAD will close automatically.")
  )
  
  ;; --- Flatten point list into a SafeArray ---
  (setq flat (apply 'append pts2d))
  (setq sa (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length flat)))))
  (vlax-safearray-fill sa flat)

  ;; --- Create the polyline object ---
  (vl-load-com)
  ;; init acad objects
  (setq *doc* (vla-get-ActiveDocument (vlax-get-acad-object))
        *ms*  (vla-get-ModelSpace *doc*))
  
  (setq pl (vla-AddLightWeightPolyline *ms* sa))
 
  ;; --- Close and style ---
  (if closed (vla-put-Closed pl :vlax-true))
  (set-layer-color pl layer color)
  
  ;; Return created object
  (prompt "\n✅ Polyline created successfully.")
  (princ)
  pl
)

(defun draw-Spline (args / pts startTangent endTangent color layer *doc* *ms* sa spl pts3d)
  ;;;-------------------------------------------------------------
  ;;; Draws a spline using AutoCAD API
  ;;; Arguments (multi form):
  ;;;   [0] pts           list of 2D or 3D control points
  ;;;   [1] startTangent  optional 3D direction vector
  ;;;   [2] endTangent    optional 3D direction vector
  ;;;   [3] color         optional AutoCAD color index
  ;;;   [4] layer         optional layer name
  ;;; Examples: 
  ;;; (draw-Spline '((0 0) (5 10) (10 0)))                 ; 2D spline, default layer/color
  ;;; (draw-Spline (list '((0 0 0) (5 10 5) (8 10 5) (10 0 10)) ))
  ;;; (draw-Spline '((0 0 0) (5 10 5) (10 0 10)))          ; 3D spline, defaults
  ;;; (draw-Spline (list '((0 0 0) (5 10 5) (10 0 10)) '(0 0 0) '(0 0 0) 4 "Walls"))
  ;;; (draw-Spline (list '((0 0 0) (5 10 5) (8 10 5) (10 0 10)) '(0 0 0) '(0 0 0) 4 "Walls"))
  ;;;
  ;;; Returns: VLA Spline object or NIL
  ;;;-------------------------------------------------------------
  
  ;; -- helper Functions --
  (defun ensure-3d (p)
    "ensure point has 3 coords "
    (cond
      ((not (listp p)) '(0.0 0.0 0.0)) ; to remove garbage "e.g: '(nil "x" 5)"
      ((= (length p) 2) (append p '(0.0)))  ; add Z=0
      ((> (length p) 3) (list (car p) (cadr p) (caddr p)))
      (T (mapcar 'float p)))
  )
  
  (defun vec-normalize (v) 
    (setq len (distance '(0 0 0) v)) ; ∣v∣​
    (if (equal len 0.0 1e-6)
      '(0.0 0.0 0.0)
      (mapcar '/ v (list len len len)) ; v^= v / ∣v∣​
    )
  )
  (defun vec* (a b / v s)
    "scalar × vector.
    Returns a new list = vector v multiplied by scalar s.
    Example: (vec* '(2 4 6) 0.5) or (vec* 0.5 '(2 4 6))"
    (cond
      ;; Case 1: (vector scalar)
      ((and (listp a) (numberp b))
      (mapcar '(lambda (x) (* x b)) a))

      ;; Case 2: (scalar vector)
      ((and (numberp a) (listp b))
      (mapcar '(lambda (x) (* a x)) b))

      ;; Invalid input
      (T
      (prompt "\n⚠️ vec*: invalid input — requires (list, number) or (number, list)")
      nil)
    )
  )
  (defun auto-tangents (pts)
    ;; Purpose:
    ;;   Estimate start and end tangent vectors for spline fitting.
    ;;
    ;; Mathematical background:
    ;;   AutoCAD’s internal spline (AcGeSplineCurve3d) uses
    ;;   parabolic end-condition estimation based on the first
    ;;   and last three fit points.
    ;;
    ;;   f'(x0) ≈ (−3f0 + 4f1 − f2) / (2h)
    ;; Here h is omitted (unit spacing assumed).
    ;;
    ;;   When fit points are P0, P1, P2, ..., Pn:
    ;;     Start tangent  T0 ≈ ½ (3·P1 − 4·P0 + P2)
    ;;     End   tangent  Tn ≈ ½ (3·P(n-1) − 4·P(n) + P(n−2))
    
    (cond
      ((< (length pts) 2)
        (list nil nil))
      ((= (length pts) 2)
        (setq v (mapcar '- (cadr pts) (car pts)))
        (list v v))
      (T
        (setq p0 (car pts)
              p1 (cadr pts)
              p2 (nth 2 pts)
              pn (last pts)
              pn-1 (nth (- (length pts) 2) pts)
              pn-2 (nth (- (length pts) 3) pts))
        ;; --- Start tangent: ½ (3·P1 − 4·P0 + P2)
        (setq t0 (vec* 0.5 (mapcar '+ (vec* 3 p1) (vec* -4 p0) p2)))
        ;; --- End tangent:   ½ (3P(n−1) − 4P(n) + P(n−2))
        (setq tn (vec* 0.5 (mapcar '+ (vec* 3 pn-1) (vec* -4 pn) pn-2)))

        (list (vec-normalize t0) (vec-normalize tn)))
    )
  )
  
  (defun auto-tangents-2pts (pts)
    "Returns (startTangent endTangent) estimated from points list."
    (cond
      ((< (length pts) 2)
      (list nil nil))  ; not enough points
      (T
      (setq p0 (car pts)
            p1 (cadr pts)
            pn (last pts)
            pn-1 (nth (- (length pts) 2) pts)
            v1 (mapcar '- p1 p0)
            v2 (mapcar '- pn pn-1))
      (list (vec-normalize v1) (vec-normalize v2)))
    )
  )
  
  ;; --------------- main ---------------
  ;; --- detect  single vs multi-arg forms ---
  (cond
    ;; case 1: user passed points directly (e.g. '((0 0) (5 10) (10 0)))
    ((and (listp args)
          (listp (car args))
          (numberp (caar args)))
     (setq pts args
           startTangent nil
           endTangent nil
           color nil
           layer nil))
    ;; case 2: user passed packed argument list (e.g. (list pts tangent tangent color layer))
    ((and (listp args)
          (listp (car args))
          (listp (caar args)))
     (setq pts          (nth 0 args)
           startTangent (nth 1 args)
           endTangent   (nth 2 args)
           color        (nth 3 args)
           layer        (nth 4 args)))

    ;; anything else: invalid
    (T (prompt "\n⚠️ Invalid arguments passed to draw-Poly.") (setq pts nil))
  )
  
  ;; --- validate points ---
  (cond
    ((not (and pts (listp pts))) (prompt "\n⚠️ Points list is missing or invalid.") (exit))
    ((< (length pts) 3) (prompt "\n⚠️ A spline requires at least 3 fit points.") (exit))
    ((= (apply '+ (mapcar '(lambda(p) (if (listp p) (length p) 0)) pts)) 0)
     (prompt "\n⚠️ Points list is empty or malformed.") (exit))
  )
  
  ;; -- ensure all control points are 3D --
  (setq pts3d (mapcar 'ensure-3d pts))
  
  ;; --- geometric diagnostics --- ### for defensive sanity checks for debugging ###
  ;; --- basic geometric validation ---
  (if (or (equal (nth 0 (nth 0 pts3d)) (nth 0 (last pts3d)))
          (equal (nth 1 (nth 0 pts3d)) (nth 1 (last pts3d))))
    (prompt "\n⚠️ Start and end points coincide — may cause failure.")
  )
  ;; detect collinearity
  (defun cross2 (a b)
    (- (* (car a) (cadr b)) (* (cadr a) (car b))))
  (setq i 0)
  (repeat (- (length pts3d) 2)
    (setq p1 (nth i pts3d)
          p2 (nth (1+ i) pts3d)
          p3 (nth (+ i 2) pts3d)
          v1 (mapcar '- p2 p1)
          v2 (mapcar '- p3 p2))
    (if (equal (cross2 v1 v2) 0.0 1e-6)
      (prompt (strcat "\n⚠️ Points " (itoa i) "," (itoa (1+ i)) "," (itoa (+ i 2))
                      " are nearly collinear — may cause spline issues."))
    )
    (setq i (1+ i))
  )
  
  
  ;; --- Flatten point list into a SafeArray ---
  (setq flat (apply 'append pts3d))
  (setq sa (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length flat)))))
  (vlax-safearray-fill sa flat)

  ; -- prepare tangent variants (if passed) --
  ; --- compute tangents automatically if not provided ---
  (if (not (and startTangent endTangent))
    (progn
      (setq tangs (auto-tangents pts3d))
      (setq startTangent (car tangs)
            endTangent   (cadr tangs))
      (prompt "\nℹ️ Auto-evaluated start/end tangents from points.")
    )
  )
  ; check for valid tangents
  (if (not (and startTangent endTangent)) 
      (progn
        (prompt "\n⚠️ No tangents provided, but necessary for vla-AddSpline method. Please provide valid start and end tangents.")
        (exit)
      ))
  (setq stVar (vlax-3d-point (ensure-3d startTangent)))
  (setq enVar (vlax-3d-point (ensure-3d endTangent)))
  
  ;; --- test before creation ---
  (if (/= (rem (length flat) 3) 0)
    (progn (prompt "\n⚠️ Array length not multiple of 3 — invalid coordinates.") (exit))
  )
  
  ;; -- create spline --
  (vl-load-com)
  ;; init acad objects
  (setq *doc* (vla-get-ActiveDocument (vlax-get-acad-object))
        *ms*  (vla-get-ModelSpace *doc*))

  (setq spl (vla-AddSpline *ms* sa stVar enVar))

  ;; -- apply optional style overrides --
  (set-layer-color spl layer color)  
  
  ;; return spline object
  (prompt "\n✅ Spline created successfully.")
  (princ)
  spl
)

(defun draw-line (startPoint endPoint / *doc* *ms* obj)
  "Draws a line from point p1 to p2 using AutoCAD API."
  (vl-load-com)
  ;; init acad objects
  (setq *doc* (vla-get-ActiveDocument (vlax-get-acad-object))
        *ms*  (vla-get-ModelSpace *doc*))
  (setq obj (vla-AddLine *ms* 
                        (vlax-3d-point startPoint)
                        (vlax-3d-point endPoint)))
  obj
)
(defun draw-circle (center radius / *doc* *ms* obj)
  "Draws a circle given center (list) and radius (number)."
  (vl-load-com)
  ;; init acad objects
  (setq *doc* (vla-get-ActiveDocument (vlax-get-acad-object))
        *ms*  (vla-get-ModelSpace *doc*))
  (setq obj (vla-AddCircle *ms* 
                           (vlax-3d-point center) 
                           radius))
  obj
)
(defun draw-Arc (center radius startAngle endAngle / *doc* *ms* obj)
  "Draws an arc by center, radius, start angle, end angle (in radians)."
  (vl-load-com)
  ;; init acad objects
  (setq *doc* (vla-get-ActiveDocument (vlax-get-acad-object))
        *ms*  (vla-get-ModelSpace *doc*))
  (setq obj (vla-AddArc *ms* (vlax-3d-point center)
                    radius startAngle endAngle))
  obj
)
(defun draw-Ellipse (center majorAxis radiusRatio / *doc* *ms* obj)
  "Draws an ellipse from center point, major axis end point, and radius ratio."
  (vl-load-com)
  ;; init acad objects
  (setq *doc* (vla-get-ActiveDocument (vlax-get-acad-object))
        *ms*  (vla-get-ModelSpace *doc*))
  (setq obj (vla-AddEllipse *ms* (vlax-3d-point center)
                            (vlax-3d-point majorAxis)
                            radiusRatio))
  obj
)

(defun draw-Point (pt / *doc* *ms* obj)
  "Creates a point entity at coordinates PT."
  (vl-load-com)
  ;; init acad objects
  (setq *doc* (vla-get-ActiveDocument (vlax-get-acad-object))
        *ms*  (vla-get-ModelSpace *doc*))
  (setq obj (vla-AddPoint *ms* (vlax-3d-point pt)))
  obj
)

(defun draw-Donut (center inner outer / *doc* *ms* ename width rad x y pts obj)
  "Creates a donut (ring) using a closed lightweight polyline."
  (vl-load-com)
  ;; init acad objects
  (setq *doc*   (vla-get-ActiveDocument (vlax-get-Acad-Object))
        *ms*    (vla-get-ModelSpace *doc*))
  (setq width (- outer inner)            ; donut thickness
        rad   (/ (+ inner outer) 2.0)    ; mid radius
        x     (car center)
        y     (cadr center)
  )
  ;; Two endpoints (right, left)
  (setq pts (list (+ x rad) y  (- x rad) y))

  ;; Create base 2-vertex LWPolyline
  (setq obj
         (vla-AddLightWeightPolyline
           *ms*
           (vlax-make-variant
             (vlax-safearray-fill
               (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length pts))))
               pts
             )
           )
         )
  )

  ;; Close and set width
  (vla-put-Closed obj :vlax-true)
  (vla-put-ConstantWidth obj width)

  ;; --- Set bulges via DXF access ---
  ;; COM interface has no direct Bulge property per vertex,
  (setq ename (vlax-vla-object->ename obj))
  (update-bulges-at-indices ename '(0 1) 1) ; bulge per vertex

  (prompt (strcat
    "\n✅ Donut created at "
    (rtos x 2 2) "," (rtos y 2 2)
    "  inner=" (rtos inner 2 2)
    "  outer=" (rtos outer 2 2)))

  obj
)


(defun draw-Text (ins height value / *doc* *ms* obj)
  "Inserts text at given point."
  (vl-load-com)
  ;; init acad objects
  (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-Object))
        *ms*  (vla-get-ModelSpace *doc*))
  (setq obj (vla-AddText *ms* value (vlax-3d-point ins) height))
  obj
)

(defun draw-MText (ins width text height / *doc* *ms* obj)
  "Draws multiline text (MText) at point INS with given WIDTH, HEIGHT."
  (vl-load-com)
  ;; init acad objects
  (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-Object))
        *ms*  (vla-get-ModelSpace *doc*))
  (setq obj (vla-AddMText *ms* (vlax-3d-point ins) width text))
  (if (not height) (setq height 2.5))
  (vla-put-Height obj height)
  obj
)

(defun draw-Leader (pts annotation type / *doc* *ms* sa obj) 
  "Creates a Leader through PTS (list of points) with attached annotation (string or text object)."
  (vl-load-com)
  ;; init acad objects
  (setq *doc* (vla-get-ActiveDocument (vlax-get-acad-object))
        *ms*  (vla-get-ModelSpace *doc*))
  (setq sa (vlax-make-safearray vlax-vbDouble (cons 0 (- (* (length pts) 3) 1))))
  (vlax-safearray-fill sa (apply 'append pts))
  ;; type is one of AcLeaderType enums (0,1,2,...) per API spec
  (setq obj (vla-AddLeader *ms* sa annotation type))
  obj
)

(defun draw-MLeader (pts text stylename / *doc* *ms* obj sa)
  "Creates a MultiLeader with given fit points, text, and MLeaderStyle name."
  (vl-load-com)
  ;; init acad objects
  (setq *doc* (vla-get-ActiveDocument (vlax-get-acad-object))
        *ms*  (vla-get-ModelSpace *doc*))
  (setq sa (vlax-make-safearray vlax-vbDouble (cons 0 (- (* (length pts) 3) 1))))
  (vlax-safearray-fill sa (apply 'append pts))
  (setq obj (vla-AddMLeader *ms* sa))
  (if stylename (vla-put-StyleName obj stylename))
  (if text  (vla-put-TextString obj text))
  obj
)

(defun draw-Hatch (out-boundary in-boundary1 in-boundary2 
                   patternType patternName bAssociativity scale angl 
                   / *doc* *ms* vla-boundary loop sa obj) ;  isSolid
  "Creates a hatchs inside one or more boundary entities (ename or VLA object). ;debug.
   Arguments:
     boundaries (outter, inner1, inner2) - single ename/VLA object or list of them
     patternName  - e.g. \"SOLID\", \"ANSI31\", etc.
     bAssociativity - :vlax-true or :vlax-false
     scale, angle - optional (ignored for SOLID)"

    ;; ------------------
    ;; helper function
    ;; ------------------
    (defun Normalize-boundary (boundary-lst / vla-boundary-lst)
      "Normalize boundary to a list of VLA objects.
      arg:    the boundary is single ename/VLA object or list of them.
      return: list of VLA object"
      
      (if (and boundary-lst (not (listp boundary-lst)))
        (setq boundary-lst (list boundary-lst)))
      (setq vla-boundary-lst '())
      (foreach b boundary-lst
        (setq vla-boundary-lst (cons (if (eq (type b) 'ENAME)
                          (vlax-ename->vla-object b)
                          b)
                        vla-boundary-lst)))
      (setq vla-boundary-lst (reverse vla-boundary-lst))
      vla-boundary-lst
    )

    (defun make-boundary-array (objList / sa indx)
      "create and fill a safearray with given VLA objects.
      objList = list of VLA objects (converted already)"
      ; initialize the safearray
      (setq sa (vlax-make-safearray vlax-vbObject (cons 0 (1- (length objList)))))
      (setq indx 0)
      ; fill the safearray
      (foreach o objList
        (vlax-safearray-put-element sa indx o)
        (setq indx (1+ indx))
      )
      sa ; return the filled safearray
    )
  
    ;; ------------------
    ;; Main
    ;; ------------------

    (vl-load-com)
    ;; --- Get active doc and modelspace ---
    (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-Object))
          *ms*  (vla-get-ModelSpace *doc*))
      
    ;; --- Create the associative Hatch object in model space ---
    (setq obj (vla-AddHatch *ms* patternType patternName bAssociativity acHatchObject))

    ;; --- Create and append the loop(s) for the hatch ---
    (foreach boundary-lst (list out-boundary in-boundary1 in-boundary2)
      (setq i 0)
      ; prepare safearrays with given VLA objects
      (if boundary-lst
        (setq loop (make-boundary-array (Normalize-boundary boundary-lst))))
      ; Append the outer, inner boundary loops to the hatch object 
      (if (= i 0)
        (vla-AppendOuterLoop obj loop) ; outer loop (for i= 0)
        (vla-AppendInnerLoop obj loop) ; inner loop (for i= 1,2)
      )
      (setq i (1+ i))
    )
    
    ;; --- Apply hatch properties ---
    ; Only apply Scale/Angle when it's **not** SOLID
    (if (not (= (strcase patternName) "SOLID"))
      (if scale (vla-put-PatternScale obj scale))
      (if angl  (vla-put-PatternAngle obj angl)))
  
    ;; --- Generate hatch pattern and display ---
    (vla-Evaluate obj)
    (vla-Regen *doc* :vlax-true)
    ; (vla-Regen *doc* acAllViewports)
    

    (prompt "\n✅ Hatch created successfully.")
    (princ)  
    obj
)

(defun draw-Wipeout (pts / *doc* *ms* sa obj)
  "Creates a wipeout polygon through given 2D points."
  (vl-load-com)
  (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-Object))
        *ms*  (vla-get-ModelSpace *doc*))
  (ensure-closed pts) ; Wipeout requires closed polygon
  (setq sa (vlax-make-safearray vlax-vbDouble (cons 0 (- (* (length pts) 3) 1))))
  (vlax-safearray-fill sa (apply 'append pts))
  (setq obj (vla-AddWipeout *ms* sa))
  obj
)

(defun draw-RevCloud (pts arcLength variation / *doc* *ms* sa obj)
  "Creates a revision cloud along the given points."
  (vl-load-com)
  (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-Object))
        *ms*  (vla-get-ModelSpace *doc*))
  (setq sa (vlax-make-safearray vlax-vbDouble (cons 0 (- (* (length pts) 3) 1))))
  (vlax-safearray-fill sa (apply 'append pts))
  (setq obj (vla-AddRevCloud *ms* sa arcLength variation))
  obj
)

(defun draw-Table (ins rows cols rowHeight colWidth / *doc* *ms* obj)
  "Creates an empty table at insertion INS with given dimensions."
  (vl-load-com)
  (setq *doc* (vla-get-ActiveDocument (vlax-get-acad-object))
        *ms*  (vla-get-ModelSpace *doc*))
  (setq obj (vla-AddTable *ms* (vlax-3d-point ins) rows cols rowHeight colWidth))
  obj
)

(defun draw-Dimension (p1 p2 dimLinePt dimStyle / *doc* *ms* obj)
  "Draws a normal aligned dimension between p1 and p2 with optional style, layer, and color.
   Arguments:
     p1, p2       → definition points (list '(x y [z]))
     dimLinePt    → point on dimension line
     dimStyle     → name of dimension style (optional)
     layer, color → optional"
  (vl-load-com)
  (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-Object))
        *ms*  (vla-get-ModelSpace *doc*)
        obj (vla-AddDimAligned *ms*
                (vlax-3d-point p1)
                (vlax-3d-point p2)
                (vlax-3d-point dimLinePt)))

  ;; --- apply style if exists
  (safe-put-StyleName obj dimStyle)
  
  obj
)

(defun draw-DimHorizontal (p1 p2 dimLinePt / *doc* *ms* obj)
  (vl-load-com)
  (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-Object))
        *ms*  (vla-get-ModelSpace *doc*)
        obj (vla-AddDimRotated *ms*
                (vlax-3d-point p1)
                (vlax-3d-point p2)
                (vlax-3d-point dimLinePt)
                0.0)) ; 0 radians = horizontal
  obj
)

(defun insert-Block (blkName insPt scale rot / *doc* *ms* blkRef)
  "Inserts a block reference.
   blkName → existing block name
   insPt   → insertion point (list '(x y [z]))
   scale   → uniform scale factor (number)
   rot     → rotation angle in radians"
  (vl-load-com)
  (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-Object))
        *ms*  (vla-get-ModelSpace *doc*))

  (if (tblsearch "BLOCK" blkName)
    (progn
      (setq blkRef (vla-InsertBlock *ms*
                     (vlax-3d-point insPt)
                     blkName
                     scale scale scale
                     rot))
      blkRef)
    (prompt (strcat "\n⚠️ Block \"" blkName "\" not found."))
  )
)

(defun define-Block (blkName basePt entList / *doc* blocks blkObj)
  "Creates or redefines a block definition (AcDbBlockTableRecord) in the current drawing.
   Arguments:
     blkName  → String name of the block.
     basePt   → List '(x y [z]) base point of the block.
     entList  → List of entity names (ename) to include inside the block.
   Returns:
     The VLA block definition object if successful, NIL otherwise."
  
  ;; ---------------
  ;; Helper Function
  ;; ---------------
  (defun enames->vla-variant-array (enames / objs sa)
    ;; Convert a list of enames to a VARIANT safearray of IDispatch objects
    (setq objs (mapcar 'vlax-ename->vla-object enames))
    (setq sa (vlax-make-safearray vlax-vbObject (cons 0 (1- (length objs)))))
    (vlax-safearray-fill sa objs)
    (vlax-make-variant sa)                    ; return a VARIANT wrapper
  )
  
  
  ;; ---------------
  ;; Main
  ;; ---------------
  (vl-load-com)
  (setq *doc*  (vla-get-ActiveDocument (vlax-get-Acad-Object))
        blocks (vla-get-Blocks *doc*))

  ;; --- Validation ---
  (if (or (not blkName) (not basePt) (not entList))
    (progn (prompt "\n⚠️ Missing arguments for define-Block.") (exit))
  )

  ;; --- Delete existing definition (redefine) ---
  (if (tblsearch "BLOCK" blkName)
    (progn
      (prompt (strcat "\nℹ️ Redefining block \"" blkName "\"..."))
      (vla-Delete (vla-Item blocks blkName))
    )
  )

  ;; --- Create new block definition ---
  (setq blkObj (vla-Add blocks (vlax-3d-point basePt) blkName))

  ;; --- Copy selected entities into the block definition (owner = blkObj)
  (setq varArr (enames->vla-variant-array entList))
  (vla-CopyObjects *doc* varArr blkObj)

  (prompt (strcat "\n✅ Block \"" blkName "\" defined successfully."))
  blkObj
)
; -----------------
; tests

(defun ss->list (ss / i lst)
  (if ss
    (repeat (setq i (sslength ss))
      (setq i (1- i)
            lst (cons (ssname ss i) lst)))
  )
  lst
)
(defun c:MAKEBLOCK ( / ss ents)
  (if (setq ss (ssget "_:L"))
    (progn
      (setq ents (ss->list ss))
      (define-Block "MyBlock" '(0 0 0) ents)
    )
    (prompt "\nNo selection made.")
  )
  (princ)
)


(defun c:DRAW_ALL_TEST (/ p0 p1 p2 p3 pts blkObj)

  (vl-load-com)
  (prompt "\n🚀 Drawing all entity types by API functions...")

  ;; --- base test points ---
  (setq p0 '(0 0 0)
        p1 '(50 0 0)
        p2 '(50 50 0)
        p3 '(0 50 0)
        pts (list p0 p1 p2 p3))

  ;; === 1. LINE ===
  (draw-Line p0 p1)

  ;; === 2. POLYLINE (closed rectangle) ===
  (draw-Poly (list pts T))

  ;; === 3. CIRCLE ===
  (draw-Circle '(100 25 0) 15)

  ;; === 4. ARC ===
  (draw-Arc '(150 25 0) 15 0 (/ pi 2))

  ;; === 5. ELLIPSE ===
  (draw-Ellipse '(200 25 0) '(215 25 0) 0.5)

  ;; === 6. POINT ===
  (draw-Point '(250 25 0))

  ;; === 7. TEXT ===
  (draw-Text '(0 100 0) 3.0 "TEXT Sample")

  ;; === 8. MTEXT === 
  (draw-MText '(40 90 0) 60 "MTEXT Sample — multiline\nSecond line" 3.0)

  ;; === 9. SPLINE ===
  (draw-Spline '((100 100 0) (120 110 0) (140 90 0) (160 100 0)) )

  ;; === 10. DONUT ===
  (draw-Donut '(200 100 0) 5 10 )

  ;; === 11. HATCH (simple poly hatch) ===
  (setq hatchPts (list '(230 90 0) '(260 90 0) '(260 110 0) '(230 110 0) '(230 90 0)))
  (setq hatchPl (draw-Poly (list hatchPts T)))
  (draw-Hatch (list hatchPl) 0 "SOLID" 1.0 0.0)

  ;; === 12. TABLE ===
  (draw-Table '(0 150 0) 3 3 10 20 ) ; (setq ins  '(0 150 0) rows 3 cols 3 rowHeight 10 colWidth 20)

  ;; === 13. DIMENSION ===
  (draw-Dimension '(100 150 0) '(140 150 0) '(120 160 0) "ISO-25" )

  ;; === 14. LEADER (simple leader if defined) ===
  (if (fboundp 'draw-Leader) ; fix.
    (draw-Leader '((180 150 0) (200 160 0)) "Leader Text" 0))

  ;; === 15. MLEADER ===
  (if (fboundp 'draw-MLeader) ; fix.
    (draw-MLeader '((230 150 0) (250 160 0)) "MLeader Text" "Standard"))

  ;; === 16. WIPEOUT ===
  (if (fboundp 'draw-Wipeout) ; fix.
    (draw-Wipeout '((0 200 0) (50 200 0) (50 230 0) (0 230 0)) ))

  ;; === 17. REVISION CLOUD ===
  (if (fboundp 'draw-RevCloud) ; fix.
    (draw-RevCloud '((100 200 0) (120 210 0) (140 200 0) (120 190 0) (100 200 0)) 5.0 0.3))

  ;; === 18. BLOCK DEFINITION + INSERT ===
  (if (fboundp 'define-Block) ; fix.
    (progn
      ;; define block from small rectangle
      (setq blkObj (define-Block "TestBlock" '(0 0 0)
                      (list (entmakex '((0 . "CIRCLE") (10 0.0 0.0 0.0) (40 . 5.0))))))
      ;; insert block
      (insert-Block "TestBlock" '(250 200 0) 1.0 0.0 )
    )
  )

  (prompt "\n✅ All available entity creation functions tested.")
  (princ)
)

(defun c:tst-Draw-hatch (/center radius startAngle endAngle arc line circle objlst1 objlst2 hatchPts hatchPl)

  (setq center (vlax-3d-point 5 3 0)
            radius 3
            startAngle 0
            endAngle 3.141592)
  (progn 
    (setq arc (vla-AddArc *ms* center radius startAngle endAngle))
    (setq line (vla-AddLine modelSpace (vla-get-StartPoint arc) (vla-get-EndPoint arc)))
    (setq objlst1 (list arc line))
  )

  (progn
    (setq circle (vla-addcircle *ms* center 10))
    (setq objlst2 circle)
  )

  (setq hatchPts (list '(20 20 0) '(-20 20 0) '(-20 -10 0) '(20 -10 0) ))
  (setq hatchPl (draw-Poly (list hatchPts T)))

  (setq patternName "ANSI31" patternType acPreDefinedGradient bAssociativity :vlax-true
        scale 10.0 angl 30.0)


  (draw-Hatch hatchPl objlst1 objlst2 
              patternType patternName bAssociativity
              scale angl)
)
  