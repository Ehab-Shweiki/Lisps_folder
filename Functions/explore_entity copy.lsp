
;; ----------------------------------------------
;; Main Command
;; ----------------------------------------------
(defun c:Explore_Entity_Props ( / ent ename edata vlaobj code val meaning ent_type codeStr valStr meaningStr
                                  rows maxCode maxVal maxMeaning row)

  (vl-load-com)
  (prompt "\nSelect an entity to explore its properties: ")
  (setq ent (entsel))

  (if ent
    (progn
      (setq ename (car ent)
            edata (entget ename)
            vlaobj (vlax-ename->vla-object ename)
            ent_type (cdr (assoc 0 edata))
      )

      (prompt (strcat "\nEntity Type: " ent_type))
      
	  ;; --- Show VLA Props ---
      (princ "\n;\n--- VLA Properties ---\n")
      (show-vla-properties vlaobj)  ;or  (vlax-dump-object vlaobj )

      ;; --- Show DXF Props ---
      (prompt "\n---------------------------------\n")
      (prompt "\n;\n--- DXF Properties ---\n")
      (prompt (strcat "\nEntity Type: " ent_type))
	  (show-dxf-properties edata ent_type)
      
    )
    (prompt "\nNothing selected.")
  )
  (princ)
)

(princ "\nType Explore_Entity_Props to run Explore_Entity_Props function.")
(princ)

; -------------------------------------
; Main Helper Functions
; -------------------------------------


(defun repeat-space (n / s)
  ;; Return a string of `n` space characters
  (setq s "")
  (repeat (max 0 n) (setq s (strcat s " ")))
  s
)

(defun make-right-pad (s w / s)
  ;; Pad string s on the left to reach total width w
  (strcat (repeat-space (- w (strlen s))) s)
)

(defun make-left-pad (s w / s)
  ;; Pad string s on the right to reach total width w
  (strcat s (repeat-space (- w (strlen s))))
)

(defun show-vla-properties (vlaobj / props prop val valStr nameStr meanStr rows maxName maxVal maxMean row)
  (princ "\n;\n--- VLA Properties (Aligned) ---\n")

  ;; Default known property list (can be extended per object type)
  (setq props
    '("Application" "Color" "Handle" "Layer" "Linetype" "LinetypeScale"
      "Length" "Center" "Radius" "StartPoint" "EndPoint" "Normal"
      "Thickness" "Height" "TextString" "InsertionPoint" "Rotation"
    )
  )

  (setq rows '()
        maxName 0
        maxVal 0
        maxMean 0)

  ;; Attempt to get each property
  (foreach prop props
    (if (vlax-property-available-p vlaobj prop)
      (progn
        (setq val (vl-catch-all-apply 'vlax-get (list vlaobj prop)))
        (if (vl-catch-all-error-p val)
          (setq valStr "[Error]")
          (setq valStr (vl-princ-to-string val))
        )

        (setq nameStr prop)
        (setq meanStr (or (get-vla-meaning nameStr) ""))
        (setq maxName (max maxName (strlen nameStr)))
        (setq maxVal  (max maxVal  (strlen valStr)))
        (setq maxMean (max maxMean (strlen meanStr)))
        (setq rows (cons (list nameStr valStr meanStr) rows))
      )
    )
  )

  (setq rows (reverse rows))
  (foreach row rows
    (setq nameStr (car row)
          valStr  (cadr row)
          meanStr (caddr row))
    (princ
      (strcat "\n"
        (make-left-pad nameStr maxName) " : "
        (make-left-pad valStr  maxVal)  "   "
        meanStr
      )
    )
  )
)

(defun show-dxf-properties (edata ent_type / rows maxCode maxVal maxMeaning code val codeStr valStr meaning meaningStr row)
  (princ "\n;\n--- DXF Properties (Aligned) ---\n")
  (prompt (strcat "\nEntity Type: " ent_type))

  (setq rows '()
        maxCode 0
        maxVal 0
        maxMeaning 0)

  ;; First pass: collect info and determine max widths
  (foreach pair edata
    (setq code (car pair)
          val  (cdr pair)
          codeStr (itoa code)
          valStr  (vl-princ-to-string val)
          meaning (get-dxf-meaning-by-type ent_type code)
          meaningStr (if meaning (strcat "--[" meaning "]--") "")
          row (list codeStr valStr meaningStr)
    )

    (setq maxCode (max maxCode (strlen codeStr)))
    (setq maxVal  (max maxVal  (strlen valStr)))
    (setq maxMeaning (max maxMeaning (strlen meaningStr)))

    (setq rows (cons row rows))
  )

  (setq rows (reverse rows)) ; Preserve original order

  ;; Print aligned result
  (foreach row rows
    (setq codeStr (car row)
          valStr  (cadr row)
          meaningStr (caddr row))
    
    (princ
      (strcat "\n"
        (make-right-pad codeStr maxCode) " : "
        (make-left-pad  valStr  maxVal)  "   "
        meaningStr
      )
    )
  )
)

; -------------------------------------
; Internal Helper Functions
; -------------------------------------

(defun get-vla-meaning (prop)
  (cdr
    (assoc (strcase prop)
     '(
       ("APPLICATION" . "AutoCAD application object")
       ("COLOR" . "ACI Color index")
       ("HANDLE" . "Unique object ID")
       ("LAYER" . "Layer name")
       ("LINETYPE" . "Linetype name")
       ("LINETYPESCALE" . "Linetype scale factor")
       ("LENGTH" . "Entity length")
       ("CENTER" . "Center point of arc/circle")
       ("RADIUS" . "Radius")
       ("STARTPOINT" . "Start point")
       ("ENDPOINT" . "End point")
       ("ELEVATION" . "Elevation or Z value")
       ("EXTRUSION" . "Extrusion vector [X Y Z]")
	   ("TEXTSTRING" . "Text string")
	   ("STYLE" . "Text style name")
	   ("BLOCKNAME" . "Block name for INSERT entities")
	   ("INSERTIONPOINT" . "Insertion point [X Y Z]")
	   ("SCALEFACTOR" . "Scale factor for INSERT entities")
	   ("ROTATION" . "Rotation angle for INSERT entities")
	   ("VISIBLE" . "Visibility state (0=Invisible, 1=Visible)")
	   ("OBJECTNAME" . "Type of the object (e.g., AcDbLine, AcDbCircle)")
	   ("OBJECTID" . "Unique ID of the object")
	   ("OBJECTTYPE" . "Type of the object (e.g., AcDbText, AcDbMText)")
	   ("SUBCLASS" . "Subclass marker for DXF entities")
	   ("EXTRUSIONVECTOR" . "Extrusion vector [X Y Z]")
	   ("TEXTHEIGHT" . "Height of text entities")
	   ("TEXTROTATION" . "Rotation angle of text entities")
	   ("ATTACHMENTPOINT" . "Attachment point for dimension text")
	   ("DIMSTYLE" . "Dimension style name")
	   ("PATTERNNAME" . "Hatch pattern name")
	   ("BULGE" . "Bulge value for polylines")
	   ("VERTICES" . "Vertices of a polyline or polygon")
	   ("NUMBEROFVERTICES" . "Number of vertices in a polyline")
	   ("ASSOCIATIVITY" . "Associativity flag for hatch entities")
	   ("PATTERNSCALE" . "Scale factor for hatch patterns")
	   ("PATTERNANGLE" . "Angle of hatch pattern")
	   ("BULGEWIDTH" . "Width of bulge for polylines")
	   ("TEXTALIGNMENT" . "Text alignment flags")
	   ("TEXTDIRECTION" . "Direction of text in MTEXT entities")
	   ("LINEWEIGHT" . "Line weight of the entity")
	   ("BLOCKREFCOUNT" . "Number of block references")
	   ("BLOCKREFS" . "Block references in the drawing")
	   ("DIMENSIONSTYLE" . "Dimension style name for dimensions")
	   ("HATCHSTYLE" . "Style of the hatch pattern")
	   ("HATCHBOUNDARY" . "Boundary paths for hatch entities")
	   ("HATCHELEVATION" . "Elevation of the hatch entity")
	   ("HATCHSOLIDFILL" . "Solid fill flag for hatch entities")
	   ("HATCHASSOCIATIVITY" . "Associativity flag for hatch entities")
	   ("REVISIONCLOUDVERTICES" . "Vertices of revision cloud")
	   ("REVISIONCLOUDBULGE" . "Bulge value for revision cloud vertices")
	   ("REVISIONCLOUDFLAGS" . "Flags for revision cloud entity")
	   ("DIMENSIONBLOCKNAME" . "Block name for dimension graphics")
	   ("DIMENSIONDEFINITIONPOINT" . "Definition point for dimension")
	   ("DIMENSIONTEXTPOINT" . "Text point for dimension text")
	   ("DIMENSIONLINEARMEASUREMENT" . "Actual measurement for dimension")
	   ("DIMENSIONTEXTANGLE" . "Angle of dimension text")
	   ("DIMENSIONHORIZONTALDIRECTION" . "Horizontal direction for dimension")
	   ("DIMENSIONEXTRUSIONVECTOR" . "Extrusion vector for dimension")
	   ("DIMENSIONATTACHMENTPOINT" . "Attachment point for dimension text")
	   ("DIMENSIONSPACINGSTYLE" . "Spacing style for dimension text")
	   ("DIMENSIONLINEARSPACINGFACTOR" . "Line spacing factor for dimension text")
	   ("DIMENSIONTEXT" . "Text string for dimension")
	   ("DIMENSIONANGLE" . "Angle of dimension text")
	   ("DIMENSIONSTYLENAME" . "Dimension style name for dimensions")
	   ("PATTERNENTTYPE" . "Entity type of the hatch pattern")
	   ("HATCHPATTERNANGLE" . "Angle of the hatch pattern")
	   ("HATCHPATTERNSCALE" . "Scale factor of the hatch pattern")
	   ("HATCHDOUBLEFLAG" . "Double flag for hatch patterns")
	   ("HATCHSTYLE" . "Style of the hatch pattern")
	   ("HATCHBOUNDARYPATHS" . "Boundary paths for hatch entities")
	   ("HATCHELEVATION" . "Elevation of the hatch entity")
	   ("HATCHSOLIDFILL" . "Solid fill flag for hatch entities")
	   ("HATCHASSOCIATIVITY" . "Associativity flag for hatch entities")
	   ("REVISIONCLOUDVERTICES" . "Vertices of revision cloud")
	   ("REVISIONCLOUDBULGE" . "Bulge value for revision cloud vertices")
	   ("REVISIONCLOUDFLAGS" . "Flags for revision cloud entity")
	   ("DIMENSIONBLOCKNAME" . "Block name for dimension graphics")
	   ("DIMENSIONDEFINITIONPOINT" . "Definition point for dimension")
	   ("DIMENSIONTEXTPOINT" . "Text point for dimension text")
	   ("DIMENSIONLINEARMEASUREMENT" . "Actual measurement for dimension")
	   ("DIMENSIONTEXTANGLE" . "Angle of dimension text")
	   ("DIMENSIONHORIZONTALDIRECTION" . "Horizontal direction for dimension")
	   ("DIMENSIONEXTRUSIONVECTOR" . "Extrusion vector for dimension")
       ;; Add more as needed
     ))
  )
)

;--------------------------
(defun get-dxf-meaning-by-type (ent_type code / specific)
  (setq ent_type (strcase ent_type)) ; ensure uppercase string for comparison

  ;; Get specific meaning based on entity type

  (setq specific
    (cond
      ((= ent_type "TEXT")        (get-dxf-meaning-text code))
      ((= ent_type "MTEXT")       (get-dxf-meaning-mtext code))
      ((= ent_type "LINE")        (get-dxf-meaning-line code))
      ((= ent_type "LWPOLYLINE")  (get-dxf-meaning-lwpolyline code))
      ((= ent_type "ARC")         (get-dxf-meaning-arc code))
      ((= ent_type "CIRCLE")      (get-dxf-meaning-circle code))
      ((= ent_type "INSERT")      (get-dxf-meaning-insert code))
      ((= ent_type "HATCH")       (get-dxf-meaning-hatch code))
      ((= ent_type "REVCLOUD")    (get-dxf-meaning-revcloud code))
      ((= ent_type "DIMENSION")   (get-dxf-meaning-dimension code))
      (T "None")
    )
  )

  ;; If specific returns "None", then try general meaning
  (if (= specific "None")
    (get-dxf-meaning-general code)
  )
)

; (defun get-dxf-meaning-by-type (ent_type code)

;   (cond
;     ((equal ent_type "TEXT")        (or (get-dxf-meaning-text code) (get-dxf-meaning-general code)))
;     ((equal ent_type "MTEXT")       (or (get-dxf-meaning-mtext code) (get-dxf-meaning-general code)))
;     ((equal ent_type "LINE")        (or (get-dxf-meaning-line code) (get-dxf-meaning-general code)))
;     ((equal ent_type "LWPOLYLINE")  (or (get-dxf-meaning-lwpolyline code) (get-dxf-meaning-general code)))
;     ((equal ent_type "ARC")         (or (get-dxf-meaning-arc code) (get-dxf-meaning-general code)))
;     ((equal ent_type "CIRCLE")      (or (get-dxf-meaning-circle code) (get-dxf-meaning-general code)))
;     ((equal ent_type "INSERT")      (or (get-dxf-meaning-insert code) (get-dxf-meaning-general code)))
;     ((equal ent_type "HATCH")       (or (get-dxf-meaning-hatch code) (get-dxf-meaning-general code)))
;     ((equal ent_type "REVCLOUD")    (or (get-dxf-meaning-revcloud code) (get-dxf-meaning-general code)))
;     ((equal ent_type "DIMENSION")   (or (get-dxf-meaning-dimension code) (get-dxf-meaning-general code)))
;     (T                          (get-dxf-meaning-general code))))

; -------------------------------------

(defun get-dxf-meaning-text (code)
  (cond ((= code 1) "Text string")
        ((= code 7) "Text style name")
        ((= code 10) "Insertion point [X]")
        ((= code 20) "Insertion point [Y]")
        ((= code 30) "Insertion point [Z]")
        ((= code 40) "Text height")
        ((= code 50) "Text rotation angle")
        ((= code 41) "X scale")
        ((= code 51) "Oblique angle")
        ((= code 62) "Color (ACI)")
        ((= code 71) "Text flags (2=Backward, 4=Upside down)")
        ((= code 72) "Horizontal alignment")
        ((= code 73) "Vertical alignment")
        ((= code 11) "Alignment point [X]")
        ((= code 21) "Alignment point [Y]")
        ((= code 31) "Alignment point [Z]")
        (T "None"))
  
)

(defun get-dxf-meaning-mtext (code)
  (cond ((= code 1) "Contents")
        ((= code 7) "Text style name")
        ((= code 10) "Insertion point [X]")
        ((= code 20) "Insertion point [Y]")
        ((= code 30) "Insertion point [Z]")
        ((= code 40) "Text height")
        ((= code 41) "Rect width")
        ((= code 71) "Attachment")
        ((= code 72) "Direction")
        ((= code 73) "Line spacing style")
        ((= code 44) "Line spacing factor")
        ((= code 50) "Rotation angle")
        ((= code 210) "Extrusion [X]")
        ((= code 220) "Extrusion [Y]")
        ((= code 230) "Extrusion [Z]")
        (T "None")))

(defun get-dxf-meaning-line (code)
  (cond ((= code 10) "Start point [X]")
        ((= code 20) "Start point [Y]")
        ((= code 30) "Start point [Z]")
        ((= code 11) "End point [X]")
        ((= code 21) "End point [Y]")
        ((= code 31) "End point [Z]")
        ((= code 210) "Extrusion [X]")
        ((= code 220) "Extrusion [Y]")
        ((= code 230) "Extrusion [Z]")
        (T "None")))

(defun get-dxf-meaning-lwpolyline (code)
  (cond ((= code 90) "Number of vertices")
        ((= code 70) "Flags (1=Closed)")
        ((= code 10) "Vertex X")
        ((= code 20) "Vertex Y")
        ((= code 40) "Start width")
        ((= code 41) "End width")
        ((= code 42) "Bulge")
        ((= code 43) "Const width")
        ((= code 210) "Extrusion [X]")
        ((= code 220) "Extrusion [Y]")
        ((= code 230) "Extrusion [Z]")
        (T "None")))

(defun get-dxf-meaning-arc (code)
  (cond ((= code 10) "Center [X]")
        ((= code 20) "Center [Y]")
        ((= code 30) "Center [Z]")
        ((= code 40) "Radius")
        ((= code 50) "Start angle")
        ((= code 51) "End angle")
        ((= code 210) "Extrusion [X]")
        ((= code 220) "Extrusion [Y]")
        ((= code 230) "Extrusion [Z]")
        (T "None")))

(defun get-dxf-meaning-circle (code)
  (cond ((= code 10) "Center [X]")
        ((= code 20) "Center [Y]")
        ((= code 30) "Center [Z]")
        ((= code 40) "Radius")
        ((= code 210) "Extrusion [X]")
        ((= code 220) "Extrusion [Y]")
        ((= code 230) "Extrusion [Z]")
        (T "None")))

(defun get-dxf-meaning-insert (code)
  (cond ((= code 2) "Block name")
        ((= code 10) "Insert point [X]")
        ((= code 20) "Insert point [Y]")
        ((= code 30) "Insert point [Z]")
        ((= code 41) "X scale")
        ((= code 42) "Y scale")
        ((= code 43) "Z scale")
        ((= code 50) "Rotation")
        ((= code 66) "Attributes follow?")
        (T "None")))

(defun get-dxf-meaning-hatch (code)
  (cond ((= code 10) "Elevation [X]")
        ((= code 20) "Elevation [Y]")
        ((= code 30) "Elevation [Z]")
        ((= code 2) "Pattern name")
        ((= code 70) "Solid fill flag")
        ((= code 71) "Associativity")
        ((= code 91) "Boundary paths")
        ((= code 75) "Style")
        ((= code 76) "Pattern ent_type")
        ((= code 52) "Pattern angle")
        ((= code 41) "Pattern scale")
        ((= code 47) "Double flag")
        (T "None")))

(defun get-dxf-meaning-revcloud (code)
  (cond ((= code 10) "Vertex [X]")
        ((= code 20) "Vertex [Y]")
        ((= code 40) "Bulge/width")
        ((= code 42) "Bulge")
        ((= code 70) "Flags")
        (T "None")))

(defun get-dxf-meaning-dimension (code)
  (cond ((= code 2) "Block name for graphics")
        ((= code 10) "Definition pt [X]")
        ((= code 20) "Definition pt [Y]")
        ((= code 30) "Definition pt [Z]")
        ((= code 11) "Text pt [X]")
        ((= code 21) "Text pt [Y]")
        ((= code 31) "Text pt [Z]")
        ((= code 70) "Dimension ent_type")
        ((= code 71) "Attachment pt")
        ((= code 72) "Text spacing style")
        ((= code 41) "Line spacing factor")
        ((= code 42) "Actual measurement")
        ((= code 1) "Dimension text")
        ((= code 53) "Text angle")
        ((= code 51) "Horizontal dir")
        ((= code 210) "Extrusion [X]")
        ((= code 220) "Extrusion [Y]")
        ((= code 230) "Extrusion [Z]")
        ((= code 3) "Dim style name")
        (T "None")))

(defun get-dxf-meaning-general (code)
  (cond ((= code 0) "Entity Type")
        ((= code 5) "Handle")
        ((= code 6) "Linetype")
        ((= code 8) "Layer")
        ((= code 62) "Color (ACI)")
        ((= code 67) "Model or Paper space")
        ((= code 48) "Linetype scale")
        ((= code 60) "Visibility")
        ((= code 100) "Subclass")
        ((>= code 210) "Extrusion vector")
        ((>= code 300) "Text / Names")
        ((>= code 1000) "XDATA")
        (T "None")))
