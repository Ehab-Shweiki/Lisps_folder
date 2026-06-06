; TODO: add text manualy with \n (for multiple lines of info)
; TODO: diff ways to draw Top Bars
; TODO: set layers and dims
; TODO: draw extension dimentions for top bars 
; TODO: UI to input all detailed info (such as extension dimenstion)
((obj (vlax-ename->vla-object ename))
             (p1 (vlax-get obj 'StartPoint))
             (p2 (vlax-get obj 'EndPoint))
             (p3 (vlax-get-property obj 'EndPoint)))
(defun c:LReinf (/ AddLine AddPline AddDim ;; calculation functions
                   UnitFactorAdjust GetUserOptions GetVertices GetTopPoints DrawTop DrawBot ProcessPolylines
                 
                )
  (vlax-curve-getStartPoint o)

  ;; =============================================================
  ;; 1) Define LOCAL helper functions (API drawing)
  ;; =============================================================

  ;; -----------------------------
  ;; Create LINE entity using API
  ;; -----------------------------
  (setq AddLine
    (lambda (p1 p2 / ms)
      (setq ms (vla-get-ModelSpace
                 (vla-get-ActiveDocument (vlax-get-acad-object))))
      (vla-AddLine ms
        (vlax-3d-point p1)
        (vlax-3d-point p2))
    )
    ; (command "line" ptP ptN "")
  )

  ;; --------------------------------------------------
  ;; Create LWPolyline from flat XY list using API
  ;; --------------------------------------------------
  (setq AddPline
    (lambda (flat / ms arr)
      (setq ms (vla-get-ModelSpace
                 (vla-get-ActiveDocument (vlax-get-acad-object))))
      (setq arr (vlax-make-safearray vlax-vbDouble 
                    (cons 0 (1- (length flat)))))
      (vlax-safearray-fill arr flat)
      (vla-AddLightweightPolyline ms arr)
    )
    ; (command "Pline" ptP ptP2 ptN2 ptN "")
  )

  ;; --------------------------------------------------
  ;; Add aligned dimension using API
  ;; --------------------------------------------------
  (setq AddDim
    (lambda (p1 p2 dimpt text / ms dim)
      (setq ms (vla-get-ModelSpace
                 (vla-get-ActiveDocument (vlax-get-acad-object))))
      (setq dim
        (vla-AddDimAligned
          ms (vlax-3d-point p1)
             (vlax-3d-point p2)
             (vlax-3d-point dimpt)
        )
      )
      (if text (vla-put-TextOverride dim text))
      dim
    )
    ; (command "dimaligned" 
    ;           ptP
    ;           ptN
    ;           "t"
    ;           (strcat "Ø" (rtos BRdia 2 0)   "@" (rtos BRs 2 0)
    ;                        sUnt " "   BdimPretxt "<>"
    ;           )
    ;           (polar ptN TangOff (* dimsgn dimOff))
    ; )
  )
  ;; --------------------------------------------------
  (setq *UnitTable*
    '(
      ("mm"  . 0.001)        ; 1 mm  = 0.001 m
      ("cm"  . 0.01)         ; 1 cm  = 0.01 m
      ("m"   . 1.0)          ; base unit
      ("km"  . 1000.0)

      ("inch" . 0.0254)      ; 1 inch = 0.0254 m
      ("ft"   . 0.3048)      ; 1 ft   = 0.3048 m
      ("yd"   . 0.9144)
    )
  )
  (defun GetUnitFactor (unit / val)
    (setq val (cdr (assoc unit *UnitTable*)))
    (if val val
      (progn
        (prompt "\nUnknown unit.")
        nil
      )
    )
  )
  (defun roundTo (value precision)
    (if precision
      (setq factor (/ 1.0 precision))
      (setq factor 1.0)
    )
    (/ (fix (+ (* value factor) 0.5)) factor)
  )
  (defun UnitScale (oldUnit newUnit / oldF newF)
    (setq oldF (GetUnitFactor oldUnit))
    (setq newF (GetUnitFactor newUnit))
    (if (and oldF newF)
      (roundTo (/ newF oldF) 1e-8) ; the needed factor rounded to FP errors
      nil
    )
  )
  (defun ConvertValue (value oldUnit newUnit / factor)
    
    (setq factor (UnitScale oldUnit newUnit))
    (if factor
      (* value factor)
    )
  )


  
  ;; =============================================================
  ;; 2) Define core calculation functions
  ;; =============================================================

  ;; ------------------------------------------------------------
  ;; Local function: Get original parameters
  ;; ------------------------------------------------------------
  
  (setq GetUserOptions
    (lambda ( /
            ;; original parameters
            TminDist BminDist offDist diffDist splice barEnd BbarOff
            TRdia TRs BRdia BRs unit sUnt TdimPretxt BdimPretxt dimOff dimsgn manual-unit?
            kw inp unitF continue unit-old
            )
      
      ;; default values
      (if (null *LR-Opts*)
        (setq *LR-Opts* (list 
          (setq TminDist    1.0) ; TminDist: (for Top Reinf. & for support length) min distance between vertix to introduce a point in between
          (setq BminDist    1.0) ; BminDist: (for Bot Reinf. & for support length) min distance between supports to cut at end of 2nd support
          (setq offDist     0.5) ; offDist: offset distance for Top/Bot Reinf. from the Polyline
          (setq diffDist    0.08) ; diffDist: a distance between two adjacent bars (Top&Top / Bot&BOt)
          (setq splice      0.40) ; splice: splice length (total) between Top Reinf.
          (setq barEnd      0.02) ; barend: an 45 degree shape at end of bar "if zero then none"
          (setq BbarOff     0.05) ; BbarOff: Bot bar end offset from end of support
          (setq TRdia       10.0) ; TRdia: dia. of Top reinf
          (setq TRs         20.0) ; TRs: spacing between T.R
          (setq BRdia       10.0) ; TRdia: dia. of Top reinf
          (setq BRs         20.0) ; TRs: spacing between T.R
          (setq unit        "m")  ; unit: the unit of lengths
          (setq sUnt        "cm") ; sUnt:the unit of spacing
          (setq TdimPretxt  "T.B. L=") ; dimPretxt: the prefix text before Length value for Top reinf.
          (setq BdimPretxt  "B.B. L=") ; dimpretxt: the prefix text before Length value for Top reinf.
          (setq dimOff      0.25) ; dimOff: dimention offset from Reinforcement bar (+ve: Top dim above Top Reinf.)
          (setq dimsgn      -1.0) ; dimsgn: dimention direction sign for Top&Bot Reinf. (1.0: both at same direction, -1.0: directions are opposite to each other)
          (setq manual-unit? nil) ; for auto calculating unit factor
        ))
        (progn
          (setq TminDist      (nth 0  *LR-Opts*)
                BminDist      (nth 1  *LR-Opts*)
                offDist       (nth 2  *LR-Opts*)
                diffDist      (nth 3  *LR-Opts*)
                splice        (nth 4  *LR-Opts*)
                barEnd        (nth 5  *LR-Opts*)
                BbarOff       (nth 6  *LR-Opts*)
                TRdia         (nth 7  *LR-Opts*)
                TRs           (nth 8  *LR-Opts*)  
                BRdia         (nth 9  *LR-Opts*)
                BRs           (nth 10 *LR-Opts*) 
                unit          (nth 11 *LR-Opts*)
                sUnt          (nth 12 *LR-Opts*)
                TdimPretxt    (nth 13 *LR-Opts*)
                BdimPretxt    (nth 14 *LR-Opts*)
                dimOff        (nth 15 *LR-Opts*)
                dimsgn        (nth 16 *LR-Opts*)
                manual-unit?  (nth 17 *LR-Opts*))
        )
      )

      ;; ======= OPTIONS MENU using initget =======
      (princ "\nOptions:")
      (princ "\n Tmin    ? top midpoint minimum distance")
      (princ "\n Bmin    ? bottom cut minimum distance")
      (princ "\n Offset  ? offDist diffDist BbarOff")
      (princ "\n Splice  ? splice length")
      (princ "\n Bars    ? TRdia/TRs/BRdia/BRs")
      (princ "\n BarEnd  ? barEnd & BbarOff")
      (princ "\n unit    ? unit of lengths")
      (princ "\n Texts   ? sUnt, TdimPretxt, BdimPretxt")
      (princ "\n Dims    ? dimOff & dimsgn")
      (princ "\n All     ? edit all parameters")
      (princ "\nPress ENTER to skip.\n")

      ;; ===============================
      ;;      MULTI-CHOICE MENU LOOP
      ;; ===============================
      (setq continue T)
      (while continue
        (initget "Tmin Bmin Offset Splice Bars EndBar Unit Texts Dims All")
        (setq kw (getkword "\nSelect option [Tmin/Bmin/Offset/Splice/Bars/EndBar/Unit/Texts/Dims/All] <Done>: "))
        
        ;; user pressed ENTER ? stop the menu
        (if (null kw)
          (setq continue nil)
        )
        
        ;; ========== HANDLE EACH OPTION ==========
        
        ;; ---- Tmin ----
        (if (= kw "Tmin")
          (if (setq inp (getreal (strcat "\nTminDist <" (rtos TminDist) ">: ")))
            (setq TminDist inp)
          )
        )

        ;; ---- Bmin ----
        (if (= kw "Bmin")
          (if (setq inp (getreal (strcat "\nBminDist <" (rtos BminDist) ">: ")))
            (setq BminDist inp)
          )
        )
        
        ;; ---- Offset group ---- : offDist, diffDist, BbarOff
        (if (= kw "Offset")
          (progn
            (if (setq inp (getreal (strcat "\noffDist <" (rtos offDist) ">: ")))   (setq offDist  inp))
            (if (setq inp (getreal (strcat "\ndiffDist <" (rtos diffDist) ">: "))) (setq diffDist inp))
            (if (setq inp (getreal (strcat "\nBbarOff <" (rtos BbarOff) ">: ")))   (setq BbarOff  inp))
          )
        )

        ;; ---- Splice ----
        (if (= kw "Splice")
          (if (setq inp (getreal (strcat "\nsplice <" (rtos splice) ">: ")))
            (setq splice inp)
          )
        )

        ;; ---- Bars group ----
        (if (= kw "Bars")
          (progn
            (if (setq inp (getreal (strcat "\nTop dia TRdia <" (rtos TRdia) ">: "))) (setq TRdia inp))
            (if (setq inp (getreal (strcat "\nTop spacing TRs <" (rtos TRs) ">: "))) (setq TRs   inp))
            (if (setq inp (getreal (strcat "\nBot dia BRdia <" (rtos TRs) ">: ")))   (setq BRdia inp))
            (if (setq inp (getreal (strcat "\nBot spacing BRs <" (rtos BRs) ">: "))) (setq BRs   inp))
          )
        )

        ;; BarEnd group: barEnd
        (if (= kw "EndBar")
          (if (setq inp (getreal (strcat "\nbarEnd <" (rtos barEnd) ">: "))) (setq barEnd inp))
        )
        
        ;; Unit
        (if (= kw "Unit")
          (progn
            (setq unit-old unit)
            (initget "m cm mm")
            (setq unit (getkword (strcat "\nSelect option [m/cm/mm] <" unit-old ">: ")))
            (setq unitF (UnitScale unit unit-old))
            
            ; convert others based on unit change
            (if (/= unitF 1.0) 
              (setq TminDist (* unitF TminDist)
                    BminDist (* unitF BminDist)
                    offDist  (* unitF offDist)
                    diffDist (* unitF diffDist)
                    splice   (* unitF splice)
                    barend   (* unitF barend)
                    BbarOff  (* unitF BbarOff)
                    dimOff   (* unitF dimOff)
              )
            )
          )
        )
        
        ;; Texts group: sUnt, TdimPretxt, BdimPretxt
        (if (= kw "Texts")
          (progn
            (if (setq inp (getstring T (strcat "\nTdimPretxt <" TdimPretxt ">: ")))   (setq TdimPretxt inp))
            (if (setq inp (getstring T (strcat "\nBdimPretxt <" BdimPretxt ">: ")))   (setq BdimPretxt inp))
            (if (setq inp (getstring T (strcat "\nsUnt <" sUnt ">: ")))               (setq sUnt       inp))
          )
        )
        
        ;; ---- Dims ----
        (if (= kw "Dims")
          (progn
            (if (setq inp (getreal (strcat "\ndimOff <" (rtos dimOff) ">: "))) (setq dimOff inp))
            (if (setq inp (getreal (strcat "\ndimsgn <" (rtos dimsgn) ">: "))) (setq dimsgn inp))
          )
        )

        ;; ---- All ----
        (if (= kw "All")
          (progn
            (if (setq inp (getreal     (strcat "\nTminDist <" (rtos TminDist) ">: ")))         (setq TminDist inp))
            (if (setq inp (getreal     (strcat "\nBminDist <" (rtos BminDist) ">: ")))         (setq BminDist inp))
            (if (setq inp (getreal     (strcat "\noffDist <" (rtos offDist) ">: ")))           (setq offDist  inp))
            (if (setq inp (getreal     (strcat "\ndiffDist <" (rtos diffDist) ">: ")))         (setq diffDist inp))
            (if (setq inp (getreal     (strcat "\nsplice <" (rtos splice) ">: ")))             (setq splice   inp))
            (if (setq inp (getreal     (strcat "\nbarEnd <" (rtos barEnd) ">: ")))             (setq barEnd   inp))
            (if (setq inp (getreal     (strcat "\nBbarOff <" (rtos BbarOff) ">: ")))           (setq BbarOff  inp))
            (if (setq inp (getreal     (strcat "\nTRdia <" (rtos TRdia) ">: ")))               (setq TRdia    inp))
            (if (setq inp (getreal     (strcat "\nTRs <" (rtos TRs) ">: ")))                   (setq TRs      inp))
            (if (setq inp (getreal     (strcat "\nBRdia <" (rtos BRdia) ">: ")))               (setq BRdia    inp))
            (if (setq inp (getreal     (strcat "\nBRs <" (rtos BRs) ">: ")))                   (setq BRs      inp))
            (if (setq inp (getstring T (strcat "\nsUnt <" sUnt ">: ")))                        (setq sUnt     inp))
            (if (setq inp (getstring T (strcat "\nTdimPretxt <" TdimPretxt ">: ")))            (setq TdimPretxt inp))
            (if (setq inp (getstring T (strcat "\nBdimPretxt <" BdimPretxt ">: ")))            (setq BdimPretxt inp))
            (if (setq inp (getreal     (strcat "\ndimOff <" (rtos dimOff) ">: ")))             (setq dimOff   inp))
            (if (setq inp (getreal     (strcat "\ndimsgn <" (rtos dimsgn) ">: ")))             (setq dimsgn   inp))
          )
        )
      ) ; end while

      ;; RETURN FINAL LIST
      (setq *LR-Opts* (list 
        TminDist BminDist offDist diffDist splice barEnd BbarOff
        TRdia TRs BRdia BRs unit sUnt TdimPretxt BdimPretxt dimOff dimsgn manual-unit?
      ))
    )
  )


  ;; ------------------------------------------------------------
  ;; Local function: Unit factor calculate & redefine distances
  ;; (depend on max polyline length)
  ;; ------------------------------------------------------------
  (setq UnitFactorAdjust
    ;Note: variables got from parent function:
    ;      sset TminDist BminDist offDist diffDist splice barend BbarOff dimOff
    (lambda ( / maxTlength j obj Tlength unitF)
      ; Unit factor calculate & redefine distances (depend on max polyline length)
      (setq maxTlength 0.0) ; maxTlength: max total length of all objects (polylines)
      
      (setq j 0)
      (while (< j (sslength sset)) 
        (setq obj (ssname sset j))
        (setq Tlength (vlax-curve-getDistAtPoint obj (vlax-curve-getEndPoint obj))) ; Tlength: total length for an object
        (if (> Tlength Tlength) 
          (setq maxTlength Tlength)
        )
        (setq j (1+ j))
      )
      
      ; auto calculate unitF (1.0 if meter, 100.0 if cm 1000.0 if mm) based on the total length of polyline
      (cond 
        ((>= maxTlength 7000.0) 
          (setq unitF 1000.0
                unit  "mm"))
        ((>= maxTlength 200.0)
          (setq unitF 100.0
                unit  "cm"))
        (T
          (setq unitF 1.0
                unit  "m")))
            
      (if (/= unitF 1.0)
        (setq TminDist (* unitF TminDist)
              BminDist (* unitF BminDist)
              offDist  (* unitF offDist)
              diffDist (* unitF diffDist)
              splice   (* unitF splice)
              barend   (* unitF barend)
              BbarOff  (* unitF BbarOff)
              dimOff   (* unitF dimOff)
        )
      )
    )
  )
  
  ;; --------------------------------------------------
  ;; Extract unique vertices from LWPolyline
  ;; --------------------------------------------------
  (setq GetVertices
    (lambda (ent unitF / raw vrs vr)
      (setq raw (vl-remove-if '(lambda(x)(/= (car x) 10)) ent)) ; all Pline vertices (including replicated)
      (setq vrs nil vr nil)
      (foreach r raw
        ;; avoide nearly duplicated vertices
        (if (not (equal (cdr r) vr (* 0.01 unitF)))  ; 0.01 meter
          (progn
            (setq vr (cdr r))
            (setq vrs (append vrs (list vr)))
          )
        )
      )
      vrs
    )
  )

  ;; --------------------------------------------------
  ;; Compute list of Top reinforcement calculation points
  ;; start, midpoints, end
  ;; --------------------------------------------------
  (setq GetTopPoints
    (lambda (vrs TminDist / Tpts vrP vrN i n)
      (setq Tpts (list (car vrs)))
      (setq vrP (cadr vrs))
      (setq i 2  n (length vrs))
      (while (< i n)
        (setq vrN (nth i vrs)) ; vrN: variable next vertix
        (if (/= i (1- n))
          (if (>= (distance vrP vrN) TminDist)
            (progn
              (setq Tpts (append Tpts
                        (list (mapcar '(lambda(a b)(/ (+ a b) 2.0)) vrP vrN))))
              (setq vrP vrN)
            )
          )
          (setq Tpts (append Tpts (list vrN)))
        )
        (setq i (1+ i))
      )
      Tpts
    )
  )

  ;; --------------------------------------------------
  ;; Draw Top Reinforcement bars using API
  ;; --------------------------------------------------
  (setq DrawTop
    (lambda (Tpts ang sgn offDist diffDist splice barEnd dimOff TRdia TRs sUnt TdimPretxt / 
                 TangOff Dist TtDist ptP i n pt midpt ptN rev flat text dimpt
                 ptP2 ptN2 TptPs TptNs)
      
      ;; compute offset direction
      (setq TangOff (+ ang (* sgn (/ pi 2))))

      ;; initial offset switching
      (setq Dist 0.0) ; this is a choice for started bar (to be above or below=0.0)
      (setq TtDist (+ offDist Dist)) ; TtDist: total distance of offset (variable)
      (setq ptP (polar (car Tpts) TangOff TtDist)) ; ptP: previus point (start point of bar)

      (setq TptPs nil TptNs nil)
      
      (setq i 1  n (length Tpts))

      (while (< i n)

        (if (/= i (1- n))
          (progn
            ;; midpoint splice point
            (setq midpt (polar (nth i Tpts) ang (/ splice 2.0))) 
            (setq ptN   (polar midpt TangOff TtDist)) ; ptN: next point (end point of bar)
            (setq TptPs (append TptPs (list ptP))) ; TptPs: previus (1st) end points for Top Reinf.
            (setq TptNs (append TptNs (list ptN))) ; TptPs: next (last) end points for Top Reinf.

            ;; prepare barEnd polyline points if needed
            (if (= barEnd 0.0)
              (AddLine ptP ptN)
              (progn
                (setq ptP2 (polar ptP (+ ang (* sgn (/ pi 4))) barend))
                (setq ptN2 (polar ptN (+ ang (* sgn (* 0.75 pi))) barend))
                (setq flat
                  (list
                    (car ptP)  (cadr ptP)
                    (car ptP2) (cadr ptP2)
                    (car ptN2) (cadr ptN2)
                    (car ptN)  (cadr ptN)
                  )
                )
                (AddPline flat)
              )
            )

            ;; dimension
            (setq text (strcat "Ø" (rtos TRdia 2 0)
                               "@" (rtos TRs  2 0)
                               sUnt " "
                               TdimPretxt "<>"))
            (setq dimpt (polar ptN TangOff dimOff))
            (AddDim ptP ptN dimpt text)

            ;; alternate bar distance
            (setq rev (if (= Dist 0.0) diffDist (- diffDist)))

            ;; prepare next starting point after splice back
            (setq pt  (polar ptN ang (- splice)))
            (setq ptP (polar pt TangOff rev))

            (setq Dist (if (= Dist 0.0) diffDist 0.0))
            (setq TtDist (+ offDist Dist))
          )

          ;; last bar segment
          (progn
            (setq ptN (polar (nth i Tpts) TangOff TtDist))
            (setq TptNs (append TptNs (list ptN))) ; TptPs: next (last) end points for Top Reinf.
            
            (if (= barEnd 0.0)
              (AddLine ptP ptN)
              (progn
                (setq ptP2 (polar ptP (+ ang (* sgn (/ pi 4))) barend))
                (setq ptN2 (polar ptN (+ ang (* sgn (* 0.75 pi))) barend))
                (setq flat
                  (list
                    (car ptP)  (cadr ptP)
                    (car ptP2) (cadr ptP2)
                    (car ptN2) (cadr ptN2)
                    (car ptN)  (cadr ptN)
                  )
                )
                (AddPline flat)
              )
            )

            (setq text (strcat "Ø" (rtos TRdia 2 0)
                               "@" (rtos TRs  2 0)
                               sUnt " "
                               TdimPretxt "<>"))
            (setq dimpt (polar ptN TangOff dimOff))
            (AddDim ptP ptN dimpt text)
          )
        )

        (setq i (1+ i))
      )

    )
  )

  ;; --------------------------------------------------
  ;; Draw Bottom Reinforcement bars
  ;; (same organization as DrawTop)
  ;; --------------------------------------------------
  (setq DrawBot
    (lambda (vrs ang sgn BminDist offDist diffDist BbarOff barEnd dimOff dimsgn BRdia BRs sUnt BdimPretxt /
                 BangOff Dist TtDist ptP i n pt1 pt2 ptN rev flat text dimpt
                 ptP2 ptN2 BptPs BptNs)
      
      ;; compute offset direction
      (setq BangOff (- ang (* sgn (/ pi 2))))

      ;; initial starting point
      (setq Dist 0.0) ; this is a choice for started bar (to be above=0.0 or below)
      (setq TtDist (+ offDist Dist))
      (setq ptP (polar (car vrs) BangOff TtDist))
      (setq BptPs (list ptP)
            BptNs nil )
      
      (setq i 1  n (length vrs))

      ;; skip cantilever
      (if (< (distance (car vrs) (cadr vrs)) BminDist)
        (setq i (1+ i))
      )

      (while (< i n)

        (if (< i (- n 2))

          ;; normal support logic
          (if (>= (distance (nth i vrs) (nth (1- i) vrs)) BminDist)
            (progn
              (setq pt2 (nth (1+ i) vrs)) ; pt2: variable calculation point (at end of support / slab strip)
              (setq ptN (polar (polar pt2 ang (- BbarOff)) BangOff TtDist)) ; ptN: next point (end point of bar)
              (setq BptNs (append BptNs (list ptN))) ; BptPs: next (last) end points for Bot Reinf.

              ;; draw
              (if (= barEnd 0.0)
                (AddLine ptP ptN)
                (progn
                  (setq ptP2 (polar ptP (- ang (* sgn (/ pi 4))) barend))
                  (setq ptN2 (polar ptN (- ang (* sgn (* 0.75 pi))) barend))
                  (setq flat
                    (list
                      (car ptP)  (cadr ptP)
                      (car ptP2) (cadr ptP2)
                      (car ptN2) (cadr ptN2)
                      (car ptN)  (cadr ptN)
                    )
                  )
                  (AddPline flat)
                )
              )

              ;; dimension
              (setq text (strcat "Ø" (rtos BRdia 2 0)
                                 "@" (rtos BRs  2 0)
                                 sUnt " "
                                 BdimPretxt "<>"))
              (setq dimpt (polar ptN BangOff (* dimsgn dimOff)))
              (AddDim ptP ptN dimpt text)

              ;; alternate switching
              (setq Dist (if (= Dist 0.0) diffDist 0.0))
              (setq TtDist (+ offDist Dist))

              ;; next start
              (setq pt1 (nth i vrs)) ; pt1: variable calculation point (at start of support / slab strip)
              (setq ptP (polar (polar pt1 ang (+ BbarOff)) BangOff TtDist)) ; ptN: next point (end point of bar)
              (setq BptPs (append BptPs (list ptP))) ; BptPs: previus (1st) end points for Bot Reinf.
            )

            ;; else small span
            (setq i (1- i))
          )

          ;; last segment
          (progn
            (setq ptN (polar (last vrs) BangOff TtDist)) ;whatever the end was ("cantileaver end" or "support end")
            (setq BptNs (append BptNs (list ptN)))

            (if (= barEnd 0.0)
              (AddLine ptP ptN)
              (progn
                (setq ptP2 (polar ptP (- ang (* sgn (/ pi 4))) barend))
                (setq ptN2 (polar ptN (- ang (* sgn (* 0.75 pi))) barend))
                (setq flat
                  (list
                    (car ptP)  (cadr ptP)
                    (car ptP2) (cadr ptP2)
                    (car ptN2) (cadr ptN2)
                    (car ptN)  (cadr ptN)
                  )
                )
                (AddPline flat)
              )
            )

            (setq text (strcat "Ø" (rtos BRdia 2 0)
                               "@" (rtos BRs  2 0)
                               sUnt " "
                               BdimPretxt "<>"))
            (setq dimpt (polar ptN BangOff (* dimsgn dimOff)))
            (AddDim ptP ptN dimpt text)
          )
        )

        (setq i (+ i 2))
      )
    )
  )

  ;; ============================================
  ;; ProcessPolylines – functionalized main loop
  ;; ============================================ 
  (setq ProcessPolylines
    (lambda (sset *LR-Opts* / oldsnap oldblipmode
              TminDist BminDist offDist diffDist splice barEnd BbarOff
              TRdia TRs BRdia BRs sUnt TdimPretxt BdimPretxt dimOff dimsgn
              j obj ent vrs ang sgn Tpts
            )
      
      ;; =============================================================
      ;; 1) UNPACK OPTIONS (passed from GetUserOptions)
      ;; =============================================================
      (setq TminDist     (nth 0  *LR-Opts*)
            BminDist     (nth 1  *LR-Opts*)
            offDist      (nth 2  *LR-Opts*)
            diffDist     (nth 3  *LR-Opts*)
            splice       (nth 4  *LR-Opts*)
            barEnd       (nth 5  *LR-Opts*)
            BbarOff      (nth 6  *LR-Opts*)
            TRdia        (nth 7  *LR-Opts*)
            TRs          (nth 8  *LR-Opts*)
            BRdia        (nth 9  *LR-Opts*)
            BRs          (nth 10 *LR-Opts*)
            unit         (nth 11 *LR-Opts*)
            sUnt         (nth 12 *LR-Opts*)
            TdimPretxt   (nth 13 *LR-Opts*)
            BdimPretxt   (nth 14 *LR-Opts*)
            dimOff       (nth 15 *LR-Opts*)
            dimsgn       (nth 16 *LR-Opts*)
            manual-unit? (nth 17 *LR-Opts*))
      
      ;; Select polylines
      (if sset
        (progn
          ;; apply unit factor adjustments (for first use and no manual set for unit)
          (if (null manual-unit?)
            (UnitFactorAdjust))
          
          ; switch off system variables
          (setq oldsnap (getvar "osmode"))
          (setq oldblipmode (getvar "blipmode"))
          (setvar "osmode" 0)
          (setvar "blipmode" 0)

          ;; loop polylines
          (setq j 0)
          (while (< j (sslength sset))

            ;; get obj
            (setq obj (ssname sset j))
            (setq ent (entget obj))

            ;; --- build vertices ---
            (setq vrs (GetVertices ent 1.0))

            ;; --- direction angle ---
            (setq ang (angle (car vrs) (last vrs)))

            ;; --- offset side sign ---
            (setq sgn (if (and (> ang (/ pi 2)) (< ang (* 1.5 pi))) -1.0 1.0))

            ;; --- compute top points ---
            (setq Tpts (GetTopPoints vrs TminDist))

            ;; --- draw top bars ---
            (DrawTop
              Tpts ang sgn offDist diffDist splice barEnd
              dimOff TRdia TRs sUnt TdimPretxt
            )

            ;; --- draw bottom bars ---
            (DrawBot
              vrs ang sgn BminDist offDist diffDist BbarOff barEnd
              dimOff dimsgn BRdia BRs sUnt BdimPretxt
            )

            (setq j (1+ j))
          )

          ;; restore system vars
          (setvar "osmode" oldsnap)
          (setvar "blipmode" oldblipmode)
        )
      )
    )
  )

  ;; =============================================================
  ;; 3) Main program execution
  ;; =============================================================
  (vl-load-com)

  ;; get Active Drawing
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  ;; --------------------------------------------
  ;; START UNDO MARK (begin one grouped undo block)
  ;; --------------------------------------------
  (vla-StartUndoMark doc)
  
  ;; ---- User Options ----
  (setq *LR-Opts* (GetUserOptions)) ; saved for auto restore values

  ;; ---- Select polylines ----
  (setq sset (ssget '((0 . "LWPOLYLINE"))))

  ;; ---- Execute Processing ----
  (if sset
    (ProcessPolylines sset *LR-Opts*)
  )

  ;; --------------------------------------------
  ;; END UNDO MARK (end grouped undo)
  ;; --------------------------------------------
  (vla-EndUndoMark doc)
  
  (princ)
)
