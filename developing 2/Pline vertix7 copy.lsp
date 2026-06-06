(defun c:LReinf ()
  (setq scale (setq scale (getreal "\nSet Scale <default 100>")) 100)
  (setq sset (ssget '((0 . "LWPOLYLINE")) ))
  (if sset
    (progn
      ; determine Definitions
      (setq TminDist (* scale 1.0)) ; TminDist: (for Top Reinf. & for support length) min distance between vertix to introduce a point in between
      (setq BminDist (* scale 1.0)) ; BminDist: (for Bot Reinscalf. & for support length) min distance between supports to cut at end of 2nd support
      (setq offDist (* scale 0.5)) ; offDist: offset distance for Top/Bot Reinf. from the Polyline
      (setq diffDist (* scale 0.08)) ; diffDist: a distance between two adjacent bars (Top&Top / Bot&BOt)
      (setq splice (* scale 0.40)) ; splice: splice length (total) between Top Reinf.
      (setq barend (* scale 0.02)) ; barend: an 45 degree shape at end of bar "if zero then none"
      (setq BbarOff (* scale 0.05)) ; BbarOff: Bot bar end offset from end of support
      (setq TRdia 10 TRs 20) ; TRdia: dia. of Top reinf & TRs: spacing between T.R
      (setq BRdia 10 BRs 20) ; TRdia: dia. of Top reinf & TRs: spacing between T.R
      (setq dimOff (* scale 0.25)) ; dimOff: dimention offset from Reinforcement bar (+ve: Top dim above Top Reinf.) 
      (setq dimsgn (* scale -1.0)) ; dimsgn: dimention direction sign for Top&Bot Reinf. (1.0: both at same direction, -1.0: directions are opposite to each other)

      ; switch off system variables
      (setq oldsnap (getvar "osmode"))
      (setq oldblipmode (getvar "blipmode"))
      (setvar "osmode" 0)
      (setvar "blipmode" 0)
      
      (setq itmA 0 numA (sslength sset)) ; itmA & numA for Polylines
      (while (< itmA numA)
        (setq Avrs nil vrs nil ; Avrs: all vertix of Pline (including replicated) & vrs: vertices of Pline
              obj (ssname sset itmA)
              ent (entget obj)
              Avrs (vl-remove-if '(lambda (x) (/= (car x) 10)) ent) 
              Tlength (distance (car Avrs) (last Avrs)) ; Tlength: total length for the polyline
              unitF (if (>= Tlength 200.0) 200.0 1.0)) ; unitF: unit factor (1.0 if meter, 100.0 if cm) based on the total length of polyline

        ; Define List the vertices & angle of direction
        (setq vr nil) ; vr: variable vertix of Pline
        (foreach rec Avrs
          (if (not (equal (cdr rec) vr (* 0.01 unitF) )) ; 0.01 meter
            (progn
              (setq vr (cdr rec))
              (setq vrs (append vrs (list vr))) ; define a list of vertices
            )
          )
        )
        (setq ang (angle (car vrs) (last vrs)) )
        (if (and (> ang (/ pi 2)) (< ang (* 1.5 pi))) ; TangOff: angle of offset from Polyline to Top reinf.
          (setq sgn -1.0) ; sgn: sign (-ve or +ve) to reflict posisions of T.R and B.R
          (setq sgn 1.0))
          
          ; Define the List of Top Reinforcement Calculation points (ends and mid points at spans)
        (setq Tpts (list (setq Tpt (car vrs))) ; Tpt: variable Top reinforcement point (end or mid point at span) & Tpts: all that points
              vrP (cadr vrs) ; vrP: variable previus vertix 
              itmB 2 numB (length vrs))  ; itmB & numB for vertices
        (while (< itmB numB)
          (setq vrN (nth itmB vrs)) ; vrN: variable next vertix 
          (if (/= itmB (1- numB))  
            (progn
              (if (>= (distance vrP vrN) TminDist)
                (progn
                  (setq Tpt (mapcar '(lambda (x1 x2) (/ (+ x1 x2) 2.0)) vrP vrN)
                        Tpts (append Tpts (list Tpt))) ; define a list of points
                ))
              (setq vrP vrN)
            )
            (setq Tpts (append Tpts (list (setq Tpt vrN))))
          )
          (setq itmB (1+ itmB))
        )

        ; Draw Top Reinforcement
        (setq TangOff (+ ang (* sgn (/ pi 2)))
              Dist    0.0 ; this is a choice for started bar (to be above or below=0.0)
              TtDist  (+ offDist Dist) ; TtDist: total distance of offset (variable)
              ptP     (polar (car Tpts) TangOff TtDist) ; ptP: previus point (start point of bar)
              TptPs   nil   TptNs     nil
              itmC    1     numC      (length Tpts)) ; itmC & numC for Top reinforcement calculation points
        (while (< itmC numC)
          (if (/= itmC (1- numC))
            (progn
              (setq pt    (polar (nth itmC Tpts) ang (/ splice 2.0)) ; pt: variable point for calculations
                    ptN   (polar pt TangOff TtDist) ; ptN: next point (end point of bar)
                    TptPs (append TptPs (list ptP)) ; TptPs: previus (1st) end points for Top Reinf.
                    TptNs (append TptNs (list ptN))) ; TptPs: next (last) end points for Top Reinf.
              (if (= barend 0.0)
                (command "line" ptP ptN "")
                (progn
                  (setq ptP2 (polar ptP (+ ang (* sgn (/ pi 4))) barend)
                        ptN2 (polar ptN (+ ang (* sgn (* 0.75 pi))) barend) )
                  (command "Pline" ptP ptP2 ptN2 ptN "")
                ))
              (command "dimaligned" ptP ptN (polar ptN TangOff dimOff))
              (setq RdiffDist (if (= Dist 0.0) ; RdiffDist: reversed diffDistance to change between adjacent bars
                  diffDist
                  (- diffDist)
                  ))
              (setq pt (polar ptN ang (- splice)) )								   
              (setq ptP (polar pt TangOff RdiffDist ))
              (if (= Dist 0.0)
                (setq Dist diffDist
                      Dist 0.0))
              (setq TtDist (+ offDist Dist))
            )
            (progn
              (setq ptN (polar (nth itmC Tpts) TangOff TtDist)
                    TptNs (append TptNs (list ptN)))
              (if (= barend 0.0)
                (command "line" ptP ptN "")
                (progn
                  (setq ptP2 (polar ptP (+ ang (* sgn (/ pi 4))) barend) )
                  (setq ptN2 (polar ptN (+ ang (* sgn (* 0.75 pi))) barend) )
                  (command "Pline" ptP ptP2 ptN2 ptN "")
                )
              )
              (command "dimaligned" ptP ptN (polar ptN TangOff dimOff))
            )
          )
        (setq itmC (1+ itmC))
        )

        ; Draw Bot Reinforcement
        (setq BangOff (- ang (* sgn (/ pi 2)))
              Dist    0.0 ; this is a choice for started bar (to be above=0.0 or below)
              TtDist  (+ offDist Dist)
              ptP     (polar (car vrs) BangOff TtDist) ; ptP: previus point (start point of bar)
              BptPs   (list ptP)    BptNs     nil)
        
        (setq itmB 2 numB (length vrs))
        (if (< (distance (car vrs) (cadr vrs)) TminDist) ; if true => support at first (not cantileaver)
          (setq count 0)
          (setq count 1))
        (while (< itmB numB)
          (if (< itmB (- numB 2))
            (if (>= (distance (nth itmB vrs) (nth (1- itmB) vrs)) BminDist) ; in this case: small spans will be ignored, & not first adjacent support will be included withing next Bot bar
              (progn
                (setq pt2 (nth (1+ itmB) vrs) ; pt2: variable calculation point (at end of support / slab strip)
                      ptN (polar (polar pt2 ang (- BbarOff)) BangOff TtDist) ; ptN: next point (end point of bar)
                      BptNs (append BptNs (list ptN))) ; BptPs: next (last) end points for Bot Reinf.
                (if (= barend 0.0)
                  (command "line" ptP ptN "")
                  (progn
                    (setq ptP2 (polar ptP (- ang (* sgn (/ pi 4))) barend)
                          ptN2 (polar ptN (- ang (* sgn (* 0.75 pi))) barend) )
                    (command "Pline" ptP ptP2 ptN2 ptN "")
                    ))
                (command "dimaligned" ptP ptN (polar ptN TangOff (* dimsgn dimOff)))
                (if (= Dist 0.0) 
                  (setq Dist diffDist)
                  (setq Dist 0.0))
                (setq TtDist  (+ offDist Dist)
                      pt1     (nth itmB vrs) ; pt1: variable calculation point (at start of support / slab strip)
                      ptP     (polar (polar pt1 ang (+ BbarOff)) BangOff TtDist) ; ptN: next point (end point of bar)
                      BptPs   (append BptPs (list ptP))) ; BptPs: previus (1st) end points for Bot Reinf.
              )
            )
            (progn
              (setq ptN   (polar (last vrs) BangOff TtDist) ;whatever the end was ("cantileaver end" or "support end")
                    BptNs (append BptNs (list ptN)))
              (if (= barend 0.0)
                (command "line" ptP ptN "")
                (progn
                  (setq ptP2 (polar ptP (- ang (* sgn (/ pi 4))) barend)
                        ptN2 (polar ptN (- ang (* sgn (* 0.75 pi))) barend) )
                  (command "Pline" ptP ptP2 ptN2 ptN "")))
              (command "dimaligned" ptP ptN (polar ptN TangOff (* dimsgn dimOff)))
            )
          )
          (setq itmB (+ itmB 2))
        )	
        (setq itmA (1+ itmA))
      )
	  )
  )
  (princ vrs)(princ)

  ; return system variables to old values
  (setvar "osmode" oldsnap)
  (setvar "blipmode" oldblipmode)
  
)

; variables not used (maybe used later):
; TptPs , TptNs , BptPs , BptNs