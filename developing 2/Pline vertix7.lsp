;;; ---------------------------------------------------------------
;;; LReinf – Reinforcement Generator (API Only)
;;; Full Version: Top + Bottom Reinforcement
;;; Comments inside code: English only
;;; ---------------------------------------------------------------

(vl-load-com)

;; ---------------------------------------------------------------
;; Helper: Return Active Model Space
;; ---------------------------------------------------------------
(defun LRA:MSpace ( / *doc*)
  (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-object)))
  (vla-get-ModelSpace *doc*)
)

;; ---------------------------------------------------------------
;; Helper: Add 2D Line
;; ---------------------------------------------------------------
(defun LRA:AddLine (pt1 pt2 / *ms*)
  (setq *ms* (LRA:MSpace))
  (vla-AddLine *ms* (vlax-3d-point pt1) (vlax-3d-point pt2))
)

;; ---------------------------------------------------------------
;; Helper: Add 2D LWPolyline (flattened list)
;; ---------------------------------------------------------------
(defun LRA:AddLWPolyline (pts / *ms* arr)
  (setq *ms*  (LRA:MSpace))
  (setq arr (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length pts)))))
  (vlax-safearray-fill arr pts)
  (vla-AddLightweightPolyline *ms* arr)
)

;; ---------------------------------------------------------------
;; Helper: Add Aligned Dimension
;; ---------------------------------------------------------------
(defun LRA:AddDimAligned (pt1 pt2 dimpt / *ms*)
  (setq *ms* (LRA:MSpace))
  (vla-AddDimAligned
    *ms*
    (vlax-3d-point pt1)
    (vlax-3d-point pt2)
    (vlax-3d-point dimpt)
  )
)

;; ---------------------------------------------------------------
;; Main Command
;; ---------------------------------------------------------------
(defun c:LReinf ( / sset oldsnap oldblipmode)

  ;; Select target polylines
  (setq sset (ssget '((0 . "LWPOLYLINE"))))

  (if sset
    (progn

      ;; Parameters (Top + Bottom)
      (setq TminDist  1.0    ;; Min span to insert mid-point (Top)
            BminDist  1.0    ;; Min support spacing for Bottom bars
            offDist   0.5    ;; Base offset for bars
            diffDist  0.08   ;; Alternation offset between bars
            splice    0.40   ;; Splice length (Top)
            barend    0.02   ;; Hook end (45°) length
            BbarOff   0.05   ;; Bottom bar offset from support end
            dimOff    0.25   ;; Dimension offset
            dimsgn   -1.0)    ;; Dimension direction sign bottom

      ;; Disable snaps
      (setq oldsnap (getvar "osmode"))
      (setq oldblipmode (getvar "blipmode"))
      (setvar "osmode" 0)
      (setvar "blipmode" 0)

      (setq iPoly 0
            nPoly (sslength sset))

      ;; Process each polyline
      (while (< iPoly nPoly)

        (setq obj (ssname sset iPoly)
              ent (entget obj))

        ;; Extract polyline points (DXF 10)
        (setq raw (vl-remove-if '(lambda (x) (/= (car x) 10)) ent))
        (setq vrs nil last nil)

        ;; Remove duplicates
        (foreach rec raw
          (if (not (equal (cdr rec) last 1e-6))
            (progn
              (setq last (cdr rec))
              (setq vrs (append vrs (list last)))
            )
          )
        )

        ;; Direction angle
        (setq ang (angle (car vrs) (last vrs)))

        ;; Determine offset side sign
        (setq sgn
          (if (and (> ang (/ pi 2)) (< ang (* 1.5 pi)))
            -1.0
            1.0))

        ;; -----------------------------------------------------------
        ;; TOP REINFORCEMENT  (already implemented)
        ;; -----------------------------------------------------------

        ;; Compute top reinforcement key points (start, mids, end)
        (setq Tpts (list (car vrs)))
        (setq pv (cadr vrs))
        (setq j 2
              nv (length vrs))

        (while (< j nv)
          (setq nx (nth j vrs))

          (if (/= j (1- nv))
            (if (>= (distance pv nx) TminDist)
              (setq Tpts
                (append Tpts
                  (list
                    (mapcar '(lambda (a b) (/ (+ a b) 2.0)) pv nx)
                  )
                )
              )
            )
          )

          (setq pv nx
                j (1+ j))
        )

        (setq Tpts (append Tpts (list (last vrs))))
        (setq TangOff (+ ang (* sgn (/ pi 2))))
        (setq Dist 0.0
              TtDist (+ offDist Dist))
        (setq ptP (polar (car Tpts) TangOff TtDist))

        ;; Plot Top reinforcement bars
        (setq j 1
              nv (length Tpts))

        (while (< j nv)
          (if (/= j (1- nv))
            (progn
              (setq mid (polar (nth j Tpts) ang (/ splice 2.0)))
              (setq ptN (polar mid TangOff TtDist))

              ;; draw
              (if (= barend 0.0)
                (LRA:AddLine ptP ptN)
                (LRA:AddLWPolyline
                  (list
                    (car ptP) (cadr ptP)
                    (car (polar ptP (+ ang (* sgn (/ pi 4))) barend))
                    (cadr (polar ptP (+ ang (* sgn (/ pi 4))) barend))
                    (car (polar ptN (+ ang (* sgn (* 0.75 pi))) barend))
                    (cadr (polar ptN (+ ang (* sgn (* 0.75 pi))) barend))
                    (car ptN) (cadr ptN)
                  )
                )
              )

              ;; dimension
              (LRA:AddDimAligned ptP ptN (polar ptN TangOff dimOff))

              ;; alternate offset
              (setq rd (if (= Dist 0.0) diffDist (- diffDist)))
              (setq Dist  (if (= Dist 0.0) diffDist 0.0))

              (setq base (polar ptN ang (- splice)))
              (setq ptP  (polar base TangOff rd))
              (setq TtDist (+ offDist Dist))
            )

            ;; last bar
            (progn
              (setq ptN (polar (nth j Tpts) TangOff TtDist))

              (if (= barend 0.0)
                (LRA:AddLine ptP ptN)
                (LRA:AddLWPolyline
                  (list
                    (car ptP) (cadr ptP)
                    (car (polar ptP (+ ang (* sgn (/ pi 4))) barend))
                    (cadr (polar ptP (+ ang (* sgn (/ pi 4))) barend))
                    (car (polar ptN (+ ang (* sgn (* 0.75 pi))) barend))
                    (cadr (polar ptN (+ ang (* sgn (* 0.75 pi))) barend))
                    (car ptN) (cadr ptN)
                  )
                )
              )

              (LRA:AddDimAligned ptP ptN (polar ptN TangOff dimOff))
            )
          )
          (setq j (1+ j))
        )

        ;; -----------------------------------------------------------
        ;; BOTTOM REINFORCEMENT
        ;; -----------------------------------------------------------

        (setq BangOff (- ang (* sgn (/ pi 2))))
        (setq Dist 0.0)
        (setq BtDist (+ offDist Dist))

        ;; bottom start point
        (setq ptP (polar (car vrs) BangOff BtDist))

        (setq BptPs (list ptP))
        (setq BptNs nil)

        ;; detect first support vs cantilever
        (setq j 2
              nv (length vrs))

        (if (< (distance (car vrs) (cadr vrs)) TminDist)
          (setq support 0)
          (setq support 1))

        ;; iterate supports
        (while (< j nv)

          (if (< j (- nv 2))

            ;; interior support logic
            (if (>= (distance (nth j vrs) (nth (1- j) vrs)) BminDist)
              (progn
                ;; support end
                (setq p2  (nth (+ j 1) vrs))
                (setq ptN (polar (polar p2 ang (- BbarOff)) BangOff BtDist))

                (setq BptNs (append BptNs (list ptN)))

                ;; draw bottom bar
                (if (= barend 0.0)
                  (LRA:AddLine ptP ptN)
                  (LRA:AddLWPolyline
                    (list
                      (car ptP) (cadr ptP)
                      (car (polar ptP (- ang (* sgn (/ pi 4))) barend))
                      (cadr (polar ptP (- ang (* sgn (/ pi 4))) barend))
                      (car (polar ptN (- ang (* sgn (* 0.75 pi))) barend))
                      (cadr (polar ptN (- ang (* sgn (* 0.75 pi))) barend))
                      (car ptN) (cadr ptN)
                    )
                  )
                )

                ;; dimension bottom
                (LRA:AddDimAligned ptP ptN (polar ptN BangOff (* dimsgn dimOff)))

                ;; alternate
                (setq Dist (if (= Dist 0.0) diffDist 0.0))
                (setq BtDist (+ offDist Dist))

                ;; prepare next start point
                (setq p1  (nth j vrs))
                (setq ptP (polar (polar p1 ang (+ BbarOff)) BangOff BtDist))
                (setq BptPs (append BptPs (list ptP)))
              )
            )

            ;; last support or cantilever end
            (progn
              (setq ptN (polar (last vrs) BangOff BtDist))
              (setq BptNs (append BptNs (list ptN)))

              (if (= barend 0.0)
                (LRA:AddLine ptP ptN)
                (LRA:AddLWPolyline
                  (list
                    (car ptP) (cadr ptP)
                    (car (polar ptP (- ang (* sgn (/ pi 4))) barend))
                    (cadr (polar ptP (- ang (* sgn (/ pi 4))) barend))
                    (car (polar ptN (- ang (* sgn (* 0.75 pi))) barend))
                    (cadr (polar ptN (- ang (* sgn (* 0.75 pi))) barend))
                    (car ptN) (cadr ptN)
                  )
                )
              )
              (LRA:AddDimAligned ptP ptN (polar ptN BangOff (* dimsgn dimOff)))
            )
          )

          (setq j (+ j 2))
        )

        (setq iPoly (1+ iPoly))
      )
    )
  )

  ;; restore
  (setvar "osmode" oldsnap)
  (setvar "blipmode" oldblipmode)
  (princ)
)

