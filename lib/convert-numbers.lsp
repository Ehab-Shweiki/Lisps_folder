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