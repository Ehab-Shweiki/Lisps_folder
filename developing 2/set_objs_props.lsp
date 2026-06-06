(vl-load-com)

(defun safe-prompt (func / result)
  (setq result (vl-catch-all-apply func))
  (if (vl-catch-all-error-p result)
    (progn (princ "\n⛔ Canceled.") nil)
    result))

(defun set-entity-property-dxf (entdata prop-code newval)
  "Safely update a DXF code (color=62, layer=8, linetype=6)"
  (if (assoc prop-code entdata)
    (subst (cons prop-code newval) (assoc prop-code entdata) entdata)
    (append entdata (list (cons prop-code newval)))
  )
)

(defun set-entity-properties (obj color layer ltype / ent entdata)
  (if (and obj (vlax-property-available-p obj 'ObjectName))
    (progn
      (setq ent (vlax-vla-object->ename obj))
      (setq entdata (entget ent))

      ; classify simple vs complex entity
      (if (member (cdr (assoc 0 entdata)) '("LINE" "CIRCLE" "ARC" "LWPOLYLINE" "TEXT"))
        ;; --- Simple entity: modify via DXF ---
        (progn
          (if color (setq entdata (set-entity-property-dxf entdata 62 color)))
          (if layer (setq entdata (set-entity-property-dxf entdata 8 layer)))
          (if ltype (setq entdata (set-entity-property-dxf entdata 6 ltype)))
          (entmod entdata)
          (entupd ent)
        )
        ;; --- Complex entity: use VLA ---
        (progn
          (if color (vla-put-Color obj color))
          (if layer (vla-put-Layer obj layer))
          (if ltype (vla-put-Linetype obj ltype))
        )
      )
    )
  )
)

(defun c:SetObjsProps ( / ss color layer ltype acDoc idx obj)
  (princ "\nSelect objects to modify:")
  (setq ss (safe-prompt '(lambda () (ssget))))
  (if ss
    (progn
      ;; Ask for new properties (optional)
      (setq color (safe-prompt '(lambda () (getint "\nEnter color index (0=ByBlock, 256=ByLayer, or number): "))))
      (setq layer (safe-prompt '(lambda () (getstring T "\nEnter new layer name (or Enter to skip): "))))
      (setq ltype (safe-prompt '(lambda () (getstring T "\nEnter new linetype name (or Enter to skip): "))))
      
      ;;; --------
      (setq t0 (ms-now))
      ;;; --------
      
      ;; Convert "" → nil
      (if (or (null layer) (= layer "")) (setq layer nil))
      (if (or (null ltype) (= ltype "")) (setq ltype nil))
      
      (setq acDoc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
      (vla-StartUndoMark acDoc)
      (repeat (setq idx (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss (setq idx (1- idx)))))
        (set-entity-properties obj color layer ltype)
      )
      (vla-EndUndoMark acDoc)
      (princ "\n✅ Properties updated.")
      
      ;;; --------
      (princ (strcat "\n⏱️ : " (itoa (- (ms-now) t0)) " ms" ))
      ;;; --------
    )
  )
  (princ)
)
