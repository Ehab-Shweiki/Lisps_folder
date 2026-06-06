; TODO: Develop and check

;; Helper Functions
(defun safe:undo (fn / doc result error-obj)
  "Execute function within an undo group with error handling"
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (vla-StartUndoMark doc)
  (setq result (vl-catch-all-apply fn '()))
  (if (vl-catch-all-error-p result)
    (progn
      (setq error-obj result)
      (prompt (strcat "\n❌ Error in safe:undo: " (vl-catch-all-error-message error-obj)))
      (vla-EndUndoMark doc)
      nil
    )
    (progn
      (vla-EndUndoMark doc)
      result
    )
  )
)

(defun objs->vlaObjs (objs / obj vla-objs)
  (setq vla-objs '())
  
  (if (not objs)
    (progn
      (princ "\nNo objects selected")
      nil)
    (progn
      (foreach obj objs
        (cond
          ;; If already a VLA object
          ((and obj (eq (type obj) 'VLA-OBJECT) (not (vlax-object-released-p obj)))
          (cons obj vla-objs)
          )

          ;; If it's an entity name
          ((and obj (eq (type obj) 'ENAME) (not (entdel obj)))
          (cons (vlax-ename->vla-object obj) vla-objs)
          )

          ;; If it's a DXF list, try to find the entity name
          ((and (listp obj) (assoc -1 obj))
          (cons (vlax-ename->vla-object (cdr (assoc -1 obj))) vla-objs)
          )

          ;; Invalid or unsupported type
          (T
            (prompt (strcat "\n⚠️ Cannot convert to VLA: " (vl-princ-to-string obj)))
            nil
          )
        )
      )
      (reverse vla-objs)
    )
  )
)
    
(defun validate-property (obj prop-name prop-value / result)
  "Validate if a property can be set on an object"
  (and obj
       prop-value
       (not (vlax-object-released-p obj))
       (vlax-property-available-p obj prop-name)
       (progn
         (setq result (vl-catch-all-apply 'vlax-property-available-p (list obj prop-name)))
         (not (vl-catch-all-error-p result))
       )
  )
)

(defun set-property-safe (obj prop-name prop-value / result)
  "Safely set a property on a VLA object with error handling"
  (if (validate-property obj prop-name prop-value)
    (progn
      (setq result (vl-catch-all-apply 'vlax-put-property (list obj prop-name prop-value)))
      (if (vl-catch-all-error-p result)
        (progn
          (prompt (strcat "\n❌ Failed to set " prop-name ": " (vl-catch-all-error-message result)))
          nil
        )
        T
      )
    )
    nil
  )
)

;; ===============================
;; Reset or Set Properties via VLA
;; ===============================
(defun modify-objs (objs layer color lineTyp lineTypScl lineWgt PltStyNam / 
                   *doc* vla-objs obj-count success-count prop-map prop)
  "Enhanced modify-objs: modify VLA objects' properties with comprehensive error handling
   
   Parameters:
   - objs: List of objects (entity names, VLA objects, or DXF lists)
   - layer: Layer name (string) or nil to skip
   - color: Color value (integer) or nil to skip (256 = BYLAYER, 0 = BYBLOCK)
   - lineTyp: Linetype name (string) or nil to skip
   - lineTypScl: Linetype scale (real) or nil to skip
   - lineWgt: Lineweight value (integer) or nil to skip (-1 = BYLAYER, -2 = BYBLOCK)
   - PltStyNam: Plot style name (string) or nil to skip
   
   Returns: Number of successfully modified objects"

  (vl-load-com)
  
  ;; Initialize counters
  (setq success-count 0
        obj-count 0)
  
  ;; Initialize counters
  (setq success-count 0
        obj-count 0)

  ;; Convert objects to VLA objects
  (setq vla-objs (objs->vlaObjs objs))
  (setq obj-count (length vla-objs))
  
  (if (= obj-count 0)
    (progn
      (prompt "\n⚠️ No valid objects to modify")
      0
    )
    (progn
      (prompt (strcat "\n🔧 Modifying " (itoa obj-count) " objects..."))
      
      ;; Start undo group
      (setq *doc* (vla-get-ActiveDocument (vlax-get-Acad-Object)))
      (vla-StartUndoMark *doc*)
      
      ;; Create property map for easier processing
      (setq prop-map (list
        (list "Layer" layer)
        (list "Color" color)
        (list "Linetype" lineTyp)
        (list "LinetypeScale" lineTypScl)
        (list "Lineweight" lineWgt)
        (list "PlotStyleName" PltStyNam)
      ))
      
      ;; Process each object
      (foreach obj vla-objs
        (if (and obj (not (vlax-object-released-p obj)))
          (progn
            ;; Apply each property if specified
            (foreach prop prop-map
              (if (cadr prop) ; If property value is not nil
                (set-property-safe obj (car prop) (cadr prop))
              )
            )
            (setq success-count (1+ success-count))
          )
          (prompt "\n⚠️ Skipping invalid or released object")
        )
      )
      
      ;; End undo group
      (vla-EndUndoMark *doc*)
      
      ;; Report results
      (prompt (strcat "\n✅ Successfully modified " 
                     (itoa success-count) 
                     " of " 
                     (itoa obj-count) 
                     " objects"))
      
      success-count
    )
  )
)
      
