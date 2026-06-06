(defun dump-vla-object (obj)
  "Print all properties and their values of a VLA-Object."
  (vlax-dump-object obj T)
)

(defun vla-obj-type (obj / typename val)
  "Return a string describing the type of a given AutoLISP object OBJ.
  such as:
  VLA-COLLECTION, VLA-DICTIONARY, VLA-GROUP, VLA-SELECTIONSET, VLA-OBJECT,
  VARIANTs, SAFEARRAYs, LISP atoms and lists."
  
  (cond
    ;; --- Nil ---
    ((null obj) "nil")

    ;; --- SAFEARRAY ---
    ((= (type obj) 'SAFEARRAY)
     "SAFEARRAY"
    )

    ;; --- VARIANT ---
    ((= (type obj) 'VARIANT)
     (setq val (vlax-variant-value obj))
     (strcat "VARIANT -> " (vlax-obj-type val))
    )

    ;; --- VLA-OBJECT ---
    ((= (type obj) 'VLA-OBJECT)
     (setq typename (vla-get-ObjectName obj))
     (cond
       ;; Collection: Count and Item
       ((and (vlax-property-available-p obj 'Count)
             (vlax-method-applicable-p obj 'Item))
        (strcat "VLA-COLLECTION -> " typename)
       )

       ;; Dictionary / Group / SelectionSet
       ((wcmatch typename "*Dictionary*")
        "VLA-DICTIONARY"
       )
       ((wcmatch typename "*Group*")
        "VLA-GROUP"
       )
       ((wcmatch typename "*SelectionSet*")
        "VLA-SELECTIONSET"
       )

       ;; Any normal VLA
       (T
        (strcat "VLA-OBJECT -> " typename)
       )
     )
    )

    ;; --- أنواع LISP العادية (int, real, str, sym, …) ---
    ((atom obj)
     ;; هنا (type obj) = رمز مثل INT / REAL / STR …
     (strcat "LISP "
             (strcase (vl-symbol-name (type obj)))
     )
    )

    ;; --- قوائم LISP ---
    ((listp obj)
     (strcat "LIST (" (itoa (length obj)) " items)")
    )

    ;; --- أي شيء غريب كـ fallback ---
    (T
     (strcat "UNKNOWN: " (vl-princ-to-string obj))
    )
  )
)



(defun vlax-collection->list (col / i n result)
  "Convert a VLA-Collection to a standard Lisp list.
  This works for any COM collection: layers, layouts, blocks, Linetypes ,TextStyles ,DimStyles ,entities, etc."
  (setq n (vlax-get-property col 'Count))
  (setq i 0 result '())
  (while (< i n)
    (setq result (cons (vla-item col i) result))
    (setq i (1+ i))
  )
  (reverse result)
)

(defun nth-vlax (col n)
  "Get the N-th item (0-based) from a VLA-Collection."
  (vla-item col n)
)

(defun vlax-any->list (obj / n result)
  (cond
    ((= (type obj) 'SAFEARRAY)
     (vlax-safearray->list obj))

    ((and (vlax-property-available-p obj 'Count)
          (vlax-method-applicable-p obj 'Item))
     (vlax-collection->list obj))

    (t
     (prompt "\n[vlax-any->list] Unsupported object type.")
     nil)
  )
)

; ---------------- test functions -------------------
(defun tst1 ( / doc layers lst)
  (vl-load-com)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vla-get-Layers doc))
  (setq lst (vlax-collection->list layers))
)
(vlax-dump-object layers T)

(defun tst2 ( / acObjectId doc props)
  (vl-load-com)
  (setq obj (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-prop-list obj)
  (vlax-for p obj
            ; (princ p)
    ; (setq props (cons (vlax-get p 'Name) props))
  )
)