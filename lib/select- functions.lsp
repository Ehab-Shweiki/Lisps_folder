"selection built-in functions: ssget, entsel, nentsel"

;; simple example of ssget filtering
;; --------------------------
(setq ss (ssget '((0 . "LWPOLYLINE") (-4 . "<OR") (90 . 4) (90 . 5) (-4 . "OR>"))))

;; filter one element selection:
(defun entsel-filteredDynamic (prompt-str allowedTypes / sel ename edata entType)
  "select one element mathes belongs to filter list"
  (while
    (progn
      (setq sel (entsel prompt-str))
      (or
        (null sel) ; user pressed Enter / cancelled
        (progn
          (setq ename (car sel)
                edata (entget ename)
                entType (cdr (assoc 0 edata)))
          (not (member entType allowedTypes)) ; type not in allowed list
        )
      )
    )
    (princ (strcat "\nObject must be one of: "
            (apply 'strcat
              (cons (car allowedTypes)
                    (mapcar '(lambda (x) (strcat ", " x))
                            (cdr allowedTypes))))
            " — or press ESC to cancel."))
  )
  sel  ; returns the selection result list or nil if cancelled
)

(defun nentsel-filteredDynamic (prompt-str allowedTypes / sel ename edata entType)
  "select one element mathes belongs to filter list"
  (while
    (progn
      (setq sel (nentsel prompt-str))
      (or
        (null sel) ; user pressed Enter / cancelled
        (progn
          (setq ename (car sel)
                edata (entget ename)
                entType (cdr (assoc 0 edata)))
          (not (member entType allowedTypes)) ; type not in allowed list
        )
      )
    )
    (princ (strcat "\nObject must be one of: "
            (apply 'strcat
              (cons (car allowedTypes)
                    (mapcar '(lambda (x) (strcat ", " x))
                            (cdr allowedTypes))))
            " — or press ESC to cancel."))
  )
  sel  ; returns the selection result list or nil if cancelled
)

(defun get-parent-nth (pickData n / parentsList maxLevel)
  "Return the Nth parent from the pickData result of nentsel.
   Given the pick data list from nentsel or nentselp.
   pickData = returned value from nentsel (list: (ename pickPt maybeMatrix parentsList))
   n = 1 returns the *immediate parent*, n = 2 the grandparent, etc.
   If not enough levels, returns nil."
  (setq len        (length pickData)
        parentsList (if (= len 4) (nth 3 pickData) nil)  ; nested case: parent list is at position 4
  )
  (if (null parentsList)
    ;; no parentsList (i.e., not nested)
    (progn
      (princ "\nNo parent entities available (object is not nested).")
      nil
    )
    (progn
      (setq maxLevel (length parentsList))
      (if (or (< n 1) (> n maxLevel))
        (progn
          (princ (strcat "\nRequested parent level " (itoa n)
                         " does not exist. Max available is " (itoa maxLevel) "."))
          nil
        )
        (nth (1- n) parentsList)  ; zero-based index: immediate parent for n=1
      )
    )
  )
)

;; pre selection ssget
;; -------------------
(defun GetPreOrPromptSS ( / ss)
  ;; Try to get pre-selected objects (PICKFIRST)
  (setq ss (ssget "_I"))

  ;; If no pre-selection, prompt user to select
  (if (not ss)
    (prompt "\nSelect objects:")
    (setq ss (ssget)) ; standard prompt selection
  )

  ss ; return the selection set (or nil)
)

(defun GetFilteredPreOrPromptSS (filterList / ss)
  ;; filterList: DXF filter list, e.g. '((0 . "LINE")) or '((0 . "TEXT,MTEXT"))
  ;; Returns: filtered selection set (ss) from PICKFIRST or prompt

  ;; First try to get matching objects from PICKFIRST
  (setq ss (ssget "_I" filterList))

  ;; If no match, prompt user with same filter
  (if (not ss)
    (progn
      (prompt "\nNo matching pre-selected objects. Please select:")
      (setq ss (ssget filterList))
    )
  )

  ss ; return the final filtered selection set (may be nil)
)


