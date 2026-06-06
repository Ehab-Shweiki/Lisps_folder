;;; -------- Speedy, hashed, timed POINT markers for block insertions --------
(vl-load-com)

;;;===============================================================
;;; 🔧 Utility Functions
;;;===============================================================
(defun ms-now () (getvar "MILLISECS"))
(defun timed (fn args label / t0 r)
  (setq t0 (ms-now))
  (setq r (apply fn args))
  (princ (strcat "\n⏱️  " label ": " (itoa (- (ms-now) t0)) " ms" ))
  r)

;; Key builder: snap coords to tolerance grid, then stringify (hash key)
(defun ptkey (pt tol / scale)
  (if pt
    (progn
      (setq scale (/ 1.0 tol)) ; e.g., tol=0.001 -> scale=1000
      (strcat
        (itoa (fix (* (car   pt) scale))) ","
        (itoa (fix (* (cadr  pt) scale))) ","
        (itoa (fix (* (caddr pt) scale)))))
  "")
)

(defun dict:close (dict)
  (if dict
    (progn
      (vl-catch-all-apply 'vlax-invoke-method (list dict 'RemoveAll))
      (vl-catch-all-apply 'vlax-release-object (list dict))
      (setq dict nil)
      (princ "\n🧹 Dictionary released from memory.")
    )
  )
)

;;; ===============================================================
;;; ⚙️ Core Step Functions
;;; ===============================================================

(defun step:ensure-layer (lay color / doc layers new)
  ;; Create layer if it doesn't exist
  (vl-load-com)
  (if (not (tblsearch "LAYER" lay))
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
      (setq layers (vla-get-Layers doc))
      (setq new (vla-Add layers lay))
      (vla-put-Color new color)
      (princ (strcat "\nCreated layer: " lay))
    )
  )
)

(defun step:collect-points-dict (tol / key dict ss i pt)
  ;; Collect all POINT entities into a dictionary
  (vl-load-com)
  (setq dict (vlax-get-or-create-object "Scripting.Dictionary"))
  (vlax-invoke-method dict 'RemoveAll)
  (if (setq ss (ssget "_X" '((0 . "POINT"))))
    (repeat (setq i (sslength ss))
      (setq pt (cdr (assoc 10 (entget (ssname ss (setq i (1- i)))))))
      (setq pts (cons pt pts))
      (setq key (ptkey pt tol))
      (if (and key (/= key ""))
        (if (= :vlax-false (vlax-invoke-method dict 'Exists key))
          (vlax-invoke-method dict 'Add key 1))
      )
    )
  )
  dict
)

(defun step:collect-blocks ( / )
  (ssget "_X" '((0 . "INSERT"))))

(defun step:create-new-points-dict (ss dict tol color / i en ed pt key count)
  ;; Add new POINTs for block insertions, skip if already exists
  (setq count 0) ; initialize counter
  
  (if ss
    (repeat (setq i (sslength ss))
      (setq en (ssname ss (setq i (1- i))))
      (setq ed (entget en))
      (setq pt (cdr (assoc 10 ed)))
      (setq key (ptkey pt tol))
      
      ;; ensure key and dictionary are valid
      (if (and key (/= key ""))
        (if (= :vlax-false (vlax-invoke-method dict 'Exists key))
          (progn
            ;; create point
            (entmake 
              (list 
                (cons 0 "POINT")
                (cons 8 "BLOCK_INSERT_PTS")  ; layer defines color
                (cons 10 pt)
                ; (cons 62 color) ; red
              )
            )
            ;; add to dictionary
            (vlax-invoke-method dict 'Add key 1)
            ;; mark as created
            (setq count (1+ count))
          )
        )
      )
    )
  )
  
  ;; print summary at end
  (princ (strcat "\n✅  " (itoa count) " points created."))
  (princ)
)

;;; ===============================================================
;;; 🚀 Main Command
;;; ===============================================================

(defun c:ShowBlkPts ( / lay tol oldlay dict ss t0)
  
  ;; --- Settings ---
  (setq lay "BLOCK_INSERT_PTS" tol 0.001) ; 1 mm tolerance
  (setvar "PDMODE" 34) ; circle+X
  (setvar "PDSIZE" 10)

  ;; --- Save current layer ---
  (setq oldlay (getvar "CLAYER"))
  (setq t0 (ms-now))

  ;; --- Ensure working layer ---
  (step:ensure-layer lay 1) ; red layer for visibility
  (if (tblsearch "LAYER" lay) (setvar "CLAYER" lay))

  ;; --- Step 1️⃣: Collect existing points into dictionary ---
  (setq dict  (timed 'step:collect-points-dict (list tol) "Collect existing points"))
  
  ;; --- Step 2️⃣: Collect block insertions ---
  (setq ss (timed 'step:collect-blocks nil "Collect inserts"))
  
  ;; --- Step 3️⃣: Create missing points (using hash lookup) ---
  (timed 'step:create-new-points-dict (list ss dict tol 1) "Create new points")

  ;; --- Finalize ---
  (princ "\n")
  (command "._REGEN")

  ;; --- Restore previous layer ---
  (if (tblsearch "LAYER" oldlay) (setvar "CLAYER" oldlay))
  (dict:close dict) ; release dictionary from memory
  
  ;; --- Summary ---
  (princ (strcat
    "\n------------------------------------"
    "\nTotal time: " (itoa (- (ms-now) t0)) " ms"
    "\nPrevious layer restored.\n"))
  (princ))

(princ "\nType ShowBlkPts to run the Show_Block_Pts command.")
(princ)