(defun ptkey (pt tol / scale)
  ;; Convert 3D point to tolerance-based integer key
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


(defun hash_points (pts tol / h pt)
  ;;-------------------------------------------------------------
  ;;  Create a hash of all unique points from possibly nested lists
  ;;  Arguments:
  ;;    pts - list of points or nested lists of points
  ;;    tol - tolerance for uniqueness
  ;;-------------------------------------------------------------
  (defun flatten-points (lst)
    (apply 'append
      (mapcar
        '(lambda (x)
           (cond
             ((and (listp x)
                   (= (length x) 3)
                   (numberp (car x))) (list x)) ; single point
             ((listp x) (flatten-points x))   ; nested list
             (T nil)
           )
         )
        lst
      )
    )
  )

  (if pts
    (progn
      (vl-load-com)
      (setq dict (vlax-get-or-create-object "Scripting.Dictionary"))
      (vlax-invoke-method dict 'RemoveAll)
      (foreach pt (flatten-points pts)
        (setq key (ptkey pt tol))
        (if (and key (/= key ""))
          (if (= :vlax-false (vlax-invoke-method dict 'Exists key))
            (vlax-invoke-method dict 'Add key 1))
        )
      )
      dict
    )
  )
)

(defun collect-points-dict (tol / key dict ss i pt)
  ;; Collect all POINT entities into a dictionary
  (vl-load-com)
  (setq dict (vlax-get-or-create-object "Scripting.Dictionary"))
  (vlax-invoke-method dict 'RemoveAll)
  (if (setq ss (ssget "_X" '((0 . "POINT"))))
    (repeat (setq i (sslength ss))
      (setq pt (cdr (assoc 10 (entget (ssname ss (setq i (1- i)))))))
      (setq key (ptkey pt tol))
      (if (and key (/= key ""))
        (if (= :vlax-false (vlax-invoke-method dict 'Exists key))
          (vlax-invoke-method dict 'Add key 1))
      )
    )
  )
  dict
)
