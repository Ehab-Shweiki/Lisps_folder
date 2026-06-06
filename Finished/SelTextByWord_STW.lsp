(defun c:SelTextByWord (/ word ss1 ss2 ent i txt)
  (setq word (getstring T "\nEnter the word to search in text: "))
  (setq ss1 (ssget '((0 . "TEXT,MTEXT"))))
  (setq ss2 (ssadd))

  (if ss1
    (progn
      (setq i 0)
      (while (< i (sslength ss1))
        (setq ent (ssname ss1 i))
        (setq txt (cdr (assoc 1 (entget ent))))
        (if (wcmatch (strcase txt) (strcat "*"(strcase word)"*"))
          (ssadd ent ss2)
        )
        (setq i (1+ i))
      )
      (if (> (sslength ss2) 0)
        (progn
          (princ (strcat "\nFound " (itoa (sslength ss2)) " matching text(s)."))
          (sssetfirst nil ss2) ;; highlight selected
        )
        (princ "\nNo matching text found.")
      )
    )
    (princ "\nNo text or mtext entities in drawing.")
  )
  (princ)
)

(defun c:FSW (/ ss word newSS ent i txt elist)
  ;; Step 1: Get all text & mtext via existing logic
  (FilterSelection_Type "T" nil nil)

  ;; Step 2: Ask user for word to filter by
  (setq word (getstring T "\nEnter word to filter by (leave empty to keep all): "))

  ;; Step 3: Proceed only if user entered word
  (if (and word (/= word ""))
    (progn
      ;; Get current selection
      (setq ss (ssget "_I")) ; _I = Previous selection set

      (if ss
        (progn
          (setq newSS (ssadd)
                i 0)
          (while (< i (sslength ss))
            (setq ent (ssname ss i))
            (setq elist (entget ent))
            (setq txt (cdr (assoc 1 elist)))
            (if (and txt (wcmatch (strcase txt) (strcat "*"(strcase word)"*")))
              (ssadd ent newSS)
            )
            (setq i (1+ i))
          )

          (if (> (sslength newSS) 0)
            (progn
              (sssetfirst nil newSS)
              (prompt (strcat "\nFiltered to " (itoa (sslength newSS)) " matching texts."))
            )
            (prompt "\nNo matching texts found.")
          )
        )
        (prompt "\nNo texts selected initially.")
      )
    )
    (prompt "\nNo word entered, keeping all selected texts.")
  )

  (princ)
)

(defun c:STW () (c:SelTextByWord))
(princ "\nType STW to run the SelTextByWord command.")
(princ "\nType FSW to run the FilterSelection_Type by word command.")
(princ)
