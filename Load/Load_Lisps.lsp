(defun c:Load_Lisps ( / currentPath baseFolder folders files folder f path )

  ;; Get the full path of the currently loaded LISP file
  (setq currentPath (findfile "Load_Lisps.lsp")) ; ⬅️ change to actual filename

  (if currentPath
    (progn
      ;; Get parent "Base Folder" of the folder containing the file
      (setq baseFolder
        (vl-filename-directory
          (vl-filename-directory currentPath)
        )
      )

      ;; Ensure trailing slash
      (if (/= (substr baseFolder (strlen baseFolder)) "\\")
        (setq baseFolder (strcat baseFolder "\\"))
      )

      ;; Define subfolders to load from
      (setq folders
        (list
          (strcat baseFolder "LISPS imported")
          (strcat baseFolder "Finished")
          (strcat baseFolder "Functions")
        )
      )

      ;; Helper: check if file is a loadable LISP file
      (defun is-valid-lisp-file (filename / upper)
        (setq upper (strcase filename))
        (or
          (wcmatch upper "*.LSP")
          (wcmatch upper "*.FAS")
          (wcmatch upper "*.VLX")
        )
      )

      ;; Load all valid Lisp files from a folder
      (defun load-lisps-from-folder (folder)
        (if (vl-file-directory-p folder)
          (progn
            (setq files (vl-directory-files folder nil 1))

            ;; Ensure trailing slash
            (if (/= (substr folder (strlen folder)) "\\")
              (setq folder (strcat folder "\\"))
            )

            (foreach f files
              (if (and
                    (not (wcmatch (strcase f) "*.LNK"))
                    (is-valid-lisp-file f)
                  )
                (progn
                  (setq path (strcat folder f))
                  (princ (strcat "\nLoading: " path))
                  (load path)
                )
              )
            )
          )
          (princ (strcat "\nFolder not found: " folder))
        )
      )

      ;; Loop through folders
      (foreach folder folders
        (load-lisps-from-folder folder)
      )

      (princ "\n✔ All Lisp files loaded successfully.\n")
    )
    (princ "\n⚠ Could not determine current script path.\n")
  )

  (princ)
)

(princ "\nType Load_Lisps to load all LISP files from sibling folders.\n")
(princ)

(c:Load_Lisps) ; Automatically call the function when loaded
;; This will load all LISP files from the specified folders
;; and print a message when done.
