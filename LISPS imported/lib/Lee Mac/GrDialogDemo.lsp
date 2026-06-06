;;--------------------=={ GrDialog Demo }==-------------------;;
;;                                                            ;;
;;  A simple example program demonstrating how the LM:GrText  ;;
;;  function may be utilised to display text in a DCL dialog. ;;
;;                                                            ;;
;;  This program requires GrText.lsp to be loaded before      ;;
;;  loading and running this program. The latest version of   ;;
;;  the LM:GrText function may be downloaded from:            ;;
;;                                                            ;;
;;  http://lee-mac.com/grtext.html                            ;;
;;                                                            ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:grdialog ( / *error* dch des tmp vec )

    (defun *error* ( msg )
        (if (= 'file (type des))
            (close des)
        )
        (if (< 0 dch)
            (unload_dialog dch)
        )
        (if (and (= 'str (type tmp)) (setq tmp (findfile tmp)))
            (vl-file-delete tmp)
        )
        (if (and msg (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*")))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (if
        (and
            (setq tmp (vl-filename-mktemp nil nil ".dcl"))
            (setq des (open tmp "w"))
            (foreach x
               '(
                    "img : image { width = 18.16; height = 3.69; fixed_width = true; fixed_height = true; alignment = centered; }"
                    "grdialog : dialog { label = \"GrText Dialog\"; spacer;"
                    "  : img { key = \"img1\"; color = -15; }"
                    "  : img { key = \"img2\"; color =   0; }"
                    "  : img { key = \"img3\"; color =   7; }"
                    "  spacer; ok_only; }"
                )
                (write-line x des)
            )
            (not (setq des (close des)))
            (< 0 (setq dch (load_dialog tmp)))
            (new_dialog "grdialog" dch)
        )
        (progn
            (setq vec (LM:GrText "\n   GrText"))
            (LM:GrDialog "img1" vec 5)
            (LM:GrDialog "img2" vec 2)
            (LM:GrDialog "img3" vec 1)
            (start_dialog)
        )
    )
    (*error* nil)
    (princ)
)

;; GrDialog  -  Lee Mac
;; key  -  DCL tile key
;; vec  -  GrText vector list
;; col  -  Text Colour (ACI Colour)

(defun LM:GrDialog ( key vec col )
    (start_image key)
    (repeat (/ (length vec) 2)
        (vector_image
            (caar  vec) (abs (- 16 (cadar  vec)))
            (caadr vec) (abs (- 16 (cadadr vec)))
            col
        )
        (setq vec (cddr vec))
    )
    (end_image)
)

(vl-load-com) (princ)