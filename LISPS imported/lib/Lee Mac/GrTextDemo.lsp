;; GrText Demo Program 1  -  Lee Mac
;; Prompts the user to type a message and displays
;; the entered text on screen beside the cursor
;;
;; Requires: GrText.lsp
;; https://www.lee-mac.com/grtext.html

(defun c:demo1 ( / *error* col gr1 gr2 str vec )

    (defun *error* ( m ) (princ m) (redraw) (princ))

    (setq str ""
          col 2
    )
    (princ "\nType your message...")
    (while
        (progn
            (setq gr1 (grread nil 15 0)
                  gr2 (cadr gr1)
                  gr1 (car  gr1)
            )
            (cond
                (   (= 5 gr1)
                    (redraw)
                    (if vec (LM:DisplayGrText gr2 vec col 15 -31))
                    t
                )
                (   (= 2 gr1)
                    (cond
                        (   (= 08 gr2)
                            (if (< 0 (strlen str))
                                (setq str (substr str 1 (1- (strlen str))))
                            )
                        )
                        (   (= 13 gr2)
                            (setq str (strcat str "\n"))
                        )
                        (   (<= 32 gr2 255)
                            (setq str (strcat str (chr gr2)))
                        )
                    )
                    (setq vec (LM:GrText str))
                )
            )
        )
    )
    (redraw) (princ)
)

;; GrText Demo Program 2  -  Lee Mac
;; Displays the coordinates of the cursor position
;; as the cursor is moved across the screen.
;;
;; Requires: GrText.lsp

(defun c:demo2 ( / *error* pnt str )

    (defun *error* ( m ) (princ m) (redraw) (princ))

    (while (= 5 (car (setq pnt (grread nil 13 0))))
        (redraw)
        (setq str (mapcar 'rtos (trans (cadr pnt) 1 0)))
        (LM:DisplayGrText (cadr pnt) (LM:GrText (strcat "X=" (car str) "\nY=" (cadr str))) 3 15 -31)
    )
    (redraw) (princ)
)

;; GrText Demo Program 3  -  Lee Mac
;; Prompts the user for a base point and displays
;; the area of the rectangular region enclosed by the
;; base point and cursor as the cursor is moved across the screen.
;;
;; Requires: GrText.lsp

(defun c:demo3 ( / *error* dis pt1 pt2 pt3 suf )

    (defun *error* ( m ) (princ m) (redraw) (princ))

    (if (setq pt1 (getpoint "\nSpecify Base Point: "))
        (progn
            (setq pt1 (reverse (cdr (reverse pt1)))
                  pt2 (trans pt1 1 2)
                  suf (strcat (if (< (getvar 'lunits) 3) "mm" "in") (chr 178))
            )
            (while (= 5 (car (setq pt3 (grread nil 13 0))))
                (redraw)
                (setq pt3 (cadr pt3)
                      dis (mapcar '- pt3 pt1)
                      pt3 (trans pt3 1 2)
                )
                (LM:DisplayGrText pt3 (LM:GrText (strcat (rtos (abs (apply '* dis)) 2 4) suf)) 3 15 -31)
                (grvecs
                    (list (if (< (car dis) 0.0) -256 256)
                        pt2 (list (car pt3) (cadr pt2))
                        pt3 (list (car pt3) (cadr pt2))
                        pt2 (list (car pt2) (cadr pt3))
                        pt3 (list (car pt2) (cadr pt3))
                    )
                )
            )
        )
    )
    (redraw) (princ)
)

;; Display GrText  -  Lee Mac
;; pnt  -  cursor point in UCS
;; vec  -  GrText vector list
;; col  -  Text Colour (ACI Colour)
;; xof  -  x-offset from cursor in pixels
;; yof  -  y-offset from cursor in pixels

(defun LM:DisplayGrText ( pnt vec col xof yof / scl )
    (setq scl (/ (getvar 'viewsize) (cadr (getvar 'screensize)))
          pnt (trans pnt 1 2)
    )
    (grvecs (cons col vec)
        (list
            (list scl 0.0 0.0 (+ (car  pnt) (* xof scl)))
            (list 0.0 scl 0.0 (+ (cadr pnt) (* yof scl)))
            (list 0.0 0.0 scl 0.0)
           '(0.0 0.0 0.0 1.0)
        )
    )
)

(princ)