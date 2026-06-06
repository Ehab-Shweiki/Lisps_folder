; TODO: make a UI library for buttons, inputs, etc.
; TODO: make function to draw rectangle neer mouse and some text

(defun ui:pt-in-rect (pt xmin ymin xmax ymax)
  "Return T if a point PT lies inside rectangle"
  (and pt
       (>= (car pt) xmin)
       (<= (car pt) xmax)
       (>= (cadr pt) ymin)
       (<= (cadr pt) ymax))
)

(defun ui:draw-rect (xmin ymin xmax ymax color thickness)
  "Draw rectangle with given color"
  (grdraw (list xmin ymin 0) (list xmax ymin 0) color thickness)
  (grdraw (list xmin ymax 0) (list xmax ymax 0) color thickness)
  (grdraw (list xmin ymin 0) (list xmin ymax 0) color thickness)
  (grdraw (list xmax ymin 0) (list xmax ymax 0) color thickness)
)

(defun ui:draw-button (btn hovered)
  (setq xmin  (nth 0 btn))
  (setq ymin  (nth 1 btn))
  (setq xmax  (nth 2 btn))
  (setq ymax  (nth 3 btn))
  (setq label (nth 4 btn))
  (setq color (if hovered 2 1)) ; 2=green hover, 1=white normal

  (ui:draw-rect xmin ymin xmax ymax color 1)

  ;; write label
  (grtext (+ xmin 10) (+ ymin 15)
          (if hovered
            (strcat ">> " label " <<")
            label
          )
          2
  )
)


(defun ui:button-hovered-p (btn pt)
  "Return true if mouse point is hovering over button BTN"
  (ui:pt-in-rect pt (nth 0 btn) (nth 1 btn) (nth 2 btn) (nth 3 btn))
)

(defun ui:button-clicked-p (btn pt)
  "Same as hover, but used for click events"
  (ui:button-hovered-p btn pt)
)


(defun c:UI-DEMO (/ btn ev pt hover run)

  ;; Define button: (xmin ymin xmax ymax label)
  (setq btn (list 100 200 250 250 "OK"))

  ;; main loop
  (setq run T)
  (while run
    (setq ev (grread T 1 0))

    (cond
      ;; Mouse move
      ((= (car ev) 5)
        (setq pt (cadr ev))
        (setq hover (ui:button-hovered-p btn pt))
        (ui:draw-button btn hover)
      )

      ;; Mouse click
      ((= (car ev) 3)
        (setq pt (cadr ev))
        (if (ui:button-clicked-p btn pt)
          (progn
            (alert "Button clicked!")
            (setq run nil)
          )
        )
      )

      ;; ESC pressed
      ((and (= (car ev) 2) (= (cadr ev) 27))
        (setq run nil)
      )
    )
  )

  (princ)
)
