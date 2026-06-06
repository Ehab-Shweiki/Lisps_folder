(defun c:ClothoidBetween (/ R1 R2 L step s x y angle pts pt)
  ;; --- طلب المعطيات من المستخدم ---
  (setq R1 (getreal "\nأدخل نصف القطر الابتدائي R1: ")) ; مثلاً 100
  (setq R2 (getreal "\nأدخل نصف القطر النهائي R2: "))   ; مثلاً 50
  (setq L (getreal "\nأدخل الطول الكلي للكلوثويد L: ")) ; مثلاً 50
  (setq step (getreal "\nأدخل طول كل قطعة (step): "))   ; مثلاً 1

  ;; --- حساب معامل الكلوثويد A لكل نصف قطر ---
  (setq A1 (sqrt (* R1 L)))
  (setq A2 (sqrt (* R2 L)))

  ;; --- قائمة النقاط ---
  (setq s 0)
  (setq pts '())

  (while (<= s L)
    ;; تقريب باستخدام صيغة بولي نوميال:
    (setq x (- s (/ (expt s 5) (* 40.0 (expt R2 2) (expt L 2)))))
    (setq y (- (/ (expt s 3) (* 6 R2 L)) (/ (expt s 7) (* 336.0 (expt R2 3) (expt L 3)))))
    
    ;; تحويل للنقطة (بدون دوران - يمكن إضافة زاوية لاحقًا)
    (setq pt (list x y))
    (setq pts (append pts (list pt)))

    (setq s (+ s step))
  )

  ;; --- رسم الخط المنكسر polyline ---
  (command "_.PLINE")
  (foreach p pts
    (command p)
  )
  (command "") ; إنهاء الأمر
  (princ "\nتم رسم الكلوثويد بنجاح.")
  (princ)
)
