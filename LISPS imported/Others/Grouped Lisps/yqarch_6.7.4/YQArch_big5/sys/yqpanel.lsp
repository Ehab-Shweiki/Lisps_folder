;;;=========================================================================
;;;         源 泉 設 計    [組合命令面板]相關LSP程序例子
;;;-------------------------------------------------------------------------
;;; 文件夾 sys\
;;;=========================================================================
;;; 註：代碼行中分號";"後面是註釋，行首加分號則暫時取消該行。

;;; 代碼編寫: Walter Lam   QQ: 575825448
;;; 複製傳播本文件請保留以上信息


;;;======================
;;; 一些自定義命令的例子
;;;======================

(defun c:yq_mi (/ ss p1 p2)
    (yq_cbegin)
    (princ "\n****鏡像")
    (if (and (setq ss (ssget ":l"))
             (yq_ssredraw ss 3)
             (setq p1 (getpoint "\n指定鏡像線的第一點:"))
             (setq p2 (getpoint p1 "\n指定鏡像線的第二點:"))
        )
        (if (getpoint "\n點擊鼠標刪除原圖元: <空格保留原圖元> ")
            (yq_mirror ss p1 p2 t)
            (yq_mirror ss p1 p2 nil)
        )
    )
    (yq_cend)
)


(defun c:yq_ccb(/ pt)
    (princ "\n****帶基點剪切")
    (and (setq pt (getpoint "\n基點: "))
         (ssget)
         (command "copybase" "non" pt "p" "" "erase" "p" "")
    )
    (princ)
)


(defun c:yq_rz(/ pt1 pt2)
    (princ "\n****旋轉到0度")
    (and (ssget)
         (setq pt1 (getpoint "\n基點: "))
         (setq pt2 (getpoint pt1 "\n點取角度,此角度將改為0度: "))
         (command "rotate" "p" "" "non" pt1 "r" "non" pt1 "non" pt2 "0")
    )
    (princ)
)


(defun c:yq_rg()
    (princ "\n****局部再生")
    (if (ssget) (command "._erase" "p" "" "._oops"))
    (princ)
)


(defun c:yq_zd()
    (princ "\n****視圖縮小0.5倍")
    (command "._zoom" "0.5x")
    (princ)
)


(defun c:yq_ze()
    (princ "\n****視圖縮放到範圍")
    (command "._zoom" "e")
    (princ)
)


(defun c:yq_zz()
    (princ "\n****縮放到上一個視圖")
    (command "._zoom" "p")
    (princ)
)


(defun c:yq_edb(/ ss la)
    (princ "\n****炸開塊到塊所在層")
    (if (setq ss (ssget '((0 . "INSERT"))))
        (foreach e (yq_ss2lst ss)
            (yq_mark)
            (setq la (cdr (assoc 8 (entget e))))
            (command "._explode" e) (yq_cmdenter)
            (command "._chprop" (yq_newss) "" "la" la "")
        )
    )
    (princ)
)


(defun c:yq_edc(/ ss)
    (princ "\n****炸開塊到當前層")
    (if (setq ss (ssget '((0 . "INSERT"))))
        (foreach e (yq_ss2lst ss)
            (yq_mark)
            (command "._explode" e) (yq_cmdenter)
            (command "._chprop" (yq_newss) "" "la" (getvar "CLAYER") "")
        )
    )
    (princ)
)


(defun c:yq_vxx(/ pt1 pt2 ss)
    (princ "\n****不拖拽移動")
    (and (setq ss (ssget))
         (yq_ssredraw ss 3)
         (setq pt1 (getpoint "\n基點: "))
         (setq pt2 (getpoint pt1 "\n目標點: "))
         (command "._move" ss "" "non" pt1 "non" pt2)
    )
    (if ss (yq_ssredraw ss 4))
    (princ)
)


(defun c:yq_cxx(/ pt1 pt2 ss)
    (princ "\n****不拖拽複製")
    (if (and (setq ss (ssget))
             (yq_ssredraw ss 3)
             (setq pt1 (getpoint "\n基點: "))
        )
        (while (setq pt2 (getpoint pt1 "\n目標點: "))
             (command "._copy" ss "" "non" pt1 "non" pt2)
        )
    )
    (if ss (yq_ssredraw ss 4))
    (princ)
)


(defun c:yq_date ( / pt str )
    (princ "\n****插入日期時間字串\n")
    (if (setq pt (getpoint))
        (progn
            (setq str (rtos (getvar "CDATE") 2 6)
                  str (strcat (substr str 1 4) "-" (substr str 5 2) "-" (substr str 7 2)
                              " " (substr str 10 2) ":" (substr str 12 2))
            )
            (yq_text pt (* 3 (getvar "DIMSCALE")) str)
        )
    )
    (princ)
)


(defun c:yq_cm(/ ss pt1 pt2 n d a ss0)
    (yq_cbegin)
    (princ "\n****按次數複製")
    (if (and (setq ss (ssget))
             (setq pt1 (getpoint "\n基點: "))
             (setq pt2 (getpoint pt1 "\n第二點: "))
        )
        (progn
            (setq ss0 (yq_copy2pt ss pt1 pt2))
            (initget 2)
            (setq yq_cm (if yq_cm yq_cm 1)
                  n (getint (strcat "\n複製次數(負數內等分): <" (itoa yq_cm) "> "))
                  yq_cm (if n n yq_cm)
                  d (distance pt1 pt2)
                  a (angle pt1 pt2)
            )
            (if (minusp yq_cm) 
                (setq d (/ d (abs yq_cm)))
                (setq pt1 pt2 ss ss0)
            )
            (setq pt2 (polar pt1 a d))
            (repeat (1- (abs yq_cm))
                (yq_copy2pt ss  pt1 pt2)
                (setq pt2 (polar pt2 a d))
            )
        )
    )
    (yq_cend)
)


;;;==============
;;; SOLID變HATCH
;;;==============
;;; 2012.7.17 寫
(defun c:yq_s2h(/ ss e)
    (princ "\n****SOLID變HATCH")
    (if (setq ss (ssget '((0 . "SOLID,TRACE"))))
        (foreach x (yq_ss2lst ss)
            (setq e (yq_pline (yq_getvertexes x nil) 1))
            (command "_bhatch" "p" "s" "s" e "" "")
            (command "_matchprop" x (entlast) "")
            (entdel x)(entdel e)
        )
    )
    (princ)
)


;;;========
;;; 弧變圓
;;;========
;;; 2012.12.20 寫
(defun c:yq_a2c(/ ss en)
    (princ "\n****弧變圓")
    (if (setq ss (ssget '((0 . "ARC"))))
        (foreach x (yq_ss2lst ss)
            (setq en (cddr (entget x))
                  en (vl-remove-if '(lambda (x) (member (car x) '(50 51))) en)
            )
            (entmake (cons '(0 . "CIRCLE") en))
            (entdel x)
        )
    )
    (princ)
)



;;;============
;;; 圓變多段線
;;;============
;;; 2012.6.30 寫
(defun c:yq_c2x(/ ss)
    (princ "\n****圓變多段線")
    (if (setq ss (ssget '((0 . "CIRCLE"))))
        (foreach x (yq_ss2lst ss)
            (yq_cirle2donut x)
        )
    )
    (princ)
)



(defun c:yq_expattblk(/ ss)
    (yq_cbegin)
    (princ "\n****炸開屬性塊,文字不變")
    (if (setq ss (ssget (list (cons 0 "INSERT"))))
    (foreach e (yq_ss2lst ss)
        (yq_explodeattblk e)
    )
    )
    (yq_cend)
)



(defun c:yq_fxx(/ ss)
    (yq_cbegin)
    (princ "\n****批量圓角多段線")
    (setq yq_fxx (if yq_fxx yq_fxx 0))
    (setq yq_fxx (yq_getdist "\n圓角半徑: " yq_fxx))
    (and (setq ss (ssget '((0 . "LWPOLYLINE"))))
         (foreach e (yq_ss2lst ss)
             (yq_filletpline e yq_fxx)
         )
    )
    (yq_cend)
)



(defun c:yq_cfxx(/ ss)
    (yq_cbegin)
    (princ "\n****批量倒角多段線")
    (setq yq_cfxx (if yq_cfxx yq_cfxx 0))
    (setq yq_cfxx (yq_getdist "\n倒角距離: " yq_cfxx))
    (and (setq ss (ssget '((0 . "LWPOLYLINE"))))
         (foreach e (yq_ss2lst ss)
             (yq_chamferpline e yq_cfxx)
         )
    )
    (yq_cend)
)



(defun c:yq_delendspaces(/ ss)
    (yq_cbegin)
    (princ "\n****去除文字前後空格")
    (and (setq ss (ssget (list (cons 0 "*TEXT"))))
         (foreach x (yq_ss2lst ss)
             (yq_updent x (list (cons 1 (yq_allTrim (yq_dxf x 1))))) 
         )
    )
    (yq_cend)
)



;;;==================
;;; 快速切割提取圖像
;;;==================
(defun c:yq_qgtx(/ e1 e2 e3 e4)
    (yq_cbegin)
    (princ "\n****快速切割提取圖像")
    (if (and (setq e1 (car (yq_entsel "\n選取要剪切的圖像: " (list (cons 0 "IMAGE")))))
             (setq e2 (car (yq_entsel "\n選取多段線邊界: " (list (cons 0 "*POLYLINE,CIRCLE,ELLIPSE")))))
             (setq e3 (yq_clone e1))
        )
    (prgon
        (setq e4 (yq_pline (yq_curve2ptlst_d e2) 1))
        (if (= (yq_dxf e3 280) 0)
            (command "_imageclip" e3 "n" "s" e4)
            (command "_imageclip" e3 "d" "_imageclip" e3 "n" "s" e4)
        )
        (entdel e4)
        (command "_move" (entlast) "" (car (yq_getvertexes e2 0)))
    )
    )
    (yq_cend)
)



;;;============
;;; 炸開參照塊
;;;============
(defun c:yq_refx (/ e)
    (yq_cbegin)
    (princ "\n****炸開參照塊")
    (if (setq e (car (yq_entsel "\n請選取參照塊: " (list (cons 0 "INSERT")))))
    (progn
        (command "_xref" "b" (yq_dxf e 2))
        (command "_explode" e)
    )
    )
    (yq_cend)
)



;;;=================================
;;; 顏色/線性/線寬恢復為隨層bylayer
;;;=================================
(defun c:yq_00(/ ss)
    (yq_cbegin)
    (princ "\n****顏色/線性/線寬恢復為隨層bylayer")
    (cond 
        ((setq ss (yq_sspre))
            (command "._chprop" ss "" "lt" "bylayer" "lw" "bylayer" "c" "bylayer" "")
            (princ "\n所選圖元顏色/線性/線寬恢復為隨層bylayer.")
        )
        (t
           	(setvar "CECOLOR" "bylayer")
           	(setvar "CELTYPE" "bylayer")
           	(setvar "CELWEIGHT" -1)
           	(princ "\n當前顏色/線性/線寬恢復為隨層bylayer.")
	       )
	   )
   	(yq_cend)
)


;;----- 新的語句可放在這裡 -----







(princ)                           ; 為最後一行
