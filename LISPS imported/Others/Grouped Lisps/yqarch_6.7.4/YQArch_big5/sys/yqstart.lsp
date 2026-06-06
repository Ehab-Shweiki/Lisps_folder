;;;==========================================================================================
;;; 「源泉設計」 啟動程序 yqstart.lsp
;;;------------------------------------------------------------------------------------------
;;; 註：1. 這是「源泉設計」的啟動程序，通常由sys\acad.lsp自動加載。假如acad.lsp與你的其他
;;;        程序產生衝突，令「源泉設計」未能自動加載，請將sys\acad.lsp刪除，並請將本文件
;;;        yqstart.lsp 加入AutoCAD啟動組；
;;;     2. 代碼行中分號";"後面是註釋，行首加分號則暫時取消該行；
;;;     3. 請不要修改本文件，除非您熟悉AutoLISP；
;;;     4. 「源泉設計」v5.8.1版以後只能運行在AutoCAD 2000以上版，不再支持AutoCAD r14版了。
;;;==========================================================================================
(setq old_cmdecho (getvar "CMDECHO"))
(setvar "CMDECHO" 0)
(if (getvar "SECURELOAD") (setvar "SECURELOAD" 0))       ; AutoCAD 2014版 加載lisp不警告

(if (null c:yq_about)(progn
(if (or (and (getvar "GCADVER") (<= "13.0" (substr (getvar "GCADVER") 2 4)))
        (<= "15.0" (substr (getvar "ACADVER") 1 4))
    )
(progn


;;; 加載「源泉設計」程序核心
(if (and (null c:yq_about)(findfile "yqarch.vlx"))(load "yqarch.vlx"))


;;; 將 acad.lsp 文件加載到每一個圖形
(if (getvar "acadlspasdoc") (setvar "acadlspasdoc" 1))


;;; 在屏幕下狀態行顯示當前「標注比例 DIMSCALE」「標注樣式 DIMSTY」「文字樣式 STYLE」
;(setvar "MODEMACRO" (strcat "DIMSCALE:<1:" "$(substr,$(getvar,DIMSCALE),1,6)"
;    "> DIMSTY:<" "$(getvar,DIMSTYLE)" "> STYLE:<" "$(getvar,TEXTSTYLE)" ">"))


;;; 加載源泉系統面板程序
(if (findfile "yqpanel.lsp") (load "yqpanel.lsp"))


;;; 運行源泉設計系統目錄下「library」文件夾的程序文件
(if (and yq_library (/= yq_library ""))
    (progn
        (princ (yq_ec (strcat "\nLoading .lsp files in folder <" yq_library "\\>...")
                      (strcat "\n正在加載公用文件夾 <" yq_library "\\> 中的程序...")))
        (yq_run yq_library "*.lsp,*.vlx,*.fas")
        (princ "\n...")
    )
)


;;; 運行源泉設計系統目錄下「當前用戶」文件夾的程序文件
(if (and yq_user (/= yq_user ""))
    (progn
        (princ (yq_ec (strcat "\n[YQArch] Current User is <" yq_user ">.")
                      (strcat "\n[源泉設計]當前用戶是 <" yq_user ">.")))
        (princ (yq_ec (strcat "\nLoading .lsp files in folder <" yq_user "\\>...")
                      (strcat "\n正在加載用戶文件夾 <" yq_user "\\> 中的程序...")))
        (yq_runsc)             ; 命令快捷鍵設定
        (yq_yqpanel)           ; 系統面板設定
        (yq_runpanel)          ; 用戶面板設定
        (yq_runlaysc)          ; 設置轉換圖層快捷鍵
        (yq_runwlaysc)         ; 設置轉換通配符圖層快捷鍵
        (yq_runltpsc)          ; 設置轉換線型快捷鍵
        (yq_runhatsc)          ; 設置填充圖案快捷鍵
        (yq_run yq_user "*.lsp,*.vlx,*.fas")
        (princ "\n...")
    )
)


;;; 加載源泉菜單為 AutoCAD 原菜單的最後一項
(if (not (menugroup "yqarch")) (c:yq_menu))


(princ (yq_ec "\nYQArch <yqstart.lsp> loaded! Enter 'yqarch' to launch\n\n"
              "\n源泉設計啟動文件 <yqstart.lsp> 載入完畢!\n加載源泉菜單請鍵入 'yq_menu', 命令總覽請鍵入 'yqarch'\n\n"))


)(princ (yq_ec "\nAvailable only at AutoCAD2000/GstarCAD8 or later!" "\n請在AutoCAD2000/浩辰CAD8以上版運行源泉設計."))
)
)
)

(if (and c:yq_about (not (menugroup "yqarch"))) (c:yq_menu))

(if old_cmdecho (progn (setvar "CMDECHO" old_cmdecho) (setq old_cmdecho nil)))
(princ)
