;;;=========================================================================
;;;         源 泉 設 計    圖檔初始化文件，用於打造一個適合自己的運行環境
;;;-------------------------------------------------------------------------
;;; 文件夾 sys\用戶文件夾\
;;;
;;;     「用戶文件夾」為自建文件夾。將自編或收集到的「.lsp/.vlx/.arx」程序
;;; 文件放入此文件夾下，再設定此「用戶文件夾」為當前用戶(用命令yq_setuser)，
;;; 則「源泉建築」在啟動的時候會自動加載這些程序。
;;;=========================================================================
;;; 註：1. 此文件是用戶圖檔初始化文件，用於打造一個適合自己的運行環境；
;;;     2. 代碼行中分號";"後面是註釋，行首加分號則暫時取消該行。

;;; 代碼編寫: Walter Lam   QQ: 575825448
;;; 複製傳播本文件請保留以上信息



;;;----------------------------------------
;;; 加載docbar圖形文檔切換標籤(如果有的話)
;;;----------------------------------------
(setq tmp_ver (substr (getvar "ACADVER") 1 4)
      tmp_str (cond ((= tmp_ver "15.0") "docbar_2002.arx")
                    ((= tmp_ver "16.0") "docbar_2004.arx")
                    ((= tmp_ver "16.1") "docbar_2005.arx")
                    ((= tmp_ver "16.2") "docbar_2006.arx")
                    ((= tmp_ver "17.0") "docbar_2007.arx")
                    ((= tmp_ver "17.1") "docbar_2008.arx")
                    ((= tmp_ver "17.2") "docbar_2009.arx")
                    ((= tmp_ver "18.0") "docbar_2010.arx")
                    ((= tmp_ver "18.1") "docbar_2011.arx")
                    ((= tmp_ver "18.2") "docbar_2012.arx")
                    (T nil)
              )
)
(if (and tmp_str (not (member tmp_str (arx)))(findfile tmp_str))(vl-catch-all-apply 'arxload (list tmp_str)))


;;;-----------------------------------------
;;; 加載iDwgTab圖形文檔切換標籤(如果有的話)
;;;-----------------------------------------
(cond 
    ((= (getenv "PROCESSOR_ARCHITECTURE") "x86")
        (setq tmp_str (cond ((= (substr tmp_ver 1 2) "15") "iDwgTab2000.arx")
                            ((= (substr tmp_ver 1 2) "16") "iDwgTab2004.arx")
                            ((= (substr tmp_ver 1 2) "17") "iDwgTab2007.arx")
                            ((= (substr tmp_ver 1 2) "18") "iDwgTab2010.arx")
                            ((= (substr tmp_ver 1 4) "19.0") "iDwgTab2013.arx")
                            (T nil)
                      )
        )
    )
    (t  (setq tmp_str (cond ((= tmp_ver "17.0") "iDwgTab2007x.arx")
                            ((= (substr tmp_ver 1 2) "17") "iDwgTab2008x.arx")
                            ((= (substr tmp_ver 1 2) "18") "iDwgTab2010x.arx")
                            ((= (substr tmp_ver 1 4) "19.0") "iDwgTab2013x.arx")
                            (T nil)
                      )
        )
    )
)
(if (and tmp_str (not (member tmp_str (arx)))(findfile tmp_str))(vl-catch-all-apply 'arxload (list tmp_str)))


;;;-------------------------------------------
;;; 加載cl-DwgMan圖形文檔切換標籤(如果有的話)
;;;-------------------------------------------
(cond 
    ((= (getenv "PROCESSOR_ARCHITECTURE") "x86")
        (setq tmp_str (cond ((= (substr tmp_ver 1 2) "16") "cl-DwgMan_2006-x86.arx")
                            ((= tmp_ver "17.0") "cl-DwgMan_2007-x86.arx")
                            ((= tmp_ver "17.1") "cl-DwgMan_2008-x86.arx")
                            ((= tmp_ver "17.2") "cl-DwgMan_2009-x86.arx")
                            ((= tmp_ver "18.0") "cl-DwgMan_2010-x86.arx")
                            ((= tmp_ver "18.1") "cl-DwgMan_2011-x86.arx")
                            ((= tmp_ver "18.2") "cl-DwgMan_2012-x86.arx")
                            ((= tmp_ver "19.0") "cl-DwgMan_2013-x86.arx")
                            (T nil)
                      )
        )
    )
    (t  (setq tmp_str (cond ((= tmp_ver "17.1") "cl-DwgMan_2008-x64.arx")
                            ((= tmp_ver "17.2") "cl-DwgMan_2009-x64.arx")
                            ((= tmp_ver "18.0") "cl-DwgMan_2010-x64.arx")
                            ((= tmp_ver "18.1") "cl-DwgMan_2011-x64.arx")
                            ((= tmp_ver "18.2") "cl-DwgMan_2012-x64.arx")
                            ((= tmp_ver "19.0") "cl-DwgMan_2013-x64.arx")
                            (T nil)
                      )
        )
    )
)
(if (and tmp_str (not (member tmp_str (arx)))(findfile tmp_str))(vl-catch-all-apply 'arxload (list tmp_str)))


;;;------------------------------
;;; 加載亂刀去教育版(如果有的話)
;;;------------------------------
(setq tmp_str (substr (getvar "ACADVER") 1 2)
      tmp_str (strcat "BladeR" tmp_str (if (= (getenv "PROCESSOR_ARCHITECTURE") "x86") "" "_X64") ".arx")
)
(if (and (not (member tmp_str (arx)))(findfile tmp_str))(vl-catch-all-apply 'arxload (list tmp_str)))


;;;------------------------------
;;; 加載UnEdu去教育版(如果有的話)
;;;------------------------------
(setq tmp_str (substr (getvar "ACADVER") 1 2)
      tmp_str (strcat "UnEdu_R" tmp_str (if (= (getenv "PROCESSOR_ARCHITECTURE") "x86") "" "_X64") ".arx")
)
(if (and (not (member tmp_str (arx)))(findfile tmp_str))(vl-catch-all-apply 'arxload (list tmp_str)))
(setq tmp_str nil tmp_ver nil)



;;;----------------------------------------------------
;;;  打造一個適合自己的運行環境 (前置分號等於取消該行)
;;;----------------------------------------------------
;(setvar "MIRRTEXT" 0)             ; 鏡像文字:否
;(setvar "SORTENTS" 127)          ; 圖元排序順序
;(setvar "CHAMFERA" 0)             ; 倒角的長度1
;(setvar "CHAMFERB" 0)             ; 倒角的長度2
;(setvar "AUNITS"   0)             ; 角度單位:十進制度數
;(setvar "AUPREC"   4)            ; 角度精度
;(setvar "LUNITS"   2)             ; 線性單位
;(setvar "LUPREC"   4)            ; 線性精度
;(setvar "FILEDIA"  1)             ; 顯示文件定位對話框:是
;(setvar "CMDDIA"   1)             ; 顯示外部命令及plot對話框:是
;(setvar "BLIPMODE" 0)            ; 控制點標記(小十字)是否可見:否
;(setvar "GRIDMODE" 0)             ; 顯示點柵格:否
;(setvar "SNAPMODE" 0)             ; 捕捉間距:否
;(setvar "LISPINIT" 1)             ; AutoLISP 函數和變量只在當前繪圖任務中有效
;(setvar "UCSICON"  1)            ; 使UCS光標不移動
;(setvar "CURSORSIZE" 100)        ; 十字光標最大化
;(setvar "PSLTSCALE" 0)           ; 1:視口比例決定線型比例
;(setvar "DIMZIN"   8)            ; 控制是否對主單位值作消零處理, 8:消除後續零
(if (> (getvar "SAVETIME") 60)    ; 以指定的時間間隔自動保存圖形，
    (setvar "SAVETIME" 10)        ; 若大於1小時則改為10分鐘，缺省保存在系統的臨時文件夾
)

;(if (getvar "DIMASSOC") (setvar "DIMASSOC" 1))          ; 1:使用非關聯標注,2:使用關聯標注對像
;(if (getvar "QAFLAGS") (setvar "QAFLAGS" 0))
;(if (getvar "PICKSTYLE") (setvar "PICKSTYLE" 1))        ; 1:使用編組選擇,3:使用編組選擇和關聯填充選擇
;(if (getvar "DBLCLKEDIT") (setvar "DBLCLKEDIT" 1))      ; 控制繪圖區域中的雙擊編輯操作
;(if (getvar "OSOPTIONS") (setvar "OSOPTIONS" 2))        ; 1:對像捕捉忽略圖案填充對像
;(if (getvar "AUTOSNAP") (setvar "AUTOSNAP" 63))         ; 控制自動捕捉標記、工具欄提示和磁吸的顯示等
;(if (getvar "MEASUREMENT") (setvar "MEASUREMENT" 1))    ; 控制當前圖形是使用英制還是公制填充圖案和線型文件 0:英制 1:公制
;(if (getvar "STARTMODE") (setvar "STARTMODE" 0))        ; 控制「開始」選項卡的顯示 初始值1


;;----- 可選一些缺省設定 -----
;(setq YQ_NOWALLSEAL nil)                   ; ww 畫牆命令無封口線
;(setq YQ_PIERIGNOREINTERS nil)             ; 開門窗命令時牆垛起點計算忽略所有交點
;(setq YQ_VAR_DOOR '(900 200 nil nil))      ; 設置缺省門寬、牆跺、是否居牆中、淨寬以門套為準
;(setq YQ_VAR_WIN '(1500 200 T))            ; 設置缺省窗寬、牆跺、是否居牆中
;(setq YQ_VAR_AZHFACTOR 0.2)                ; 自動軸號因子:短於x倍軸線長度的軸線不畫軸號
;(setq YQ_VAR_LTFACTOR 0.15)                ; 自動調節軸線線型比例係數=此數字*圖紙比例/LTSCALE
;(setq YQ_VAR_HIDDEN '("HIDDEN" 15 0.5))    ; 虛線設定: 虛線名稱,模型空間中的線型比例,圖紙空間中的線型比例

;(setq YQ_NOLAYERSTACK nil)                 ; 關閉/鎖定/隔離等圖層命令不再使用還原用堆棧
;(setq YQ_NOAUTOFITEXCEL nil)               ; 實時輸出Excel表格時不再自動調節單元格寬度
;(setq YQ_ISUPDATEDIMLAYER nil)             ; sd/ddu 縮放/更新標注時，是否同時修改為當前標注圖層
;(setq YQ_VAR_WINDOW '(45 30 25 50))        ; 參數窗常量: 落地門扇厚,重疊部分半寬,半玻璃厚,半看線牆厚
;(setq YQ_NOUPDATELAYER t)                  ; 轉換圖層快捷鍵ersc時，不更新原有圖層

;(setq YQ_HASCMDSTAT t)                     ; 可開啟cmdstats.txt文件統計源泉設計命令使用頻率
;(setq YQ_WARNINGMINUTES 240)               ; 當前圖紙連續工作若干分鐘後給出溫馨提示，nil/0: 不提示
;(setq YQ_NORECOMMENDVPSCALE t)             ; sd命令時，不自動「推薦當前視口比例」
;(setq YQ_ZXSIZE 2)                         ; 控制原始折斷線符號的大小，缺省值:2
;(setq YQ_BLKPREFIX "YQ")                   ; 塊名前綴，缺省值"YQ"

;(if (/= (getvar "INSUNITS") 4)
;(if (yq_warning "提示" (strcat "此圖繪圖單位是 <" (yq_unitsstr) ">，是否修正為毫米?"))
;    (yq_init_mm nil)    ; 用於圖紙單位自動初始化為毫米, nil 可修改為預設比例，如100
;)
;)


;;----- 新的語句可放在這裡 -----








(princ) ;為最後一行
