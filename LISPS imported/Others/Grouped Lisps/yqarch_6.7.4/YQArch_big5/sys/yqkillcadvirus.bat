@ECHO OFF
@CLS 
@COLOR 0A
@ECHO.
@ECHO ========================================
@ECHO =                                      =
@ECHO =  　 "源泉"刪除CAD病毒程序　V1.3      =
@ECHO =                                      =
@ECHO ========================================
@ECHO.
@ECHO 將刪除電腦中的以下文件(如果有使用這些文件，請Ctrl-C中斷並先各自備份):
@ECHO acaddoc.lsp,acad.lsp,acad.fas,acadapq.lsp,acadappp.lsp,acadiso.lsp,
@ECHO acadapp.lsp,acad.vlx,lcm.fas,acad.mnl
@ECHO.
@ECHO 請關閉正在運行的CAD程序，確定後請按任意鍵繼續......
@pause>nul
@pushd
@ECHO.
@ECHO 查找並刪除「CAD病毒文件」，分析中......請稍候......
@ECHO.
set yqdir=%CD%
for %%i in (C D E F G H I J K L M N O P Q R S T U V W X Y Z) do ( 
if exist %%i: ( 
%%i:
@cd\
@del /f /q /s /a acaddoc.lsp
@del /f /q /s /a acaddoc.fas
@del /f /q /s /a acad20??doc.lsp
@del /f /q /s /a acad20??.lsp
@del /f /q /s /a acad.lsp
@del /f /q /s /a acad.fas
@del /f /q /s /a acadapq.lsp
@del /f /q /s /a acadappp.lsp
@del /f /q /s /a acadapp.lsp
@del /f /q /s /a acad.vlx
@del /f /q /s /a lcm.fas
@del /f /q /s /a acad.mnl
@del /f /q /s /a acad.sys
@del /f /q /s /a acadiso.lsp
@del /f /q /s /a acadiso.fas
@del /f /q /s /a acadsmu.fas
))
if exist %yqdir%\yqarch.mnl (
@copy %yqdir%\yqarch.mnl %yqdir%\acad.lsp
)
@ECHO.
del /f /q %windir%\system32\dwgrun.bat
REG DELETE HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\Run /v dwgrun /f
@ECHO OK，所有「懷疑CAD病毒文件」已經完全清除！請按任意鍵退出......
@pause>nul
@popd
