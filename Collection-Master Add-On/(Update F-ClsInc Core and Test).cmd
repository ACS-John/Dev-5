@echo off
rem type "%~dp0bqLogo.txt"

@echo                       BBBBBBBBBBBBBBBBB        QQQQQQQQQ      
@echo                       B::::::::::::::::B     QQ:::::::::QQ    
@echo                       B::::::BBBBBB:::::B  QQ:::::::::::::QQ  
@echo                       BB:::::B     B:::::BQ:::::::QQQ:::::::Q 
@echo                         B::::B     B:::::BQ::::::O   Q::::::Q 
@echo                         B::::B     B:::::BQ:::::O     Q:::::Q 
@echo                         B::::BBBBBB:::::B Q:::::O     Q:::::Q 
@echo                         B:::::::::::::BB  Q:::::O     Q:::::Q 
@echo                         B::::BBBBBB:::::B Q:::::O     Q:::::Q 
@echo                         B::::B     B:::::BQ:::::O     Q:::::Q 
@echo                         B::::B     B:::::BQ:::::O  QQQQ:::::Q 
@echo                         B::::B     B:::::BQ::::::O Q::::::::Q 
@echo                       BB:::::BBBBBB::::::BQ:::::::QQ::::::::Q 
@echo                       B:::::::::::::::::B  QQ::::::::::::::Q  
@echo                       B::::::::::::::::B     QQ:::::::::::Q   
@echo                       BBBBBBBBBBBBBBBBB        QQQQQQQQ::::QQ 
@echo                                                        Q:::::Q
@echo                                                         QQQQQQ




set pathFrom=D:\Brumbaugh\clsinc
set pathTo=F:\clsinc

@echo Source:       %pathFrom%
@echo Destination:  %pathTo%
REM copy "%pathFrom%\custom\menu_win.txt" "%pathTo%\custom\*.*"
REM REM DO NOT DO THIS - IT BREAKS conditional theme editor.		copy "%pathFrom%\fileio.ini" "%pathTo%\*.*"
REM roboCopy "%pathFrom%\Collection-Master Add-On" "%pathTo%\Collection-Master Add-On" *.* /XF *.brs /S

rem @echo CORE SYNC COMMENTED OUT!!!
rem @echo CORE SYNC COMMENTED OUT!!!
rem @echo CORE SYNC COMMENTED OUT!!!
roboCopy "%pathFrom%\Core" "%pathTo%\Core" *.* /s

roboCopy "%pathFrom%\Test" "%pathTo%\Test" *.* /XF *.brs /S
rem pause