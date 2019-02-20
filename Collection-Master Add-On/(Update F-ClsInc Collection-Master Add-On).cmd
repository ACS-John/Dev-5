@echo off

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
copy "%pathFrom%\custom\menu_win.txt" "%pathTo%\custom\*.*"
REM DO NOT DO THIS - IT BREAKS conditional theme editor.		copy "%pathFrom%\fileio.ini" "%pathTo%\*.*"
roboCopy "%pathFrom%\Collection-Master Add-On" "%pathTo%\Collection-Master Add-On" *.* /XF *.brs /S

rem @echo CORE and TEST SYNC COMMENTED OUT!!!
rem @echo CORE and TEST SYNC COMMENTED OUT!!!
rem @echo CORE and TEST SYNC COMMENTED OUT!!!
rem roboCopy "%pathFrom%\Core" "%pathTo%\Core" *.* /s
rem 
rem roboCopy "%pathFrom%\Test" "%pathTo%\Test" *.* /XF *.brs /S
rem pause
rem type "%~dp0bqLogo.txt"

rem   echo "   /\   /¯¯¯¯\ '‚         /¯¯¯\‚               ";
rem   echo " /    \|   |:'\    \‘   '   /        '\'              ";
rem   echo "|\         |:::|    | '‚  /      _    '\‚            ";
rem   echo "|:|        |::/    /|‘  '|      /::\     \      °    ";
rem   echo "|:|        |/    /::|°  |     |:::::\     '\         ";
rem   echo " \|            (:::'|  °|\     \::::::\     \°      ";
rem   echo "  |        |\    \:/‘   |::\     \:::::|     |‘      ";
rem   echo "  |        |::\    \‘   |::::\     \|\¯      ¯\    ";
rem   echo "  |        |::::\    \ ‘ \:::::\    '\/     ___\'‚ ";
rem   echo " /         |:::::|    |    \:::::\_____/|:::::|‘ ";
rem   echo "|\    /|\   \:::/    /|      \::::|:::::::|:|:::::|  ";
rem   echo "|::\/::|::\____ /::|        \::|:::::::|:|:::::|  ";
rem   echo "|:::|::|:::|:::::::|:::|‘         \|:::::::|/ ¯¯¯° ";
rem   echo "\::'|::/\::|:::::::|::/'            ¯¯¯¯          ";
rem   echo "  '\|/    \|:::::::|/'                               ";
rem   echo "           ¯¯¯¯'‚                                ";
rem   

rem   echo "      ,.  - · - .,  '                  ,.-·~·-., '        ";
rem   echo ",·'´,.-,   ,. -.,   \`';,'          ,.·´ ,. - .,   '\`.       ";
rem   echo " \::\.'´  ;'\::::;:'  ,·':\'      ,'´ ,·´\::::::::\`;  ';\  '  ";
rem   echo "  '\:';   ;:;:·'´,.·'´\::::';    ,'  ;'::::\;::-::;:';  ;:\   ";
rem   echo "  ,.·'   ,.·:'´:::::::'\;·´     ';  ';::;·´       ,'  ,'::';  ";
rem   echo "  '·,   ,.\`' ·- :;:;·'´        .';'\  '\;'       .'  .':::::;' ";
rem   echo "     ;  ';:\:\`*·,  '\`·,  °  ';  \:'.   '·,  ,·´ .·'::::::;'  ";
rem   echo "     ;  ;:;:'-·'´  ,.·':\     \   \`·:\`·   '´ ;´::::::::;' '  ";
rem   echo "  ,·',  ,. -~:*'´\:::::'\‘     \\` ·- · :\\`·.  \`·:;:·´ '    ";
rem   echo "   \:\\`'´\:::::::::'\;:·'´       '\::::::::\:;\` · .,.'·  '   ";
rem   echo "    '\;\:::\;: -~*´‘             \` ·- · '´\`·:::::\::\     ";
rem   echo "             '                                \` · :\_\‚   ";

