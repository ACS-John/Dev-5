@echo off
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

echo "      ,.  - · - .,  '                  ,.-·~·-., '        ";
echo ",·'´,.-,   ,. -.,   \`';,'          ,.·´ ,. - .,   '\`.       ";
echo " \::\.'´  ;'\::::;:'  ,·':\'      ,'´ ,·´\::::::::\`;  ';\  '  ";
echo "  '\:';   ;:;:·'´,.·'´\::::';    ,'  ;'::::\;::-::;:';  ;:\   ";
echo "  ,.·'   ,.·:'´:::::::'\;·´     ';  ';::;·´       ,'  ,'::';  ";
echo "  '·,   ,.\`' ·- :;:;·'´        .';'\  '\;'       .'  .':::::;' ";
echo "     ;  ';:\:\`*·,  '\`·,  °  ';  \:'.   '·,  ,·´ .·'::::::;'  ";
echo "     ;  ;:;:'-·'´  ,.·':\     \   \`·:\`·   '´ ;´::::::::;' '  ";
echo "  ,·',  ,. -~:*'´\:::::'\‘     \\` ·- · :\\`·.  \`·:;:·´ '    ";
echo "   \:\\`'´\:::::::::'\;:·'´       '\::::::::\:;\` · .,.'·  '   ";
echo "    '\;\:::\;: -~*´‘             \` ·- · '´\`·:::::\::\     ";
echo "             '                                \` · :\_\‚   ";

set pathFrom=D:\Brumbaugh\clsinc
set pathTo=F:\clsinc

@echo Source:       %pathFrom%
@echo Destination:  %pathTo%
copy "%pathFrom%\custom\menu_win.txt" "%pathTo%\custom\*.*"
REM DO NOT DO THIS - IT BREAKS conditional theme editor.		copy "%pathFrom%\fileio.ini" "%pathTo%\*.*"
roboCopy "%pathFrom%\Collection-Master Add-On" "%pathTo%\Collection-Master Add-On" *.* /XF *.brs /S
roboCopy "%pathFrom%\Core" "%pathTo%\Core" *.* /s
rem pause