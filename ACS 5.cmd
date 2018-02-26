@Echo Off
Rem Cls
@Echo Advanced Computer Services LLC

rem Set BR_Version=431ie
rem Set BR_Date=2015-12-28

ver | find "XP" > nul
if %ERRORLEVEL% == 0 goto WIN_XP

rem Set BR_XP_Mode=False
rem 
rem If /I "%ACSDeveloper%" NEQ "" (
    Set BR_Version=432d
    Set BR_Date=2017-03-06
    rem Set BR_Version=432g
    rem Set BR_Date=2017-04-19
    Set BRListener_Version=430
    Set BRListener_Date=2012-10-16
    Set BRClient_Date=2017-04-19
rem ) else (
rem     Set BR_Version=431h
rem     Set BR_Date=2015-06-29
rem )
goto POST_SET_BR

:WIN_XP
Set BR_XP_Mode=True
Set BR_Version=431h
Set BR_Date=2015-06-29
goto POST_SET_BR

:POST_SET_BR
set ACSForce32Bit=Yes
set ACSForceRelease=Yes
Rem 2016/04/03 - BR does not support PDF printing in 64 bit mode - only 32...  
Rem so we are stuck with the 32bit architecture until they do (which should be no time soon)
Rem  see   http://brforum.brulescorp.com/viewtopic.php?p=3296#p3296
If Exist %SystemRoot%\SysWow64 (
    Set BR_Architecture=x64
) else (
    Set BR_Architecture=Win32
)
If /I "%ACSForce32Bit%" NEQ "" (
    @Echo      ACSForce32Bit:  Yes - %ACSDeveloper%
    Set BR_Architecture=Win32
)

If /I "%ACSDeveloper%" NEQ "" (
    @Echo      ACSDeveloper:  Yes - %ACSDeveloper%
    rem  (changed at 432f)  Set BR_Model=DebugEfence
    Set BR_Model=Debug
) else (
    Set BR_Model=Release
)
If /I "%ACSForceRelease%" NEQ "" (
    @Echo      ACSForce32Bit:  Yes - %ACSDeveloper%
    Set BR_Model=Release
)
    @Echo              Mode:          %BR_Model%

Set BR=brserver-%BR_Version%-%BR_Architecture%-%BR_Model%-%BR_Date%.exe
Set default=%~dp0ACS 5.exe
rem echo default is %default%
rem If Exist "%default%" (
rem   set executable=%default%
rem ) else (
      set executable=%~dp0%BR%
rem )
Start "ACS 5" "%executable%" %*
