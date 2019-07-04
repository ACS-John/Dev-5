@Echo Off
@Echo Advanced Computer Services LLC
@Echo Launching Client...

rem Set BR_Date=2017-04-19
rem Set BR_Date=2018-07-28
Set BR_Date=2019-05-31

Rem If Exist %SystemRoot%\SysWow64 (
Rem     Set BR_Architecture=x64
Rem ) else (
    Set BR_Architecture=Win32
Rem )

Rem If /I "%ACSDeveloper%" NEQ "" (
Rem     @Echo      ACSDeveloper:  Yes (%ACSDeveloper%)
Rem     Set BR_Model=DebugEfence
Rem ) else (
    Set BR_Model=Release
Rem )
@Echo              Mode:          %BR_Model%

Set BR=%~dp0brclient-%BR_Architecture%-%BR_Model%-%BR_Date%.exe
Rem @Echo BR is %BR%

If /I "%ACSDeveloper%" NEQ "" (
@Echo      Start "ACS 5 Client" "%BR%"  %*
)

cd %temp%
Start "ACS 5 Client" "%BR%" %*
