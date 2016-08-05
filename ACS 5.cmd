@Echo Off
Rem Cls
@Echo Advanced Computer Services LLC

Set Config=brConfig.sys

Set BR_Version=431ie
Set BR_Date=2015-12-28


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
    Set BR_Model=DebugEfence
) else (
    Set BR_Model=Release
)
    @Echo              Mode:          %BR_Model%

Set BR=brserver-%BR_Version%-%BR_Architecture%-%BR_Model%-%BR_Date%.exe


If /I "%ACSRunAsAdmin%" NEQ "" (
    @Echo      ACSRunAsAdmin:  Yes - %ACSRunAsAdmin%
    :: BatchGotAdmin (Run as Admin code starts)
    REM --> Check for permissions
    >nul 2>&1 "%SYSTEMROOT%\system32\cacls.exe" "%SYSTEMROOT%\system32\config\system"
    REM --> If error flag set, we do not have admin.
    if '%errorlevel%' NEQ '0' (
    echo Requesting administrative privileges...
    goto UACPrompt
    ) else ( goto gotAdmin )
    :UACPrompt
    echo Set UAC = CreateObject^("Shell.Application"^) > "%temp%\getadmin.vbs"
    echo UAC.ShellExecute "%~s0", "", "", "runas", 1 >> "%temp%\getadmin.vbs"
    "%temp%\getadmin.vbs"
    exit /B
    :gotAdmin
    if exist "%temp%\getadmin.vbs" ( del "%temp%\getadmin.vbs" )
    pushd "%CD%"
    CD /D "%~dp0"
    :: BatchGotAdmin (Run as Admin code ends)
    :: Your codes should start from the following line
    rem above code from http://www.techgainer.com/create-batch-file-automatically-run-administrator/

    echo     Launch Command:  Start "ACS 5" "%~dp0%BR%"  %*

)

Start "ACS 5" "%~dp0%BR%" %*

