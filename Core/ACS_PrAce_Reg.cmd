@echo off
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


If Exist %SystemRoot%\SysWow64 (
    rem Set BR_Architecture=x64
    Set system32=%SystemRoot%\SysWow64
) else (
    rem Set BR_Architecture=Win32
    Set system32=%SystemRoot%\System32
)

"%system32%\regsvr32.exe" "%system32%\ChadoSpellText.ocx" %*
"%system32%\regsvr32.exe" "%system32%\msFlxGrd.ocx" %*
"%system32%\regsvr32.exe" "%system32%\msComCt2.ocx" %*
"%system32%\regsvr32.exe" "%system32%\ComDlg32.ocx" %*
"%system32%\regsvr32.exe" "%system32%\msComCt3N.ocx" %*
