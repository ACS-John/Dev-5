rem caused win10 endless loop           @echo off
rem caused win10 endless loop           :: BatchGotAdmin (Run as Admin code starts)
rem caused win10 endless loop           REM --> Check for permissions
rem caused win10 endless loop           >nul 2>&1 "%SYSTEMROOT%\system32\cacls.exe" "%SYSTEMROOT%\system32\config\system"
rem caused win10 endless loop           REM --> If error flag set, we do not have admin. 
rem caused win10 endless loop           if '%errorlevel%' NEQ '0' (
rem caused win10 endless loop           echo Requesting administrative privileges...
rem caused win10 endless loop           goto UACPrompt
rem caused win10 endless loop           ) else ( goto gotAdmin )
rem caused win10 endless loop           :UACPrompt
rem caused win10 endless loop           echo Set UAC = CreateObject^("Shell.Application"^) > "%temp%\getadmin.vbs"
rem caused win10 endless loop           echo UAC.ShellExecute "%~s0", "", "", "runas", 1 >> "%temp%\getadmin.vbs"
rem caused win10 endless loop           "%temp%\getadmin.vbs"
rem caused win10 endless loop           exit /B
rem caused win10 endless loop           :gotAdmin
rem caused win10 endless loop           if exist "%temp%\getadmin.vbs" ( del "%temp%\getadmin.vbs" )
rem caused win10 endless loop           pushd "%CD%"
rem caused win10 endless loop           CD /D "%~dp0"
rem caused win10 endless loop           :: BatchGotAdmin (Run as Admin code ends)
rem caused win10 endless loop           :: Your codes should start from the following line
rem caused win10 endless loop           
rem caused win10 endless loop           
rem caused win10 endless loop           If Exist %SystemRoot%\SysWow64 (
rem caused win10 endless loop               rem Set BR_Architecture=x64
rem caused win10 endless loop               Set system32=%SystemRoot%\SysWow64
rem caused win10 endless loop           ) else (
rem caused win10 endless loop               rem Set BR_Architecture=Win32
rem caused win10 endless loop               Set system32=%SystemRoot%\System32
rem caused win10 endless loop           )
rem caused win10 endless loop           
rem caused win10 endless loop           Rem Add your code to run as an admin here...

