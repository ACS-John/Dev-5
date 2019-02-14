@echo off


If Exist %SystemRoot%\SysWow64 (
    rem Set BR_Architecture=x64
    Set system32=%SystemRoot%\SysWow64
) else (
    rem Set BR_Architecture=Win32
    Set system32=%SystemRoot%\System32
)
@echo on
copy "%~dp0..\vb32\OSSMTP.ocx" "%system32%\*.*"
"%system32%\regsvr32.exe" "%system32%\OSSMTP.ocx" %*
pause
rem "%system32%\regsvr32.exe" "%system32%\msFlxGrd.ocx" %*
rem "%system32%\regsvr32.exe" "%system32%\msComCt2.ocx" %*
rem "%system32%\regsvr32.exe" "%system32%\ComDlg32.ocx" %*
rem "%system32%\regsvr32.exe" "%system32%\msComCt3N.ocx" %*
