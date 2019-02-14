@echo off


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
