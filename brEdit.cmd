@echo off
rem r: custom settings
  set editor_launcher="C:\ACS\Program\Notepad++\Notepad++.exe"
  set brLauncher="%~dp0ACS 5.cmd"
  set prefferedFormat=.brs
rem /r


goto CALLED_FROM_WIN 
REM if not "%BR%"=="" ( goto CALLED_FROM_BR ) else ( goto CALLED_FROM_WIN )
REM changed to always "CALLED_FROM_WIN" because I using PROC ED instaed of EDIT to avoid the recreation of .brs files

pause
:CALLED_FROM_BR
  @echo ________________________________________________________________________________
  @echo detected being called by %BR%
  @echo.
  
  %editor_launcher% %*
  goto :eof
) 

goto :eof

:CALLED_FROM_WIN
@echo ________________________________________________________________________________
@echo.
@echo editor_launcher: %editor_launcher%
@echo brLauncher: %brLauncher%
@echo prefferedFormat: %prefferedFormat%
@echo.

rem try preffered format first
set tryWhat="%~1%prefferedFormat%"
if exist %tryWhat% (
  %editor_launcher% %tryWhat%
  @echo   %editor_launcher% %tryWhat%
  goto :eof
)
rem .brs and .wbs
set tryWhat="%~1s"
if exist %tryWhat% (
  %editor_launcher% %tryWhat%
  @echo   %editor_launcher% %tryWhat%
  goto :eof
)
rem .br.brs and .wb.brs
set tryWhat="%~1.brs"
if exist %tryWhat% (
  %editor_launcher% %tryWhat%
  @echo %editor_launcher% %tryWhat%
  goto :eof
)
rem .br.wbs and .wb.wbs
set tryWhat="%~1.wbs"
if exist %tryWhat% (
  %editor_launcher% %tryWhat%
  @echo %editor_launcher% %tryWhat%
  goto :eof
)


@echo.
@echo.
@echo "%~1%prefferedFormat%" could not be found.
@echo This message will self distruct in 4 seconds.
choice /c YN  /t 4 /d N /m "Create it"

if errorlevel 2 goto :eof
set tryWhat="%~1%prefferedFormat%"
@echo %brLauncher% edit %tryWhat%
%brLauncher% edit "%*"
