@echo off
@echo ___________________________________________________________
@echo %0 %*
@echo put call to new executable here followed by percent astrik


CHOICE /C YN /M "Call NebraskaWebClient.exe?"
@echo %ERRORLEVEL%
If %ERRORLEVEL%==1 (
	@echo calling NebraskaWebClient
	rem @echo start "NebraskaWebClient" /B /WAIT /D "C:\Test Folder" "C:\Test Folder\NebraskaWebClient.exe" %*
	rem @echo start "NebraskaWebClient" /WAIT /D "C:\Test Folder" "NebraskaWebClient.exe %*"
	@echo "C:\Test Folder\NebraskaWebClient.exe" %*"
	"C:\Test Folder\NebraskaWebClient.exe" %*
) else (
	@echo execute skipped.
)
@echo ___________________________________________________________
pause
