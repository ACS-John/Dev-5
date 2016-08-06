echo off
if '%1'=='' goto HELP
if '%2'=='' goto HELP
set homepath="%~1"
set homedir=%~d1
for %%A in (%HOMEPATH%) do set HOMEPATH=%%~spnxA
echo %homedir%%homepath%
echo %homedir%%homepath% >%2
goto XIT
:HELP
echo Proper Usage is:
echo   %0 [path_input] [outputfile]
echo   [path_input] - a long path to parse.  quote encapsulation is required
echo                  i.e. "C:\Long Path To Parse\etc"
echo   [outputfile] - file to output resulting short path to.
:XIT
