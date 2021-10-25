autoLibrary
fnTop(program$)
on error goto Ertn
fnWriteProc('swithmail.cmd','"C:\ACS\Dev-5\Core\SwithMail\SwithMail.exe" /s /x "C:\ACS\Dev-5\Core\SwithMail\SwithMailSettings.xml"')
fnWriteProc(''              ,'pause')
exe 'sy swithmail.cmd'
Xit: end ! fnXit
include: Ertn
