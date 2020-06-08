! Here is a very simple example of how to retrieve a Webpage from a Webserver from right within BR!
exe 'cd c:'
exe 'cd '&env$('temp')(3:inf)

open #hHttpIni:=1: 'name=http.ini,recl=1024,replace',d,o
pr #hHttpIni: "user-agent 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2486.0 Safari/537.36 Edge/14.14271'"
! pr #hHttpIni: 'DUMP-HEADERS '&env$('temp')&'\acsHttpHeaderDump'
cl #hHttpIni:

! url$='http://planetacs.net/index.htm'    < works
! url$='http://planetacs.net/acs5update/ACS-5-Update-CO.exe'  
dim url$*1024
url$='http://planetacs.net/acs5update/Release_Notes.txt'
dim localCopy$*256
localCopy$=url$(pos(url$,'/',-1)+1:len(url$))
ope #hHtml:=2: 'name='&url$&',control=http.ini,http=client',d,outin
! ***** Wake Up the Server
!pr #hHtml: ""
! ***** get response
ope #hOut:=3: 'name='&localCopy$&',replace',d,o
do
	dim line$*2048
	lin #hHtml: line$ eof EoHtml
	pr #hOut: line$
	dim column$(0)*2048
	str2mat(line$,mat column$,chr$(9))
	testVal=val(column$(3)) conv ignore
	if testVal>0 t pr 'version on the web is '&str$(testVal) : pause
	! pr #0: line$
	longestLen=max(longestLen,len(line$))
loop
EoHtml: !
pr 'file$(hHtml, "httpinfo")="'&file$(hHtml, "httpinfo")&'"'
pr "Maximum Segment Length";longestLen
pr 'created: '&file$(hOut)
cl #hOut:
cl #hHtml: