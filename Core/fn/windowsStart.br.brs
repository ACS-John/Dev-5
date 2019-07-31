def library fnWindowsStart(wsFile$*1024)
	if ~wsSetup then
		wsSetup=1
		option retain
		dim hWsCmdFile$*256
	end if
	library 'S:\Core\Library': fngethandle
	if shortTermUniqueNumber=>9999 then shortTermUniqueNumber=0
	shortTermUniqueNumber$=cnvrt$('pic(####)',shortTermUniqueNumber+=1)
	hWsCmdFile$=env$('client_temp')&'\acsWinStart_'&session$&'-'&shortTermUniqueNumber$&'.cmd'
	wsFile$=os_filename$(wsFile$)
	open #hWsCmd:=fngethandle: 'Name='&env$('at')&hWsCmdFile$&',RecL=512,Replace',display,output 
	pr #hWsCmd: '@echo off'
	pr #hWsCmd: '@echo Advanced Computer Services LLC'
	pr #hWsCmd: '@echo Opening: "'&wsFile$&'"'
	! pr #hWsCmd: '@echo .'
	! pr #hWsCmd: '@echo Relative To: '&os_filename$('[Q]\')
	pr #hWsCmd: '@echo .'
	pr #hWsCmd: '"'&wsFile$&'"'
	close #hWsCmd: 
	execute 'sy -M -C '&hWsCmdFile$
fnend
