20000   def library fnWindowsStart(wsFile$*1024)
32000     if ~wsSetup then
32020       wsSetup=1
32040       option retain
32080       dim hWsCmdFile$*256
32100     end if
32200     library 'S:\Core\Library': fngethandle
44000     if shortTermUniqueNumber=>9999 then shortTermUniqueNumber=0
44020     shortTermUniqueNumber$=cnvrt$('pic(####)',shortTermUniqueNumber+=1)
44040     hWsCmdFile$=env$('client_temp')&'\acsWinStart_'&session$&'-'&shortTermUniqueNumber$&'.cmd'
44060     wsFile$=os_filename$(wsFile$)
44080     open #hWsCmd:=fngethandle: 'Name='&env$('at')&br_filename$(hWsCmdFile$)&',RecL=512,Replace',display,output 
44100     pr #hWsCmd: '@echo off'
44120     pr #hWsCmd: '@echo Advanced Computer Services LLC'
44140     pr #hWsCmd: '@echo Opening: "'&wsFile$&'"'
44160     ! pr #hWsCmd: '@echo .'
44180     ! pr #hWsCmd: '@echo Relative To: '&os_filename$('[Q]\')
44200     pr #hWsCmd: '@echo .'
44220     pr #hWsCmd: '"'&wsFile$&'"'
44240     close #hWsCmd: 
44260     execute 'sy -M -C '&hWsCmdFile$
60000   fnend
