02000   library 'S:\Core\Library': fnxit
02010   library : fnerror
02020   on error goto ERTN
02040   pr 1/0
03000 XIT: let fnxit
08000 ! <updateable region: ertn>
08040 ERTN: let fnerror(program$,err,line,act$,"xit")
08060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
08080   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
08100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
08120 ERTN_EXEC_ACT: execute act$ : goto ERTN
08140 ! </updateable region: ertn>
10000 ! 
10020 ! replace S:\Core\fnerror (the acs 5.o version - S:\Core\ace\fnerror is the 4.0 version)
10040 ! ______________________________________________________________________
10060   def library fnerror(callingProgram$*256, errornumber, linenumber, &act$, stopable$)
10080     library 'S:\Core\Library': fnmsgbox,fnlog,fngethandle
10100     on error goto NEVER
10120     let xcnt=cnt
10140 ! ______________________________________________________________________
10160     dim response$(5)*80,msgline$(2)*60,log$*128,acshelp$*200
10180     dim caption$*42
10200 ! ______________________________________________________________________
10220 ! act$  =  the returning action to be executed
10240 ! stopable$  =  if it is "no" or "NO" etc it'll not let them stop
10260 ! otherwise it should be the paragraph heading of where to goto on
10280 ! a selected exit (must be 15 characters or less)
10300 ! ______________________________________________________________________
10320 !   pr border: "Error "&str$(errornumber)&" on line "&str$(linenumber)
10340     pr f "1,73,C 7,N": bell$
10360     log$="Error "&str$(errornumber)&" at line "&str$(linenumber)
10380     log$=log$&" of "&callingProgram$(1:100)
10400     fnlog(log$,2)
10440     act$="Go "&str$(linenumber)
10460     acshelp$="Help\co\error\br\"
10480     acshelp$=acshelp$&"Err"&cnvrt$("Pic(####)",errornumber)&".html"
12000 MENU1: ! 
12020     open #win:=fngethandle: "SRow=4,SCol=4,Rows=17,Cols=72,Border=S:[screen],N=[screen],Caption=<Error",display,outin  ! ,border=Sr   ...   
12040     if errornumber=61 then 
12060       pr #win,fields "1,1,Cc 69,N": "A record is needed but is locked by another user."
12080       pr #win,fields "2,1,Cc 69,N": "Please ask other users to return to the Menu and"
12100       pr #win,fields "3,1,Cc 69,N": "then you may select Retry."
12120     else if errornumber=4148 then 
12140       pr #win,fields "1,1,Cc 69,N": "A file lock is needed but the file is already locked by someone else."
12160       pr #win,fields "2,1,Cc 69,N": "Please ask other users to return to the Menu and"
12180       pr #win,fields "3,1,Cc 69,N": "then you may select Retry."
12200     else if errornumber=632 then 
12220       pr #win,fields "1,1,Cc 69,N": "Problem with Index File"
12240     end if 
12260 ! 
12280     lc=7
12300     pr #win,fields str$(lc)&",2,Cr 13,N": "Program:"
12320     pr #win,fields str$(lc)&",16,C 53,P[textboxes]": env$('Core_Program_Current')(1:53)
12340     pr #win,fields str$(lc+=1)&",2,Cr 13,N": "File:"
12360     pr #win,fields str$(lc)&",16,C 53,P[textboxes]": callingProgram$(1:53)
12380     lc+=1
12400     pr #win,fields str$(lc+=1)&",2,Cr 13,[screen]": "Error Number:"
12420     pr #win,fields str$(lc)&",16,C 5,P[textboxes]": str$(errornumber) ! ,r,n
12440     lc+=1
12460     pr #win,fields str$(lc+=1)&",2,Cr 13,[screen]": "Line Number:"
12480     pr #win,fields str$(lc)&",16,C 5,P[textboxes]": str$(linenumber) ! ,r,n
12500     pr #win,fields str$(lc+=1)&",2,Cr 13,N": "Count+1:"
12520     pr #win,fields str$(lc)&",16,C 5,P[textboxes]": str$(xcnt+1) ! ,r,n
12540     pr #win,fields str$(lc+=1)&",2,Cr 13,N": "Session:"
12560     pr #win,fields str$(lc)&",16,C 5,P[textboxes]": session$ ! ,r,n
12580     button_pos$='47'
12600     pr #win,fields "9,"&button_pos$&",Cc 22,,B01": "Retry (Enter)"
12620     pr #win,fields "10,"&button_pos$&",Cc 22,,B99": "Exit (Esc)"
12640 !   pr #win,fields "6,"&button_pos$&",C 22,B,10": "WB Help (F10)"
12660     pr #win,fields "11,"&button_pos$&",Cc 22,,B08": "BRWiki Help (F8)"
12680 !   if exists(acshelp$)<>0 then
12700 !     pr #win,fields "12,"&button_pos$&",Cc 22,,B09": "ACS Help (F9)"
12720 !   end if
12740     pr #win,fields "14,"&button_pos$&",Cc 22,,B12": "Developer Pause (F12)" ! 1,19,12/CC 12,,B1000
12760 ERR_INP: ! 
14000     input #0,fields "4,4,C 1,AE,N": pause$
14020     if cmdkey=0 then 
14040       log$="action taken = Retry" : let fnlog(log$,2)
14060       act$="Go "&str$(linenumber)
14080       goto ERROR_XIT
14100     else if cmdkey=5 or cmdkey=99 then 
14120       log$="action taken = Quit" : let fnlog(log$,2)
14140       goto ERR_QUITER
14160     else if cmdkey=8 then 
14180       log$="action taken = BRWiki Help" : let fnlog(log$,2)
14200       gosub BRWIKIHELP
14220 !   else if cmdkey=9 then
14240 !     log$="action taken = ACS Help" : let fnlog(log$,2)
14260 !     gosub ACSHELP
14280 !   else if cmdkey=10 then
14300 !     log$="action taken = WB Help" : let fnlog(log$,2)
14320 !     gosub ERR_WBHELP
14340     else if cmdkey=12 then 
14360       act$="PAUSE"
14380       log$="action taken = Program Pause" : let fnlog(log$,2)
14400       goto ERROR_XIT
14420     end if 
14440     goto ERR_INP
14460 ! ______________________________________________________________________
16000 ! ERR_WBHELP: ! r:
16020 !     on error goto NO_ERR_WBHELP
16040 !     let help$("ERR"&cnvrt$("PIC(####)",errornumber)&",WBCmd")
16060 !     goto ERR_WBHELP_RETURN ! /r
16080 ! NO_ERR_WBHELP: ! r:
16100 !     mat msgline$(2)
16120 !     let msgline$(1)="Sorry, No Workstation Basic Help "
16140 !     let msgline$(2)="is available for this Error Number."
16160 !     fnmsgbox(mat msgline$,response$(1),env$('program_caption'),0)
16180 !     let response$(1)=response$(1)(1:1)
16200 ! ERR_WBHELP_RETURN: !
16220 !     return  ! /r
18000 BRWIKIHELP: ! r:
18020     execute "Sy -c -m Start /MIN http://brwiki2.brulescorp.com/index.php?search="&cnvrt$('pic(####)',errornumber)
18040     return  ! /r
20000 ! ACSHELP: ! r:
20100 !     if exists(acshelp$)<>0 then
20200 !       execute "Sy -c -m "&acshelp$
20300 !     end if
20400 !     return  ! /r
20600 ERR_QUITER: ! r:
20700     if uprc$(rtrm$(stopable$))="NO" then 
20710       setenv('ExitNow','no')
20800       mat msgline$(2)
20900       let msgline$(1)="Do not stop the processing of this program."
21000       let msgline$(2)="Please contact ACS Technical Support."
21100       fnmsgbox(mat msgline$,response$(1),env$('program_caption'),0)
21200       let response$(1)=response$(1)(1:1)
21300       goto MENU1
21400     end if 
21500     act$="go "&stopable$
21600     goto ERROR_XIT ! /r
21800 NEVER: ! r:
21900     pr "The Error routine had an error!"
22000     pr "error "&str$(err)&" on line "&str$(line)
22100     pr "Please call ACS support."
22200     pause  ! input fields "1,1,C 1,AEN": pause$
22300     retry  ! /r
22400 ERROR_XIT: ! 
22500     if file(win)<>-1 then close #win: 
22600   fnend 
