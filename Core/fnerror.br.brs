library 'S:\Core\Library': fnxit
on error goto Ertn
pr 1/0
XIT: fnxit
! 
def library fnError(callingprogram$*256, errornumber, linenumber, &ertnAct$, stopable$)
	library 'S:\Core\Library': fnmsgbox,fnlog,fngethandle
	on error goto NEVER
	xcnt=cnt

	! ! r: ertnAct$ Size Test  ( sets enableBigErtnAct=0 or 1 )
	! enableBigErtnAct=0 ! ertnAct$ is dimmed less than 256
	! dim ertnActHold$*256
	! pause
	! ertnActHold$=ertnAct$
	! ertnAct$=rpt$(' ',256) soflow ErtnActSizeTestFinis
	! enableBigErtnAct=1 ! ertnAct$ is dimmed at least 256
	! ErtnActSizeTestFinis: !
	! ertnAct$=ertnActHold$
	! ! /r

	dim response$(5)*80,msgline$(2)*60,log$*128,acshelp$*200
	dim caption$*42
	! ertnAct$  =  the returning action to be executed
	! stopable$ =  if it is "no" or "NO" etc it'll not allow them to stop
	!              otherwise it should be the paragraph heading of where to goto on
	!              a selected exit (must be 15 characters or less)
	
	!   pr border: "Error "&str$(errornumber)&" on line "&str$(linenumber)
	pr fields "1,73,C 7,N": bell$
	log$="Error "&str$(errornumber)&" at line "&str$(linenumber)
	log$=log$&" of "&callingprogram$(1:100)
	fnlog(log$,2)
	ertnAct$="Go "&str$(linenumber)
	acshelp$="Help\co\error\br\"
	acshelp$=acshelp$&"Err"&cnvrt$("Pic(####)",errornumber)&".html"
	MENU1: ! 
		open #win:=fngethandle: "SRow=4,SCol=4,Rows=17,Cols=72,Border=S:[screen],N=[screen],Caption=<Error",display,outin  ! ,border=Sr   ...
		if errornumber=61 then 
			pr #win,f "1,1,Cc 69,N": "A record is needed but is locked by another user."
			pr #win,f "2,1,Cc 69,N": "Please ask other users to return to the Menu and"
			pr #win,f "3,1,Cc 69,N": "then you may select Retry."
		else if errornumber=4148 then 
			pr #win,f "1,1,Cc 69,N": "A file lock is needed but the file is already locked by someone else."
			pr #win,f "2,1,Cc 69,N": "Please ask other users to return to the Menu and"
			pr #win,f "3,1,Cc 69,N": "then you may select Retry."
		else if errornumber=632 then 
			pr #win,f "1,1,Cc 69,N": "Problem with Index File"
		end if 

		lc=7
		pr #win,f str$(lc)&",2,Cr 13,N": "Program:"
		pr #win,f str$(lc)&",16,C 53,P[textboxes]": env$('Core_Program_Current')(1:53)
		pr #win,f str$(lc+=1)&",2,Cr 13,N": "File:"
		pr #win,f str$(lc)&",16,C 53,P[textboxes]": callingprogram$(1:53)
		lc+=1
		pr #win,f str$(lc+=1)&",2,Cr 13,[screen]": "Error Number:"
		pr #win,f str$(lc)&",16,C 5,P[textboxes]": str$(errornumber) ! ,r,n
		lc+=1
		pr #win,f str$(lc+=1)&",2,Cr 13,[screen]": "Line Number:"
		pr #win,f str$(lc)&",16,C 5,P[textboxes]": str$(linenumber) ! ,r,n
		pr #win,f str$(lc+=1)&",2,Cr 13,N": "Count+1:"
		pr #win,f str$(lc)&",16,C 5,P[textboxes]": str$(xcnt+1) ! ,r,n
		pr #win,f str$(lc+=1)&",2,Cr 13,N": "Session:"
		pr #win,f str$(lc)&",16,C 5,P[textboxes]": session$ ! ,r,n
		button_pos$='47'
		pr #win,f "9,"&button_pos$&",Cc 22,,B01": "Retry (Enter)"
		pr #win,f "10,"&button_pos$&",Cc 22,,B99": "Exit (Esc)"
	!   pr #win,f "6,"&button_pos$&",C 22,B,10": "WB Help (F10)"
		pr #win,f "11,"&button_pos$&",Cc 22,,B08": "BRWiki Help (F8)"
	!   if exists(acshelp$)<>0 then
	!     pr #win,f "12,"&button_pos$&",Cc 22,,B09": "ACS Help (F9)"
	!   end if
		pr #win,f "14,"&button_pos$&",Cc 22,,B12": "Developer Pause (F12)" ! 1,19,12/CC 12,,B1000
		
		
		if env$('acsDeveloper')<>'' then ! enableBigErtnAct and
			pr #win,f "16,20,Cc 35,,B21": 'Recompile, Reload and Run (Ctrl+F1)' ! 1,19,12/CC 12,,B1000
		end if
		
		
	ERR_INP: ! 
		in #0,f "4,4,C 1,AE,N": pause$
		if cmdkey=0 then 
			log$="action taken = Retry" : fnlog(log$,2)
			ertnAct$="Go "&str$(linenumber)
			goto ERROR_XIT
		else if cmdkey=5 or cmdkey=99 then 
			log$="action taken = Quit" : fnlog(log$,2)
			goto ERR_QUITER
		else if cmdkey=8 then 
			log$="action taken = BRWiki Help" : fnlog(log$,2)
			gosub BRWIKIHELP
	!   else if cmdkey=9 then
	!     log$="action taken = ACS Help" : fnlog(log$,2)
	!     gosub ACSHELP
	!   else if cmdkey=10 then
	!     log$="action taken = WB Help" : fnlog(log$,2)
	!     gosub ERR_WBHELP
		else if cmdkey=12 then 
			ertnAct$="PAUSE"
			log$="action taken = Program Pause" : fnlog(log$,2)
			goto ERROR_XIT
	
		else if cmdkey=21 then 
			! if ~enableBigErtnAct then 
			! 	pr 'NO enableBigErtnAct.   this might not work. G to try.'
			! 	pause
			! end if
			ertnAct$='Proc r3.prc'
			! ertnAct$='Proc "Recompile Reload and Run.prc"'
			library 'S:\Core\Library': fnWriteProc
			log$='action taken = '&ertnAct$ : fnlog(log$,2)
			
			
			if ~exists('in') then
				fnWriteProc('in','end')
				fnWriteProc(''  ,"setenv('source',program$&program$(pos(program$,'.',-1):inf)&'s')")
				fnWriteProc(''  ,"setenv('source',os_filename$(env$('source')))")
				fnWriteProc(''  ,"exec 'sy """"C:\ACS\Util\Sad Panda\Compile.cmd"" ""'&env$('source')&'""""'")
				fnWriteProc(''  ,'execute ''load "''&program$&''"''')
			end if
			! fnWriteProc('Recompile Reload and Run.prc','subproc in')
			fnWriteProc('r3.prc','subproc in')
			fnWriteProc(''      ,'run')
			
			
			goto ERROR_XIT
	
		end if 
	goto ERR_INP
	
	! ERR_WBHELP: ! r:
	!     on error goto NO_ERR_WBHELP
	!     help$("ERR"&cnvrt$("PIC(####)",errornumber)&",WBCmd")
	!     goto ERR_WBHELP_RETURN ! /r
	! NO_ERR_WBHELP: ! r:
	!     mat msgline$(2)
	!     msgline$(1)="Sorry, No Workstation Basic Help "
	!     msgline$(2)="is available for this Error Number."
	!     fnmsgbox(mat msgline$,response$(1),env$('program_caption'),0)
	!     response$(1)=response$(1)(1:1)
	! ERR_WBHELP_RETURN: !
	!     return  ! /r
	BRWIKIHELP: ! r:
		execute "Sy -c -m Start /MIN http://brwiki2.brulescorp.com/index.php?search="&cnvrt$('pic(####)',errornumber)
	return  ! /r
! ACSHELP: ! r:
!     if exists(acshelp$)<>0 then
!       execute "Sy -c -m "&acshelp$
!     end if
!     return  ! /r
	ERR_QUITER: ! r:
		if uprc$(rtrm$(stopable$))="NO" then 
			setenv('ExitNow','no')
			mat msgline$(2)
			msgline$(1)="Do not stop the processing of this program."
			msgline$(2)="Please contact ACS Technical Support."
			fnmsgbox(mat msgline$,response$(1),env$('program_caption'),0)
			response$(1)=response$(1)(1:1)
			goto MENU1
		end if 
		ertnAct$="go "&stopable$
		goto ERROR_XIT ! /r
	NEVER: ! r:
		pr "The Error routine had an error!"
		pr "error "&str$(err)&" on line "&str$(line)
		pr "Please call ACS support."
		pause  ! input fields "1,1,C 1,AEN": pause$
		retry  ! /r
	ERROR_XIT: ! 
		if file(win)<>-1 then close #win: 
fnend 
include: Ertn
