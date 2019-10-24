library 'S:\Core\Library': fnxit
on error goto ERTN
print 1/0
XIT: let fnxit
! 
def library fnError(callingprogram$*256, errornumber, linenumber, &act$, stopable$)
	library 'S:\Core\Library': fnmsgbox,fnlog,fngethandle
	on error goto NEVER
	let xcnt=cnt

	dim response$(5)*80,msgline$(2)*60,log$*128,acshelp$*200
	dim caption$*42
	! ______________________________________________________________________
	! act$  =  the returning action to be executed
	! stopable$  =  if it is "no" or "NO" etc it'll not them stop
	! otherwise it should be the paragraph heading of where to goto on
	! a selected exit (must be 15 characters or less)
	! ______________________________________________________________________
	!   pr border: "Error "&str$(errornumber)&" on line "&str$(linenumber)
	print fields "1,73,C 7,N": bell$
	let log$="Error "&str$(errornumber)&" at line "&str$(linenumber)
	let log$=log$&" of "&callingprogram$(1:100)
	let fnlog(log$,2)
	let act$="Go "&str$(linenumber)
	let acshelp$="Help\co\error\br\"
	let acshelp$=acshelp$&"Err"&cnvrt$("Pic(####)",errornumber)&".html"
	MENU1: ! 
		open #win:=fngethandle: "SRow=4,SCol=4,Rows=17,Cols=72,Border=S:[screen],N=[screen],Caption=<Error",display,outin  ! ,border=Sr   ...
		if errornumber=61 then 
			print #win,fields "1,1,Cc 69,N": "A record is needed but is locked by another user."
			print #win,fields "2,1,Cc 69,N": "Please ask other users to return to the Menu and"
			print #win,fields "3,1,Cc 69,N": "then you may select Retry."
		else if errornumber=4148 then 
			print #win,fields "1,1,Cc 69,N": "A file lock is needed but the file is already locked by someone else."
			print #win,fields "2,1,Cc 69,N": "Please ask other users to return to the Menu and"
			print #win,fields "3,1,Cc 69,N": "then you may select Retry."
		else if errornumber=632 then 
			print #win,fields "1,1,Cc 69,N": "Problem with Index File"
		end if 

		let lc=7
		print #win,fields str$(lc)&",2,Cr 13,N": "Program:"
		print #win,fields str$(lc)&",16,C 53,P[textboxes]": env$('Core_Program_Current')(1:53)
		print #win,fields str$(lc+=1)&",2,Cr 13,N": "File:"
		print #win,fields str$(lc)&",16,C 53,P[textboxes]": callingprogram$(1:53)
		let lc+=1
		print #win,fields str$(lc+=1)&",2,Cr 13,[screen]": "Error Number:"
		print #win,fields str$(lc)&",16,C 5,P[textboxes]": str$(errornumber) ! ,r,n
		let lc+=1
		print #win,fields str$(lc+=1)&",2,Cr 13,[screen]": "Line Number:"
		print #win,fields str$(lc)&",16,C 5,P[textboxes]": str$(linenumber) ! ,r,n
		print #win,fields str$(lc+=1)&",2,Cr 13,N": "Count+1:"
		print #win,fields str$(lc)&",16,C 5,P[textboxes]": str$(xcnt+1) ! ,r,n
		print #win,fields str$(lc+=1)&",2,Cr 13,N": "Session:"
		print #win,fields str$(lc)&",16,C 5,P[textboxes]": session$ ! ,r,n
		let button_pos$='47'
		print #win,fields "9,"&button_pos$&",Cc 22,,B01": "Retry (Enter)"
		print #win,fields "10,"&button_pos$&",Cc 22,,B99": "Exit (Esc)"
	!   pr #win,fields "6,"&button_pos$&",C 22,B,10": "WB Help (F10)"
		print #win,fields "11,"&button_pos$&",Cc 22,,B08": "BRWiki Help (F8)"
	!   if exists(acshelp$)<>0 then
	!     pr #win,fields "12,"&button_pos$&",Cc 22,,B09": "ACS Help (F9)"
	!   end if
		print #win,fields "14,"&button_pos$&",Cc 22,,B12": "Developer Pause (F12)" ! 1,19,12/CC 12,,B1000
	ERR_INP: ! 
		input #0,fields "4,4,C 1,AE,N": pause$
		if cmdkey=0 then 
			let log$="action taken = Retry" : let fnlog(log$,2)
			let act$="Go "&str$(linenumber)
			goto ERROR_XIT
		else if cmdkey=5 or cmdkey=99 then 
			let log$="action taken = Quit" : let fnlog(log$,2)
			goto ERR_QUITER
		else if cmdkey=8 then 
			let log$="action taken = BRWiki Help" : let fnlog(log$,2)
			gosub BRWIKIHELP
	!   else if cmdkey=9 then
	!     log$="action taken = ACS Help" : fnlog(log$,2)
	!     gosub ACSHELP
	!   else if cmdkey=10 then
	!     log$="action taken = WB Help" : fnlog(log$,2)
	!     gosub ERR_WBHELP
		else if cmdkey=12 then 
			let act$="PAUSE"
			let log$="action taken = Program Pause" : let fnlog(log$,2)
			goto ERROR_XIT
		end if 
	goto ERR_INP
	! ______________________________________________________________________
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
			let setenv('ExitNow','no')
			mat msgline$(2)
			let msgline$(1)="Do not stop the processing of this program."
			let msgline$(2)="Please contact ACS Technical Support."
			let fnmsgbox(mat msgline$,response$(1),env$('program_caption'),0)
			let response$(1)=response$(1)(1:1)
			goto MENU1
		end if 
		let act$="go "&stopable$
		goto ERROR_XIT ! /r
	NEVER: ! r:
		print "The Error routine had an error!"
		print "error "&str$(err)&" on line "&str$(line)
		print "Please call ACS support."
		pause  ! input fields "1,1,C 1,AEN": pause$
		retry  ! /r
	ERROR_XIT: ! 
		if file(win)<>-1 then close #win: 
fnend 
include: Ertn
