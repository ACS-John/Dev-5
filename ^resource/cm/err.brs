! r: Updateable Region . Error Handler . Top 
	! This region was last updated on 2019.01.10
Ertn: ! 
	on error system 
	dim err_dummy$*80
	print bell;
	e1=err : l1=line : if err=5 then goto ErrorHelpBr
	ScreenError1: ! 
	err_win=127
	if file(err_win)<>-1 then close #err_win: ioerr ignore
	open #err_win: 'SCol=8,SRow=10,Cols=62,Rows=10,Border=S[E],Caption=Collection-Master Add-On Error Trap',display,outin 
	dim errFieldPos$ (5)*64
	dim errFieldText$(5)*23
	pr #err_win,f '1,1,Cc 60,[E]' : 'Error: '&str$(e1)
	pr #err_win,f '2,1,Cc 60,[E]' : ' Line: '&str$(l1)
	pr #err_win,f '4,1,C  60,[E]' : 'Select a course of action:'
	errFieldPos$(1)='5,5,C 23,[E]' : errFieldText$(1)="1. Online Help"
	errFieldPos$(2)='6,5,C 23,[E]' : errFieldText$(2)="2. Retry"
	errFieldPos$(3)='7,5,C 23,[E]' : errFieldText$(3)="3. Quit to MAIN MENU"
	errFieldPos$(4)='8,5,C 23,[E]' : errFieldText$(4)="4. Exit"
	errFieldPos$(5)='9,5,C 23,[E]' : errFieldText$(5)="5. Restart this Program"
	! print #err_win,fields "1,1,C 61,[E]": "1. Go into on-line help"
	! print #err_win,fields "2,1,C 61,[E]": "2. Retry last command"
	! print #err_win,fields "3,1,C 61,[E]": "3. Return to MAIN MENU"
	! print #err_win,fields "4,1,C 61,[E]": "4. Return to SYSTEM MENU"
	! print #err_win,fields "5,1,C 61,[E]": "5. Restart this Program"
	! input #err_win,select "1,1,C 61,[F]AE;2,1,C 61,[F]AE;3,1,C 61,[F]AE;4,1,C 61,[F]AE;5,1,C 61,[F]AE",attr '[L]': err_dummy$,err_dummy$,err_dummy$,err_dummy$,err_dummy$ : hchoice=curfld
	rinput #err_win,select mat errFieldPos$,attr '[L]': mat errFieldText$
	hchoice=curfld
	if cmdkey=20 then ! Hidden Continue Key 
		close #err_win: 
		on error goto Ertn 
		continue 
	else if cmdkey=12 or cmdkey=19 then ! Fix Program 
		close #err_win: 
		execute "List -"&str$(line) 
		pause 
		goto ScreenError1
	else if cmdkey=18 then ! Hidden Retry But No Help Window 
		close #err_win:
		retry 
	end if
	close #err_win: 
	on hchoice goto ErrorHelpBr, ErrorRetry, ErrorXitToMenu, ErrorXitSys, ErrorRerun
	ErrorHelpBr: !
	hhelp_$=("ERR"&cnvrt$("PIC(####)",err)&",WBCMD.WBH") 
	if err=5 then hhelp_$="HELP,WBCMD.WBH"
	err_dummy$=help$(hhelp_$)
	goto ScreenError1
	pause 
	ErrorRetry: on error goto Ertn
	retry 
	ErrorXitToMenu: goto XIT
	ErrorXitSys: execute "System"
	ErrorRerun: on error goto Ertn
	chain "Proc=ReRun//8"
! Updateable Region . Error Handler . End