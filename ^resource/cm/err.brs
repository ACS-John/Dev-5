! r: doNotInclude
on error goto Ertn
pr 1/0
Xit: end
! /r doNotInclude
! r: Updateable Region . Error Handler . Top
	! This region was last updated on 2019.07.22
Ertn: !
	on error system
	dim err_dummy$*80
	print bell;
	e1=err : l1=line : if err=5 then goto ErrorHelpBr
	ScreenError1: !
	err_win=127
	if file(err_win)<>-1 then close #err_win: ioerr ignore
	if env$('acsDeveloper')<>'' then
		errOptionCount=7
	else
		errOptionCount=5
	end if
	open #err_win: 'SCol=8,SRow=10,Cols=62,Rows=11,Border=S[E],Caption=Collection-Master Add-On Error Trap',display,outin
	dim errFieldPos$ (0)*64
	dim errFieldText$(0)*23
	mat errFieldPos$ (errOptionCount)
	mat errFieldText$(errOptionCount)
	pr #err_win,f '1,1,Cc 60,[E]' : 'Error: '&str$(e1)
	pr #err_win,f '2,1,Cc 60,[E]' : ' Line: '&str$(l1)
	pr #err_win,f '4,1,C  60,[E]' : 'Select a course of action:'
	errFieldPos$(1)='5,5,C 23,[E]' : errFieldText$(1)="1. Online Help"
	errFieldPos$(2)='6,5,C 23,[E]' : errFieldText$(2)="2. Retry"
	errFieldPos$(3)='7,5,C 23,[E]' : errFieldText$(3)="3. Quit to MAIN MENU"
	errFieldPos$(4)='8,5,C 23,[E]' : errFieldText$(4)="4. Exit"
	errFieldPos$(5)='9,5,C 23,[E]' : errFieldText$(5)="5. Restart this Program"
	if env$('acsDeveloper')<>'' then
		errFieldPos$(6)='10,5,C 23,[E]' : errFieldText$(6)="6. Proc Reload"
		errFieldPos$(7)='11,5,C 23,[E]' : errFieldText$(7)="7. Reload and Run"
	end if
	! print #err_win,fields "1,1,C 61,[E]": "1. Go into on-line help"
	! print #err_win,fields "2,1,C 61,[E]": "2. Retry last command"
	! print #err_win,fields "3,1,C 61,[E]": "3. Return to MAIN MENU"
	! print #err_win,fields "4,1,C 61,[E]": "4. Return to SYSTEM MENU"
	! print #err_win,fields "5,1,C 61,[E]": "5. Restart this Program"
	! input #err_win,select "1,1,C 61,[F]AE;2,1,C 61,[F]AE;3,1,C 61,[F]AE;4,1,C 61,[F]AE;5,1,C 61,[F]AE",attr '[L]': err_dummy$,err_dummy$,err_dummy$,err_dummy$,err_dummy$ : errSelection=curfld
	rinput #err_win,select mat errFieldPos$,attr '[L]': mat errFieldText$
	errSelection=curfld
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
	if errSelection=1 then
		goto ErrorHelpBr
	else if errSelection=2 then
		on error goto Ertn
		retry
	else if errSelection=3 then
		library 'S:\Core\Library.br': fnXit
		fnXit
		! goto XIT
	else if errSelection=4 then
		execute "System"
	else if errSelection=5 then
		on error goto Ertn
		chain "Proc=ReRun//8"
	else if errSelection=6 then
		on error goto Ertn
		chain "Proc=Reload"
	else if errSelection=7 then ! reload and run
		on error goto Ertn
		if file(err_win)<>-1 then close #err_win: ioerr ignore
		open #err_win: 'name=rerun,replace',d,o
		pr #err_win: 'subproc Reload'
		pr #err_win: 'run'
		close #err_win:
		execute 'proc rerun'
	end if
	ErrorHelpBr: ! r:
		hhelp_$=("ERR"&cnvrt$("PIC(####)",err)&",WBCMD.WBH")
		if err=5 then hhelp_$="HELP,WBCMD.WBH"
		err_dummy$=help$(hhelp_$)
	goto ScreenError1 ! /r
	pause
! /r Updateable Region . Error Handler . End