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
	errorNumber=err
	lineNumber=line 
	! if errornumber=5 then goto OfflineHelp
	ScreenError1: !
	Do
		err_win=127
		if file(err_win)<>-1 then close #err_win: ioerr ignore
		if env$('acsDeveloper')<>'' then
			errOptionCount=8
		else
			errOptionCount=6
		end if
		open #err_win: 'SCol=8,SRow=04,Cols=62,Rows=13,Border=S[E],Caption=Collection-Master Add-On Error Handler',display,outin
		dim errFieldPos$ (0)*64
		dim errFieldText$(0)*64
		mat errFieldPos$ (errOptionCount)
		mat errFieldText$(errOptionCount)
		pr #err_win,f '1,1,Cc 60,[E]' : 'Error: '&str$(errorNumber)
		pr #err_win,f '2,1,Cc 60,[E]' : ' Line: '&str$(lineNumber)
		pr #err_win,f '4,1,C  60,[E]' : 'Select a course of action:'
		errFieldPos$(1)=' 5,5,C 30,[E]' : errFieldText$(1)="1. Offline Help"
		errFieldPos$(2)=' 6,5,C 30,[E]' : errFieldText$(2)="2. Retry"
		errFieldPos$(3)=' 7,5,C 30,[E]' : errFieldText$(3)="3. Quit to MAIN MENU"
		errFieldPos$(4)=' 8,5,C 30,[E]' : errFieldText$(4)="4. Exit"
		errFieldPos$(5)=' 9,5,C 30,[E]' : errFieldText$(5)="5. Restart this Program"
		errFieldPos$(6)='12,5,C 30,[E]' : errFieldText$(8)="6. BRWiki Lookup (online help)"
		if env$('acsDeveloper')<>'' then
			errFieldPos$(7)='10,5,C 30,[E]' : errFieldText$(7)="7. Proc Reload"
			errFieldPos$(8)='11,5,C 30,[E]' : errFieldText$(8)="8. Reload and Run"
		end if
		rinput #err_win,select mat errFieldPos$,attr '[L]': mat errFieldText$
		errSelection=curfld
		if cmdkey=20 then ! Hidden Continue Key
			close #err_win:
			on error goto Ertn
			continue
		else if cmdkey=12 or cmdkey=19 then ! Fix Program
			close #err_win:
			execute "List -"&str$(lineNumber)
			pause
			goto ScreenError1
		else if cmdkey=18 then ! Hidden Retry But No Help Window
			close #err_win:
			retry
		end if
		close #err_win:
		if errSelection=1 then
			dim hhelp_$*40
			hhelp_$=("ERR"&cnvrt$("PIC(####)",errornumber)&",WBCMD.WBH")
			if errornumber=5 then hhelp_$="HELP,WBCMD.WBH"
			err_dummy$=help$(hhelp_$)
		else if errSelection=2 then
			on error goto Ertn
			retry
		else if errSelection=3 then
			library 'S:\Core\Library.br': fnXit
			fnXit
		else if errSelection=4 then
			execute "System"
		else if errSelection=5 then
			on error goto Ertn
			chain "Proc=ReRun//8"
		else if errSelection=6 then
			execute "Sy -c -m Start /MIN http://brwiki2.brulescorp.com/index.php?search="&cnvrt$('pic(####)',errornumber)
			goto ScreenError1
		else if errSelection=7 then
			on error goto Ertn
			chain "Proc=Reload"
		else if errSelection=8 then ! reload and run
			on error goto Ertn
			if file(err_win)<>-1 then close #err_win: ioerr ignore
			open #err_win: 'name=rerun,replace',d,o
			pr #err_win: 'subproc Reload'
			pr #err_win: 'run'
			close #err_win:
			execute 'proc rerun'
		end if
	loop
! /r Updateable Region . Error Handler . End