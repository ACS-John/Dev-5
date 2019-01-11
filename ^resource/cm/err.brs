! r: Updateable Region . Error Handler . Top 
	! This region was last updated on 2019.01.10
Error_Hanler: ! 
	on error system 
	dim err_dummy$*80
	print bell;
	let e1=err: let l1=line : if err=5 then goto ErrorHelpBr
	ScreenError1: ! 
	open #err_win=127: "SCol=10,SRow=10,ECol=70,ERow=14,Border=S[E],Caption=Error:"&str$(e1)&" on Line:"&str$(l1),display,outin 
	print #err_win,fields "1,1,C 61,[E];2,1,C 61,[E];3,1,C 61,[E];4,1,C 61,[E];5,1,C 61,[E]": "1. Go into on-line help","2. Retry last command","3. Return to MAIN MENU","4. Return to SYSTEM MENU","5. Restart this Program"
	input #err_win,select "1,1,C 61,[F]AE;2,1,C 61,[F]AE;3,1,C 61,[F]AE;4,1,C 61,[F]AE;5,1,C 61,[F]AE",attr '[L]': err_dummy$,err_dummy$,err_dummy$,err_dummy$,err_dummy$ : let hchoice=curfld
	if cmdkey=20 then ! Hidden Continue Key 
		close #err_win: 
		on error goto Error_Hanler 
		continue 
	else if cmdkey=19 then ! Fix Program 
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
	ErrorHelpBr: let hhelp_$=("ERR"&cnvrt$("PIC(####)",err)&",WBCMD.WBH") : if err=5 then let hhelp_$="HELP,WBCMD.WBH"
	let err_dummy$=help$(hhelp_$)
	goto ScreenError1
	pause 
	ErrorRetry: on error goto Error_Hanler
	retry 
	ErrorXitToMenu: goto XIT
	ErrorXitSys: execute "System"
	ErrorRerun: on error goto Error_Hanler
	chain "Proc=ReRun//8"
! Updateable Region . Error Handler . End