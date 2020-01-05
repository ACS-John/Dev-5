! Replace S:\acsPR\adjustpaydate.br
! This program corrects a pay date in history even after later payrolls have been processed
! ______________________________________________________________________
	library 'S:\Core\Library': fntop,fnxit,fnerror,fncno,fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs,fngethandle
	fntop("S:\acsPR\adjustpaydate","Adjust Historical Pay Date")
	on error goto Ertn
	fn_adjustpaydate
! ______________________________________________________________________
	def fn_adjustpaydate
! main routine
		if fn_getdates then 
			fncno(cno,cnam$)
			open #(h_prchecks:=fngethandle): "Name=PRmstr\Payrollchecks.h[cno],KFName=PRmstr\checkidx3.h[cno]",internal,outIn,keyed 
CHECKSFORM: form pos 1,n 8,n 3,pd 6
			do 
				read #h_prchecks,using CHECKSFORM: eno,tdn,prd eof CHECKSDONE
				if prd=val(prdate$(1)) then prd=val(prdate$(2))
				rewrite #h_prchecks,using CHECKSFORM: eno,tdn,prd
			loop 
CHECKSDONE: ! 
			close #h_prchecks: 
		end if 
	fnend  ! fn_adjustpaydate
	def fn_getdates
! gets the old and new payroll dates
		dim prdate$(2)*8
		fnTos(sn$="getpradjustdates")
		mylen=42 : mypos=45
		fnLbl(1,1,"Payroll Date to Adjust:",mylen)
		fnTxt(1,mypos,10,0,1,"3",0,"Enter the payroll date you want to change.")
		prdate$(1)=""
		fnLbl(2,1,"New Payroll Date:",mylen)
		fnTxt(2,mypos,10,0,1,"3",0,"To reset checks on the above date to a new payroll date, enter it here.")
		prdate$(2)=""
		fnCmdKey("Next",1,1,0,"Proceed with date adjustment.")
		fnCmdKey("Cancel",5,0,1,"Return to menu without changing the payroll date as indicated.")
		fnAcs(sn$,0,mat prdate$,ckey)
		if ckey=5 then 
			fn_getdates=0
		else 
			if prdate$(1)="00000000" or prdate$(2)="00000000" then let fn_getdates=fn_getdates else let fn_getdates=1
		end if 
	fnend  ! fn_getdates
XIT: fnxit
ERTN: fnerror(program$,err,line,act$,"NO")
	if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
	execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN
