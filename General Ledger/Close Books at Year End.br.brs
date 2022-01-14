! formerly S:\acsGL\acGLClos
! Close Books at Year End
! r: setup and read constants
	autoLibrary
	on error goto Ertn
 
	dim acno$*12,bc(13),bp(13),bud(13)
	dim resp$(10)*80
	fnTop(program$)
	open #hCompany=fnH: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,i,r
	read #hCompany,using 'form pos 384,N 2',rec=1: nap
	close #hCompany:
	fnGetFundList(mat fund_list)
! /r
do ! r: the first screen
	fnTos
	lc=rc=frame=0 : mylen=30 : mypos=mylen+2 : width=0
	fnLbl(lc+=1,1,"* * *   Warning   * * *",width,2)
	fnLbl(lc+=1,1,"This program is to be used only at the end of the",width,2)
	fnLbl(lc+=1,1,"year, after all reports have been processed.",width,2)
	fnLbl(lc+=1,1,"Enter CLOSE to continue:",mylen,1)
	fnTxt(lc,mypos,5)
	resp$(rc_erase:=rc+=1)=""
	lc+=1
	fnLbl(lc+=1,1,"Year being closed:",mylen,1)
	fnTxt(lc,mypos,2,0,1,'30',0,"Enter the two digit code for the year you are closing.")
	resp$(rc_year:=rc+=1)=""
 
	if fnUseDeptNo then
 
		! lc+=1 : col3_pos=1 ! mypos+20
		! resp_lrea_fund_1=rc+1
		! col4_pos=mypos ! col3_pos+10
		! fnLbl(lc+=1,col3_pos,'Last Retained Earnings Account(s)')
		! for fund_item=1 to udim(mat fund_list)
		!   fnLbl(lc+=1,col3_pos,"Fund "&str$(fund_list(fund_item))&":",mylen,1)
		!   fnqgl(lc,col4_pos)
		!   rc+=1
		!   fncreg_read("last retained earnings account - fund "&str$(fund_list(fund_item)),resp$(rc)) : resp$(rc)=fnrgl$(resp$(rc))
		! next fund_item
 
			lc+=1
			mylen=30 : mypos=mylen+3 : width=0
			fnFra(lc+=1,1,2,70,"Method of Closing","You must indicate if you will be closing to one equity account or to multiple accounts.",0)
			frame+=1
			fnOpt(1,3,"Close each fund to a separate account",0,frame)
			fncreg_read('Close each fund to a separate account',resp$(rc_close1:=rc+=1),'False')
			fnOpt(2,3,"Close all departments to one retained earnings (equity) account",0,frame)
			fncreg_read('Close all departments to one retained earnings (equity) account',resp$(rc_close2:=rc+=1),'False')
 
	! else
	!   col4_pos=col3_pos+32
	!   fnLbl(lc+=1,col3_pos,'Last Retained Earnings Account:',31,1)
	!   fnqgl(lc,col4_pos)
	!   rc+=1
	!   fncreg_read("last retained earnings account - no fund ",resp$(rc)) : resp$(rc)=fnrgl$(resp$(rc))
	end if
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	pas$=resp$(rc_erase)
	yr$=cnvrt$("pic(##)",val(resp$(rc_year)))
	if fnUseDeptNo then
		!   for fund_item=1 to udim(mat fund_list)
		!     last_retained_earnings_acct$(fund_item)=fnagl$(resp$(rc+=1))
		!     fncreg_write("last retained earnings account - fund "&str$(fund_list(fund_item)),last_retained_earnings_acct$(fund_item))
		!   next fund_item
		fncreg_write('Close each fund to a separate account',resp$(rc_close1))
		fncreg_write('Close all departments to one retained earnings (equity) account',resp$(rc_close2))
		if resp$(rc_close1)='True' then
			closeDeptToRetainedEarnings=0
		else
			closeDeptToRetainedEarnings=1
		end if
	else
		last_retained_earnings_acct$(1)=fnagl$(resp$(rc+=1))
		fncreg_write("last retained earnings account - no fund ",last_retained_earnings_acct$(1))
	end if
loop until lwrc$(pas$)=lwrc$("CLOSE") ! /r

fnAutomatedSavePoint('before')

open #hGlMstr1=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLINDEX.h[cno],Shr",internal,outIn,keyed
open #hGlMstr2=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\glIndx2.h[cno],Shr",internal,outIn,keyed
fGlMstr1: form pos 1,c 12,pos 81,41*pd 6.2
open #hBudgetInfo=fnH: "Name=[Q]\GLmstr\BudgetInfo.h[cno],KFName=[Q]\GLmstr\BudIndx.h[cno],Shr",internal,outIn,keyed
! r: empty GLmstr\acprcks - file handle (#1) used to conflict hGlMstr1 (also #1) and it didn't close, but it did ioerr ignore, so it probably didn't do anything for years
open #hAcPrCks=fnH: "Name=[Q]\GLmstr\acprcks.h[cno],SIZE=0,RecL=110,Replace",internal,output ioerr ignore
close #hAcPrCks: ioerr ignore
! /r
! r: reset some stuff in "[Q]\GLmstr\PRmstr.h[cno]"
open #hPrMstr=fnH: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno]",internal,outIn,keyed ioerr SCR2
do
	read #hPrMstr,using 'form pos 271,2*N 5': n1,n2 eof L500
	rewrite #hPrMstr,using 'form pos 271,2*N 5': 0,0
loop
L500: !
close #hPrMstr:
! /r
SCR2: !
	t5=0
	fnTos
	lc=0 : mylen=30 : mypos=mylen+2 : width=80
	fnLbl(lc+=1,1,"Enter the Last Retained Earnings Account or Equity Account.",width,2)
	fnLbl(lc+=1,1,"The account that dividend, income, and expenses will be closed to.",width,2)
	lc+=1
	if fnUseDeptNo=0 or closeDeptToRetainedEarnings=1 then
		fnLbl(lc+=1,1,"All accounts after this ",width,2)
	else
		fnLbl(lc+=1,1,"All Accounts for this Cost Center after this ",width,2)
	end if
	fnLbl(lc+=1,1,"be reset with zero balances.",width,2)
	fnLbl(lc+=1,1,"account will be reset with zero balances.",width,2)
	fnLbl(lc+=1,1,"Enter Account Number:",mylen,1)
	fnqgl(lc,mypos)
	resp$(1)=''
	fnCmdSet(11)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	glnumber$=fnagl$(resp$(1))
	if closeDeptToRetainedEarnings then
		fncreg_write("last retained earnings account - fund "&str$(val(glnumber$(1:3))),resp$(1))
	else
		fncreg_write("last retained earnings account - no fund",resp$(1))
	end if
	read #hGlMstr1,using 'form pos 1,c 3,c 6,c 3',key=glnumber$: dno$,ano$,sno$ nokey SCR2
	acno$=glnumber$(1:3)&"         "

read #hGlMstr1,using fGlMstr1,key>=acno$: acno$,bb,cb,mat bc,mat bp,mat bud nokey SCR2
goto L770
do
	read #hGlMstr1,using fGlMstr1: acno$,bb,cb,mat bc,mat bp, mat bud eof EoAcct
	L770: !
	if fnUseDeptNo=0 or closeDeptToRetainedEarnings=1 then 
		goto L790
	else if glnumber$(1:3)><acno$(1:3) then 
		dno=ano=sno=0
		goto SCR2
	end if
	L790: !
	if acno$=glnumber$ then
		cb=-t5
		! bC(NAP)=CB   ! SET RETAINED BALANCE IN HISTORY AFTER CLOSING
		if nap=0 or nap>13 then nap=12
	end if
	pbp=bp(nap)
	mat bp=bc
	mat bc=(0)
	bb=cb
	t5+=cb
	if acno$>glnumber$ then  ! create a budget history record
		write #hBudgetInfo,using "form pos 1,c 12,c 2,2*pd 6.2": acno$,yr$,cb,sum(bud)
		cb=bb=0
	end if
	rewrite #hGlMstr1,using 'form pos 1,c 12,pos 81,41*pd 6.2,pos 327,pd 6.2': acno$,bb,cb,mat bc,mat bp,mat bud,pbp
loop
EoAcct: !
	if fnUseDeptNo=0 or closeDeptToRetainedEarnings=1 then
		goto FINIS
	end if
	dno=ano=sno=0
goto SCR2

FINIS: ! r:
	close #hGlMstr1:
	close #hGlMstr2:
	close #hBudgetInfo:
goto Xit ! /r
Xit: fnXit
include: ertn
