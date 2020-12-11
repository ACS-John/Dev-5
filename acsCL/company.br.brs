! Replace S:\acsCL\Company
! maintain company information file for checkbook management
!
	autoLibrary
	on error goto Ertn
!
	dim a$(3)*40
	dim b$(2)*12
	dim c$*5
	dim d(2)
	dim e$(2)*12
	dim lastact$*12
	dim tb$*30
	dim prgl(5,3)
	dim d$(2)*1
	dim miscname$(10)*20
	dim dedcode(10)
	dim dedfed(10)
	dim dedfica(10)
	dim dedst(10)
	dim deduc(10)
	dim miscgl$(10)*12
	dim actr$*1
	dim reccode$*1
	dim resp$(150)*40
!
	fnTop(program$)
	right=1 : center=2 : left=0
	cancel=5 : save=1 : pointtwo$='32'
	pointthree$='33'
	open #glmstr=11: "Name=[Q]\CLmstr\GLmstr.h[cno],KFName=[Q]\CLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed ioerr ignore
	open #company=1: "Name=[Q]\CLmstr\Company.h[cno],Shr",internal,outIn,relative ioerr BUILD_COMPANY
	goto READ_COMPANY
!
READ_COMPANY: ! r:
	read #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,N 2,N 1,C 9,C 12,c 12,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,wbc,ar1,mat e$,lastact$,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,prenum,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
	if actr$="0" or actr$="1" then actr=val(actr$)
	if uprc$(actr$)="Y" then actr=1 else if uprc$(actr$)="N" then actr=0
	gosub NEWSCREEN
	if ckey=save then
		gosub SAVE
		goto Xit
	else if ckey=cancel then
		goto Xit
	else if ckey=ck_page1 then
		page=1 : gosub NEWSCREEN
	else if ckey=ck_page2 then
		page=2 : gosub NEWSCREEN
	else if ckey=ck_page3 then
		page=3 : gosub NEWSCREEN
	else if ckey=ck_page4 then
		page=4 : gosub NEWSCREEN
	else if ckey=save then
		gosub SAVE : goto Xit
	else if ckey=cancel then
		goto Xit
	else
		pr 'unhandled ckey value' : pause
	end if
! /r
!
NEWSCREEN: ! r:
	fnTos
	lc=0
	fnButton(1,01,'&Basic            ',ck_page1:=6,'',height=02,23)
	fnButton(1,26,'&General Ledger   ',ck_page2:=7,'',height=2,23)
	fnButton(1,51,'&Rates and Maxs   ',ck_page3:=8,'',height=2,23)
	fnButton(1,76,'&Deductions       ',ck_page4:=9,'',height=2,23)
	if page=1 then
		gosub PAGE1
	else if page=2 then
		gosub PAGE2
	else if page=3 then
		gosub PAGE3
	else if page=4 then
		gosub PAGE4
	else
		page=1
		gosub PAGE1
	end if
	fnCmdSet(4) ! Save and Cancel
	fnAcs(mat resp$,ckey)
	if page=1 then
		a$(1)=resp$(1)
		a$(2)=resp$(2)
		a$(3)=resp$(3)
		b$(1)=resp$(4)
		b$(2)=resp$(5)
		tb$=resp$(6)
		nap=val(resp$(7))
		wbc=val(resp$(8)(1:2))
		if resp$(9)='True' then prenum=1 else prenum=2
		if resp$(10)='True' then reccode=1 else reccode=0
	end if
	if page=2 then
		if resp$(1)='True' then d(1)=1 else d(1)=0
		if resp$(2)='True' then d(2)=1 else d(2)=0
		lastact$=fnagl$(resp$(3))
		if resp$(4)='True' then ar1=1 else ar1=0
		resp$(5)=fnagl$(resp$(5)) : prgl(1,1)=val(resp$(5)(1:3)) : prgl(1,2)=val(resp$(5)(4:9)) : prgl(1,3)=val(resp$(5)(10:12))
		resp$(6)=fnagl$(resp$(6)) : prgl(2,1)=val(resp$(6)(1:3)) : prgl(2,2)=val(resp$(6)(4:9)) : prgl(2,3)=val(resp$(6)(10:12))
		resp$(7)=fnagl$(resp$(7)) : prgl(3,1)=val(resp$(7)(1:3)) : prgl(3,2)=val(resp$(7)(4:9)) : prgl(3,3)=val(resp$(7)(10:12))
		resp$(8)=fnagl$(resp$(8)) : prgl(4,1)=val(resp$(8)(1:3)) : prgl(4,2)=val(resp$(8)(4:9)) : prgl(4,3)=val(resp$(8)(10:12))
		resp$(9)=fnagl$(resp$(9)) : prgl(5,1)=val(resp$(9)(1:3)) : prgl(5,2)=val(resp$(9)(4:9)) : prgl(5,3)=val(resp$(9)(10:12))
	end if
	if page=3 then
		c$=resp$(1)
		udm=val(resp$(2))
		ficarate=val(resp$(3))
		ficawage=val(resp$(4))
		feducrat=val(resp$(5))
		feducwag=val(resp$(6))
		mcr=val(resp$(7))
		mcm=val(resp$(8))
	else if page=4 then
		rc=0
		for j=1 to 10
			miscname$(j)=resp$(rc+=1)
			rc+=1 : if resp$(rc)=item$(1) then dedcode(j)=1 else dedcode(j)=2
			if resp$(rc+=1)='True' then dedfed(j)=1 else dedfed(j)=0
			if resp$(rc+=1)='True' then dedfica(j)=1 else dedfica(j)=0
			if resp$(rc+=1)='True' then dedst(j)=1 else dedst(j)=0
			if resp$(rc+=1)='True' then deduc(j)=1 else deduc(j)=0
			rc+=1: resp$(rc)=fnagl$(resp$(rc))
			miscgl$(j)=resp$(rc)(1:12)
		next j
	end if
!
	if ckey=ck_page1 then
		page=1
	else if ckey=ck_page2 then
		page=2
	else if ckey=ck_page3 then
		page=3
	else if ckey=ck_page4 then
		page=4
	else if ckey=save then
		gosub SAVE
		goto Xit
	else if ckey=cancel then
		goto Xit
	end if ! /r
goto NEWSCREEN ! /r
PAGE1: ! r:
	lc=3 : mylen=40 : mypos=mylen+2
	fnLbl(lc+=1,1,'Company Name:',mylen,right)
	fnTxt(lc,mypos,40,0,left)
	resp$(1)=a$(1)
	fnLbl(lc+=1,1,'Address:',mylen,right)
	fnTxt(lc,mypos,40,0,left)
	resp$(2)=a$(2)
	fnLbl(lc+=1,1,'City State and Zip Code:',mylen,right)
	fnTxt(lc,mypos,40,0,left)
	resp$(3)=a$(3)
	fnLbl(lc+=1,1,'Federal Identification Number:',mylen,right)
	fnTxt(lc,mypos,12,0,left)
	resp$(4)=b$(1)
	fnLbl(lc+=1,1,'State Identification Number:',mylen,right)
	fnTxt(lc,mypos,12,0,left)
	resp$(5)=b$(2)
	fnLbl(lc+=1,1,'Type of Business:',mylen,right)
	fnTxt(lc,mypos,30,0,left)
	resp$(6)=tb$
	fnLbl(lc+=1,1,'Number of Periods:',mylen,right)
	fnTxt(lc,mypos,30,0,left,number$)
	resp$(7)=str$(nap)
	fnLbl(lc+=1,1,'Working Bank:',mylen,right)
	fncombof('bank',lc,mypos,0,"[Q]\CLmstr\BankMstr.h[cno]",1,2,3,30,"[Q]\CLmstr\BankIdx1.h[cno]",1)
	resp$(8)=str$(wbc)
	fnChk(lc+=1,mypos,'My Checks are Pre-Numbered',right)
	if prenum=1 then resp$(9)='True' else resp$(9)='False'
	fnChk(lc+=1,mypos,'Utilize Bank Reconciliation Features',right)
	if reccode=1 then resp$(10)='True' else resp$(10)='False'
return ! /r
PAGE2: ! r:
	lc=3 : mylen=40 : mypos=mylen+2
	fc=0 ! framecount
	fnFra(04,1,5,framewidth=110,'General Ledger')
	frame=fc+=1 : lc=0
	fnChk(lc+=1,mypos,'Utilize Department Number Field',right,frame)
	if d(1)=1 then resp$(1)='True' else resp$(1)='False'
	fnChk(lc+=1,mypos,'Utilize Sub Account Number Field',right,frame)
	if d(2)=1 then resp$(2)='True' else resp$(2)='False'
	fnLbl(lc+=1,1,'Last Balance Sheet Account Number:',mylen,right,0,frame)
	fnqgl(lc,mypos,frame,2)
	resp$(3)=fnrgl$(lastact$)
	fnFra(11,1,2,framewidth,'Accounts Receivable')
	frame=fc+=1 : lc=0
	fnChk(lc+=1,mypos,'Post Deposits from Accounts Receivable',right,frame)
	if ar1=1 then resp$(4)='True' else resp$(4)='False'
	fnFra(15,1,5,framewidth,'Payroll')
	frame=fc+=1 : lc=0 : mylen=32 : mypos=mylen+2
	fnLbl(lc+=1,1,'FICA Withholding GL Account:',mylen,right,0,frame)
	fnqgl(lc,mypos,frame,2)
	resp$(5)=cnvrt$('pic(zz#)',prgl(1,1))&cnvrt$('pic(zzzzz#)',prgl(1,2))&cnvrt$('pic(zz#)',prgl(1,3))
	resp$(5)=fnrgl$(resp$(5))
	fnLbl(lc+=1,1,'Federal Withholding GL Account:',mylen,right,0,frame)
	fnqgl(lc,mypos,frame,2)
	resp$(6)=cnvrt$('pic(zz#)',prgl(2,1))&cnvrt$('pic(zzzzz#)',prgl(2,2))&cnvrt$('pic(zz#)',prgl(2,3))
	resp$(6)=fnrgl$(resp$(6))
	fnLbl(lc+=1,1,'State Withholding GL Account:',mylen,right,0,frame)
	fnqgl(lc,mypos,frame,2)
	resp$(7)=cnvrt$('pic(zz#)',prgl(3,1))&cnvrt$('pic(zzzzz#)',prgl(3,2))&cnvrt$('pic(zz#)',prgl(3,3))
	resp$(7)=fnrgl$(resp$(7))
	fnLbl(lc+=1,1,'Local Withholding GL Account:',mylen,right,0,frame)
	fnqgl(lc,mypos,frame,2)
	resp$(8)=cnvrt$('pic(zz#)',prgl(4,1))&cnvrt$('pic(zzzzz#)',prgl(4,2))&cnvrt$('pic(zz#)',prgl(4,3))
	resp$(8)=fnrgl$(resp$(8))
	fnLbl(lc+=1,1,'Earned Income Credit GL Account:',mylen,right,0,frame)
	fnqgl(lc,mypos,frame,2)
	resp$(9)=cnvrt$('pic(zz#)',prgl(5,1))&cnvrt$('pic(zzzzz#)',prgl(5,2))&cnvrt$('pic(zz#)',prgl(5,3))
	resp$(9)=fnrgl$(resp$(9))
return ! /r
PAGE3: ! r:
	lc=3 : mylen=44 : mypos=mylen+2
	fc=0 ! frame count
	fnFra(04,1,2,framewidth=110,'State Unemployment Compensation')
	frame=fc+=1 : lc=0
	fnLbl(lc+=1,1,'State Unemployment Compensation Rate:',mylen,right,0,frame)
	fnTxt(lc,mypos,5,0,left,'',0,'',frame)
	resp$(1)=c$
	fnLbl(lc+=1,1,'State Unemployment Compensation Maximum:',mylen,right,0,frame)
	fnTxt(lc,mypos,13,0,left,pointtwo$,0,'',frame)
	resp$(2)=str$(ucm)
	fnFra(08,1,2,framewidth=110,'Social Security')
	frame=fc+=1 : lc=0
	fnLbl(lc+=1,1,'Social Security Rate:',mylen,right,0,frame)
	fnTxt(lc,mypos,13,0,left,pointthree$,0,'',frame)
	resp$(3)=str$(ficarate)
	fnLbl(lc+=1,1,'Social Security Maximum:',mylen,right,0,frame)
	fnTxt(lc,mypos,13,0,left,pointtwo$,0,'',frame)
	resp$(4)=str$(ficawage)
	fnFra(12,1,2,framewidth=110,'Federal Unemployment Compensation')
	frame=fc+=1 : lc=0
	fnLbl(lc+=1,1,'Federal Unemployment Compensation Rate:',mylen,right,0,frame)
	fnTxt(lc,mypos,13,0,left,pointthree$,0,'',frame)
	resp$(5)=str$(feducrat)
	fnLbl(lc+=1,1,'Federal Unemployment Compensation Maximum:',mylen,right,0,frame)
	fnTxt(lc,mypos,13,0,left,pointtwo$,0,'',frame)
	resp$(6)=str$(feducwag)
	fnFra(16,1,2,framewidth=110,'MediCare')
	frame=fc+=1 : lc=0
	fnLbl(lc+=1,1,'MediCare Rate:',mylen,right,0,frame)
	fnTxt(lc,mypos,13,0,left,pointthree$,0,'',frame)
	resp$(7)=str$(mcr)
	fnLbl(lc+=1,1,'MediCare Maximum:',mylen,right,0,frame)
	fnTxt(lc,mypos,13,0,left,pointtwo$,0,'',frame)
	resp$(8)=str$(mcm)
return ! /r
PAGE4: ! r:
	lc=3 : mylen=40 : mypos=mylen+2
	rc=0 ! Resp$ Counter
	fnLbl(lc+=1,1,'Enter the names of the 10 miscellaneous deductions.',width,center)
	fnLbl(lc+=1,1,'Indicate how the deductions are to be handled by the system.',width,center)
	fnLbl(lc+=1,1,'Place a Check in the appropriate column to indicate if it should be',width,center)
	fnLbl(lc+=1,1,'deducted for Federal, FICA, State, or State Unemployment Compensation.',width,center)
!
	fnLbl(lc+=1,24,'Deduction or Addition',10,center)
	fnLbl(lc+=1,1,'Deduction Name')
	fnLbl(lc,40,'Fed')
	fnLbl(lc,45,'FICA')
	fnLbl(lc,50,'State')
	fnLbl(lc,56,'UC')
	fnLbl(lc,60,'General Ledger Number')
	for j=1 to 10
		fnTxt(j+lc,1,20)
		resp$(rc+=1)=miscname$(j)
		item$(1)='Deduction' : item$(2)='Addition' : mat item$(2)
		fncomboa('ded_or_add',j+lc,24,mat item$)
		resp$(rc+=1)=item$(dedcode(j))
		fnChk(j+lc,40,'',right)
		rc+=1 : if dedfed(j)=1 then resp$(rc)='True' else resp$(rc)='False'
		fnChk(j+lc,45,'',right)
		rc+=1 : if dedfica(j)=1 then resp$(rc)='True' else resp$(rc)='False'
		fnChk(j+lc,50,'',right)
		rc+=1 : if dedst(j)=1 then resp$(rc)='True' else resp$(rc)='False'
		fnChk(j+lc,55,'',right)
		rc+=1 : if deduc(j)=1 then resp$(rc)='True' else resp$(rc)='False'
		fnqgl(lc+j,62,0,2)
		resp$(rc+=1)=fnrgl$(miscgl$(j))
	next j
return ! /r
BUILD_COMPANY: ! r:
	open #company=1: "Name=[Q]\CLmstr\Company.h[cno],Size=0,RecL=882,Replace",internal,outIn,relative
	write #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,N 2,N 1,C 9,C 12,c 12,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,1,0,mat e$,lastact$,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,prenum,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
goto READ_COMPANY ! /r
SAVE: ! r:
	rewrite #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,N 2,N 1,C 9,C 12,c 12,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,wbc,ar1,mat e$,lastact$,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,prenum,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
return ! /r
Xit: ! r:
	close #company:
fnXit ! /r
include: ertn
