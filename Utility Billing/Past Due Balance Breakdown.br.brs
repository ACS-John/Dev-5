autoLibrary
on error goto Ertn
fnTop(program$)
! r: init
	fnLastBillingDate(lbill)
	dim serviceName$(10)*20
	dim srv$(10)*2
	dim tax_code$(10)*1
	dim penalty$(10)*1
	fnGetServices(mat serviceName$, mat srv$, mat tax_code$,mat penalty$)	! need to build headings from this information
	fncreg_read('Route Low',bkno1$) : bkno1=val(bkno1$)
	fncreg_read('Route High',bkno2$) : bkno2=val(bkno2$)
	if trim$(serviceName$(3))<>"Electric" and trim$(srv$(3))="EL" then needelecused=1
	if trim$(serviceName$(4))<>"Gas" and trim$(srv$(4))="GA" then needgasused=1
	x=0
	dim hdr$*230
	hdr$="{\ul Account No}  {\ul Customer Name }       "
	for j=1 to 10
		if j=3 and needelecused=1 then goto L200
		if j=4 and needgasused=1 then goto L200
		if trim$(serviceName$(j))<>"" then
			hdr$=hdr$&" {\ul "&lpad$((trim$(serviceName$(j)(1:9))),9)&"}"
			services+=1
		end if
		L200: !
	next j
	dim detail(11)
	mat detail(services+1)
	dim gt(11)
	mat gt(services+1)
! /r
Scr1: ! r:
	dim resp$(10)*60
	fnTos
	fnLbl(1,1,"As of Date:",19,1)
	fnTxt(1,21,8,8,0,"1001")
	if d1<>0 then resp$(1)=str$(d1) else resp$(1)=date$("mmddyy")
	fnLbl(2,1,"Last Billing Date:",19,1)
	fnTxt(2,21,8,0,0,"1001")
	resp$(2)=str$(lbill)
	fnChk(3,1,"skip customers who only owe current bill")
	resp$(3)='False'
	fnChk(4,1,"skip customers with credit balance")
	resp$(4)='False'
	fnChk(5,1,"Only show past due amounts (not current month)")
	resp$(5)='True'
	fnChk(6,1,"skip accounts with Zero balances")
	resp$(6)='True'
	fnCmdSet(3)
	L350: !
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	d1=val(resp$(1))
	lbill=val(resp$(2))
	if uprc$(resp$(3))=uprc$('True') then skipcurrent=1
	if uprc$(resp$(4))=uprc$('True') then skipcredits=1
	if uprc$(resp$(5))=uprc$('True') then pastdueonly=1
	if uprc$(resp$(6))=uprc$('True') then skipzero=1
	if lbill<10100 or lbill>123199 then goto L350
	if d1<10100 or d1>123199 then goto L350
	d7=int(d1/10000)
	d6=d7*10000
	d5=int((d1-d6)/100)
	d8=d1-(d7*10000+d5*100)
	if d7<1 or d7>12 then goto L350
	if d5<1 or d5>31 then goto L350
goto DoReport ! /r
DoReport: ! r: do the report
! on fkey 5 goto Finis
fnOpenPrn
gosub PrHeader
v=bkno1
open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
ReadCustomer: !
	dim z$*10
	dim e$*30
	dim g(12)
	dim gb(10)
	read #hCustomer,using L580: z$,e$,f,bal,mat g,mat gb eof Finis
	L580: form pos 1,c 10,pos 41,c 30,pos 296,pd 4,pos 292,pd 4.2,pos 300,12*pd 4.2,pos 388,10*pd 5.2
	if f<>lbill then mat g=(0)
	if skipcurrent=1 and bal-g(11)=<0 then goto ReadCustomer		! skip anyone who only owes last times bill
	if skipzero=1 and bal=0 then goto ReadCustomer 							! skip Zero Balance Accounts
	if skipcredits=1 and bal<=0 then goto ReadCustomer					! skip credit balance accounts
	if pastdueonly=1 then goto L640 else goto L690
	L640: !
	for j=1 to 10 ! subtract current bill out of balance breakdown
		if penalty$(j)="Y" then goto L680 ! skip penalties
		if gb(j)=0 and j<10 then g(j+1)=g(j+1)+g(j) : g(j)=0 ! try to prevent negative amounts in columns
		gb(j)=gb(j)-g(j)
		L680: !
	next j
	L690: !
	x=0
	if pastdueonly=1 and skipzero=1 and sum(gb)=0 then goto ReadCustomer ! if only owe the current bill then consider it zero for the skipzero test and pastdueonly test
	for j=1 to 10
		if j=3 and needelecused=1 then goto L730
		if j=4 and needgasused=1 then goto L730
		if trim$(serviceName$(j))<>"" then detail(x+=1)=gb(j)
		L730: !
	next j
	detail(x+1)=sum(gb)
	pr #255,using L760: z$,e$(1:20),mat detail pageoflow PgOf
	L760: form pos 1,c 12,c 21,11*n 10.2
	mat gt=gt+detail
goto ReadCustomer ! /r
PrHeader: ! r:
	pr #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs22 \b "&env$('program_caption')&"}"
	pr #255: "\qc {\f181 \fs18 \b As of "&cnvrt$("pic(zz/zz/zz)",d1)&"}"
	pagetab=41+services*10
	pr #255,using L860: "\ql "&date$,"Page "&str$(p2+=1)
	L860: form pos 1,c 20,pos pagetab,c 10
	pr #255: hdr$&" {\ul     Total}"
return ! /r
PrTotals: ! r:
	pr #255: ""
	pr #255: ""
	pr #255,using L760: "","****** Grand Totals",mat gt
return ! /r
Finis: ! r:
	close #hCustomer: ioerr ignore
	gosub PrTotals
	fnClosePrn
goto Xit ! /r
Xit: fnXit
PgOf: ! r:
	pr #255: newpage
	gosub PrHeader
continue ! /r
include: ertn

