autoLibrary
on error goto Ertn
fnTop(program$)

dim code$(4)
code$(1)="Water"
code$(2)="Sewer"
code$(3)="Electric"
code$(4)="Gas"
fnLastBillingDate(bdate)

MAIN: ! r:
	fnTos
	mylen=20
	mypos=mylen+2
	fnLbl(1,1,"Billing Date:",mylen,1)
	fnTxt(1,mypos,8,8,0,"1")
	resp$(1)=str$(bdate)
	fnLbl(2,1,"Type of Service:",mylen,1)
	fnComboA("Service",2,mylen+3,mat code$,"",16)
	fnLbl(3,1,"Rate Code",mylen,1)
	fnTxt(3,mypos,3,3,0,"1030")
	resp$(3)=""
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	bdate= val(resp$(1))
	if resp$(2)="Water" then
		dim srvc$*11
		srvc=1 : srvc$=resp$(2)
	else if resp$(2)="Sewer" then
		srvc=2 : srvc$=resp$(2)
	else if resp$(2)="Electric" then
		srvc=3 : srvc$=resp$(2)
	else if resp$(2)="Gas" then
		srvc=4 : srvc$=resp$(2)
	else
		goto MAIN
	end if
	rcode=val(resp$(3))
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
	fnOpenPrn
	gosub PRINTIT
goto DONE ! /r
DONE: ! r:
	close #1: ioerr ignore
	fnClosePrn
goto Xit ! /r
Xit: fnXit
 
PRINTIT: ! r:
	p2=0
	count=0
	gosub HDR
	do
		READ_CUSTOMER: !
		dim z$*10
		! dim xa(4)
		dim xa(7)

		! dim xd(12)
		dim xd(15)
		dim g(10)

		dim e$(4)*30
		dim f$(3)*12


		read #1,using F_CUSTOMER: z$,mat e$,mat xa,mat f$,mat xd,mat g,last_billing_date eof PR_TOTALS ! READ MASTER RECORD
		F_CUSTOMER: form pos 1,c 10,pos 11,4*c 30,pos 143,7*pd 2,pos 131,c 12,pos 361,c 12,pos 373,c 12,pos 217,15*pd 5,pos 300,10*pd 4.2,pos 296,pd 4
		if bdate<>0 and bdate<>last_billing_date then goto READ_CUSTOMER
		if xa(srvc)=0 then goto READ_CUSTOMER ! no service
		if xa(srvc)<>rcode then goto READ_CUSTOMER
		usage=0
		if srvc=1 then usage=xd(3): amount=g(1): meter$=f$(1) ! water
		if srvc=2 then usage=xd(3): amount=g(2): meter$="" ! sewer
		if srvc=3 then usage=xd(7): amount=g(3): meter$=f$(2) ! electric
		if srvc=4 then usage=xd(11): amount=g(4): meter$=f$(3) ! gas
		if xa(srvc)=tc or tc=0 then
			pr #255,using F_PR_LINE: z$,e$(2),e$(1),meter$,usage,amount pageoflow PgOf
			F_PR_LINE: form x 5,c 10,x 5,c 30,x 7,c 30,x 2,c 12,x 2,pic(zzzzzzzzz),x 2,n 12.2
			count+=1
			totusage+=usage
			totamount+=amount
		end if
	loop
! /r
PR_TOTALS: ! r:
	pr #255,using "form pos 101,C 28": "____________  ____________"
	pr #255,using "form pos 101,N 12,X 2,N 12.2": totusage,totamount
	pr #255,using "form pos 101,C 28": "============  ============"
	pr #255,using 'form pos 1,c 20,pic(zzzz,zzz,zzz)': "Total Customers:",count
	if count>0 then pr #255,using 'form pos 1,c 20,pic(zzzz,zzz,zzz)': "Average Usage:",round(totusage/count,0)
	if count>0 then pr #255,using 'form pos 1,c 20,pic(z,zzz,zzz.##)': "Average Amount:",round(totamount/count,2)
	close #1:
return ! /r
PgOf: ! r:
	pr #255: newpage
	gosub HDR
continue  ! /r
HDR: ! r:
	p2=p2+1
	pr #255,using "form pos 1,CC 80": env$('cnam')
	pr #255,using "form pos 1,CC 80": "Consumption List - "&srvc$
	pr #255,using " form pos 1,CC 80": "Rate Code "&str$(rcode)
	pr #255,using "form pos 110,C 5,PIC(ZZZ)": "Page ",p2
	pr #255: ""
	if tc<>0 then pr #255,using L740: srvc$&" Code ",tc
	L740: form pos 41,c 9,n 2,skip 2
	pr #255: tab(7);"Customer #";tab(21);"Name";tab(58);"Meter Address";tab(90);"   Meter #    Consumption  Dollar Amt"
	pr #255: tab(7);"__________";tab(21);"________________________________";tab(58);"______________________________  ____________  ___________  __________"
return  ! /r
include: ertn
