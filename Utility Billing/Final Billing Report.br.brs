autoLibrary
on error goto Ertn
fnTop(program$)
fnLastBillingDate(d1)
dim dat$*20
fndat(dat$,1)
Scr1: ! r:
	dim resp$(4)*40
	fnTos
	fnLbl(1,1,"Billing Date:",15,1)
	fnTxt(1,17,8,8,1,"1",0,"Only enter the billing date if you wish to limit the report to those billed and finaled this month. (mmddyy)")
	resp$(1)=str$(d1)
	fnLbl(2,1,"Route Number:",15,1)
	fncmbrt2(2,17,0)
	resp$(2)="[All]"
	fnChk(4,2,"Outstanding Balances Only")
	resp$(3)='False'
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	d1=val(resp$(1)) conv Scr1
	if uprc$(resp$(2))=uprc$("[All]") then route=0 else route=val(resp$(2))
	if resp$(3)='False' then oob$="N" else oob$="Y"
goto ReportInit ! /r
ReportInit: ! r:
	! fnwait("Printing: please wait...",1)
	! on fkey 5 goto Finis
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
	fnopenprn
	gosub PrHeader
goto PrReport ! /r
PrReport: ! r:
	do
		dim z$*10
		dim e$(4)*30
		read #1,using 'form pos 1,C 10,4*C 30,pos 1821,N 1,pos 292,PD 4.2,PD 4,pos 227,PD 5,pos 1741,N 2,pos 185,4*pd 4.2': z$,mat e$,finalbil,bal,lastbilldate,usage,extra(1),watdep,sewdep,elecdep,gasdep eof Finis
		deposit=watdep+sewdep+elecdep+gasdep
	loop until finalbil>0
	if d1<>0 and d1<>lastbilldate then goto PrReport
	if route>0 and extra(1)<>route then goto PrReport
	if oob$="Y" and bal<=0 then goto PrReport
	pr #255,using 'form pos 1,C 10,X 1,C 30,X 1,N 10.2,X 3,PIC(ZZ/ZZ/ZZ),X 2,N 9.2': z$,e$(2),bal,lastbilldate,deposit pageoflow PgOf
goto PrReport
! /r
PrHeader: ! r:
	dim temp$(3)*26
	if d1<>0 then temp$(1)="Billing Date: "&cnvrt$("pic(zz/zz/zz)",d1)
	if oob$="Y" then temp$(2)="Only Outstanding Balances"
	pr #255: "\qc  {\f181 \fs20 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs22 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
	if d1<>0 or oob$="Y" then
		pr #255: "\qc "&trim$(temp$(1))&"   "&temp$(2)
	end if
	pr #255,using L480: "\ql  ","Page "&str$(pg+=1)
	L480: form pos 1,c 82,c 10,skip 1
	pr #255: "{\ul Act.Number} {\ul Customer Name                 } {\ul    Balance} {\ul Billing Date} {\ul  Deposit}"
return ! /r
PgOf: ! r:
	pr #255: newpage
	gosub PrHeader
continue ! /r
Finis: ! r:
	fncloseprn
goto Xit ! /r
Xit: fnXit
include: ertn

