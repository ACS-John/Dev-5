! Replace S:\acsUB\Bill-Rpt
! pr utility billing reports based on bills
 
	autoLibrary
	on error goto Ertn
 
	dim z$*10,e$(4)*30,temp$(3)*26,resp$(4)*40,dat$*20
 
	fnTop("S:\acsUB\Bill-Rpt", "Final Billing")
	fnLastBillingDate(d1)
	fndat(dat$,1)
 
SCR1: !
	fnTos
	fnLbl(1,1,"Billing Date:",15,1)
	fnTxt(1,17,8,8,1,"1",0,"Only enter the billing date if you wish to limit the report to those billed and finaled this month. (mmddyy)") : _
	resp$(1)=str$(d1)
	fnLbl(2,1,"Route Number:",15,1)
	fncmbrt2(2,17,0) : _
	resp$(2)= "[All]"
	fnChk(4,2,"Outstanding Balances Only") : _
	resp$(3)="False"
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	d1= val(resp$(1)) conv SCR1 : _
	if uprc$(resp$(2))=uprc$("[All]") then route=0 else : _
		route=val(resp$(2))
	if resp$(3)="False" then oob$="N" else oob$="Y"
	goto STARTREPORT
 
DONE: !
	fncloseprn
Xit: fnXit
 
STARTREPORT: !
	fnwait("Printing: please wait...",1)
	on fkey 5 goto DONE
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
	fnopenprn
	gosub HEADER
	goto REPORT
 
HEADER: !
	if d1<>0 then temp$(1)="Billing Date: "&cnvrt$("pic(zz/zz/zz)",d1)
	if oob$="Y" then temp$(2)="Only Outstanding Balances"
	pr #255: "\qc  {\f181 \fs20 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs22 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
	if d1<>0 or oob$="Y" then : _
		pr #255: "\qc "&trim$(temp$(1))&"   "&temp$(2)
	pr #255,using L480: "\ql  ","Page "&str$(pg+=1)
L480: form pos 1,c 82,c 10,skip 1
	pr #255: "{\ul Act.Number} {\ul Customer Name                 } {\ul    Balance} {\ul Billing Date} {\ul  Deposit}"
return
 
PGOF: ! : _
	pr #255: newpage : _
	gosub HEADER : _
	continue
 
REPORT: !
L550: read #1,using 'Form POS 1,C 10,4*C 30,POS 1821,N 1,POS 292,PD 4.2,PD 4,POS 227,PD 5,POS 1741,N 2,pos 185,4*pd 4.2': z$,mat e$,finalbil,bal,lastbilldate,usage,extra(1),watdep,sewdep,elecdep,gasdep eof DONE
	deposit=watdep+sewdep+elecdep+gasdep
	if finalbil>0 then goto L570 else goto L550
L570: if d1<>0 and d1<>lastbilldate then goto REPORT
	if route>0 and extra(1)<>route then goto REPORT
	if oob$="Y" and bal<=0 then goto REPORT
	pr #255,using 'Form POS 1,C 10,X 1,C 30,X 1,N 10.2,X 3,PIC(ZZ/ZZ/ZZ),X 2,N 9.2': z$,e$(2),bal,lastbilldate,deposit pageoflow PGOF
	goto REPORT
 
include: ertn
 
