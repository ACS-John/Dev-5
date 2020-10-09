! Replace S:\acsGL\FundBal
! Fund Balance Report
 
	autoLibrary
	fnTop(program$)
	on error goto Ertn
 
	dim cnam$*40,pedat$*20,d$*50,tr(7),tr$*12,td$*30,n$*12,t$*12,x$*3
	dim a$(9)*3,cogl$(2)*12,u$*12,address$(2)*40,b$(2)*12,c$*5,d(2),ta(2)
	dim desc$(10)*20,beg(10),inc(10),disb(10),end(10),bank$(90)*25,begb(90),endb(90),bankdr(90),bankcr(90),tr$*12,td$*30,tr(7)
 
	open #20: "Name=CNO.H"&wsid$,internal,input,relative  : _
	read #20,using 'Form POS 145,2*N 1,POS 159,2*C 12,POS 195,C 20',rec=1: mat d,mat cogl$,pedat$ : _
	close #20:
	on fkey 5 goto Xit
	data "GENERAL"
	data "WATER"
	data "WASTEWATER"
	data "SINKING FUND"
	data "PARK"
	data "STORMWATER"
	data "CAPITAL IMPROVEMENTS"
	data "HUD"
	data " "
	data " "
	read mat desc$
	fnopenprn
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,input,keyed
	open #2: "Name=[Q]\GLmstr\GLTrans.h[cno],Shr",internal,input,relative
	pr newpage
	pr f "10,20,C 30,H,N": " FUND BALANCE IN PROCESS"
	pr f "12,2,C 18,B,5": " Press F5 to stop"
L310: read #1,using L520: n$,d$,bb,cb,mat ta eof L550
	fund=val(n$(1:3)): acct=val(n$(4:9))
	if acct>90 then goto L460
	if acct<1 then goto L310
	bank$(acct)=d$(1:25)
	begb(acct)=bb
	endb(acct)=cb
	nta=ta(1)
L390: if nta=0 then goto L310
	read #2,using L410,rec=nta: mat tr,tr$,td$,nta noRec L310
L410: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,n 2,n 2,c 12,c 30,pd 3
	if tr(5)>0 then bankdr(acct)=bankdr(acct)+tr(5)
	if tr(5)<0 then bankcr(acct)=bankcr(acct)+tr(5)
	goto L390
 
L460: if fund=0 then goto L310
	if fund<>1 then goto L510
	if acct<140 then inc(fund)=inc(fund)+cb-bb else : _
		disb(fund)=disb(fund)+cb-bb
	goto L530
 
L510: if fp(acct*.01)*100<20 then inc(fund)=inc(fund)+cb-bb else : _
		disb(fund)=disb(fund)+cb-bb
L520: form pos 1,c 12,c 50,pos 81,2*pd 6.2,pos 333,2*pd 3
L530: goto L310
 
L550: ! EOF pr INFO
	pr newpage
	gosub L1020
	gosub L750
Xit: fnXit
 
	pr #255: newpage
L620: pr #255,using L630: date$('mm/dd/yy'),cnam$
L630: form pos 1,c 8,cc 74
	pr #255,using L650: time$,"Fund Balance Report"
L650: form pos 1,c 8,pos 31,c 28,skip 1
	p1=p1+1
	pr #255,using L680: rtrm$(pedat$),"PAGE ",p1
L680: form pos 20,cc 40,pos 70,c 5,n 4,skip 2
	pr #255,using L700: "  BEGINNING"," "," ","     ENDING"
L700: form pos 29,4*c 13,skip 1
	pr #255,using L720: "   BALANCE","       INCOME","      EXPENSE","    BALANCE"
L720: form pos 29,4*c 13,skip 0
	pr #255,using L700: " ___________"," ____________"," ____________"," ___________"
return
L750: gosub L620
	for j=1 to 10
		pr #255,using L780: desc$(j),beg(j),-inc(j),disb(j),beg(j)-inc(j)-disb(j)
L780: form pos 1,c 25,pos 28,4*pic(--,---,---.--),skip 1
	next j
	pr #255,using L700: "------------","------------","------------","------------"
	pr #255,using L780: "TOTAL",sum(beg),-sum(inc),sum(disb),sum(beg)-sum(inc)-sum(disb)
	pr #255,using L700: "============","============","============","============"
	pr #255,using L870: "ACCOUNT BALANCES"
	pr #255,using L700: "  BEGINNING"," "," ","     ENDING"
	pr #255,using L720: "   BALANCE","       INCOME","      EXPENSE","    BALANCE"
	pr #255,using L700: " ___________"," ____________"," ____________"," ___________"
L870: form skip 4,pos 34,c 20,skip 2
	for j=1 to 90
		if rtrm$(bank$(j))="" then goto L910
		pr #255,using L780: bank$(j),begb(j),bankdr(j),-bankcr(j),endb(j)
L910: next j
	pr #255,using L700: "------------","------------","------------","------------"
	pr #255,using L780: "TOTAL",sum(begb),sum(bankdr),-sum(bankcr),sum(endb)
	pr #255,using L700: "============","============","============","============"
	fncloseprn
return
 
	pr #255: newpage
	gosub L620
return
 
L1020: pr newpage
	close #101: ioerr L1040
L1040: open #101: "SROW=4,SCOL=18,EROW=17,ECOL=64,BORDER=DR,CAPTION=ENTER FUND BALANCES",display,outIn
	pr f "18,25,C 32,R,N": "Press F1 to continue; F5 to stop"
	pr f "5,20,c 45,n": "ENTER THE FUND BALANCE AT BEGINNING OF MONTH"
	for j=1 to 10
		pr f str$(j+6)&",25,C 20,N": desc$(j) : _
		io1$(j)=str$(j+6)&",45,N 12.2,UT,N"
	next j
L1100: input fields mat io1$,attr "R": mat beg conv L1100
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L1190 else ce=curfld
L1130: ce=ce+1: if ce>udim(io1$) then ce=1
L1140: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1) : _
	if ce1=0 then goto L1130
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L1100
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L1140
L1190: !
	pr newpage
return
 
include: ertn
