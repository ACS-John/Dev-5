! Replace S:\acsGL\BudWksh
! pr Budget Worksheet

autoLibrary
on error goto Ertn

dim d$*50,n$*12,bp(13),bm(13),name$*40,gln1$*12,gln2$*12
dim resp$(10)*80,revb(13)

fnTop(program$,"Budget Worksheet")
gosub DETERMINE_DATE
on fkey 5 goto L760
open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLINDEX.h[cno],Shr",i,i,k
open #12: "Name=[Q]\GLmstr\BudgetInfo.h[cno],KFName=[Q]\GLmstr\BudIndx.h[cno],Use,RecL=28,KPs=1,KLn=14,Shr",internal,outIn,keyed
if fnprocess=1 then goto L440
! defaults
	pba$='False' : bud$='False'
	SCREEN1: !
	fnTos(sn$='BudgetWksht')
	lc=0 : mylen=40 : mypos=mylen+2
	fnLbl(lc+=1,1,'Fund:',mylen,1)
	fnTxt(lc,mypos,40)
	resp$(1)=name$
	fnLbl(lc+=1,1,'Starting Account:',mylen,1)
	fnQgl(lc,mypos)
	resp$(2)=fnrgl$(gln1$)
	fnLbl(lc+=1,1,'Ending Account:',mylen,1)
	fnQgl(lc,mypos)
	resp$(3)=fnrgl$(gln2$)
	fnChk(lc+=1,mypos,'Enter proposed budget amounts',1)
	resp$(4)=pba$
	fnChk(lc+=1,mypos,'Revised Budget instead of Origional',1)
	resp$(5)=bud$
	fnLbl(lc+=1,1,'Closing date for previous budget year:',mylen,1)
	fnTxt(lc,mypos,8,8,1,"1",0,"Reqired for prior year's budget to appear on worksheet.")
	resp$(6)=str$(priordate) ! previous year end
	fnLbl(lc+=1,1,'Closing date for two years ago:',mylen,1)
	fnTxt(lc,mypos,8,8,1,"1",0,"Reqired only if you want balance and budget from two years ago.")
	resp$(7)=str$(priorpriordate) ! two years ago
	fnChk(lc+=1,mypos,"Year Already Closed:",1)
	resp$(8)='False'
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	name$=resp$(1)
	gln1$=fnagl$(resp$(2))
	gln2$=fnagl$(resp$(3))
	pba$=resp$(4)
	bud$=resp$(5)
	priordate=val(resp$(6))
	priorpriordate=val(resp$(7))
	if resp$(8)='True' then yearclosed=1
	yr1$=resp$(6)(5:6) : if len(yr1$)=1 then let yr1$=resp$(6)(4:5) ! fixed kln conflict error for len(5) dates in resp$
	yr2$=resp$(7)(5:6) : if len(yr2$)=1 then let yr2$=resp$(7)(4:5)
	read #1,using L530,key>=gln1$: n$,d$,cb,mat bp,mat bm,mat revb nokey L440
L440: namtab=66-int(len(rtrm$(name$))/2)
	fnopenprn
	gosub HEADING
	goto L540
L480: read #1,using L530: n$,d$,cb,mat bp,mat bm,mat revb eof L760 ! READ MASTER FILE
	if trim$(gln1$)="" then goto L510
	if n$<gln1$ then goto L480
L510: if trim$(gln2$)="" then goto L530
	if n$>gln2$ then goto L760
L530: form pos 1,c 12,c 50,pos 87,pd 6.2,pos 171,26*pd 6.2,pos 339,13*pd 6.2
L540: dno=val(n$(1:3))
	ano=val(n$(4:9))
	sno=val(n$(10:12))
	if yearclosed=1 then cb=bp(12) ! use last year end balance if year already closed.
	if uprc$(bud$)="R" then mat bm=revb
	cyb=bm(1)+bm(2)+bm(3)+bm(4)+bm(5)+bm(6)+bm(7)+bm(8)+bm(9)+bm(10)+bm(11)+bm(12)
	tpriorcb+=priorcb
	tpriorbud+=priorbud
	toldcb+=oldcb
	toldbud+=oldbud
	tpyb=tpyb+bp(12)
	tcb=tcb+cb
	tcyb=tcyb+cyb
	if yr1$="" then goto L710
	budkey$=n$&yr1$ ! gl number plus year for last year
	budacno$="": yr$="": oldcb=oldbud=0
	read #12,using "form pos 1,c 12,c 2,2*pd 6.2",key=budkey$: budacno$,yr$,oldcb,oldbud nokey L720 ! read old budget history record
L710: if yr2$="" then goto L740
L720: priorbudkey$=n$&yr2$ ! get two years ago
	read #12,using "form pos 1,c 12,c 2,2*pd 6.2",key=priorbudkey$: budacno$,prioryr$,priorcb,priorbud nokey L740 ! read old budget history record from two years ago
L740: gosub PRINT_MASTER_RECORD
	goto L480
L760: ! EOF OR EOJ ON MASTER FILE
	pr #255:
	if priordate=0 and priorpriordate=0 then pr #255,using L810: "Totals",tpyb,tcb,tcyb,tbud
	if priordate>0 and priorpriordate=0 then pr #255,using L820: "Totals",toldcb,toldbud,tcb,tcyb,tbud
	if priordate>0 and priorpriordate>0 then pr #255,using L820: "Totals",tpriorcb,tpriorbud,toldcb,toldbud,tcb,tcyb,tbud
L810: form pos 38,c 6,pos 45,pic(---,---,---.##),pos 73,pic(---,---,---.##),pos 102,pic(---,---,---.##),pos 123,pic(----,---,---.zz),skip 2
L820: form pos 25,c 6,pos 34,7*pic(----,---,---.##),skip 1
	tpyb=0
	tcb=0
	tcyb=0
	tbud=0
	tpriorcb=tpriorcb=toldcb=toldbud=0
	fncloseprn
goto SCREEN1
goto Xit

HEADING: ! r: pr PAGE HEADING
	pr #255:
	pr #255,using L950: env$('cnam')
L950: form pos 20,cc 40
	pr #255,using L950: "Budget Worksheet"
	if trim$(name$)<>"" then pr #255,using L950: rtrm$(name$)
	pr #255,using L950: rtrm$(fnpedat$)
	pr #255: ""
	if priordate=0 and priorpriordate=0 then goto L1010 else goto L1030
L1010: pr #255,using L1080: "Account"
	goto L1090
L1030: if priordate>0 and priorpriordate=0 then goto L1040 else goto L1060
L1040: pr #255,using L1080: "Account","         "&str$(priordate),"         "&str$(priordate),"        Current","        Current","       Proposed"
	goto L1090
L1060: if priordate>0 and priorpriordate>0 then goto L1070 else goto L1090
L1070: pr #255,using L1080: "Account","         "&str$(priorpriordate),"         "&str$(priorpriordate),"         "&str$(priordate),"         "&str$(priordate),"        Current","        Current","       Proposed"
L1080: form pos 3,c 8,pos 34,7*c 15
L1090: if priordate=0 and priorpriordate=0 then goto L1100 else goto L1120
L1100: pr #255,using L1190: "Number","Description","Prior Year Balance","Current Balance","Current Budget","Proposed Budget"
	goto L1200
L1120: if priordate>0 and priorpriordate=0 then goto L1130 else goto L1160
L1130: pr #255,using L1180: "Number","Description","        Balance","         Budget","        Balance","         Budget","         Budget"
	form pos 4,c 6,pos 22,c 11,pos 34,5* c 15
	goto L1200
L1160: if priordate>0 and priorpriordate>0 then goto L1170 else goto L1200
L1170: pr #255,using L1180: "Number","Description","        Balance","         Budget","        Balance","         Budget","        Balance","         Budget","         Budget"
L1180: form pos 4,c 6,pos 22,c 11,pos 34,7*c 15
L1190: form pos 4,c 6,pos 22,c 11,pos 44,c 18,pos 74,c 15,pos 101,c 19,pos 123,c 15
L1200: !
return ! /r

PRINT_MASTER_RECORD: ! r:
	if pba$='True' then
		fnTos(sn$='Budwksh-add-ba')
		lc=0 : mylen=50 : mypos=mylen+2
		fnLbl(lc+=1,1,'Enter Proposed Budget',40,2,+2)
		lc+=1
		fnLbl(lc+=1,1,"Proposed Budget for Account "&str$(dno)&"-"&str$(ano)&"-"&str$(sno)&":",mylen,1)
		fnTxt(lc,mypos,12,0,0,'currency')
		resp$=''
		fnLbl(lc+=1,1,d$,mylen,1)
		fnCmdSet(3)
		ckey=fnAcs(mat resp$)
		if ckey=5 then goto Xit
		bud=val(resp$(1)) : tbud+=bud
	end if
	if priordate=0 and priorpriordate=0 then goto L1360 else goto L1430
	L1360: if pba$='True' then goto L1400
	pr #255,using L1380: dno,ano,sno,d$(1:22),bp(12),cb,cyb," ______________" pageoflow NWPGE
	L1380: form pos 1,pic(zzz),x 1,pic(zzzzzz),x 1,pic(zzz),x 2,c 22,pos 47,pic(-----,---.##),pos 75,pic(-----,---.##),pos 104,pic(-----,---.##),pos 122,c 15,skip 1
	goto L1410
	L1400: pr #255,using L1410: dno,ano,sno,d$(1:22),bp(12),cb,cyb,cnvrt$("PIC(----,---,---.##)",bud) pageoflow NWPGE
	L1410: form pos 1,pic(zzz),x 1,pic(zzzzzz),x 1,pic(zzz),x 2,c 22,pos 47,pic(-----,---.##),pos 75,pic(-----,---.##),pos 104,pic(-----,---.##),pos 122,c 15,skip 1
	goto L1580
	L1430: if priordate>0 and priorpriordate=0 then goto L1440 else goto L1510
	L1440: if pba$='True' then goto L1450 else goto L1480
	L1450: pr #255,using L1460: dno,ano,sno,d$(1:22),oldcb,oldbud,cb,cyb,cnvrt$("PIC(----,---,---.zz)",bud) pageoflow NWPGE
	L1460: form pos 1,pic(zzz),x 1,pic(zzzzzz),x 1,pic(zzz),x 2,c 22,pos 34,4*pic(----,---,---.##),c 15,skip 2
	goto L1580
	L1480: pr #255,using L1490: dno,ano,sno,d$(1:22),oldcb,oldbud,cb,cyb," _____________" pageoflow NWPGE
	L1490: form pos 1,pic(zzz),x 1,pic(zzzzzz),x 1,pic(zzz),x 2,c 22,pos 34,4*pic(----,---,---.##),c 15,skip 2
	goto L1580
	L1510: if priordate>0 and priorpriordate>0 then goto L1520 else goto L1580
	L1520: if pba$='True' then goto L1530 else goto L1560
	L1530: pr #255,using L1540: dno,ano,sno,d$(1:22),priorcb,priorbud,oldcb,oldbud,cb,cyb,cnvrt$("PIC(----,---,---.zz)",bud) pageoflow NWPGE
	L1540: form pos 1,pic(zzz),x 1,pic(zzzzzz),x 1,pic(zzz),x 2,c 22,pos 34,6*pic(----,---,---.##),c 15,skip 2
	goto L1580
	L1560: pr #255,using L1570: dno,ano,sno,d$(1:22),priorcb,priorbud,oldcb,oldbud,cb,cyb," ______________" pageoflow NWPGE
	L1570: form pos 1,pic(zzz),x 1,pic(zzzzzz),x 1,pic(zzz),x 2,c 22,pos 34,6*pic(----,---,---.##),c 15,skip 2
	L1580: !
return ! /r

NWPGE: ! SPACE TO NEWPAGE
	pr #255: newpage
	gosub HEADING
continue

DETERMINE_DATE: !
	endingdate$=rtrm$(fnpedat$)
	x=pos(endingdate$," ",1)
	month$=endingdate$(1:x-1)
	day=val(endingdate$(x+1:x+2)) conv L1800
L1800: year=val(endingdate$(len(endingdate$)-1:len(endingdate$))) : prioryear=year-1
	dim month$(12),payrolldate$*20
	month$(1)="January": month$(2)="February"
	month$(3)="March": month$(4)="April": month$(5)="May"
	month$(6)="June" : month$(7)="July"
	month$(8)="August": month$(9)="September"
	month$(10)="October": month$(11)="November": month$(12)="December"
	for j=1 to 12
		if uprc$(month$)=uprc$(month$(j)) then month=j: goto L1860
	next j
L1860: priordate=(month*10000)+day*100+prioryear
	priorpriordate=priordate-1
return
Xit: fnXit
include: ertn
