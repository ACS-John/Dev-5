! Replace S:\acsTM\ARPrtBil
on fkey 5 goto L1090
on error goto Ertn
autoLibrary
dim p$*5,iv$*12,tr(6),id$*20,o(2),h$*10,ar(5),ta(2),e(5)
fnTop(program$,"Statements")
dim z$*5,e$(4)*30,name$(4)*40,d$*20,flo$(2),scr1$(2)*55
dim age(4),st1$*5,mo(12)
data 0,31,59,90,120,151,181,212,243,273,304,334
read mat mo
open #8: "Name=S:\Core\Data\acsllc\pedate.h[cno],RecL=20,use,Shr",internal,outIn,relative
if lrec(8)=0 then write #8,using "form pos 1,n 6": d1 else read #8,using "form pos 1,n 6",rec=1,release: d1
close #8:
form c 9,skip 0
open #1: "Name=S:\Core\Data\acsllc\Company.h[cno],Shr",internal,input 
read #1,using L210: mat name$,mat age,all 
L210: form pos 1,4*c 40,pos 170,4*pd 2,pos 164,n 1
close #1:
at=int(65-len(rtrm$(name$(1)))/2)
form pos 1,c 5,pos 6,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
pr newpage
pr f "7,14,c 39,h,n": "POSITION A/R STATEMENT FORMS IN PRINTER"
pr f "10,10,c 50": "ENTER STATEMENT DATE IN FORMAT: MMDDYY"
pr f "10,51,N 6,N": d1
pr f "12,28,c 20": "Press F5 to Stop"
L310: input fields "10,51,N 6,UE,N": d1 conv L310
if cmdkey=5 then goto Xit
if d1<10100 or d1>123199 then goto L310
gosub L1960
open #1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,outIn,keyed 
open #2: "Name=S:\Core\Data\acsllc\ARTrans.h[cno],Shr",internal,input,relative 
L370: pr newpage
pr f "10,5,c 53": "ENTER CLIENT NUMBER TO START PRINTING, ELSE ENTER 0"
input fields "10,60,C 5,UE,N": st1$ conv L370
pr newpage
fnopenprn
pr f "10,20,c 40,h,n": "STATEMENT pr IN PROCESS"
pr f "23,2,C 30,N": "Press F5 to stop"
if rtrm$(st1$)="0" or rtrm$(st1$)="" then goto L450 else goto L1410
L450: read #1,using L460: z$,mat e$,mat ar,mat ta eof L1090 
L460: form pos 1,c 5,4*c 30,pos 283,2*pd 5.2,pd 4.3,2*n 1,2*pd 3
L470: if ar(4)=9 then goto L450
if ar(1)<=0 then goto L450
gosub L1430 ! AGING
L500: gosub L890
prebal=0
bal=0
pbf=0
ta=ta(1)
L550: if ta=0 then goto L1130
L560: read #2,using L580,rec=ta: p$,iv$,mat tr,id$,ta 
if tr(3)=0 then goto L550
L580: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
if tr(6)><5 and pbf=0 then gosub L1010
if tr(6)><5 then goto L690
if tr(5)=4 or tr(5)=6 then goto L650
prebal=prebal+tr(3)
if all=0 then bal=bal+tr(3) else goto L690
goto L670
L650: prebal=prebal-tr(3)
if all=0 then bal=bal-tr(3) else goto L690
L670: if ta=0 then goto L1010 ! 10/19/87
if all=0 then goto L560
L690: iv$=ltrm$(iv$)
if tr(5)=4 or tr(5)=6 then bal=bal-tr(3) else bal=bal+tr(3)
if tr(5)=4 or tr(5)=6 then pap=63 else pap=48
if tr(5)=4 or tr(5)=6 then tr3=-tr(3) else tr3=tr(3)
if rtrm$(id$)><"" then goto L800
if tr(5)=1 then id$="CHARGE"
if tr(5)=2 then id$="FINANCE CHARGE"
if tr(5)=3 then id$="STANDARD CHARGE"
if tr(5)=4 then id$="COLLECTION"
if tr(5)=5 then id$="DEBIT MEMO"
if tr(5)=6 then id$="CREDIT MEMO"
L800: pr #255,using L810: tr(1),iv$(1:8),id$,tr(3),bal
L810: form pos 1,pic(zz/zz/zz),x 4,c 10,c 20,pos pap,n 10.2,pos 77,n 12.2
p=p+1
if ta=0 then goto L1130
if p<38 then goto L560
pr #255: newpage
gosub L890
form pos 124,n 6
goto L560
L890: fnopenprn
pr #255: 
pr #255,using L920: z$,d1,ar(1)
L920: form skip 4,pos 60,c 5,x 6,pic(zz/zz/zz),skip 5,pos 62,pic($$$,$$$.##),skip 2
	for j=1 to 3
		pr #255,using L960: e$(j)
	next j
L960: form pos 10,c 30,skip 1
	pr #255,using L980: " "
L980: form pos 1,c 1,skip 9
	p=23
return
L1010: !
	if all=1 or prebal=0 then goto L1050
	pr #255,using L1030: " BALANCE  FORWARD",prebal
	L1030: form pos 25,c 17,pos 77,n 12.2,skip 1
	p=p+1
	L1050: !
	pbf=1
	if tr(6)><5 then goto L1080
	if ta=0 then goto L1130
	L1080: !
return
L1090: !
close #1: ioerr ignore
close #2: ioerr ignore
fnCloseprn
Xit: fnXit
L1130: sk=37-p
	pr #255,using L1150: "CURRENT","PAST",age(1),"PAST",age(2),"PAST",age(3)
L1150: form skip sk,pos 15,c 7,x 10,c 4,pic(zzzz),x 11,c 4,pic(zzzz),x 10,c 4,pic(zzzz),skip 2
	pr #255,using L1170: e(1),e(2),e(3),e(4)+e(5),ar(1)
L1170: form pos 10,n 10.2,x 8,n 10.2,x 9,n 10.2,x 8,n 10.2,pos 77,n 12.2
! pr #255,using 1226:"Please be sure to remit to  P O Box 758"
! form skip 5,pos 15,c 60,skip 1
	if align=3 then pr #255: newpage: goto L1330
	fncloseprn
	pr newpage
	pr f "10,5,C 60": "CHECK FORM ALIGNMENT"
	pr f "12,5,C 60": "ENTER 1 TO REPRINT SAME STATEMENT"
	pr f "13,5,C 60": "      2 TO PRINT NEXT STATEMENT AND STOP"
	pr f "14,5,C 60": "      3 TO PRINT ALL REMAINING STATEMENTS"
	pr f "23,30,c 20": "Press F5 to Cancel"
L1280: input fields "15,11,N 1,UE,N": align conv L1280
	if cmdkey=5 then goto Xit
	fnopenprn
	on align goto L500,L1330,L1330 none L1280
 
L1330: p=0
	pr newpage
	pr f "10,20,c 40,h,n": "STATEMENT pr IN PROCESS"
	pr f "23,2,C 30,N": "Press F5 to stop"
	bal=0
	if ta=0 then goto L450
return
 
L1410: read #1,using L460,key=rpad$(trim$(st1$),5): z$,mat e$,mat ar,mat ta nokey L370 
	goto L470
L1430: tam1=tam1+am1
	tam6=tam6+ar(1)
	if ar(1)<0 then cb1=cb1+(-ar(1)) else db1=db1+ar(1)
	mat e=(0)
	if ta(1)=0 then goto L1950
	ta1=ta(1)
L1490: read #2,using L1500,rec=ta1: iv$,mm,dd,yy,tr3,tr5,nta
L1500: form pos 6,c 12,3*n 2,x 5,pd 5.2,x 2,n 1,x 21,pd 3
	if tr5=4 or tr5=6 then goto L1660
L1520: if mm=0 then goto L1640
	ag1=mo(mm)+dd+yy*365+int(yy/4)
	if yy-int(yy/4)*4=0 and mm>2 then ag1=ag1+1
	ag2=ag0-ag1
	if ag2>=age(4) then e(5)=e(5)+tr3 else goto L1580
	goto L1790
L1580: if ag2>=age(3) then e(4)=e(4)+tr3 else goto L1600
	goto L1790
L1600: if ag2>=age(2) then e(3)=e(3)+tr3 else goto L1620
	goto L1790
L1620: if ag2>=age(1) then e(2)=e(2)+tr3 else goto L1640
	goto L1790
L1640: e(1)=e(1)+tr3
	goto L1790
L1660: if ar(5)=2 then goto L1690
	e(5)=e(5)-tr3
	goto L1790
L1690: tr3=-tr3
	ta1=ta(1)
L1710: read #2,using L1720,rec=ta1: hv$,mm,dd,yy,tr5,cta 
L1720: form pos 6,c 12,pos 18,3*n 2,pos 36,n 1,pos 58,pd 3
	if tr5=4 or tr5=6 then goto L1750
	if iv$=hv$ then goto L1520
L1750: if cta=0 then goto L1780
	ta1=cta
	goto L1710
L1780: e(5)=e(5)+tr3
L1790: if nta=0 then goto L1820
	ta1=nta
	goto L1490
L1820: if e(5)>=0 then goto L1850
	e(4)=e(4)+e(5)
	e(5)=0
	L1850: !
	if e(4)>=0 then goto L1880
	e(3)=e(3)+e(4)
	e(4)=0
	L1880: !
	if e(3)>=0 then goto L1910
	e(2)=e(2)+e(3)
	e(3)=0
	L1910: !
	if e(2)>=0 then goto L1950
	e(1)=e(1)+e(2)
	e(2)=0
	if e(1)<0 then v6=v6+(-e(1))
L1950: !
return
L1960: !
	mm=int(d1/10000)
	dd=int((d1-mm*10000)/100)
	yy=d1-(mm*10000+dd*100)
	ag0=mo(mm)+dd+yy*365+int(yy/4)
	if yy-int(yy/4)*4=0 and mm>2 then ag0=ag0+1
return
include: ertn
