on error goto Ertn
autoLibrary
fnTop(program$,"Trial Balance")
fndat(dat$)
open #8: "Name=S:\Core\Data\acsllc\pedate.h[cno],RecL=20,use,Shr",i,outi,r 
if lrec(8)=0 then write #8,using "form pos 1,n 6": d1 else read #8,using "form pos 1,n 6",rec=1,release: d1
dim z$*5,e$(4)*30,ex(5),sx(5),c(5),u$*20,flo$(3),fli$(2),dat$*20
dim scr$(3)*50,ta(2),o(2),cnv$*6,q$*30,age(4),mo(12),iv$*12
data 0,31,59,90,120,151,181,212,243,273,304,334
read mat mo
open #1: "Name=S:\Core\Data\acsllc\Company.h[cno],Shr",internal,outIn ioerr Ertn
read #1,using L180: mat age ioerr Ertn
L180: form pos 170,4*pd 2,pos 226,c 20
	close #1: 
	at=int(66-len(rtrm$(env$('cnam')))/2)
	fli$(1)="10,56,C 20,U,N"
	fli$(2)="12,56,N 6,U,N"
	pr newpage
L230: pr f "5,30,C 18,H,N": "AGED TRIAL BALANCE"
	pr f "10,5,cr 50,n": "ENTER AS OF DATE  (EXAMPLE = "&dat$&":"
	pr f "12,5,Cr 50,N": "ENTER THE AGING DATE IN MMDDYY FORMAT:"
	pr f "10,56,c 20,n": dat$
	pr f "14,28,C 30,N": "F1 Continue   F5 Stop"
L280: rinput fields mat fli$: dat$,d1 conv L280
	if cmdkey=5 then goto XIT
	if cmdkey<>1 then goto L230
	fndat(dat$,put=2)
	rewrite #8,using "form pos 1,n 6",rec=1: d1
	close #8: 
	gosub L1510
	pr newpage
	fnopenprn
	pr f "10,15,c 50,h": "A/R AGED TRIAL BALANCE PROGRAM IN PROCESS"
	pr f "23,2,C 30,N": "Press F5 to stop"
	open #1: "Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\Client-Idx.h[cno],Shr",i,outIn,k ioerr Ertn
	open #2: "Name=S:\Core\Data\acsllc\Transactions.h[cno],Shr",i,i,r ioerr Ertn
	gosub L550
L400: read #1,using L420: z$,e$(1),am6,am16,mat ta eof L700 ioerr Ertn
	if am6=0 then goto L400
L420: form pos 1,c 5,c 30,pos 283,pd 5.2,pos 298,n 1,pos 299,2*pd 3
	if am6<=0 then goto L450
	tam6=tam6+am6
L450: if am6<0 then cb1=cb1+(-am6) else db1=db1+am6
	if am6<=0 then goto L400
	gosub L1000
	mat sx=sx+ex
	pr #255,using L500: z$,e$(1),am6,mat ex pageoflow L520
L500: form pos 1,c 12,c 30,x 10,6*n 12.2,skip 1
	goto L400
L520: pr #255: newpage
	gosub L550
	goto L400
L550: p2=p2+1
	pr #255,using L570: date$,env$('cnam'),"PAGE ",p2
L570: form skip 1,pos 1,c 8,pos at,c 40,pos 120,c 5,n 4,skip 1
	pr #255,using L590: time$,"A/R AGED TRIAL BALANCE"
L590: form pos 1,c 8,pos 55,c 22,skip 1
	q$=rtrm$("As of "&dat$)
	tabq=int(66-len(q$)/2)
	pr #255,using L630: q$
L630: form pos tabq,c 26,skip 0
	if v9=9 then goto L690
	pr #255: 
	pr #255: tab(71);"-----------------------AGING--------------------------"
	pr #255,using L680: "CLIENT #","CLIENT NAME"," ","BALANCE","CURRENT",age(1),"-",age(2),age(2)+1,"-",age(3),age(3)+1,"-",age(4),"OVER ",age(4)
L680: form pos 1,c 9,pos 12,c 13,pos 42,c 12,pos 58,c 7,pos 70,c 7,pos 83,n 3,c 1,n 2,pos 95,n 3,c 1,n 3,pos 106,n 3,c 1,n 3,pos 117,c 5,n 3,skip 1
L690: return 
L700: c6=sx(1)+sx(2)+sx(3)+sx(4)+sx(5)
	if c6=0 then goto L790
	mat c=(0)
	for j=1 to 5
		if sx(j)=0 then goto L760
		c(j)=sx(j)/c6*100
L760: next j
	pr #255: 
	pr #255,using L500: "","   COMPANY TOTALS",tam6,mat sx
L790: pr #255: newpage
	v9=9
	gosub L550
	pr #255,using L830: "A/R AGING SUMMARY"
L830: form skip 2,pos 58,c 40,skip 2
	pr #255: tab(68);"AMOUNT   PERCENT"
	pr #255: 
	pr #255,using L870: "CURRENT",sx(1),c(1)
L870: form pos 43,c 10,pos 61,n 13.2,n 10.2,skip
	pr #255,using L870: str$(age(1)+1)&"-"&str$(age(2)),sx(2),c(2)
	pr #255,using L870: str$(age(2)+1)&"-"&str$(age(3)),sx(3),c(3)
	pr #255,using L870: str$(age(3)+1)&"-"&str$(age(4)),sx(4),c(4)
	pr #255,using L870: "OVER "&str$(age(4)),sx(5),c(5)
	pr #255: 
	pr #255,using L940: "                       TOTAL",db1
L940: form pos 30,c 30,n 14.2,skip 2
	pr #255,using L940: "LESS CREDIT BALANCE ACCOUNTS",cb1
	pr #255,using L940: "NET TOTAL ACCOUNTS RECEIVABLE",db1-cb1
	fncloseprn
	close #1: ioerr XIT
XIT: fnxit
L1000: ! AGING ROUTINE
	mat ex=(0)
	if ta(1)=0 then goto L1500
	ta1=ta(1)
L1040: read #2,using L1050,rec=ta1: iv$,mm,dd,yy,tr3,tr5,nta ioerr Ertn
L1050: form pos 6,c 12,3*n 2,x 5,pd 5.2,x 2,n 1,x 21,pd 3
	if tr5=4 or tr5=6 then goto L1210
L1070: if mm=0 then goto L1190
	ag1=mo(mm)+dd+yy*365+int(yy/4)
	if yy-int(yy/4)*4=0 and mm>2 then ag1=ag1+1
	ag2=ag0-ag1
	if ag2>=age(4) then ex(5)=ex(5)+tr3 else goto L1130
	goto L1340
L1130: if ag2>=age(3) then ex(4)=ex(4)+tr3 else goto L1150
	goto L1340
L1150: if ag2>=age(2) then ex(3)=ex(3)+tr3 else goto L1170
	goto L1340
L1170: if ag2>=age(1) then ex(2)=ex(2)+tr3 else goto L1190
	goto L1340
L1190: ex(1)=ex(1)+tr3
	goto L1340
L1210: if ar(5)=2 then goto L1240
	ex(5)=ex(5)-tr3
	goto L1340
L1240: tr3=-tr3
	ta1=ta(1)
L1260: read #2,using L1270,rec=ta1: hv$,mm,dd,yy,tr5,cta ioerr Ertn
L1270: form pos 6,c 12,pos 18,3*n 2,pos 36,n 1,pos 58,pd 3
	if tr5=4 or tr5=6 then goto L1300
	if iv$=hv$ then goto L1070
L1300: if cta=0 then goto L1330
	ta1=cta
	goto L1260
L1330: ex(5)=ex(5)+tr3
L1340: if nta=0 then goto L1370
	ta1=nta
	goto L1040
L1370: if ex(5)>=0 then goto L1400
	ex(4)=ex(4)+ex(5)
	ex(5)=0
L1400: if ex(4)>=0 then goto L1430
	ex(3)=ex(3)+ex(4)
	ex(4)=0
L1430: if ex(3)>=0 then goto L1460
	ex(2)=ex(2)+ex(3)
	ex(3)=0
L1460: if ex(2)>=0 then goto L1500
	ex(1)=ex(1)+ex(2)
	ex(2)=0
	if ex(1)<0 then v6=v6+(-ex(1))
L1500: return 
L1510: mm=int(d1/10000)
	dd=int((d1-mm*10000)/100)
	yy=d1-(mm*10000+dd*100)
	ag0=mo(mm)+dd+yy*365+int(yy/4)
	if yy-int(yy/4)*4=0 and mm>2 then ag0=ag0+1
return 
include: ertn
