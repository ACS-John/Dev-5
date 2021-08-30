autoLibrary
on error goto Ertn
fnTop(program$,"Unbilled Work In Process")
fnopenprn
dim cat$(30)*30,cliprt$*5
dim l$(20,2)*25,l(20,2),s(13)
dim z$*5,cliname$*30,ca(10),ta(25,2),fb(25),empname$*25,scdesc$*30
dim cxno$*5,cna$*30,en$*9,d(8)
dim k$*5,e$*9,b(8),sc$*4,sc2$*6,iv$*12
namtab=66-int(len(rtrm$(env$('cnam')))/2)
fnRead30Categories(mat cat$)
open #8: "Name=S:\Core\Data\acsllc\pedate.h[cno],RecL=20,use,Shr",i,outi,r
if lrec(8)=0 then write #8,using "form pos 1,n 6": d1 else read #8,using "form pos 1,n 6",rec=1,release: dat
open #1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",i,i,k ioerr Ertn
open #2: "Name=S:\Core\Data\acsllc\TMTRAddr.h[cno],Shr",i,i,r ioerr Ertn
open #3: "Name=S:\Core\Data\acsllc\TMTRANS.h[cno],Shr",i,i,r ioerr Ertn
open #4: "Name=S:\Core\Data\acsllc\EMmstr.h[cno],KFName=S:\Core\Data\acsllc\EMIndex.h[cno],Shr",i,i,k ioerr Ertn
goto L460
L270: !
	read #4,using L280,key=e$: empname$ nokey L300 ioerr Ertn
	L280: form pos 10,c 25
return
L300: !
	empname$="EMPLOYEE NOT ON FILE"
	if b(7)><2 then goto L340
	empname$="OTHER CHARGES"
goto L450
L340: !
	if b(7)><3 then goto L370
	empname$="ADJUSTMENT"
	goto L450
L370: if fb(j2)=1 or fb(j2)=2 then goto L410 else goto L380
L380: if b(7)><-3 then goto L450
	empname$="*** WRITE OFF"
	goto L450
L410: if b(7)><-1 then goto L440
	empname$="PARTIAL BILLING"
	goto L450
L440: empname$="*** FINAL BILLED"
L450: return
L460: open #5: "Name=S:\Core\Data\acsllc\SCMSTR.h[cno],KFName=S:\Core\Data\acsllc\SCIndex.h[cno],Shr",i,i,k ioerr Ertn
	goto L530
	read #5,using L490,key=sc$: scdesc$ nokey L510 ioerr Ertn
L490: form pos 5,c 30
	goto L520
L510: scdesc$=" "
L520: return
L530: open #6: "Name=S:\Core\Data\acsllc\Work2.H"&wsid$,internal,input ioerr L550
	close #6,free:
L550: open #6: "Name=S:\Core\Data\acsllc\Work2.H"&wsid$&",SIZE=0,RecL=76",internal,output ioerr Ertn
L560: pr newpage
	in1$(1)="10,46,n 6,ute,n"
	in1$(2)="11,46,n 6,ute,n"
	pr f "10,10,cR 34,n": "ENTER AGING DATE IN MMDDYY FORMAT:"
	pr f "11,10,cR 34,n": "ENTER OLDEST DATE TO PRINT:"
	pr f mat in1$: dat,olddat
	pr f "12,20,c 40": "Press F1 to Continue; F5 to Cancel"
L630: input fields mat in1$: dat,olddat conv L630
	if cmdkey=5 then goto Xit
	if cmdkey<>1 then goto L560
	if dat<10100 or dat>123199 then goto L560
	if olddat<10100 or olddat>123199 then goto L560
	rewrite #8,using "form pos 1,n 6",rec=1: dat
	close #8:
	d7=int(dat/10000)
	d5=int((dat-d7*10000)/100)
	d8=dat-(d5*100+d7*10000)
	if d7>=1 and d7<=12 then goto L750
	goto L560
L750: if d5>=1 and d5<=31 then goto L770
	goto L560
L770: pr newpage
	pr f "10,10,c 57,n": "ENTER 1 IF ALL TRANSACTIONS SHOULD PRINT, ELSE ENTER 2 IF"
	pr f "11,10,c 56,n": "ONLY THOSE THAT HAVE NOT BEEN FINAL BILLED SHOULD PRINT."
L800: input fields "11,68,n 1,ue,n": prtall conv L800
	if prtall<1 or prtall>2 then goto L770
L820: pr newpage
	pr f "10,10,c 43,n": "ENTER 1 TO pr ALL CLIENTS, ELSE ENTER 2"
L840: input fields "10,55,n 1,ue,n": prtcli conv L840
	if prtcli=1 then goto L990
	if prtcli><2 then goto L820
	open #7: "Name=S:\Core\Data\acsllc\Work1.h"&wsid$,internal,input ioerr L890
	close #7,free:
L890: open #7: "Name=S:\Core\Data\acsllc\Work1.h"&wsid$&",SIZE=0,RecL=5",internal,output ioerr Ertn
L900: pr newpage
	pr f "10,10,c 52,n": "ENTER CLIENT NUMBER TO PRINT, ENTER 0 WHEN COMPLETE"
L920: input fields "10,65,n 5,ue,n": cliprt conv L920
	if cliprt=0 then goto L980
	cliprt$=rpad$(trim$(str$(cliprt)),5)
	write #7,using L960: cliprt$
L960: form pos 1,c 5
	goto L900
L980: close #7:
L990: pr newpage
	pr f "10,25,c 50,n": "NOW PRINTING UNBILLED WORK IN PROCESS"
	pr f "23,2,c 30,n": "Press F5 to stop"
	gosub L2600
	if prtcli=1 then goto L1090
	open #7: "Name=S:\Core\Data\acsllc\Work1.h"&wsid$&",NoShr",internal,input ioerr Ertn
L1050: if prtcli=1 then goto L1090
L1060: read #7,using L960: cliprt$ eof L1480 ioerr Ertn
	read #1,using L1100,key=cliprt$: z$,cliname$,pno,mat ca nokey L1060 ioerr Ertn ! READ CLIENT RECORDS
	goto L1110
L1090: read #1,using L1100: z$,cliname$,pno,mat ca eof L1480 ioerr Ertn ! READ CLIENT RECORDS
L1100: form pos 1,c 5,c 30,pos 179,n 9,pos 230,10*pd 3
L1110: for j1=1 to 10
		if ca(j1)=0 then goto L1140
		gosub L1160
L1140: next j1
	goto L1050
L1160: read #2,using L1170,rec=ca(j1): mat ta,mat fb ioerr Ertn ! READ TRANSACTION ADDRESSES
L1170: form pos 1,50*pd 3,25*n 1
	for j2=1 to 25
		if fb(j2)=2 and prtall=1 then goto L1210
		if fb(j2)=2 or fb(j2)=3 then goto L1460
L1210: if ta(j2,1)=0 and ta(j2,2)=0 then goto L1460
		if p1=0 then goto L1250
		gosub L1740 ! pr SECTION HEADING
		gosub L2100 ! CATEGORY HEADING
L1250: nta=ta(j2,1)
L1260: read #3,using L1280,rec=nta: k$,e$,mat b,sc$,iv$,nta,scdesc$ ioerr Ertn ! READ DETAIL TRANS.
		if fndate_mmddyy_to_ccyymmdd(b(4))<fndate_mmddyy_to_ccyymmdd(olddat) then goto L1410
L1280: form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3,c 30
		if b(7)>-1 then goto L1310
		b(3)=-b(3)
L1310: if p1><0 then goto L1360
		gosub L1640 ! PAGE HEADING
		gosub L2100 ! CATEGORY HEADING
		goto L1360
		gosub L1740 ! pr SECTION HEADING
L1360: gosub L270 ! READ EMPLOYEE NAME
		gosub L2270 ! ACCUMULATE EMPLOYEE SUMMARY
		gosub L1940 ! DETERMINE OLDEST DATE
		gosub L2380 ! ACCUMULATE WORK TRANSACTION INFORMATION
		gosub L1790 ! pr DETAIL LINE
L1410: if nta=0 then goto L1430
		goto L1260
L1430: gosub L2010 ! pr TOTAL CATEGORY
		gosub L2130 ! pr EMPLOYEE SUMMARY
		gosub L1890 ! WRITE WORK TRANSACTION
L1460: next j2
	return
L1480: close #1:
	close #2:
	close #3:
	close #4:
	if prtcli=1 then goto L1540
	close #7,free:
L1540: fncloseprn
	chain "S:\acsTM\TMUNAGPT",dat
	close #1: ioerr L1570
L1570: close #2: ioerr L1580
L1580: close #3: ioerr L1590
L1590: close #4: ioerr L1600
L1600: close #5: ioerr L1610
L1610: close #6: ioerr L1620
L1620: close #7: ioerr L1630
L1630: goto Xit
L1640: p1=p1+1 ! pr PAGE HEADING
	pr #255,using L1660: date$,env$('cnam')
L1660: form pos 1,c 8,pos namtab,c 40,skip 1
	pr #255,using L1680: time$,"UNBILLED WORK IN PROCESS REGISTER",p1
L1680: form pos 1,c 8,pos 49,c 33,pos 120,pic(zzzz),skip 1
	pr #255,using L1700: dat
L1700: form pos 62,pic(zz/zz/zz),skip 1
	pr #255:
	pr #255,using L1730: "       EMPLOYEE","DATE     RATE","HOURS     CHARGE"," CATEGORY  SERVICE CODE / DESCRIPTION"
L1730: form pos 12,c 15,pos 40,c 13,pos 56,c 16,pos 73,c 40,skip 1
L1740: numb1=len(rtrm$(cliname$))+at+11
	clen=len(rtrm$(cliname$))
	pr #255,using L1770: z$,rtrm$(cliname$) pageoflow L1830
L1770: form skip 1,pos 1,"*** ",c 5,"-",c clen,pos numb1," ***",skip 1
	return
L1790: if ltrm$(sc$)="0" or ltrm$(sc$)="" then sc2$=" " else sc2$=sc$&" -"
	pr #255,using L1810: e$,empname$,b(4),b(2),b(1),b(3),b(5),"-",b(8),sc2$," ",scdesc$ pageoflow L1830 ! pr DETAIL LINE
L1810: form pos 2,c 9,pos 12,c 25,pos 38,pic(zz/zz/zz),n 8.2,n 7.2,n 11.2,x 2,n 2,x 1,c 1,n 2,x 2,c 6,c 1,c 30,skip 1
	goto L1860
L1830: pr #255: newpage
	gosub L1640
	continue
L1860: g1=g1+b(1)
	g2=g2+b(3)
	return
L1890: if fb(j2)>1 then goto L1920 ! WRITE WORK TRANSACTION
	write #6,using L1910: cxno$,cna$,en$,mat d
L1910: form pos 1,c 5,c 30,c 9,n 2,n 6,pd 4.2,5*pd 4.2
L1920: mat d=(0)
	return
L1940: d3=int(b(4)/100)+(b(4)-int(b(4)/100)*100)*10000 ! DETERMINE OLDEST DATE
	if d(2)><0 then goto L1980
	d(2)=d3
	goto L2000
L1980: if d(2)<=d3 then goto L2000
	d(2)=d3
L2000: return
L2010: pr #255,using L2020: "TOTAL ",cat$(j1),g1,g2 pageoflow L2040 ! pr TOTAL CATEGORY
L2020: form skip 1,pos 12,c 6,c 30,pos 53,n 8.2,pos 61,n 11.2,skip 1
	goto L2070
L2040: pr #255: newpage
	gosub L1640
	continue
L2070: g1=0
	g2=0
	return
L2100: pr #255,using L2110: cat$(j1) ! pr CATEGORY HEADING
L2110: form pos 3,c 30,skip 1
	return
L2130: if l(1,1)=0 and l(1,2)=0 then goto L2240 ! pr EMPLOYEE SUMMARY
	pr #255,using L2150: "SUMMARY BY EMPLOYEE:"
L2150: form pos 12,c 20,skip 1
	for j=1 to 20
		if l(j,1)=0 and l(j,2)=0 then goto L2240
		pr #255,using L2190: l$(j,1)(1:9),l$(j,2),l(j,1),l(j,2) pageoflow L2210
L2190: form pos 17,c 9,x 1,c 25,pos 53,n 8.2,n 11.2,skip 1
		goto L2230
L2210: pr #255: newpage
		gosub L1640
L2230: next j
L2240: mat l$=(" ")
	mat l=(0)
return
L2270: for j=1 to 20 ! ACCUMULATE EMPLOYEE SUMMARY INFORMATION
		if b(7)<0 then goto L2370
		if l$(j,2)=empname$ then goto L2350
		if l(j,1)=0 and l(j,2)=0 then goto L2330
	next j
	goto L2370
L2330: l$(j,1)=e$
	l$(j,2)=empname$
L2350: l(j,1)=l(j,1)+b(1)
	l(j,2)=l(j,2)+b(3)
L2370: return
L2380: e7=int(b(4)/10000) ! ACCUMULATE INFORMATION FOR WORK TRANSACTIONS
	e5=int((b(4)-e7*10000)/100)
	e8=b(4)-(e5*100+e7*10000)
	f6=(d8-e8)*s(13)+(s(d7)-s(e7))+(d5-e5)
	if f6<30 then goto L2460
	if f6>30 and f6<=60 then goto L2480
	if f6>60 and f6<=90 then goto L2500
	if f6>90 then goto L2520
L2460: d(5)=d(5)+b(3)
	goto L2530
L2480: !
	d(6)=d(6)+b(3)
	goto L2530
L2500: !
	d(7)=d(7)+b(3)
	goto L2530
L2520: !
	d(8)=d(8)+b(3)
L2530: !
	d(4)=d(4)+b(3)
	d(3)=d(3)+b(1)
	cxno$=z$
	cna$=cliname$
	en$=lpad$(rtrm$(str$(pno)),5)
	d(1)=j1
	return
L2600: !
	if int(d8/4)*100=int(d8/4*100) then  ! INITIAL AGING PERIOD SETUP
		at3=1
	else
		at3=0
	end if
L2640: !
	s(1)=0
	s(2)=31
	s(3)=59+at3
	s(4)=90+at3
	s(5)=120+at3
	s(6)=151+at3
	s(7)=181+at3
	s(8)=212+at3
	s(9)=243+at3
	s(10)=273+at3
	s(11)=304+at3
	s(12)=334+at3
	s(13)=365+at3
return

Xit: fnXit

include: ertn