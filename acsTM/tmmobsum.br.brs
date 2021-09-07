	on error goto Ertn
	autoLibrary
	fnTop(program$,cap$="Monthly Billing Summary")
	fnconsole(1)
	fndat(dat$)
	def fn_cd(x)=(x-int(x*.01)*100)*10000+int(x*.01) ! /r
	dim cat$(30)*30,dat$*20,cliname$*30,enam$*25
	dim z$*5,ca(10),ta(25,2),fb(25),dat$*20,cap$*128
	dim k$*5,e$*9,b(8),sc$*4,iv$*12,tempiv$*12
	dim x$(10)*30,c$(50)*25,d(50),e(50),f(50),g(10),h(10),i(10)
	io1$(1)="10,44,C 20,U,N"
	io1$(2)="12,58,N 6,U,N"
	pr newpage
	pr f "10,5,c 35,n": "ENTER MONTHLY BILLING SUMMARY DATE:"
	pr f "12,5,C 60": "ENTER THE OLDEST BILLING DATE TO pr OR 0 FOR ALL:"
	pr f io1$(1): dat$
	pr f "23,2,c 30,n": "Press F5 to stop"
L200: input fields mat io1$: dat$,od conv L200
	if cmdkey=5 then goto Xit
	fndat(dat$,put=2)
	if od=0 or (od>10111 and od<123200) then goto L240 else goto L200
L240: fnopenprn
	pr newpage
	dattab=66-int(len(rtrm$(dat$))/2)
	pr f "10,20,c 60,n": "MONTHLY BILLING SUMMARY IN PROCESS"
	pr f "23,2,c 30,n": "Press F5 to stop"
	dattab=66-int(len(rtrm$(dat$))/2)
	dattab2=43-int(len(rtrm$(dat$))/2)
	namtab=66-int(len(rtrm$(env$('cnam')))/2)
	namtab2=43-int(len(rtrm$(env$('cnam')))/2)
	fnRead30Categories(mat cat$)
	gosub L1020
	open #1: "Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\Client-Idx.h[cno],Shr",i,i,k
	open #2: "Name=S:\Core\Data\acsllc\TMTRAddr.h[cno],Shr",i,i,r
	open #3: "Name=S:\Core\Data\acsllc\TMTRANS.h[cno],Shr",i,i,r
	open #4: "Name=S:\Core\Data\acsllc\EMmstr.h[cno],KFName=S:\Core\Data\acsllc\EMIndex.h[cno],Shr",i,i,k
	goto L480
L430: read #4,using L440,key=lpad$(str$(pno),9): enam$ nokey L460
L440: form pos 10,c 25
	goto L470
L460: enam$=" "
L470: return
L480: read #1,using L490: z$,cliname$,pno,mat ca eof L870
L490: form pos 1,c 5,c 30,pos 179,n 9,pos 230,10*pd 3
	for j1=1 to 10
		if ca(j1)=0 then goto L530
		gosub L550
L530: next j1
	goto L480
L550: read #2,using L560,rec=ca(j1): mat ta,mat fb
L560: form pos 1,50*pd 3,25*n 1
	for j2=1 to 25
		if fb(j2)>=1 and fb(j2)<=3 then goto L600
		goto L800
L600: nta=ta(j2,1)
		iv$=" "
		hrs=0
		std=0
		bil=0
L650: read #3,using L680,rec=nta: k$,e$,mat b,sc$,tempiv$,nta
		if b(7)=-1 and fn_cd(b(4))<fn_cd(od) then goto L800
		if b(7)<0 then iv$=tempiv$
L680: form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3
		if fb(j2)=2 or fb(j2)=3 then goto L720
		ast$="*"
		goto L730
L720: ast$=" "
L730: if b(7)>=0 then goto L760
		bil=bil+b(3)
		if b(7)<0 then goto L780
L760: hrs=hrs+b(1)
		std=std+b(3)
L780: if nta><0 then goto L650
		gosub L820
L800: next j2
	return
L820: gosub L430
	gosub L1110
	gosub L1590
	gosub L1710
	return
L870: close #1:
	close #2:
	close #3:
	close #4:
	gosub L1430
	gosub L1490
	gosub L1230
	gosub L1830
	goto L1000
	close #1: ioerr L970
L970: close #2: ioerr L980
L980: close #3: ioerr L990
L990: close #4: ioerr L1000
L1000: fncloseprn
Xit: fnXit
L1020: pr #255,using L1030: date$,env$('cnam'),time$,"MONTHLY BILLING SUMMARY"
L1030: form skip 3,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 55,c 23,skip 1
	pr #255,using L1050: dat$
L1050: form pos dattab,c 20,skip 2
	pr #255,using L1070: "CLIENT NAME","CATEGORY  HOURS","AT","BILLING","GAIN OR     % PARTIAL TYPE OF SERVICE","PARTNER IN CHARGE      INVOICE"
L1070: form pos 6,c 11,pos 25,c 15,pos 46,c 2,pos 53,c 7,pos 62,c 37,pos 102,c 30,skip 1
	pr #255,using L1090: "BILLED   STANDARD","LOSS","BILL ","NUMBER"
L1090: form pos 34,c 17,pos 64,c 4,pos 77,c 5,pos 126,c 6,skip 2
	return
L1110: if std=0 then goto L1180
	pr #255,using L1130: cliname$(1:26),b(5)," -",b(8),hrs,std,bil,bil-std,(bil-std)/std*100,"%",ast$,cat$(j1)(1:17),enam$(1:17),iv$ pageoflow L1150
L1130: form pos 1,c 26,pos 27,n 2,c 2,n 2,n 7.2,n 11.2,pos 51,n 9.2,n 9.2,n 6,pos 75,c 1,x 2,c 1,pos 84,c 17,pos 102,c 17,pos 120,c 12,skip 1
	goto L1190
L1150: pr #255: newpage
	gosub L1020
	goto L1190
L1180: pr #255,using L1130: cliname$(1:26),b(5),"-",b(8),hrs,std,bil,bil-std,0," ",ast$,cat$(j1)(1:17),enam$(1:17),iv$ pageoflow L1150
L1190: l1=l1+hrs
	m1=m1+std
	n1=n1+bil
	return
L1230: for y=1 to 50
		if rtrm$(c$(y))="" then goto L1370
		if c$(y)="-1" then goto L1270
		goto L1280
L1270: c$(y)="UNASSIGNED"
L1280: if e(y)=0 then goto L1320
		pr #255,using L1300: c$(y),d(y),e(y),f(y),f(y)-e(y),(f(y)-e(y))/e(y)*100,"%"
L1300: form pos 1,c 25,pos 26,n 10.2,pos 36,n 12.2,pos 49,n 11.2,pos 60,n 10.2,pos 70,n 6,pos 77,c 1,skip 1 ! 2/17/88
		goto L1330
L1320: pr #255,using L1300: c$(y),d(y),e(y),f(y),f(y),0,"%"
L1330: w=w+d(y)
		x=x+e(y)
		z=z+f(y)
	next y
L1370: if x=0 then goto L1410
	pr #255,using L1390: "  FINAL TOTALS",w,x,z,z-x,(z-x)/x*100,"%"
L1390: form skip 1,pos 1,c 15,pos 27,n 9.2,pos 37,n 11.2,pos 49,n 11.2,pos 61,n 10.2,pos 71,pic(------),pos 77,c 1,skip 1 ! 2/17/88
	goto L1420
L1410: pr #255,using L1390: "  FINAL TOTALS",w,x,z,z-x,0," "
L1420: return
L1430: if m1=0 then goto L1470
	pr #255,using L1450: "  FINAL TOTALS",l1,m1,n1,n1-m1,(n1-m1)/m1*100,"%"
L1450: form skip 2,pos 1,c 14,pos 31,n 9.2,pos 41,n 10.2,pos 51,n 9.2,n 9.2,n 6,c 1,skip 1 ! 2/17/88
	goto L1480
L1470: pr #255,using L1450: "  FINAL TOTALS",l1,m1,n1,n1,0," "
L1480: return
L1490: pr #255: newpage
	pr #255,using L1510: env$('cnam'),"BILLING ANALYSIS BY PARTNER"
L1510: form skip 3,pos namtab2,c 40,skip 1,pos 29,c 28,skip 1
	pr #255,using L1530: dat$
L1530: form pos dattab2,c 20,skip 2
	pr #255,using L1550: "PARTNER NAME","HOURS","AT","BILLING","GAIN OR     %"
L1550: form pos 7,c 12,pos 31,c 5,pos 43,c 2,pos 53,c 7,pos 64,c 13,skip 1
	pr #255,using L1570: "BILLED    STANDARD","LOSS"
L1570: form pos 30,c 18,pos 66,c 4,skip 2
	return
L1590: if rtrm$(enam$)><"" then goto L1610
	enam$="-1"
L1610: for y=1 to 50
		if c$(y)=enam$ then goto L1670
		if rtrm$(c$(y))="" then goto L1660
	next y
	goto L1700
L1660: c$(y)=enam$
L1670: f(y)=f(y)+bil
	d(y)=d(y)+hrs
	e(y)=e(y)+std
L1700: return
L1710: if rtrm$(cat$(j1))><"" then goto L1730
	cat$(j1)="-1"
L1730: for x7=1 to 10
		if x$(x7)=cat$(j1) then goto L1790
		if rtrm$(x$(x7))="" then goto L1780
	next x7
	goto L1820
L1780: x$(x7)=cat$(j1)
L1790: g(x7)=g(x7)+bil
	h(x7)=h(x7)+hrs
	i(x7)=i(x7)+std
L1820: return
L1830: pr #255: newpage
	pr #255,using L1510: env$('cnam'),"BILLING ANALYSIS BY CATEGORY"
	pr #255,using L1530: dat$
	pr #255,using L1870: "CATEGORY NAME","HOURS","AT","BILLING","GAIN OR     %"
L1870: form pos 6,c 13,pos 31,c 5,pos 43,c 2,pos 53,c 7,pos 64,c 13,skip 1
	pr #255,using L1890: "BILLED    STANDARD","LOSS"
L1890: form pos 30,c 18,pos 66,c 4,skip 2
	for x7=1 to 10
		if rtrm$(x$(x7))="" then goto L2030
		if x$(x7)="-1" then goto L1940
		goto L1950
L1940: x$(x7)="UNASSIGNED"
L1950: if i(x7)><0 then goto L1980
		pr #255,using L1300: x$(x7)(1:25),h(x7),i(x7),g(x7),g(x7),0,"%"
		goto L1990
L1980: pr #255,using L1300: x$(x7)(1:25),h(x7),i(x7),g(x7),g(x7)-i(x7),(g(x7)-i(x7))/i(x7)*100,"%"
L1990: w1=w1+g(x7)
		x1=x1+h(x7)
		y1=y1+i(x7)
	next x7
	L2030: !
	if y1=0 then goto L2060
	pr #255,using L1390: "   FINAL TOTALS",x1,y1,w1,w1-y1,(w1-y1)/y1*100,"%"
	goto L2070
L2060: pr #255,using L1390: "   FINAL TOTALS",x1,y1,w1,w1-y1,0," "
	L2070: !
return
include: ertn
 
