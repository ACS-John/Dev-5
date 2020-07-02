! Replace S:\acsGL\acglCas5
! CASH FLOW STATEMENT  WITH BUDGET
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,cch$*20 ,in3$(4),cap$*128,udf$*256
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
	dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7)
	dim pedat$*20,actpd$*6,bm(13),d(2),bp(13),by(13)
 
	fnTop(program$,cap$="Cash Flow with Budget")
	fncno(cno,cnam$)
	udf$=env$('temp')&'\'
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit : _
		! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	actpd$=fnactpd$
	pedat$=fnpedat$
	actpd$=fnactpd$
	actpd=fnactpd
	fscode=fnfscode
	priorcd=fnpriorcd
	open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative  : _
	read #20,using 'Form Pos 384,n 2',rec=1: nap : close #20:
	if nap<12 or nap> 13 then nap=12
	in3$(1)="8,5,N 12.2,UT,N" : in3$(2)="8,25,N 12.2,UT,N" : _
	in3$(3)="8,45,N 12.2,UT,N" : in3$(4)="8,65,N 12.2,UT,N"
	mp1=75
	if ps=2 then mp1=mp1+3
	fl1$="Name=[Q]\GLmstr\ACGLFNSF.h[cno],KFName=[Q]\GLmstr\agfsidx5.h[cno],Shr"
	if ps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSG.h[cno],KFName=[Q]\GLmstr\agfsidx6.h[cno],Shr"
	pr newpage
	pr f "10,20,C 30,h,n": "CASH FLOW STATEMENT IN PROCESS"
	on fkey 5 goto L2130
	fnopenprn
	open #1: fl1$,internal,input,keyed
	if process=1 or d(1)=0 then goto L390
	pr newpage
	pr f "10,5,C 75,": "ENTER THE COST CENTER OR DEPT # IF YOU WISH TO ONLY pr A STATEMENT"
L370: pr f "11,5,C 65": "ON ONE DEPARTMENT; ELSE ENTER 0 TO pr ALL DEPARTMENTS"
	input fields "11,70,N 3,ue,N": costcntr conv L370
L390: pr newpage
	pr f "10,1,Cc 80,N": "Printing: please wait..."
	pr f "12,2,c 30,B,5": "Press F5 to stop"
	if fnps=2 then goto L450 ! secondary
	execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 75 3 Replace DupKeys -N"
	goto L460
L450: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 78 3 Replace DupKeys -N"
L460: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed
L470: read #1,using L510: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2130
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L470
	if costcntr=0 then goto L510
	if costcntr><fc then goto L470
L510: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	if te$="S" or te$="F" then goto L540
	if heading=0 and te$><"R" then gosub L1980
L540: on pos ("RFHDTSBC",te$,1) goto L1520,L1570,L550,L610,L1350,L1520,L610,L2180 none L470
L550: pr #255,using L560: d$(1:40)
L560: form pos sp,c 40
	gosub L1720
	gosub L1660
	goto L470
 
L610: if te$="B" and ap>0 then goto L1350 ! ENDING BANK BALANCE
	if notrans=1 then goto L960
	if ir>=val(r$) and val(r$)><0 then goto L750
L640: ! read amounts from gl master file
L650: read #3,using L740: ir,bb,cb,mat by,mat bp,mat bm eof L950
	if ir=0 then goto L650
	if fscode=0 then goto L740
	if fscode<1 or fscode>13 then fscode=1
	if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
	if priorcd=2 then goto L730
	if fscode>1 then bb=by(fscode-1) else bb=0
	goto L740
L730: if fscode>1 then bb=bp(fscode-1) else bb=0
L740: form pos mp1,pd 3,pos 81,41*pd 6.2
L750: if ir=val(r$) then total=total+(cb-bb) else goto L930
	if te$="B" then total=total-(cb-bb): total=total - bb: total2=total2-bp(nap) : goto L780
	total2=total2+cb
L780: for z=1 to 13
		annualb=annualb+bm(z)
	next z
	if fscode=0 then monthb=monthb+bm(actpd) else monthb=monthb+bm(fscode)
	if fscode=0 then goto L830 else goto L880
L830: for j=1 to actpd
		ytdb=ytdb+bm(j)
	next j
	goto L640
 
L880: for j=1 to fscode
		ytdb=ytdb+bm(j)
	next j
	goto L640
 
L930: if ir<val(r$) then goto L640
	if ir>val(r$) then goto L960
L950: notrans=1
L960: overundr=ytdb-total2
	unexpend=annualb-total2
	for j=1 to 9
		if ac(j)=9 then goto L1070
		accum(j,1)=accum(j,1)+total
		accum(j,2)=accum(j,2)+total2
		accum(j,3)=accum(j,3)+annualb
		accum(j,4)=accum(j,4)+monthb
		accum(j,5)=accum(j,5)+ytdb
		accum(j,6)=accum(j,6)+overundr
		accum(j,7)=accum(j,7)+unexpend
L1070: next j
	if rs=1 then total=-total else goto L1150
	total2=-total2
	annualb=-annualb
	monthb=-monthb
	ytdb=-ytdb
	overundr=overundr
	unexpend=unexpend
L1150: if ds=1 then dollar$="$" else dollar$=" "
	if annualb><0 or total2><0 then goto L1190
	if total<>0 then goto L1190
	if ls+ds+ul+ic>0 then goto L1190 else goto L470
L1190: sp2=24-sp-1
	if te$="B" then total=-total: total2=-total2 ! REVERSE SIGN ON BEGINNING BANK BALANCE
	pr #255,using L1220: d$(1:sp2),dollar$,monthb,dollar$,total,dollar$,total2,dollar$,ytdb,dollar$,annualb
L1220: form pos sp,c sp2,pos 24,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),skip 0
	total=0
	total2=0
	annualb=0
	monthb=0
	ytdb=0
	overundr=0
	unexpend=0
	gosub L1660
	gosub L1850
	gosub L1720
	goto L470
 
L1350: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
	if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
	if rs=1 then accum5=-accum(ap,5) else accum5=accum(ap,5)
	if rs=1 then accum6=accum(ap,6) else accum6=accum(ap,6)
	if rs=1 then accum7=accum(ap,7) else accum7=accum(ap,7)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=24-sp-1
	if te$="B" then accum3=accum4=0
	pr #255,using L1220: d$(1:sp2),dollar$,accum4,dollar$,accum1,dollar$,accum2,dollar$,accum5,dollar$,accum3
	gosub L1660
	gosub L1850
	gosub L1720
	goto L470
 
L1520: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1720
	goto L470
 
L1570: if foot1=1 then goto L1630
	tabnote=sp
	foot1=1
	foot$=d$
	goto L470
 
L1630: foot$=rtrm$(foot$)&d$
	goto L470
 
L1660: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1690
		for j2=1 to 7 : accum(j,j2)=0 : next j2
L1690: next j
return
 
L1720: if ls=0 then goto L1830
	if ls=99 then goto L1770
	pr #255,using L1750: " "
L1750: form pos 1,c 1,skip ls
	goto L1830
L1770: sk=62-krec(255) : _
	fl=len(rtrm$(foot$))
	pr #255,using L1790: rtrm$(foot$)
L1790: form skip sk,pos tabnote,c fl
	if eofcode=1 then goto L1830
	pr #255: newpage
	gosub L1980
L1830: return
 
L1850: if ul=0 then goto L1940
	if ul=1 then goto L1910
	underlin$="=============="
	pr #255:
	goto L1920
	goto L1940
L1910: underlin$="______________"
L1920: pr #255,using L1930: underlin$,underlin$,underlin$,underlin$,underlin$
L1930: form skip 0,pos 24,5*c 15,skip 0
L1940: pr #255,using L1950: " "
L1950: form skip 1,c 1,skip 0
return
 
L1980: heading=1
	pr #255: ""
	pr #255,using L2020: cnam$
	pr #255,using L2020: rtrm$(report$)
L2020: form pos 1,cc 80
	if rtrm$(secondr$)="" then goto L2050
	pr #255,using L2020: rtrm$(secondr$)
L2050: pr #255,using L2020: "For the "&rtrm$(actpd$)&" month period ended "&rtrm$(pedat$)
	pr #255: ""
	pr #255: ""
	pr #255: tab(31);"Monthly";tab(38);cch$;tab(61);"Year To";tab(77);"Budget";tab(92);"Annual"
	pr #255: tab(32);"Budget";tab(44);"       ";tab(62);"Date";tab(77);"To Date";tab(92);"Budget"
	pr #255: ""
return
 
L2130: eofcode=1
	gosub L1770
	fncloseprn
	goto Xit
 
L2180: pr newpage
	pr f "2,5,C 75,N": "Enter the Following Information for "& rtrm$(d$)
	pr f "6,5,C 70,N": "Monthly             Current             Year To             Annual"
	pr f "7,5,C 70,N": "Budget               Month                Date              Budget"
L2220: input fields mat in3$: monthb,total,total2,annualb conv L2220
	pr newpage
	goto L960
 
Xit: fnXit
 
include: Ertn
