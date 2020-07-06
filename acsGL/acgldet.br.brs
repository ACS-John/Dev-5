! Replace S:\acsGL\AcGlDet
! -- Modified Cash Flow Statement (Detailed Transactions, Not Balances)
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,in3$(4)
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
	dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7),tr(7),cap$*128
	dim bm(13),bp(13),by(13),tr$*12,td$*30,ta(2)
 
	fnTop(program$,cap$="Cash Flow Statement - Detail") ! not on menu
	report$=cap$
	fncno(cno,cnam$)
	actpd$=fnactpd$ : _
	actpd=fnactpd : _
	fnfscode : _
	fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit : _
		! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	fscode=fnfscode : _
	priorcd=fnpriorcd
	on fkey 5 goto L1870
	open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,outIn,relative  : _
	read #20,using 'form pos 384,n 2': nap : close #20:
	pors=1
	fnopenprn
	in3$(1)="8,25,N 12.2,UT,N"
	in3$(2)="8,45,N 12.2,UT,N"
	mp1=75
	if fnps=2 then mp1+=3
	fl1$="Name=[Q]\GLmstr\ACGLFNSF.H[cno],KFName=[Q]\GLmstr\agfsidx5.H[cno],Shr"
	if fnps=2 then : _
		fl1$="Name=[Q]\GLmstr\ACGLFNSG.H[cno],KFName=[Q]\GLmstr\agfsidx6.H[cno],Shr"
	nametab=int(44-len(rtrm$(cnam$))/2)
	open #1: fl1$,internal,input,keyed
	open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],Shr",internal,input,relative
	open #4: "Name=[Q]\GLmstr\GLTRANS.H[cno],Shr",internal,outIn,relative
	if fnprocess=1 or fnUseDeptNo=0 then goto L410
	fnTos(sn$="GLInput") : _
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) : _
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
L410: read #1,using L450: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1870
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L410
	if costcntr=0 then goto L450
	if costcntr><fc then goto L410
L450: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	if te$="S" or te$="F" then goto L480
	if heading=0 and te$><"R" then gosub L1740
L480: on pos ("RFHDTSBC",te$,1) goto L1270,L1310,L490,L540,L1170,L1270,L540,L1940 none L410
L490: pr #255,using L500: d$(1:40)
L500: form pos sp,c 40,skip 1
	gosub L1450
	gosub L1380
	goto L410
L540: if te$="B" and ap>0 then goto L1170 ! ENDING BANK BALANCE
	if notrans=1 then goto L950
	if ir>=val(r$) and val(r$)><0 then goto L680
L570: ! Read #2,Using 580: RECORD Eof 940
! Form PD 3
	read #3,using L670: ir,bb,cb,mat by,mat bp,mat bm,mat ta eof L940
	if fnfscode=0 or (fscode=actpd and priorcd=1) then goto L670
	if fnfscode<1 or fnfscode>13 then let fnfscode=1
	if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
	if fnpriorcd=2 then goto L660
	if fnfscode>1 then bb=by(fnfscode-1) else bb=0
	goto L670
L660: if fnfscode>1 then bb=bp(fnfscode-1) else bb=0
L670: form pos mp1,pd 3,pos 81,41*pd 6.2,pos 333,2*pd 3
L680: if ir=val(r$) then total=total+(cb-bb) else goto L920
	if te$="B" then total-=(cb-bb): total-=bb: total2-=bp(nap) : goto L710
	total2=total2+cb
L710: for z=1 to 13
		annualb=annualb+bm(z)
	next z
	if fnfscode=0 then monthb+=bm(fnactpd) else monthb+=bm(fnfscode)
	if fnfscode=0 then goto L760 else goto L800
L760: for j=1 to fnactpd
		ytdb=ytdb+bm(j)
	next j
	goto L840
L800: for j=1 to fnfscode
		ytdb=ytdb+bm(j)
	next j
	goto L840
L840: ! pr DETAILS
	nta=ta(1)
L860: if nta=0 then goto L910
	read #4,using L880,rec=nta,release: mat tr,tr$,td$,nta
L880: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
	if rs=1 then pr #255,using L1100: td$(1:sp2)," ",-tr(5) else pr #255,using L1100: td$(1:sp2),"",tr(5)
	goto L860
L910: goto L570
L920: if ir<val(r$) then goto L570
	if ir>val(r$) then goto L950
L940: notrans=1
L950: for j=1 to 9
		if ac(j)=9 then goto L990
		accum(j,1)=accum(j,1)+total
		accum(j,2)=accum(j,2)+total2
L990: next j
	if rs=1 then total=-total else goto L1030
	total2=-total2
	ytdb=-ytdb
L1030: if ds=1 then dollar$="$" else dollar$=" "
	if total><0 or total2><0 then goto L1070
	if total<>0 then goto L1070
	if ls+ds+ul+ic>0 then goto L1070 else goto L410
L1070: sp2=30-sp-1
	if te$="B" then total=-total: total2=-total2 ! REVERSE SIGN ON BEGINNING BANK BALANCE
	if total=0 and total2=0 and ls+ds+ul+ic>0 then pr #255,using L1100: "",dollar$,0 pageoflow L1600
L1100: form pos sp,c sp2,pos 52,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),skip 1
	total=0
	total2=0
	gosub L1380
	gosub L1610
	gosub L1450
	goto L410
L1170: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=30-sp-1
	pr #255,using L1100: d$(1:sp2),dollar$,accum1 pageoflow L1600
	gosub L1380
	gosub L1610
	gosub L1450
	goto L410
L1270: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1450
	goto L410
L1310: if foot1=1 then goto L1360
	tabnote=sp
	foot1=1
	foot$=d$
	goto L410
L1360: foot$=rtrm$(foot$)&d$
	goto L410
L1380: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1420
		accum(j,1)=0
		accum(j,2)=0
L1420: next j
return
 
L1450: if ls=0 then goto L1580
	if ls=99 then goto L1500
	pr #255,using L1480: " "
L1480: form pos 1,c 1,skip ls
	goto L1580
L1500: fnpglen(pglen)
	if pglen<>42 then pglen=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
	pr #255,using L1540: rtrm$(foot$),"Page "&str$(pt1)
L1540: form skip sk,pos tabnote,c fl,pos 70,c 8,skip 1
	if eofcode=1 then goto L1580
	pr #255: newpage
	gosub L1740
L1580: return
 
L1600: gosub L1500: continue
L1610: if ul=0 then goto L1700
	if ul=1 then goto L1670
	underlin$="=============="
	pr #255:
	goto L1680
	goto L1700
L1670: underlin$="______________"
L1680: pr #255,using L1690: underlin$
L1690: form skip 1,pos 52,2*c 15,skip 1
L1700: ! f REDIR=0 Then pr #255,Using 1560: " "
	form skip 1,c 1,skip 0
return
 
L1740: heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs24 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	pr #255: tab(50);fncch$
	pr #255: tab(56);"       "
	pr #255:
return
 
L1870: eofcode=1
	gosub L1500
	fnfscode(actpd)
	fnpriorcd(1)
	fncloseprn
 
	goto Xit
L1940: !
	pr f "2,5,C 75,N": "ENTER THE FOLLOWING INFORMATION FOR "& rtrm$(d$)
	pr f "6,5,C 70,N": "                    CURRENT         "
	pr f "7,5,C 70,N": "                     MONTH       "
L1980: input fields mat in3$: total conv L1980
	goto L950
 
Xit: fnXit
 
include: Ertn
