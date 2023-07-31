! Replace S:\acsGL\AcGlInc6b
! -- INCOME STATEMENT WITH BUDGET (month compared to month and year compared to year to date
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,p$(20)*50
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
	dim b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,6)
	dim pedat$*20,actpd$*6,bm(13),bp(13),by(13),cap$*128
 
	fnTop(program$,cap$="Income Statement-Monthly & Year Budgets")
	on fkey 5 goto L2160
	actpd=fnactpd
	actpd$=fnactpd$
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit 		! sets fnPs,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
 
	pr newpage
	pors=1
	mp1=69
	if fnPs=2 then mp1=mp1+3
	fl1$="Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr"
	if fnPs=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr"
	open #1: fl1$,i,i,k
	if fnProcess=1 or fnUseDeptNo=0 then goto L390
	fnTos(sn$="ACglincb") : _
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,'30',0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) : _
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
L390: costcntr=val(resp$(1))
	report$="STATEMENT OF INCOME AND EXPENSES"
	fnOpenPrn
	redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
	fnFsIndexIncStmt
	open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[temp]\fsindex.h[cno],Shr",i,i,k
L500: read #1,using L550: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2160
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L500
	if costcntr=0 then goto L550
	if fc=0 and te$="F" then goto L560 ! 5/08/1989
	if costcntr><fc then goto L500
L550: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
L560: if te$="S" or te$="F" then goto L580
	if heading=0 and te$><"R" then gosub L2030
L580: on pos ("RFHDTS",te$,1) goto L1490,L1530,L590,L640,L1330,L1490 none L500
L590: pr #255,using L600: d$(1:40)
L600: form pos sp,c 40,skip 1
	gosub L1710
	gosub L1600
	goto L500
L640: if notrans=1 then goto L950
	if ir>=val(r$) and val(r$)><0 then goto L770
L660: ! read amounts from gl master file
L670: read #3,using L760: ir,bb,cb,mat by,mat bp,mat bm eof L940
	if ir=0 then goto L670 ! skip accounts with no income reference #
	if fnfscode=0 then goto L760
	if fnfscode<1 or fnfscode>13 then let fnfscode=1
	if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
	if fnpriorcd=2 then goto L750
	if fnfscode>1 then bb=by(fnfscode-1) else bb=0
	goto L760
L750: if fnfscode>1 then bb=bp(fnfscode-1) else bb=0
L760: form pos mp1,pd 3,pos 81,41*pd 6.2
L770: if ir=val(r$) then total=total+(cb-bb) else goto L920
	total2=total2+cb
	for z=1 to 13
		annualb=annualb+bm(z)
	next z
	if fnfscode=0 then monthb=monthb+bm(actpd) else monthb=monthb+bm(fnfscode) ! 11/24/86
	if fnfscode=0 then goto L840 else goto L880 ! 11/24/86
L840: for j=1 to actpd
		ytdb=ytdb+bm(j)
	next j
	goto L660
L880: for j=1 to fnfscode ! 11/24/86
		ytdb=ytdb+bm(j) ! 11/24/86
	next j ! 11/24/86
	goto L660 ! 11/24/86
L920: if ir<val(r$) then goto L660
	if ir>val(r$) then goto L950
L940: notrans=1
L950: overundr=ytdb-total2
	oumonth=monthb-total
! uNEXPEND=ANNUALB-TOTAL2
	for j=1 to 9
		if ac(j)=9 then goto L1060 ! 10/14/87
		accum(j,1)=accum(j,1)+monthb
		accum(j,2)=accum(j,2)+total
		accum(j,3)=accum(j,3)+oumonth
		accum(j,4)=accum(j,4)+ytdb
		accum(j,5)=accum(j,5)+total2
		accum(j,6)=accum(j,6)+overundr
L1060: next j
	if rs=1 then total=-total else goto L1140
	total2=-total2
	annualb=-annualb
	monthb=-monthb
	ytdb=-ytdb
	overundr=overundr
	unexpend=unexpend
L1140: if ds=1 then dollar$="$" else dollar$=" "
	goto L1190 ! pr all accounts even if zero balance  (if budget ever nets to zero, it messes the monthly budget column up
	if annualb><0 or total2><0 then goto L1190
	if total then goto L1190
	if ls+ds+ul+ic>0 then goto L1190 else goto L500
L1190: sp2=26-sp-1
	if ul=1 then pr #255,using L1211: d$(1:sp2),dollar$,"{\UL ",monthb,"}",dollar$,"{\UL ",total,"}",dollar$,"{\UL ",oumonth,"}",dollar$,"{\UL ",ytdb,"}",dollar$,"{\UL ",total2,"}",dollar$,"{\UL ",overundr,"}" pageoflow L1890 : goto L1210
	pr #255,using L1210: d$(1:sp2),dollar$,monthb,dollar$,total,dollar$,oumonth,dollar$,ytdb,dollar$,total2,dollar$,overundr pageoflow L1890
L1210: form pos sp,c sp2,pos 26,c 1,n 13.2,x 1,c 1,n 11.2,x 1,c 1,n 11.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,skip redir
L1211: form pos sp,c sp2,pos 26,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 11.2,c 1,x 1,c 1,c 5,n 11.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 13.2,c 1,skip redir
	total=0
	total2=0
	annualb=0
	oumonth=0
	monthb=0
	ytdb=0
	overundr=0
	gosub L1600
	if ul=1 then goto L1310
	gosub L1900
L1310: gosub L1710
	goto L500
L1330: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
	if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
	if rs=1 then accum5=-accum(ap,5) else accum5=accum(ap,5)
	if rs=1 then accum6=accum(ap,6) else accum6=accum(ap,6)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=26-sp-1
	if ul=1 then pr #255,using L1211: d$(1:sp2),dollar$,"{\UL ",accum1,"}",dollar$,"{\UL ",accum2,"}",dollar$,"{\UL ",accum3,"}",dollar$,"{\UL ",accum4,"}",dollar$,"{\UL ",accum5,"}",dollar$,"{\UL ",accum6,"}" pageoflow L1890 : goto L1440
	pr #255,using L1210: d$(1:sp2),dollar$,accum1,dollar$,accum2,dollar$,accum3,dollar$,accum4,dollar$,accum5,dollar$,accum6 pageoflow L1890
L1440: ft1=0
	gosub L1600
	if ul=1 then goto L1470
	gosub L1900
L1470: gosub L1710
	goto L500
L1490: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1710
	goto L500
L1530: if foot1=1 then goto L1580
	tabnote=sp
	foot1=1
	foot$=d$
	goto L500
L1580: foot$=rtrm$(foot$)&d$
	goto L500
L1600: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1690 ! 10/14/87
		accum(j,1)=0
		accum(j,2)=0
		accum(j,3)=0
		accum(j,4)=0
		accum(j,5)=0
		accum(j,6)=0
L1690: next j
return
L1710: if ls=0 then goto L1870
	if ls=99 then goto L1760
	pr #255,using L1740: " "
L1740: form pos 1,c 1,skip ls
	goto L1870
L1760: ! If FT1=1 Then Goto 1870
	fnPgLen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1820: rtrm$(foot$),"Page "&str$(pt1)
L1820: form skip sk,pos tabnote,c fl,pos 100,c 8,skip 1
! ft1=1
	if eofcode=1 then goto L1870
	pr #255: newpage
	gosub L2030
L1870: return
 
L1890: gosub L1760: continue
L1900: if ul=0 then goto L1990
	if ul=1 then goto L1960
	underlin$="=============="
	goto L1970
	goto L1990
L1960: underlin$="______________"
L1970: pr #255,using L1980: underlin$,underlin$(1:12),underlin$(1:12),underlin$,underlin$,underlin$
L1980: form pos 26,c 15,2*c 13,4*c 15,skip redir
L1990: if redir=0 then pr #255,using L2000: " " pageoflow L1890
L2000: form c 1
return
 
L2030: heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	pr #255: tab(31);"<-------";fncch$;"------>";tab(71);" <------------YEAR TO DATE ------------>"
	pr #255: tab(34);"BUDGET";tab(45);"ACTIVITY";tab(57);"OVER/UNDER";tab(75);"BUDGET";tab(87);"ACTIVITY";tab(101);"OVER/UNDER"
	pr #255:
return
 
L2160: eofcode=1
	gosub L1760
 
 
	fnClosePrn
 
Xit: fnXit
 
include: ertn
