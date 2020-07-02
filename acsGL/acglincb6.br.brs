! Replace S:\acsGL\AcGlIncB6
! -- INCOME STATEMENT WITH BUDGET (six columns)
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,p$(20)*50
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
	dim b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7)
	dim pedat$*20,actpd$*6,bm(13),bp(13),by(13),cap$*128,udf$*256
 
	fnTop(program$,cap$="Six Column Income Statement with Budget")
	on fkey 5 goto L2350
	fncno(cno)
	udf$=env$('temp')&'\'
	actpd=fnactpd
	actpd$=fnactpd$
	pedat$=rtrm$(fnpedat$)
	x=pos(pedat$," ",1)
	curmonth$=pedat$(1:x)
	curyear$=pedat$(len(rtrm$(pedat$))-4:len(rtrm$(pedat$)))
	curyear=val(curyear$) conv L230
L230: prioryr=curyear-1
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
 
	pr newpage
	pors=1
	mp1=69
	if fnps=2 then mp1=mp1+3
	fl1$="Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr"
	if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr"
	open #1: fl1$,internal,input,keyed
	if fnprocess=1 or fnUseDeptNo=0 then goto L450
	fnTos(sn$="ACglincb") : _
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) : _
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
L450: costcntr=val(resp$(1))
	report$="STATEMENT OF INCOME AND EXPENSES"
	fnopenprn
	redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
	if fnps=2 then goto L540 ! secondary
	execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 69 3 Replace DupKeys -N"
	goto L550
L540: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 72 3 Replace DupKeys -N"
L550: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed
L560: read #1,using L610: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2350
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L560
	if costcntr=0 then goto L610
	if fc=0 and te$="F" then goto L620 ! 5/08/1989
	if costcntr><fc then goto L560
L610: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
L620: if te$="S" or te$="F" then goto L640
	if heading=0 and te$><"R" then gosub L2190
L640: on pos ("RFHDTS",te$,1) goto L1640,L1680,L650,L700,L1430,L1640 none L560
L650: pr #255,using L660: d$(1:40)
L660: form pos sp,c 40,skip 1
	gosub L1860
	gosub L1750
	goto L560
L700: if notrans=1 then goto L1010
	if ir>=val(r$) and val(r$)><0 then goto L830
L720: ! read amounts from gl master file
L730: read #3,using L820: ir,bb,cb,mat by,mat bp,mat bm eof L1000
	if ir=0 then goto L730 ! skip accounts with no income reference #
	if fnfscode=0 or (fnfscode=actpd and fnpriorcd=1) then goto L820
	if fnfscode<1 or fnfscode>13 then let fnfscode=1
	if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
	if fnpriorcd=2 then goto L810
	if fnfscode>1 then bb=by(fnfscode-1) else bb=0
	goto L820
L810: if fnfscode>1 then bb=bp(fnfscode-1) else bb=0
L820: form pos mp1,pd 3,pos 81,41*pd 6.2
L830: if ir=val(r$) then total=total+(cb-bb) else goto L980
	total2=total2+cb
	for z=1 to 13
		annualb=annualb+bm(z)
	next z
	if fnfscode=0 then pmonth=pmonth+bm(actpd) else pmonth=pmonth+bm(fnfscode) ! 11/24/86
	if fnfscode=0 then goto L900 else goto L940 ! 11/24/86
L900: for j=1 to actpd
		lastyr=lastyr+bm(j)
	next j
	goto L720
L940: for j=1 to fnfscode ! 11/24/86
		lastyr=lastyr+bm(j) ! 11/24/86
	next j ! 11/24/86
	goto L720 ! 11/24/86
L980: if ir<val(r$) then goto L720
	if ir>val(r$) then goto L1010
L1000: notrans=1
L1010: overundr=lastyr-total2
	unexpend=annualb-total2
	for j=1 to 9
		if ac(j)=9 then goto L1120 ! 10/14/87
		accum(j,1)=accum(j,1)+total
		accum(j,2)=accum(j,2)+total2
		accum(j,3)=accum(j,3)+annualb
		accum(j,4)=accum(j,4)+pmonth
		accum(j,5)=accum(j,5)+lastyr
		accum(j,6)=accum(j,6)+overundr
		accum(j,7)=accum(j,7)+unexpend
L1120: next j
	if rs=1 then total=-total else goto L1200
	total2=-total2
	annualb=-annualb
	pmonth=-pmonth
	lastyr=-lastyr
	overundr=overundr
	unexpend=unexpend
L1200: if ds=1 then dollar$="$" else dollar$=" "
	goto L1250 ! pr all accounts even if zero balance  (if budget ever nets to zero, it messes the monthly budget column up
	if annualb><0 or total2><0 then goto L1250
	if total<>0 then goto L1250
	if ls+ds+ul+ic>0 then goto L1250 else goto L560
L1250: sp2=26-sp-1
	if ul=1 then pr #255,using L1570: d$(1:sp2),dollar$,"{\ul ",annualb,"}",ar$,"{\ul ",total,"}",ar$,"{\ul ",pmonth,"}",dollar$,"{\ul ",total2,"}",dollar$,"{\ul ",lastyr,"}",dollar$,"{\ul ",unexpend,"}" pageoflow L2040 : goto L1300
	pr #255,using L1290: d$(1:sp2),dollar$,annualb,dollar$,total,dollar$,pmonth,dollar$,total2,dollar$,lastyr,dollar$,unexpend pageoflow L2040
L1290: form pos sp,c sp2,pos 26,c 1,n 13.2,x 1,c 1,n 11.2,x 1,c 1,n 11.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,skip 1
L1300: form pos sp,c sp2,pos 26,c 1,n 13.2,x 1,c 1,n 11.2,x 1,c 1,n 11.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,skip redir
	total=0
	total2=0
	annualb=0
	pmonth=0
	lastyr=0
	overundr=0
	unexpend=0
	gosub L1750
	if ul=1 then goto L1410
	gosub L2050
L1410: gosub L1860
	goto L560
L1430: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
	if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
	if rs=1 then accum5=-accum(ap,5) else accum5=accum(ap,5)
	if rs=1 then accum6=accum(ap,6) else accum6=accum(ap,6)
	if rs=1 then accum7=accum(ap,7) else accum7=accum(ap,7)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=26-sp-1
	if ul=1 then pr #255,using L1570: d$(1:sp2),dollar$,"{\ul ",accum3,"}",dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum4,"}",dollar$,"{\ul ",accum2,"}",dollar$,"{\ul ",accum5,"}",dollar$,"{\ul ",accum7,"}" pageoflow L2040 : goto L1580
! pr #255,Using 1210: D$(1:SP2),DOLLAR$,ACCUM3,DOLLAR$,ACCUM1,DOLLAR$,ACCUM4,DOLLAR$,ACCUM2,DOLLAR$,ACCUM5,DOLLAR$,ACCUM6,DOLLAR$,ACCUM7 Pageoflow 1890
	pr #255,using L1560: d$(1:sp2),dollar$,accum3,dollar$,accum1,dollar$,accum4,dollar$,accum2,dollar$,accum5,dollar$,accum7 pageoflow L2040
L1560: form pos sp,c sp2,pos 26,c 1,n 13.2,x 1,c 1,n 11.2,x 1,c 1,n 11.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1
L1570: form pos sp,c sp2,pos 26,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 11.2,c 1,x 1,c 1,c 5,n 11.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1
L1580: ft1=0
	gosub L1750
	if ul=1 then goto L1620
	gosub L2050
L1620: gosub L1860
	goto L560
L1640: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1860
	goto L560
L1680: if foot1=1 then goto L1730
	tabnote=sp
	foot1=1
	foot$=d$
	goto L560
L1730: foot$=rtrm$(foot$)&d$
	goto L560
L1750: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1840 ! 10/14/87
		accum(j,1)=0
		accum(j,2)=0
		accum(j,3)=0
		accum(j,4)=0
		accum(j,5)=0
		accum(j,6)=0
		accum(j,7)=0
L1840: next j
return
L1860: if ls=0 then goto L2020
	if ls=99 then goto L1910
	pr #255,using L1890: " "
L1890: form pos 1,c 1,skip ls
	goto L2020
L1910: ! If FT1=1 Then Goto 1870
	fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1970: rtrm$(foot$),"Page "&str$(pt1)
L1970: form skip sk,pos tabnote,c fl,pos 115,c 8,skip 1
! ft1=1
	if eofcode=1 then goto L2020
	pr #255: newpage
	gosub L2190
L2020: return
 
L2040: gosub L1910: continue
L2050: if ul=0 then goto L2150
	if ul=1 then goto L2100
	underlin$="=============="
	goto L2110
	goto L2150
L2100: underlin$="______________"
L2110: ! pr #255,Using 1980: UNDERLIN$,UNDERLIN$(1:12),UNDERLIN$(1:12),UNDERLIN$,UNDERLIN$,UNDERLIN$,UNDERLIN$
	pr #255,using L2130: underlin$,underlin$(1:12),underlin$(1:12),underlin$,underlin$,underlin$
L2130: form pos 30,c 15,2*c 13,4*c 15,skip 0
	form skip redir,pos 26,c 15,2*c 13,4*c 15,skip redir
L2150: if redir=0 then pr #255,using L2160: " " pageoflow L2040
L2160: form skip 1,c 1,skip 0
return
 
L2190: heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
! pr #255: TAB(33);"ANNUAL";TAB(40);"<--";FNCCH$;"-->";TAB(66);" <--     YEAR TO DATE      -->";TAB(97);"<--     BUDGET TO DATE    -->"
	pr #255,using L2290: "ANNUAL",curmonth$,curmonth$,"BAL YTD","BAL YTD","UNEXPENDED"
L2290: form pos 27,cr 13,cr 13,cr 13,cr 15,cr 15,cr 15,skip 1
	pr #255,using L2290: "BUDGET",str$(curyear),str$(prioryr),str$(curyear),str$(prioryr),str$(curyear)
! pr #255: TAB(33);"BUDGET";TAB(45);"BALANCE";TAB(60);"BUDGET";TAB(73);"BALANCE";TAB(90);"BUDGET";TAB(101);"OVER/UNDER";TAB(116);"UNEXPENDED"
	pr #255:
return
 
L2350: eofcode=1
	gosub L1910
	fnfscode(actpd)
	fnpriorcd(1)
	fnfscode(actpd)
	fnpriorcd(1)
	fncloseprn
 
Xit: fnXit
 
include: Ertn
