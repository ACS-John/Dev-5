! formerly S:\acsGL\AcGlIncV
! COMPARATIVE INCOME STATEMENT WITH PERCENTAGES & VARIANCES
 ! Income Statement with Varience
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,cch$*20
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*22
	dim b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,6)
	dim bp(13),by(13)
 
	fnTop(program$)
	on fkey 5 goto L2500
	fnfscode
	fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit ! sets fnPs,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	fscode=fnfscode
	fnpriorcd
	cch$=fncch$
	pedat$=fnpedat$
	actpd$=fnactpd$
	actpd=fnactpd
	priorcd=fnpriorcd
 
	fnOpenPrn
	if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
	mp1=69
	if fnPs=2 then mp1=mp1+3
	if fnPs=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr" else : _
		fl1$="Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr"
	L270: form pos mp1,pd 3,pos 81,41*pd 6.2
	if actpd>0 and actpd<14 then goto L330
	pr f "10,2,C 78": "THIS PROGRAM CANNOT PROCESS WITHOUT THE NUMBER OF THE ACCOUNTING PERIOD END"
	pr f "12,2,C 60,N": "USE OPTION 1 ON THE MENU TO ENTER THIS INFORMATION"
	input fields "23,2,C 1,E,N": pause$
	goto L2530
	L330: !
	open #1: fl1$,i,i,k
	if fnProcess=1 or fnUseDeptNo=0 or percent=1 then goto L440
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,'30',0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) : _
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
	L440: !
	report$="Statement of Income and Expenses"
	fnFsIndexIncStmt
	open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[temp]\fsindex.h[cno],Shr",i,i,k
goto TOP_OF_LOOP
 
TOP_OF_LOOP: !
	if ic><2 then goto L550
	percent1=percent2=percent3=percent4=percent5=percent6=0 : _
	percent=2
L550: read #1,using L600: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2450
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto TOP_OF_LOOP
L570: if costcntr=0 then goto L600
	if fc=0 and te$="F" then goto L610 ! 5/08/1989
	if costcntr><fc then goto TOP_OF_LOOP
L600: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
L610: if te$="S" or te$="F" then goto L630
	if heading=0 and te$><"R" then gosub L2290
L630: on pos ("RFHDTS",te$,1) goto L1880,L1910,L640,L700,L1510,L1880 none TOP_OF_LOOP
L640: if percent=0 then goto L680
	pr #255,using L660: d$(1:40)
L660: form pos sp,c 40,skip 1
	gosub EOPAGE
L680: gosub L1940
	goto TOP_OF_LOOP
L700: if notrans=1 then goto L950
	if ir>=val(r$) and val(r$)><0 then goto L820
L720: ! read amounts from gl master file
L730: read #3,using L270: ir,bb,cb,mat by,mat bp eof L940
	if ir=0 then goto L730
	if fscode=0 or (fscode=actpd and fnpriorcd=1) then goto L820
	if fscode<1 or fscode>13 then fscode=1 ! 6/8/88
	if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
	if priorcd=2 then goto L810
	if fscode>1 then bb=by(fscode-1) else bb=0
	goto L820
L810: if fscode>1 then bb=bp(fscode-1) else bb=0
L820: if ir=val(r$) then total=total+(cb-bb) else goto L920
	total2=total2+cb
	if actpd>1 then pripd=actpd-1 else goto L870
	total3=total3+(bp(actpd)-bp(pripd))
	goto L880
L870: total3=total3+bp(actpd)
L880: total4=total4+bp(actpd)
	total5=total-total3
	total6=total2-total4
	goto L720
L920: if ir<val(r$) then goto L720
	if ir>val(r$) then goto L950
L940: notrans=1
L950: for j=1 to 9
		if ac(j)=9 then goto L1030 ! 10/14/87
		accum(j,1)=accum(j,1)+total
		accum(j,2)=accum(j,2)+total2
		accum(j,3)=accum(j,3)+total3
		accum(j,4)=accum(j,4)+total4
		accum(j,5)=accum(j,1)-accum(j,3)
		accum(j,6)=accum(j,2)-accum(j,4)
L1030: next j
	if rs=1 then total=-total else goto L1100
	total2=-total2
	total3=-total3
	total4=-total4
	total5=-total5
	total6=-total6
L1100: if ds=1 then dollar$="$" else dollar$=" "
	if ds=1 then percent$="%" else percent$=" "
	if total2><0 or total4><0 then goto L1150
	if total><0 or total3><0 then goto L1150
	if ls+ds+ul+ic>0 then goto L1150 else goto L550
L1150: if percent=0 then goto L1380
	sp2=31-sp-1
	if percent=2 and ic=1 then gosub L2560
	if percent1=0 then pdpct=0 else pdpct=total/percent1*100
	if percent2=0 then ytdpct=0 else ytdpct=total2/percent2*100
	if percent3=0 then pppd=0 else pppd=total3/percent3*100
	if percent4=0 then ppyear=0 else ppyear=total4/percent4*100
	if percent5=0 then ppcvar=0 else ppcvar=total5/percent5*100
	if percent6=0 then ppyvar=0 else ppyvar=total6/percent6*100
	if pdpct<-999.99 then pdpct=-999.99
	if pdpct>999.99 then pdpct=999.99
	if ytdpct<-999.99 then ytdpct=-999.99
	if ytdpct>999.99 then ytdpct=999.99
	if ppyear<-999.99 then ppyear=-999.99
	if ppyear>999.99 then ppyear=999.99
	if pppd<-999.99 then pppd=-999.99
	if pppd>999.99 then pppd=999.99
	if ppcvar<-999.99 then ppcvar=999.99
	if ppyvar<-999.99 then ppyvar=999.99
	if ul=1 then pr #255,using L1361: d$(1:sp2),dollar$,"{\UL ",total,"}",pdpct,percent$,dollar$,"{\UL ",total2,"}",ytdpct,percent$,dollar$,"{\UL ",total3,"}",pppd,percent$,dollar$,"{\UL ",total4,"}",ppyear,percent$,dollar$,"{\UL ",total5,"}",ppcvar,percent$,dollar$,"{\UL ",total6,"}",ppyvar,percent$ pageoflow PgOf : goto L1360
	pr #255,using L1360: d$(1:sp2),dollar$,total,pdpct,percent$,dollar$,total2,ytdpct,percent$,dollar$,total3,pppd,percent$,dollar$,total4,ppyear,percent$,dollar$,total5,ppcvar,percent$,dollar$,total6,ppyvar,percent$ pageoflow PgOf
L1360: form pos sp,c sp2,pos 31,c 1,pic(--,---,---.##),pic(-----.##),c 2,c 1,pic(--,---,---.##),pic(-----.##),c 2,c 1,pic(--,---,---.##),pic(-----.##),c 2,c 1,pic(--,---,---.##),pic(-----.##),c 2,c 1,pic(--,---,---.##),pic(-----.##),c 2,c 1,pic(--,---,---.##),pic(-----.##),c 1,skip redir
L1361: form pos sp,c sp2,pos 31,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##) ,c 1,pic(-----.##),c 1
L1370: form pos sp,c sp2,pos 31,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),c 2,c 1,c 5,pic(--,---,---.##),c 1,pic(-----.##),skip redir
L1380: if percent=0 and ic=1 then goto L2550
	total=0
	total2=0
	total3=0
	total4=0
	total5=0
	total6=0
	gosub L1940
	if ul=1 then goto L1480
	gosub L2160
L1480: gosub EOPAGE
	goto TOP_OF_LOOP
 
L1510: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
	if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
	if rs=1 then accum5=-accum(ap,5) else accum5=accum(ap,5)
	if rs=1 then accum6=-accum(ap,6) else accum6=accum(ap,6)
	if ds=1 then dollar$="$" else dollar$=" "
	if ds=1 then percent$="%" else percent$=" "
	if percent=2 and ic=1 then gosub L2640
	if percent=0 then goto L1830
	if percent1=0 then pdpct=0 else pdpct=accum1/percent1*100
	if percent2=0 then ytdpct=0 else ytdpct=accum2/percent2*100
	if percent3=0 then pppd=0 else pppd=accum3/percent3*100
	if percent4=0 then ppyear=0 else ppyear=accum4/percent4*100
	if percent5=0 then ppcvar=0 else ppcvar=accum5/percent5*100
	if percent6=0 then ppyvar=0 else ppyvar=accum6/percent6*100
	if pdpct<-999.99 then pdpct=-999.99
	if pdpct>999.99 then pdpct=999.99
	if ytdpct<-999.99 then ytdpct=-999.99
	if ytdpct>999.99 then ytdpct=999.99
	if pppd<-999.99 then pppd=-999.99
	if pppd>999.99 then pppd=999.99
	if ppyear<-999.99 then ppyear=-999.99
	if ppyear>999.99 then ppyear=999.99
	if ppyvar<-999.99 then ppyvar=-999.99
	if ppyvar>999.99 then ppyvar=999.99
	if ppcvar<-999.99 then ppcvar=-999.99
	if ppcvar>999.99 then ppcvar=999.99
	sp2=31-sp-1
	if ul=1 then pr #255,using L1370: d$(1:sp2),dollar$,"{\UL ",accum1,"}",pdpct,percent$,dollar$,"{\UL ",accum2,"}",ytdpct,percent$,dollar$,"{\UL ",accum3,"}",pppd,percent$,dollar$,"{\UL ",accum4,"}",ppyear,percent$,dollar$,"{\UL ",accum5,"}",ppcvar,percent$,dollar$,"{\UL ",accum6,"}",ppyvar,percent$ pageoflow PgOf : goto L1830
	pr #255,using L1360: d$(1:sp2),dollar$,accum1,pdpct,percent$,dollar$,accum2,ytdpct,percent$,dollar$,accum3,pppd,percent$,dollar$,accum4,ppyear,percent$,dollar$,accum5,ppcvar,percent$,dollar$,accum6,ppyvar,percent$ pageoflow PgOf
L1830: if percent=0 and ic=1 then : _
		goto L2630
	gosub L1940
	if ul=1 then goto L1860
	gosub L2160
L1860: gosub EOPAGE : goto TOP_OF_LOOP
 
L1880: if te$="R" then report$=d$ else : _
		if te$="S" then secondr$=d$
	gosub EOPAGE : goto TOP_OF_LOOP
 
L1910: if foot1=1 then foot$=rtrm$(foot$)&d$ else : _
		tabnote=sp : foot1=1 : foot$=d$
	goto TOP_OF_LOOP
 
L1940: for j=1 to 9
		if ac(j)<>0 and ac(j)<>9 then : _
			accum(j,1)=accum(j,2)=accum(j,3)=accum(j,4)=0
	next j
return
 
EOPAGE: ! i think that's what this is... could be wrong
	if percent=0 then goto L2130
	if ls=0 then goto L2130
	if ls=99 then goto FOOTER
	pr #255,using L2040: " "
L2040: form pos 1,c 1,skip ls
	goto L2130
FOOTER: sk=58-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L2090: rtrm$(foot$),"Page "&str$(pt1)
L2090: form skip sk,pos tabnote,c fl,pos 165,c 8,skip 1
	if eofcode=1 then goto L2130
	pr #255: newpage
	gosub L2290
L2130: return
 
PgOf: gosub FOOTER: continue
L2160: if percent=0 then goto L2280
	if ul=0 then goto L2260
	if ul=1 then goto L2230
	underlin$="============== ======="
	pr #255,using L2210: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$
L2210: form pos 31,c 22,pos 55,c 22,pos 79,c 22,pos 103,c 22,pos 127,c 22,pos 151,c 22,skip redir
	goto L2260
L2230: underlin$="______________ _______"
	pr #255,using L2250: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$
L2250: form skip redir,pos 31,c 22,pos 55,c 22,pos 79,c 22,pos 103,c 22,pos 127,c 22,pos 151,c 22,skip redir
L2260: if redir=0 then pr #255,using L2270: " "
L2270: form skip 1,c 1,skip 0
L2280: return
L2290: heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	pr #255: tab(48);"CURRENT YEAR";tab(97);"PRIOR YEAR";tab(147);"VARIANCE"
	pr #255,using L2390: cch$,"     YEAR TO DATE",cch$,"     YEAR TO DATE",cch$,"     YEAR TO DATE"
L2390: form pos 32,c 20,pos 55,c 22,pos 79,c 22,pos 103,c 22,pos 127,c 22,pos 151,c 22,skip redir
L2400: form pos 31,c 22,pos 55,c 22,pos 78,c 22,pos 103,c 22,pos 127,c 22,pos 151,c 22,skip redir
	pr #255,using L2400: "______________________","______________________","______________________","______________________","______________________","_____________________"
	pr #255:
return
 
L2450: if percent><0 then goto L2500
	percent=1
	percent1=percent2=percent3=percent4=percent5=percent6=0
	goto L570
 
L2500: eofcode=1
	gosub FOOTER
	fnClosePrn
	fnfscode(actpd)
	fnpriorcd(1)
L2530: goto Xit
 
L2550: percent=1
L2560: percent1=total
	percent2=total2
	percent3=total3
	percent4=total4
	percent5=total5
	percent6=total6
	goto L2700
L2630: percent=1
L2640: percent1=accum1
	percent2=accum2
	percent3=accum3
	percent4=accum4
	percent5=accum5
	percent6=accum6
L2700: if percent=2 then return
	close #1: ioerr L2720
L2720: close #3: ioerr L2730
L2730: total=0
	total2=0
	total3=0
	total4=0
	total5=0
	total6=0
	mat accum=(0)
	foot1=0
	foot$=" "
	ir=0
	goto L330
 
Xit: fnXit
 
include: ertn
