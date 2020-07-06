! formerly S:\acsGL\AcGLBalC
! Comparative Balance Sheet
! r: setup library, on error, dims, fnTop
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,cogl$(3)*12,accum(9,2),bp(13),by(13)
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*56
 
	fnTop(program$)
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Pprior,period to print)
	actpd$=fnactpd$
	actpd=fnactpd
	! fnfscode
	! fnpriorcd
	! if fnGlAskFormatPriorCdPeriod=5 then goto Xit ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Pprior,period to print)
	! fnfscode
	! fnpriorcd
	! pr newpage
	if fnps=2 then
		mp1=66
		fl1$="Name=[Q]\GLmstr\AcGLFnSc.h[cno],"
		fl1$=fl1$&"KFName=[Q]\GLmstr\agfsidx1.h[cno],Shr"
	else
		mp1=63
		fl1$="Name=[Q]\GLmstr\ACGLFNSB.h[cno],"
		fl1$=fl1$&"KFName=[Q]\GLmstr\agfsidx4.h[cno],Shr"
	end if
	! if actpd>0 and actpd<13 then goto L230
	! pr newpage
	! pr f "10,2,C 78,N": "THIS PROGRAM CANNOT PROCESS WITHOUT THE NUMBER OF THE ACCOUNTING MONTH END"
	! pr f "12,2,c 78,n": "USE OPTION 1 ON THE CURRENT PERIOD PROCESSING MENU TO ENTER THIS INFORMATION"
	! input fields "23,2,c 1,e,n": pause$
	! goto Xit
	! L230:
	open #1: fl1$,internal,input,keyed
	! /r
	! r: ask cost center
	if fnprocess=1 or fnUseDeptNo=0 then goto L320
	fnTos(sn$="Acglbalc")
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 )
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	L320: !
	costcntr=val(resp$(1))
	! /r
	! r: open glmstr with new fsindex
	if fnps=2 then ! secondary
		execute "Index [Q]\GLmstr\GLmstr.h[cno] "&env$('temp')&"\fsindex.H[cno] 66 3 Replace DupKeys -N"
	else
		execute "Index [Q]\GLmstr\GLmstr.h[cno] "&env$('temp')&"\fsindex.H[cno] 63 3 Replace DupKeys -N"
	end if
	open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&env$('temp')&'\'&"fsindex.h[cno],Shr",internal,input,keyed
	! /r
	fnopenprn
	report$=env$('program_caption')
MainRead: ! r: main loop (on financial statement design)
	read #1,using L470: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1660
	L470: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto MainRead
	if costcntr and costcntr><fc then goto MainRead
	if te$="S" or te$="F" then goto L500
	if heading=0 and te$><"R" then gosub PrHeading
	L500: !
on pos ("RFHDTSPE",te$,1) goto TypeRandS,TypeF,TypeH,TypeDandE,TypeTandP,TypeRandS,TypeTandP,TypeDandE none MainRead
! /r
TypeH: ! r:
	pr #255,using L520: d$
	L520: form pos sp,c 50,skip 1
	gosub L1180
	gosub L1120
goto MainRead ! /r
TypeDandE: ! r:
	if notrans=1 then goto L710
	if br>=val(r$) and val(r$)><0 then goto L650
	ReadGLmstr: ! read general ledger master file for amounts
	read #3,using 'Form POS MP1,PD 3,POS 87,27*PD 6.2': br,cb,mat by,mat bp eof L700
	if br=0 then goto ReadGLmstr
	if fnfscode=0 or (fnfscode=actpd and fnpriorcd=1) then goto L650
	if fnfscode<1 or fnfscode>13 then let fnfscode(1)
	if fnpriorcd=1 then cb=by(fnfscode)
	L650: !
	if br=val(r$) then
		total=total+cb
		total2+=bp(fnfscode)
		goto ReadGLmstr
	end if
	if br<val(r$) then goto ReadGLmstr
	if br>val(r$) then goto L710
	L700: !
	notrans=1
	L710: !
	if te$="E" then total=-accum(ap,1) : total2=-accum(ap,2)
	for j=1 to 9
		if ac(j)<>9 then
			accum(j,1)=accum(j,1)+total : accum(j,2)=accum(j,2)+total2
		end if
	next j
	if rs=1 then total=-total : total2=-total2
	if ds=1 then dollar$="$" else dollar$=" "
	dollar=24+14*bc
	if total><0 or total2><0 then goto L800
	if ls+ul+ds+ic>0 then goto L800 else goto MainRead
	L800: !
	sp2=dollar-sp-1
	if ul=1 then pr #255,using L816: d$(1:sp2),dollar$,"{\ul ",total,"}",dollar$,"{\ul ",total2,"}" pageoflow PgOf : goto L830
	pr #255,using L820: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow PgOf
	L816: form pos sp,c sp2,pos dollar,c 1,c 5,pic(--,---,---.##),c 1,x 28,c 1,c 5,pic(--,---,---.##),c 1,skip 1
	L820: form pos sp,c sp2,pos dollar,c 1,pic(--,---,---.##),x 28,c 1,pic(--,---,---.##),skip 1
	L830: !
	total=0
	total2=0
	gosub L1120
	if ul=1 then goto L870
	gosub L1370
	L870: !
	gosub L1180
goto MainRead ! /r
TypeTandP: ! r:
	if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if ds=1 then dollar$="$" else dollar$=" "
	dollar=24+14*bc
	sp2=dollar-sp-1
	if ul=1 then pr #255,using L816: d$(1:sp2),dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}" pageoflow PgOf : goto L960
	pr #255,using L820: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow PgOf
	L960: !
	gosub L1120
	if ul=1 then goto L980
	gosub L1370
	L980: !
	gosub L1180
	if te$><"P" then goto L1010
	for j=1 to 9
		accum(j,1)=accum(j,1)-accum(ap,1)
		accum(j,2)=accum(j,2)-accum(ap,2)
	next j
	L1010: !
goto MainRead ! /r
TypeRandS: ! r:
	if te$="R" then
		report$=d$
	else if te$="S" then
		secondr$=d$
	end if
	gosub L1180
	goto MainRead
	TypeF: !
	if foot1=1 then
		foot$=rtrm$(foot$)&d$
	else
		tabnote=sp : foot1=1 : foot$=d$
	end if
goto MainRead ! /r
L1120: ! r:
	for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1150
		accum(j,1)=0 : accum(j,2)=0
		L1150: !
	next j
return ! /r
L1180: ! r: maybe skip some lines if ls>0 - if ls=99 then do the newpage thing
	if ls<>0 then
		if ls=99 then
			gosub PrNewPageThing
		else
			pr #255,using fSkipLs: " "
			fSkipLs: form pos 1,c 1,skip ls
		end if
	end if
return ! /r
PrNewPageThing: ! ! r: newpage thing.  pr footer on page and if eofcode<>1 then pr newpage and heading
	fnpglen(pglen)
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
	if trim$(foot$)<>'' then pr #255,using L1280: rtrm$(foot$)
	L1280: form skip sk,pos tabnote,c fl,skip 1
	if eofcode<>1 then
		pr #255: newpage
		gosub PrHeading
	end if
	L1320: !
return ! /r
PgOf: ! r:
	gosub PrNewPageThing
continue ! /r
L1370: ! r:
	if ul=0 then goto L1480
	underlin=24+14*bc
	if ul=1 then goto L1450
	underlin$="==============                            =============="
	pr #255,using L1420: underlin$
	L1420: form pos underlin,c 56,skip 1
	goto L1480
 
	L1450: !
	underlin$="______________                            ______________"
	pr #255,using L1470: underlin$
	L1470: form pos underlin,c 56,skip 1
	L1480: !
	L1490: form skip 1,c 1,skip 1
return ! /r
PrHeading: ! r: heading
	heading=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(fnpedat$)&"}"
	pr #255: '' ! "\ql "  moved down to the beginning of another line
	pr #255: ""
	pr #255: "\ql {\f181                                                                                                                             Current Year                                                                    Prior Year }"
	pr #255,using L1630: "__________________________________________"," _________________________________________"
	L1630: form pos 38,cc 42,x 1,cc 42,skip 1
return ! /r
L1660: ! r:
	eofcode=1
	gosub PrNewPageThing
	fnfscode(actpd)
	fnpriorcd(1)
	fncloseprn
goto Xit ! /r
Xit: fnXit
include: Ertn
