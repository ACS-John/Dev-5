! Replace S:\acsGL\AcGlinyy
! Income Statement with Year Comparison
!
	autoLibrary
	on error goto Ertn
!
	dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,cch$*20,p$(20)*50
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*22
	dim b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,4)
	dim bp(13),d(2),by(13),tp1(4)
!
	fnTop(program$,"Income Statement with Year Comparison")
	on fkey 5 goto L2590
	
	fscode=fnfscode
	priorcd=fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	cch$=fncch$
	pedat$=fnpedat$
	actpd=fnactpd
	actpd$=fnactpd$
	fscode=fnfscode
	priorcd=fnpriorcd
!
	pors=1
	fnopenprn
	redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
	mp1=69
	if fnps=2 then mp1=mp1+3
	fl1$="Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr"
	if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr"
	form c 9,skip 0
L330: form pos mp1,pd 3,pos 81,41*pd 6.2
	form c 7,skip 0
	nametab=int(44-len(rtrm$(env$('cnam')))/2)
	pas=1 : open #4: "Name=[Temp]\Work."&session$&",KFName=IDX."&wsid$&",Replace,RecL=33,KPS=1,KLN=5",internal,outIn,keyed
	if actpd>0 and actpd<14 then goto L430
	pr f "10,2,C 78": "THIS PROGRAM CANNOT PROCESS WITHOUT THE NUMBER OF THE ACCOUNTING PERIOD END"
	pr f "12,2,C 60,N": "USE OPTION 1 ON THE MENU TO ENTER THIS INFORMATION"
	input fields "23,2,C 1,E,N": pause$
	goto Xit
L430: open #1: fl1$,internal,input,keyed
	if fnprocess=1 or fnUseDeptNo=0 then goto L550
	if percent=1 then goto L550
	fnTos(sn$="ACGlinyy")
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 )
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
L550: !
	pf1=len(env$('cnam'))+int((43-len(env$('cnam')))/2)
	close #101: ioerr L580
L580: open #101: "SROW=08,SCOL=18,EROW=12,ECOL=58,BORDER=DR,CAPTION= COMPARATIVE INCOME STATEMENT ",display,outIn
	pr f "08,18,C 41,H,N": lpad$(env$('cnam'),pf1)
	pr f "09,18,C 41,H,N": "            COMPANY NUMBER [cno]"
	pr f "11,18,C 41,R,N": "              IN PROCESS"
	pr f "13,30,C 16,R,N": "PRESS F5 TO STOP"
	if cmdkey=5 then goto L2610
	report$="STATEMENT OF INCOME AND EXPENSES"
	fnopenprn
	redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
	if fnps=2 then goto L700 ! secondary
	execute "Index [Q]\GLmstr\GLmstr.h[cno] "&env$('temp')&'\'&"fsindex.H[cno] 69 3 Replace DupKeys -N"
	goto L710
L700: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&env$('temp')&'\'&"fsindex.H[cno] 72 3 Replace DupKeys -N"
L710: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&env$('temp')&'\'&"fsindex.h[cno],Shr",internal,input,keyed
L720: !
L730: read #1,using L780: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp eof L2510
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L720
	if costcntr=0 then goto L780
	if fc=0 and te$="F" then goto L790 ! 5/08/1989
	if costcntr><fc then goto L720
L780: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3,n 5
L790: if te$="S" or te$="F" then goto L810
	if heading=0 and te$><"R" then gosub L2310
L810: on pos ("RFHDTS",te$,1) goto L1820,L1860,L820,L880,L1540,L1820 none L720
L820: if percent=0 then goto L860
	pr #255,using L840: d$(1:40)
L840: form pos sp,c 40,skip 1
	gosub L2010
L860: gosub L1930
	goto L720
L880: if notrans=1 then goto L1120
	if ir>=val(r$) and val(r$)><0 then goto L1010
L900: ! read amounts from gl master file
	form pd 3
L920: read #3,using L330: ir,bb,cb,mat by,mat bp eof L1110
	if ir=0 then goto L920
	if fscode=0 or (fscode=actpd and priorcd=1) then goto L1010
	if fscode<1 or fscode>13 then fscode=1 ! 6/8/88
	if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
	if priorcd=2 then goto L1000
	if fscode>1 then bb=by(fscode-1) else bb=0
	goto L1010
L1000: if fscode>1 then bb=bp(fscode-1) else bb=0
L1010: if ir=val(r$) then total=total+(cb-bb) else goto L1090
	total2=total2+cb
	if fscode>1 then goto L1040 else goto L1060
L1040: total3=total3+(bp(fscode)-bp(fscode-1))
	goto L1070
L1060: total3=total3+bp(fscode)
L1070: total4=total4+bp(fscode)
	goto L900
L1090: if ir<val(r$) then goto L900
	if ir>val(r$) then goto L1120
L1110: notrans=1
L1120: for j=1 to 9
		if ac(j)=9 then goto L1180 ! 10/14/87
		accum(j,1)=accum(j,1)+total
		accum(j,2)=accum(j,2)+total2
		accum(j,3)=accum(j,3)+total3
		accum(j,4)=accum(j,4)+total4
L1180: next j
	if rs=1 then total=-total else goto L1230
	total2=-total2
	total3=-total3
	total4=-total4
L1230: if ds=1 then dollar$="$" else dollar$=" "
	if ds=1 then percent$="%" else percent$=" "
	if total2><0 or total4><0 then goto L1280
	if total><0 or total3><0 then goto L1280
	if ls+ds+ul>0 then goto L1280 else goto L730
L1280: if percent=0 then goto L1450
	sp2=31-sp-1
	if pas=2 then gosub PAS2
	if percent1=0 then pdpct=0 else pdpct=total/percent1*100
	if percent2=0 then ytdpct=0 else ytdpct=total2/percent2*100
	if percent3=0 then pppd=0 else pppd=total3/percent3*100
	if percent4=0 then ppyear=0 else ppyear=total4/percent4*100
	if pdpct<-999.99 then pdpct=-999.99
	if pdpct>999.99 then pdpct=999.99
	if ytdpct<-999.99 then ytdpct=-999.99
	if ytdpct>999.99 then ytdpct=999.99
	if ppyear<-999.99 then ppyear=-999.99
	if ppyear>999.99 then ppyear=999.99
	if pppd<-999.99 then pppd=-999.99
	if pppd>999.99 then pppd=999.99
	if ul=1 then pr #255,using L1441: d$(1:sp2),dollar$,"{\ul ",total2,"}",ytdpct,percent$,dollar$,"{\ul ",total4,"}",ppyear,percent$ pageoflow L2170 : goto L1440
	pr #255,using L1440: d$(1:sp2),dollar$,total2,ytdpct,percent$,dollar$,total4,ppyear,percent$ pageoflow L2170
L1440: form pos sp,c sp2,pos 31,c 1,pic(---,---,---.##),pic(----.##),c 2,c 1,pic(---,---,---.##),pic(----.##),c 2,skip redir
L1441: form pos sp,c sp2,pos 31,c 1,c 5,pic(---,---,---.##),c 1,pic(----.##),c 2,c 1,c 5,pic(---,---,---.##),c 1,pic(----.##),c 2,skip redir
L1450: if pas=1 then tp1=total : tp2=total2 : tp3=total3 : tp4=total4 : gosub PAS1
	total=0
	total2=0
	total3=0
	total4=0
	gosub L1930
	if ul=1 then goto L1520
	gosub L2180
L1520: gosub L2010
	goto L720
L1540: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
	if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
	if ds=1 then dollar$="$" else dollar$=" "
	if ds=1 then percent$="%" else percent$=" "
	if pas=2 then gosub PAS2
	if percent=0 then goto L1770
	if percent1=0 then pdpct=0 else pdpct=accum1/percent1*100
	if percent2=0 then ytdpct=0 else ytdpct=accum2/percent2*100
	if percent3=0 then pppd=0 else pppd=accum3/percent3*100
	if percent4=0 then ppyear=0 else ppyear=accum4/percent4*100
	if pdpct<-999.99 then pdpct=-999.99
	if pdpct>999.99 then pdpct=999.99
	if ytdpct<-999.99 then ytdpct=-999.99
	if ytdpct>999.99 then ytdpct=999.99
	if pppd<-999.99 then pppd=-999.99
	if pppd>999.99 then pppd=999.99
	if ppyear<-999.99 then ppyear=-999.99
	if ppyear>999.99 then ppyear=999.99
	sp2=31-sp-1
	if ul=1 then pr #255,using L1441: d$(1:sp2),dollar$,"{\ul ",accum2,"}",ytdpct,percent$,dollar$,"{\ul ",accum4,"}",ppyear,percent$ pageoflow L2170 : goto L1770
	pr #255,using L1440: d$(1:sp2),dollar$,accum2,ytdpct,percent$,dollar$,accum4,ppyear,percent$ pageoflow L2170
L1770: if pas=1 then tp1=accum1: tp2=accum2 : tp3=accum3 : tp4=accum4 : gosub PAS1
	gosub L1930
	if ul=1 then goto L1800
	gosub L2180
L1800: gosub L2010
	goto L720
L1820: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L2010
	goto L720
L1860: if foot1=1 then goto L1910
	tabnote=sp
	foot1=1
	foot$=d$
	goto L720
L1910: foot$=rtrm$(foot$)&d$
	goto L720
L1930: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1990 ! 10/14/87
		accum(j,1)=0
		accum(j,2)=0
		accum(j,3)=0
		accum(j,4)=0
L1990: next j
	return
L2010: if percent=0 then goto L2160
	if ls=0 then goto L2160
	if ls=99 then goto L2070
	pr #255,using L2050: " "
L2050: form pos 1,c 1,skip ls
	goto L2160
L2070: fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L2120: rtrm$(foot$),"Page "&str$(pt1)
L2120: form skip sk,pos tabnote,c fl,pos 70,c 8,skip 1
	if eofcode=1 then goto L2160
	pr #255: newpage
	gosub L2310
L2160: return
L2170: gosub L2070: continue
L2180: if percent=0 then goto L2300
	if ul=0 then goto L2280
	if ul=1 then goto L2250
	underlin$="=============== ======"
	pr #255,using L2230: underlin$,underlin$
L2230: form pos 31,c 22,pos 55,c 22,skip redir
	goto L2280
L2250: underlin$="_______________ ______"
	pr #255,using L2270: underlin$,underlin$
L2270: form skip redir,pos 31,c 22,pos 55,c 22,pos 79,c 22,pos 103,c 22,skip redir
L2280: if redir=0 then pr #255,using L2290: " "
L2290: form skip 1,c 1,skip 0
L2300: return
L2310: heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs24 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	pr #255: tab(36);"CURRENT YEAR";tab(60);"PRIOR YEAR"
	pr #255,using L2450: "     YEAR TO DATE","     YEAR TO DATE"
L2450: form pos 32,c 20,pos 55,c 22,skip redir
L2460: form pos 31,c 22,pos 55,c 22,skip 1
	pr #255,using L2460: "_____________________","______________________"
	pr #255:
	return
!
L2510: if pas=2 then goto L2590
	pas=2
	percent=1
	percent1=0
	percent2=0
	percent3=0
	percent4=0
	goto L2670
L2590: eofcode=1
	gosub L2070
L2610: !
!
	fnfscode(actpd)
	fnpriorcd(1)
	fncloseprn
!
	goto Xit
!
L2670: close #1: ioerr L2680
L2680: close #3: ioerr L2690
L2690: total=0
	total2=0
	total3=0
	total4=0
	mat accum=(0)
	foot1=0
	foot$=" "
	notrans=ir=0
	goto L430
PAS1: if rnp=0 then goto L2890
	mat tp1=(0)
	read #4,using L2810,key=r$: k4$,mat tp1 nokey L2880
L2810: form pos 1,g 5,4*pd 7.2
	tp1(1)=tp1(1)+tp1
	tp1(2)=tp1(2)+tp2
	tp1(3)=tp1(3)+tp3
	tp1(4)=tp1(4)+tp4
	rewrite #4,using L2810: k4$,mat tp1
	goto L2890
L2880: write #4,using L2810: r$,tp1,tp2,tp3,tp4
L2890: return
PAS2: mat tp1=(0)
	if rnp=0 then goto L2980
	k4$=lpad$(str$(rnp),5)
	read #4,using L2810,key=k4$: k4$,mat tp1 nokey L2940
L2940: percent1=tp1(1)
	percent2=tp1(2)
	percent3=tp1(3)
	percent4=tp1(4)
L2980: return
Xit: fnXit
include: ertn
