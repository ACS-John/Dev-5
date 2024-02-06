! Replace S:\acsGL\AcGlIncC
! -- ! COMPARATIVE INCOME STATEMENT FOR 14 7/8*11 PAPER WITH PERCENTAGES
 
	autoLibrary
	fnTop(program$)
 
	on error goto Ertn
 
	dim fl1$*256,p$(20)*50
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*22
	dim b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,4)
	dim bp(13),by(13),tp1(4)
 
	actpd$=fnactpd$
	actpd=fnactpd
	fnfscode
	fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit
		! sets fnPs,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	fscode=fnfscode
	priorcd=fnpriorcd
 
	pr newpage
	pors=1
	mp1=69
	if fnPs=2 then mp1=mp1+3
	fl1$='Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr'
	if fnPs=2 then fl1$='Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr'
	form c 9,skip 0
L280: form pos mp1,pd 3,pos 81,41*pd 6.2
	form c 7,skip 0
	nametab=int(44-len(rtrm$(env$('cnam')))/2)
	pas=1 : open #4: 'Name=[Temp]\Work.[Session],KFName=[Temp]\IDX.'&wsid$&',Replace,RecL=33,KPS=1,KLN=5',i,outIn,k
	if actpd>0 and actpd<14 then goto L380
	pr newpage
	pr f '10,2,C 78': 'THIS PROGRAM CANNOT PROCESS WITHOUT THE NUMBER OF THE ACCOUNTING PERIOD END'
	pr f '12,2,C 70,N': 'USE THE SELECT DATE ROUTINE TO ENTER THIS INFORMATION'
	input fields '23,2,C 1,E,N': pause$
	goto Xit
L380: open #1: fl1$,i,i,k
	if fnProcess=1 or fnUseDeptNo=0 then goto L510
	if percent=1 then goto L510
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,'Cost Center or Department #:',mylen,right)
	fnTxt(1,mypos,3,0,right,'30',0,'Enter the cost center or department number if you wish to pr only one department, else leave blank for all.',0 ) : _
	resp$(1)=''
	fnLbl(2,1,'(Blank for all Departments)',mylen,right)
	fnCmdKey('&Next',1,1,0,'Prints the financial statement.')
	fnCmdKey('&Cancel',5,0,1,'Returns to menu without posting.')
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
L510: pf1=len(env$('cnam'))+int((43-len(env$('cnam')))/2)
	fnOpenPrn
	redir=0: if file$(255)(1:4)<>'PRN:' then redir=1
	report$='STATEMENT OF INCOME AND EXPENSES'
	fnFsIndexIncStmt
	open #3: 'Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[temp]\fsindex.h[cno],Shr',i,i,k
L600: !
L610: read #1,using L660: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp eof L2330
	if ltrm$(r$)='' or ltrm$(r$)='0' then goto L600
	if costcntr=0 then goto L660
	if fc=0 and te$='F' then goto L670 ! 5/08/1989
	if costcntr><fc then goto L600
L660: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3,n 5
L670: if te$='S' or te$='F' then goto L690
	if heading=0 and te$><'R' then gosub L2180
L690: on pos ('RFHDTS',te$,1) goto L1690,L1730,L700,L760,L1410,L1690 none L600
L700: if percent=0 then goto L740
	pr #255,using L720: d$(1:40)
L720: form pos sp,c 40,skip 1
	gosub L1880
L740: gosub L1800
	goto L600
L760: if notrans=1 then goto L990
	if ir>=val(r$) and val(r$)><0 then goto L880
L780: ! read amounts from gl master
L790: read #3,using L280: ir,bb,cb,mat by,mat bp eof L980
	if ir=0 then goto L790
	if fnfscode=0 or (fnfscode=actpd and fnpriorcd=1) then goto L880
	if fnfscode<1 or fnfscode>13 then fnfscode=1 ! 6/8/88
	if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
	if fnpriorcd=2 then goto L870
	if fnfscode>1 then bb=by(fnfscode-1) else bb=0
	goto L880
L870: if fnfscode>1 then bb=bp(fnfscode-1) else bb=0
	if cb=0 then goto L880
L880: if ir=val(r$) then total=total+(cb-bb) else goto L960
	total2=total2+cb
	if fnfscode>1 then goto L910 else goto L930
L910: total3=total3+(bp(fnfscode)-bp(fnfscode-1))
	goto L940
L930: total3=total3+bp(fnfscode)
L940: total4=total4+bp(fnfscode)
	goto L780
L960: if ir<val(r$) then goto L780
	if ir>val(r$) then goto L990
L980: notrans=1
L990: for j=1 to 9
		if ac(j)=9 then goto L1050 ! 10/14/87
		accum(j,1)=accum(j,1)+total
		accum(j,2)=accum(j,2)+total2
		accum(j,3)=accum(j,3)+total3
		accum(j,4)=accum(j,4)+total4
L1050: next j
	if rs=1 then total=-total else goto L1100
	total2=-total2
	total3=-total3
	total4=-total4
L1100: if ds=1 then dollar$='$' else dollar$=' '
	if ds=1 then percent$='%' else percent$=' '
	if total2><0 or total4><0 then goto L1150
	if total><0 or total3><0 then goto L1150
	if ls+ds+ul>0 then goto L1150 else goto L610
L1150: if percent=0 then goto L1320
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
	if ul=1 then pr #255,using L1311: d$(1:sp2),dollar$,'{\ul ',total,'}',pdpct,percent$,dollar$,'{\ul ',total2,'}',ytdpct,percent$,dollar$,'{\ul ',total3,'}',pppd,percent$,dollar$,'{\ul ',total4,'}',ppyear,percent$ pageoflow L2040 : goto L1320
	pr #255,using L1310: d$(1:sp2),dollar$,total,pdpct,percent$,dollar$,total2,ytdpct,percent$,dollar$,total3,pppd,percent$,dollar$,total4,ppyear,percent$ pageoflow L2040
L1310: form pos sp,c sp2,pos 31,c 1,pic(---,---,---.##),pic(----.##),c 2,c 1,pic(---,---,---.##),pic(----.##),c 2,c 1,pic(---,---,---.##),pic(----.##),c 2,c 1,pic(---,---,---.##),pic(----.##),c 1,skip redir
L1311: form pos sp,c sp2,pos 31,c 1,c 5,pic(---,---,---.##),c 1,pic(----.##),c 2,c 1,c 5,pic(---,---,---.##),c 1,pic(----.##),c 2,c 1,c 5,pic(---,---,---.##),c 1,pic(----.##),c 2,c 1,c 5,pic(---,---,---.##),c 1,pic(----.##),c 1,skip redir
L1320: if pas=1 then tp1=total : tp2=total2 : tp3=total3 : tp4=total4 : gosub PAS1
	total=0
	total2=0
	total3=0
	total4=0
L1370: gosub L1800
	if ul=1 then goto L1390
	gosub L2050
L1390: gosub L1880
	goto L600
L1410: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
	if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
	if ds=1 then dollar$='$' else dollar$=' '
	if ds=1 then percent$='%' else percent$=' '
	if pas=2 then gosub PAS2
	if percent=0 then goto L1640
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
	if ul=1 then pr #255,using L1311: d$(1:sp2),dollar$,'{\ul ',accum1,'}',pdpct,percent$,dollar$,'{\ul ',accum2,'}',ytdpct,percent$,dollar$,'{\ul ',accum3,'}',pppd,percent$,dollar$,'{\ul ',accum4,'}',ppyear,percent$ pageoflow L2040 : goto L1640
	pr #255,using L1310: d$(1:sp2),dollar$,accum1,pdpct,percent$,dollar$,accum2,ytdpct,percent$,dollar$,accum3,pppd,percent$,dollar$,accum4,ppyear,percent$ pageoflow L2040
L1640: if pas=1 then tp1=accum1: tp2=accum2 : tp3=accum3 : tp4=accum4 : gosub PAS1
	gosub L1800
	if ul=1 then goto L1370
	gosub L2050
	gosub L1880
	goto L600
L1690: if te$='R' then report$=d$
	if te$='S' then secondr$=d$
	gosub L1880
	goto L600
L1730: if foot1=1 then goto L1780
	tabnote=sp
	foot1=1
	foot$=d$
	goto L600
L1780: foot$=rtrm$(foot$)&d$
	goto L600
L1800: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1860 ! 10/14/87
		accum(j,1)=0
		accum(j,2)=0
		accum(j,3)=0
		accum(j,4)=0
L1860: next j
return
L1880: if percent=0 then goto L2030
	if ls=0 then goto L2030
	if ls=99 then goto L1940
	pr #255,using L1920: ' '
L1920: form pos 1,c 1,skip ls
	goto L2030
L1940: fnPgLen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1990: rtrm$(foot$),'Page '&str$(pt1)
L1990: form skip sk,pos tabnote,c fl,pos 120,c 8,skip 1
	if eofcode=1 then goto L2030
	pr #255: newpage
	gosub L2180
L2030: return
L2040: gosub L1940: continue
L2050: if percent=0 then goto L2170
	if ul=0 then goto L2150
	if ul=1 then goto L2120
	underlin$='============== ======'
	pr #255,using L2100: underlin$,underlin$,underlin$,underlin$
L2100: form pos 32,c 22,pos 56,c 22,pos 80,c 22,pos 104,c 22,skip redir
	goto L2150
L2120: underlin$='______________ _______'
	pr #255,using L2140: underlin$,underlin$,underlin$,underlin$
L2140: form skip redir,pos 31,c 22,pos 55,c 22,pos 79,c 22,pos 103,c 22,skip redir
L2150: if redir=0 then pr #255,using L2160: ' '
L2160: form skip 1,c 1,skip 0
L2170: return
L2180: heading=1
	pt1+=1
	pr #255: '\qc  {\f181 \fs24 \b '&env$('cnam')&'}'
	pr #255: '\qc  {\f181 \fs24 \b '&trim$(report$)&'}'
	if trim$(secondr$)<>'' then pr #255: '\qc  {\f181 \fs18 \b '&trim$(secondr$)&'}'
	pr #255: '\qc  {\f181 \fs16 \b For the '&rtrm$(actpd$)&' month period ended '&rtrm$(fnpedat$)&'}'
	pr #255: '\ql '
	pr #255:
	pr #255: tab(48);'CURRENT YEAR';tab(97);'PRIOR YEAR'
	pr #255,using L2280: fncch$,'     YEAR TO DATE',fncch$,'     YEAR TO DATE'
L2280: form pos 32,c 20,pos 55,c 22,pos 79,c 22,pos 103,c 22,skip redir
L2290: form pos 31,c 22,pos 55,c 22,pos 79,c 22,pos 103,c 22,skip 1
	pr #255,using L2290: '______________________','______________________','_____________________','______________________'
	pr #255:
return
L2330: if pas=2 then goto L2410
	pas=2
	percent=1
	percent1=0
	percent2=0
	percent3=0
	percent4=0
	goto L2490
L2410: eofcode=1
	gosub L1940
 
 
	fnClosePrn
	fnfscode(actpd)
	fnpriorcd(1)
	goto Xit
 
L2490: close #1: ioerr L2500
L2500: close #3: ioerr L2510
L2510: total=0
	total2=0
	total3=0
	total4=0
	mat accum=(0)
	foot1=0
	foot$=' '
	notrans=ir=0
	goto L380
PAS1: if rnp=0 then goto L2710
	mat tp1=(0)
	read #4,using L2630,key=r$: k4$,mat tp1 nokey L2700
L2630: form pos 1,g 5,4*pd 7.2
	tp1(1)=tp1(1)+tp1
	tp1(2)=tp1(2)+tp2
	tp1(3)=tp1(3)+tp3
	tp1(4)=tp1(4)+tp4
	rewrite #4,using L2630: k4$,mat tp1
	goto L2710
L2700: write #4,using L2630: r$,tp1,tp2,tp3,tp4
L2710: return
PAS2: mat tp1=(0)
	if rnp=0 then goto L2800
	k4$=lpad$(str$(rnp),5)
	read #4,using L2630,key=k4$: k4$,mat tp1 nokey L2760
L2760: percent1=tp1(1)
	percent2=tp1(2)
	percent3=tp1(3)
	percent4=tp1(4)
L2800: return
Xit: fnXit
 
include: ertn
