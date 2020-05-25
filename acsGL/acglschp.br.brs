! Replace S:\acsGL\acglSchP
! pr Schedules  (once used to pr schedules; now called glschprt)
! ________________Needs fncmo and others to run _____________________
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,dollar$*1,k$*2,by(13),bp(13)
	dim gl2$*12,d2$*50,by2(13),bp2(13)
	dim sn$*78,ft$*78,gl$(80)*12,prtsch(99),d$*50
	dim cnam$*40,d(2),actpd$*6,pedat$*20,cch$*20,d(2),cap$*128
 
	fnTop(program$,cap$="Print Schedules")
	fncno(cno,cnam$)
	open #20: "Name=CNO.H"&wsid$,internal,input,relative  : _
	read #20,using 'Form POS 141,6*N 1,3*N 2,C 6,POS 195,2*C 20',rec=1: process,ps,filno,priorcd,mat d,fscode,lmu,actpd,actpd$,pedat$,cch$ : _
	close #20:
	if process=1 then prtall=1 : goto L320
L160: pr newpage
	pr f "5,25,c 50,h,n": "SELECTION OF SCHEDULES TO PRINT"
	pr f "10,5,C 60,N": "ENTER 1 TO pr ALL SCHEDULES  "
	pr f "11,5,c 60,n": "ENTER 0 TO SELECT SPECIFIC SCHEDULES TO PRINT"
	fa$="11,65,N 1,uE,N"
L210: input fields fa$: prtall conv L210
	if prtall=1 then goto L320
	if prtall><0 then goto L160
	fb$="10,65,N 2,uE,N"
	for j=1 to 99
		pr newpage
		if j=1 then pr f "10,5,C 60,N": "ENTER SCHEDULE NUMBER TO PRINT" else pr f "10,5,c 75,n": "ENTER NEXT SCHEDULE TO PRINT, ELSE ENTER 0 WHEN COMPLETE"
L280: input fields fb$: prtsch(j) conv L280
		if prtsch(j)=0 then goto L310
	next j
L310: j=0
L320: open #1: "Name=[Q]\GLmstr\ACGLSCHS.h[cno],KFName=[Q]\GLmstr\schindex.h[cno],Shr",internal,input,keyed ioerr DONE
	open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,input,keyed
	pr newpage
	pr f "10,20,C 60,N": "PRINT SCHEDULES IN PROCESS"
	pr f "12,2,C 30,B,5": "Press F5 to stop"
	fnopenprn(cp,58,220,process)
L380: if prtall=1 then goto L440
L390: g=g+1
	if prtsch(g)=0 then goto DONE
	k$=lpad$(str$(prtsch(g)),2)
	read #1,using L450,key=k$: sn,sn$,ft$,dp,rs,cm,mat gl$ nokey L390
	goto L460
L440: read #1,using L450: sn,sn$,ft$,dp,rs,cm,mat gl$ eof DONE
L450: form pos 1,n 2,2*c 78,3*n 1,80*c 12
L460: if dp=1 then dollar$="$" else dollar$=" "
	gosub L1130
	for j=1 to 80
		if gl$(j)="  0     0  0" then goto L830
		if j1><51 then goto L530
		gosub L1060
		gosub L1130
L530: read #3,using L600,key=gl$(j): d$,bb,cb,mat by,mat bp nokey L830
		if cno<>99 then goto L600
L550: read #3,using L560: gl2$,d2$,bb2,cb2,mat by2,mat bp2 eof L600
L560: form pos 1,c 12,pos 13,c 50,pos 81,41*pd 6.2
		if gl2$=gl$(j) then goto L580 else goto L600
L580: bb=bb+bb2: cb=cb+cb2: mat by=by+by2: mat bp=bp+bp2
		goto L550
L600: form pos 13,c 50,pos 81,41*pd 6.2
		if fscode=0 then goto L690 ! CURRENT OR PRIOR
		if fscode<0 or fscode>12 then fscode=1
		if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
		if priorcd=2 then goto L680
		if fscode>1 then bb=by(fscode-1) else bb=0
		goto L690
 
L680: if fscode>1 then bb=bp(fscode-1) else bb=0
L690: curmo=cb-bb
		if rs=1 then cb=-cb
		if rs=1 then curmo=-curmo
		if cm=1 then goto L780
		pr #255,using L750: d$,dollar$,cb
		dollar$=" "
L750: form pos 1,c 50,pos 67,c 1,pic(--,---,---.##),skip 1
		goto L820
 
L780: pr #255,using L810: d$,dollar$,curmo,dollar$,cb
		j1=j1+1
		dollar$=" "
L810: form pos 1,c 50,pos 51,c 1,pic(--,---,---.##),pos 67,c 1,pic(--,---,---.##),skip 1
L820: gosub L1330
L830: next j
	j1=0
	gosub L890
	gosub L1060
	goto L380
 
L890: ! TOTAL PRINT
	if dp=1 then dollar$="$" else dollar$=" "
	if cm=1 then goto L980
	pr #255,using L930: "______________"
L930: form pos 67,c 14,skip 1
	pr #255,using L750: "    TOTAL",dollar$,ytdtot
	pr #255,using L930: "=============="
	goto L1020
 
L980: pr #255,using L990: "______________","______________"
L990: form pos 51,c 14,pos 67,c 14,skip 1
	pr #255,using L810: "    TOTAL",dollar$,cmtot,dollar$,ytdtot
	pr #255,using L990: "==============","=============="
L1020: cmtot=0
	ytdtot=0
return
 
L1060: fttab=int(43-len(rtrm$(ft$))/2)
	sk=58-krec(255): fl=len(rtrm$(ft$))
	pr #255,using L1090: rtrm$(ft$)
L1090: form skip sk,pos fttab,c fl,skip 1
	pr #255: newpage
return
 
L1130: ! PAGE HEADING
	pr #255,using L1150: cnam$,"SCHEDULE ",sn
L1150: form pos 11,cc 58,c 9,pic(zz)
	sntab=int(43-len(rtrm$(sn$))/2)
	pr #255,using L1180: sn$
L1180: form pos sntab,c 80,skip 1
	dattab=int(43-len(rtrm$(pedat$))/2)
	pr #255,using L1210: pedat$
L1210: form pos dattab,c 80,skip 2
	if cm><1 then goto L1250
	pr #255,using L1240: cch$,"YEAR TO DATE"
L1240: form pos 48,c 20,pos 69,c 12,skip 2
L1250: return
 
DONE: !
	fncloseprn
	goto Xit
 
Xit: fnXit
 
L1330: ytdtot=ytdtot+cb
	if cm><1 then goto L1360
	cmtot=cmtot+curmo
L1360: return
 
include: Ertn
