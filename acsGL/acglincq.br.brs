! Replace S:\acsGL\ACGLINCQ
! -- INCOME STATEMENT FOR 8 1/2 * 11 PAPER WITHOUT PERCENTAGES
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,bp(13)
	dim sc1$(2)*20,cap$*128,udf$*256
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
	dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,2),by(13)
 
	fnTop(program$,cap$="Quarterly Income Statement")
	on fkey 5 goto L1720
	fncno(cno,cnam$)
	udf$=env$('temp')&'\'
	actpd$=fnactpd$
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit : _
		! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
 
! pr NEWPAGE
	redir=0: if file$(255)(1:4)<>"PRN:" then redir=1: goto L210
L210: if fnps=2 then mp1=72 else mp1=69
	fl1$="Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr"
	if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr"
	form c 9,skip 0
L250: form pos mp1,pd 3,pos mp2,pd 3,pos 81,41*pd 6.2
	form c 7,skip 0
	nametab=int(44-len(rtrm$(cnam$))/2)
	open #1: fl1$,internal,input,keyed
	if fnprocess=1 or fnUseDeptNo=0 then goto L390
	fnTos(sn$="ACglincq") : _
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) : _
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
L390: fnopenprn
	if fnps=2 then goto L430 ! secondary
	execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 69 3 Replace DupKeys -N"
	goto L440
L430: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 72 3 Replace DupKeys -N"
L440: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed
	report$="STATEMENT OF INCOME AND EXPENSES"
! GOSUB BLDPCT1 ! BUILD % BASED ON REF # IN PRIMARY FUND # IN G/L ACCOUNT
L470: read #1,using L520: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1720
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L470
	if costcntr=0 then goto L520
	if fc=0 and te$="F" then goto L530 ! 5/8/89
	if costcntr><fc then goto L470
L520: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
L530: if te$="S" or te$="F" then goto L550
	if heading=0 and te$><"R" then gosub L1600
L550: on pos ("RFHDTS",te$,1) goto L1130,L1170,L560,L610,L1030,L1130 none L470
L560: pr #255,using L570: d$(1:40)
L570: form pos sp,c 40,skip 1
	gosub L1300
	gosub L1240
	goto L470
L610: if notrans=1 then goto L810
	if ir=val(r$) and val(r$)><0 then goto L740
	if ir>val(r$) then goto L740
L640: ! read balances from general ledger
	if ir=0 then goto L660
L660: read #3,using L250: ir,pcr,bb,cb,mat by,mat bp eof L800
	if fscode=0 or (fscode=actpd and priorcd=1) then goto L740
	if fscode<1 or fscode>13 then fscode=1
	if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
	if fnpriorcd=2 then goto L730
	if fscode>1 then bb=by(fscode-1) else bb=0
	goto L740
L730: if fscode>1 then bb=bp(fscode-1) else bb=0
L740: if ir=val(r$) then total=total+(cb-bb) else goto L780
	total2=total2+cb
	k$=cnvrt$("N 5",pcr)
	goto L640
L780: if ir<val(r$) then goto L640
	if ir>val(r$) then goto L810
L800: notrans=1
L810: for j=1 to 9
		if ac(j)=9 then goto L850 ! 10/14/87
		accum(j,1)=accum(j,1)+total
		accum(j,2)=accum(j,2)+total2
L850: next j
	if rs=1 then total=-total else goto L880
	total2=-total2
L880: if ds=1 then dollar$="$" else dollar$=" "
	if total><0 or total2><0 then goto L910
	if ls+ul+ds+ic>0 then goto L910 else goto L470
L910: sp2=49-sp-1
	if ul=1 then pr #255,using L931: d$(1:sp2),dollar$,"{\UL ",total,"}",dollar$,"{\UL ",total2,"}" pageoflow L1450 : goto L930
	pr #255,using L930: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow L1450
L930: form pos sp,c sp2,pos 49,c 1,pic(--,---,---.##),pos 67,c 1,pic(--,---,---.##),skip redir
L931: form pos sp,c sp2,pos 49,c 1,c 5,pic(--,---,---.##),c 1,pos 73,c 1,c 5,pic(--,---,---.##),c 1,skip redir
	if pc0=1 then gosub BLDPCT2
	if pc3>0 or pc4>0 then pr #255,using L960: pc3,pc4
L960: form pos 63,n 4,pos 82,n 4,skip redir
	total=0
	total2=0
	gosub L1240
	if ul=1 then goto L1010
	gosub L1470
L1010: gosub L1300
	goto L470
L1030: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=49-sp-1
	if ul=1 then pr #255,using L931: d$(1:sp2),dollar$,"{\UL ",accum1,"}",dollar$,"{\UL ",accum2,"}" pageoflow L1450 : goto L1090
	pr #255,using L930: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow L1450
L1090: gosub L1240
	if ul=1 then goto L1110
	gosub L1470
L1110: gosub L1300
	goto L470
L1130: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1300
	goto L470
L1170: if foot1=1 then goto L1220
	tabnote=sp
	foot1=1
	foot$=d$
	goto L470
L1220: foot$=rtrm$(foot$)&d$
	goto L470
L1240: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1280 ! 10/14/87
		accum(j,1)=0
		accum(j,2)=0
L1280: next j
return
L1300: if ls=0 then goto L1440
	if ls=99 then goto L1350
	pr #255,using L1330: " "
L1330: form pos 1,c 1,skip ls
	goto L1440
L1350: fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1400: rtrm$(foot$),"Page "&str$(pt1)
L1400: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
	if eofcode=1 then goto L1440
	pr #255: newpage
	gosub L1600
L1440: return
L1450: gosub L1350
	continue
L1470: if ul=0 then goto L1560
	if ul=1 then goto L1530
	underlin$="=============="
	pr #255,using L1510: underlin$,underlin$
L1510: form pos 49,c 14,pos 67,c 14,skip redir
	goto L1560
L1530: underlin$="______________"
	pr #255,using L1550: underlin$,underlin$
L1550: form skip redir,pos 49,c 14,pos 67,c 14,skip redir
L1560: if redir=0 then pr #255,using L1570: " "
L1570: form skip 1,c 1,skip 0
return
 
L1600: heading=1
	if pt1=0 then pt1=1 else pt1=pt1+1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the quarter ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	pr #255,using L1690: lpad$(rtrm$(fncch$),20),"Year To DATE"
L1690: form pos 43,c 20,pos 69,c 12,skip 2
return
 
L1720: eofcode=1
	gosub L1350
	fncloseprn
	fnfscode(actpd)
	fnpriorcd(1)
	goto Xit
 
BLDPCT1: open #10: "Name=[Temp]\Work."&session$&",KFName=[Temp]\Addr."&session$&",Replace,RecL=17,KPS=1,KLN=5",internal,outIn,keyed
	for j=1 to lrec(3)
		read #3,using L1810,rec=j: pc1,bb,cb noRec L1900
L1810: form pos mp1,pd 3,pos 81,2*pd 6.2
		k$=cnvrt$("N 5",pc1)
		read #10,using L1840,key=k$: pc1,pc2,yt2 nokey L1890
L1840: form pos 1,g 5,2*pd 6.2
		pc2=pc2+cb-bb
		yt2=yt2+cb
		rewrite #10,using L1840: pc1,pc2,yt2
		goto L1900
L1890: write #10,using L1840: pc1,cb-bb,cb
L1900: next j
	pc0=1
return
 
BLDPCT2: !
	pc3=pc4=0
	if val(k$)=0 then goto L2040
	read #10,using L1840,key=k$: pc1,pc2,yt2 nokey L2040
	if total=0 then goto L2010
	pc3=round(((total-pc2)/total)*100,0)
	if pc3<-999 or pc3>9999 then pc3=0
L2010: if total2=0 then goto L2040
	pc4=round(((total2-yt2)/total2)*100,0)
	if pc4<-999 or pc4>9999 then pc4=0
L2040: return
 
Xit: fnXit
 
include: ertn
