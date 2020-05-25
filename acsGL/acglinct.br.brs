! Replace S:\acsGL\acglinct
! -- pr Income Statement : _
	! FOR 8 1/2 * 11 PAPER WITHOUT PERCENTAGES
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,cch$*20,by(13),bp(13),cap$*128,udf$*256
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
	dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,2),sc1$(2)*20
 
	fnTop(program$,cap$="Income Statement")
	fncno(cno,cnam$)
	udf$=env$('temp')&'\'
	actpd$=fnactpd$
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit : _
		! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	pors=1
	mp1=69
	if fnps=2 then mp1=mp1+3
	if fnps=2 then mp2=78 else mp2=75
	if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr" else : _
		fl1$="Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr"
	open #1: fl1$,internal,input,keyed
	if fnprocess=1 or fnUseDeptNo=0 then goto L320
	fnTos(sn$="GLInput") : _
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) : _
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
L320: fnopenprn : _
	if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
	if fnps=2 then goto L360 ! secondary
	execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 69 3 Replace DupKeys -N"
	goto L370
L360: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 72 3 Replace DupKeys -N"
L370: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed
	report$="Statement of Income and Expenses"
! GOSUB BLDPCT1 ! BUILD % BASED ON REF # IN PRIMARY FUND # IN G/L ACCOUNT
L400: read #1,using L450: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1650
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L400
	if costcntr=0 then goto L450
	if fc=0 and te$="F" then goto L460 ! 5/8/89
	if costcntr><fc then goto L400
L450: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
L460: if te$="S" or te$="F" then goto L480
	if heading=0 and te$><"R" then gosub L1530
L480: on pos ("RFHDTS",te$,1) goto L1070,L1110,L490,L540,L970,L1070 none L400
L490: pr #255,using L500: d$(1:40)
L500: form pos sp,c 40,skip 1
	gosub L1240
	gosub L1180
	goto L400
L540: if notrans=1 then goto L750
	if ir=val(r$) and val(r$)><0 then goto L680
	if ir>val(r$) then goto L680
L570: ! read gl master file for amounts
L580: read #3,using L600: ir,pcr,bb,cb,mat by,mat bp eof L740
	cb=1
	if ir=0 then goto L580 ! skip any gl accounts not pointed to ic
L600: form pos mp1,pd 3,pos mp2,pd 3,pos 81,41*pd 6.2
	if fnfscode=0 then goto L680
	if fnfscode<1 or fnfscode>13 then let fnfscode(1)
! If FNPRIORCD=1 Then cB=BY(FNFSCODE) Else cB=BP(FNFSCODE)
	if fnpriorcd=2 then goto L670
	if fnfscode>1 then bb=by(fnfscode-1) else bb=0
	goto L680
L670: if fnfscode>1 then bb=bp(fnfscode-1) else bb=0
L680: if ir=val(r$) then total=total+(cb-bb) else goto L720
	total2=total2+cb
	k$=cnvrt$("N 5",pcr)
	goto L570
L720: if ir<val(r$) then goto L570
	if ir>val(r$) then goto L750
L740: notrans=1
L750: for j=1 to 9
		if ac(j)=9 then goto L790 ! 10/14/87
		accum(j,1)=accum(j,1)+total
		accum(j,2)=accum(j,2)+total2
L790: next j
	if rs=1 then total=-total else goto L820
	total2=-total2
L820: if ds=1 then dollar$="$" else dollar$=" "
	if total><0 or total2><0 then goto L850
	if ls+ul+ds+ic>0 then goto L850 else goto L400
L850: sp2=49-sp-1
	pr #255,using L870: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow L1390
L870: form pos sp,c sp2,pos 49,c 1,pic(-----,---,---.##),pos 67,c 1,pic(-------,---,---.##),skip redir
	if pc0=1 then gosub BLDPCT2
	if pc3>0 or pc4>0 then pr #255,using L900: pc3,pc4
L900: form pos 63,n 4,pos 82,n 4,skip redir
	total=0
	total2=0
	gosub L1180
	gosub L1410
	gosub L1240
	goto L400
L970: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=49-sp-1
	pr #255,using L870: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow L1390
	gosub L1180
	gosub L1410
	gosub L1240
	goto L400
L1070: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1240
	goto L400
L1110: if foot1=1 then goto L1160
	tabnote=sp
	foot1=1
	foot$=d$
	goto L400
L1160: foot$=rtrm$(foot$)&d$
	goto L400
L1180: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1220 ! 10/14/87
		accum(j,1)=0
		accum(j,2)=0
L1220: next j
return
L1240: if ls=0 then goto L1380
	if ls=99 then goto L1290
	pr #255,using L1270: " "
L1270: form pos 1,c 1,skip ls
	goto L1380
L1290: fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1340: rtrm$(foot$),"Page "&str$(pt1)
L1340: form skip sk,pos tabnote,c fl,pos 80,c 8,skip 1
	if eofcode=1 then goto L1380
	pr #255: newpage
	gosub L1530
L1380: return
L1390: gosub L1290
	continue
L1410: if ul=0 then goto L1500
	if ul=1 then goto L1470
	underlin$="============="
	pr #255,using L1450: underlin$&"====",underlin$&"======"
L1450: form skip 1,pos 49,c 17,pos 67,c 19,skip redir
	goto L1500
L1470: underlin$="______________"
	pr #255,using L1490: underlin$&"___",underlin$&"_____"
L1490: form skip redir,pos 49,c 18,pos 67,c 19,skip redir
L1500: if redir=0 then pr #255,using L1510: " "
L1510: form skip 1,c 1,skip redir
return
L1530: heading=1
	if pt1=0 then pt1=1 else pt1=pt1+1
	pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs24 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	pr #255,using L1620: lpad$(rtrm$(fncch$),20),"Year To Date"
L1620: form pos 45,c 20,pos 73,c 12,skip 2
return
 
L1650: eofcode=1
	gosub L1290
 
	fncloseprn
	goto Xit
 
BLDPCT1: open #10: "Name="&env$('temp')&"\Work."&session$&",KFName="&env$('Temp')&"\Addr."&session$&",Replace,RecL=17,KPS=1,KLN=5",internal,outIn,keyed
	for j=1 to lrec(3)
		read #3,using L1740,rec=j: pc1,bb,cb noRec L1830
		cb=1
L1740: form pos mp1,pd 3,pos 81,2*pd 6.2
		k$=cnvrt$("N 5",pc1)
		read #10,using L1770,key=k$: pc1,pc2,yt2 nokey L1820
L1770: form pos 1,g 5,2*pd 6.2
		pc2=pc2+cb-bb
		yt2=yt2+cb
		rewrite #10,using L1770: pc1,pc2,yt2
		goto L1830
L1820: write #10,using L1770: pc1,cb-bb,cb
L1830: next j
	pc0=1
return
 
BLDPCT2: !
	pc3=pc4=0
	if val(k$)=0 then goto L1970
	read #10,using L1770,key=k$: pc1,pc2,yt2 nokey L1970
	if total=0 then goto L1940
	pc3=round(((total-pc2)/total)*100,0)
	if pc3<-999 or pc3>9999 then pc3=0
L1940: if total2=0 then goto L1970
	pc4=round(((total2-yt2)/total2)*100,0)
	if pc4<-999 or pc4>9999 then pc4=0
L1970: return
 
Xit: fnXit
 
include: Ertn
