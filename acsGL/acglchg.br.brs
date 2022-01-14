! Replace S:\acsGL\acglChg
! Statement of Change in Financial Position with Comparrison  : _
	! FOR 8 1/2 * 11
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,p$(20)*50,cap$*128
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
	dim cnam$*40,accum(9,2),acct$*12,bp(13),by(13),udf$*256
 
	fnTop(program$,cap$="Comparative (FP)")
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit
	fncno(cno,cnam$)
	udf$=env$('temp')&'\'
	open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,i,r: read #20,using 'form pos 152,3*C 12',rec=1: mat cogl$ : close #20:
	actpd=fnactpd : fscode=fnfscode
	pors=1
	on fkey 5 goto L1830
	mp1=75
	if fnps=2 then mp1=mp1+3
	fl1$="Name=[Q]\GLmstr\ACGLFNSF.h[cno],KFName=[Q]\GLmstr\agfsidx5.h[cno],Shr"
	if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSG.h[cno],KFName=[Q]\GLmstr\agfsidx6.h[cno],Shr"
L230: form pos mp1,pd 3,pos 81,28*pd 6.2,pos 327,pd 6.2
L240: form pos 1,c 12,pos 87,27*pd 6.2
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",i,i,k
L260: read #1,using L240: acct$,cb,mat by,mat bp eof L340
	if acct$>cogl$(3) then goto L340
	if fnpriorcd=2 then income=income-bp(fscode) else goto L310
	pincome=0
	goto L330
L310: if fscode<=0 or fscode>12 then : _
		income=income-cb else : _
		income=income-by(fscode)
	if fscode<=0 or fscode>12 then : _
		pincome=pincome-bp(actpd) else : _
		pincome=pincome-bp(fscode)
L330: goto L260
L340: close #1:
	open #1: fl1$,i,i,k
	if fnprocess=1 or fnUseDeptNo=0 then goto L460
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
L460: fnopenprn : _
	redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
	report$="STATEMENT OF CHANGES IN FINANCIAL POSITION"
	if fnps=2 then goto L510 ! secondary
	execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 75 3 Replace DupKeys -N"
	goto L520
L510: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 78 3 Replace DupKeys -N"
L520: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",i,i,k
L530: read #1,using L570: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1830
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L530
	if costcntr=0 then goto L570
	if costcntr><fc then goto L530
L570: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	if te$="S" or te$="F" then goto L600
	if heading=0 and te$><"R" then gosub L1620
L600: on pos ("RFHDTSP",te$,1) goto L1110,L1160,L610,L660,L1000,L1110,L1890 none L530
L610: pr #255,using L620: d$
L620: form pos sp,c 50
	gosub L1320
	gosub L1250
	goto L530
L660: if notrans=1 then goto L840
	if fr=val(r$) and val(r$)><0 then goto L750
	if fr>val(r$) then goto L750
L690: ! read amounts from gl master file
L700: read #3,using L230: fr,bb,cb,mat by,mat bp,pbp eof L830
	if fr=0 then goto L700
	if fscode=0 then goto L750
	if fscode<1 or fscode>12 then fscode=1
	if fnpriorcd=2 then cb=bp(fscode) else cb=by(fscode)
L750: if fr=val(r$) then goto L760 else goto L810
L760: if fnpriorcd=2 then : _
		total=total+(cb-pbp) else : _
		total=total+(cb-bp(12))
	if fnpriorcd=2 then total2=0 else goto L790
	goto L800
L790: if fscode<=0 or fscode>12 then : _
		total2=total2+(bp(actpd)-pbp) else : _
		total2=total2+(bp(fscode)-pbp)
L800: goto L690
L810: if fr<val(r$) then goto L690
	if fr>val(r$) then goto L840
L830: notrans=1
L840: for j=1 to 9 : accum(j,1)+=total : accum(j,2)+=total2 : next j
	if rs=1 then total=-total else goto L870
	total2=-total2
L870: if ds=1 then dollar$="$" else dollar$=" "
	if total><0 or total2><0 then goto L900
	if ls+ul+ds+ic>0 then goto L900 else goto L530
L900: sp2=49-sp-1
	pr #255,using L920: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow L1480
L920: form pos sp,c sp2,pos 49,c 1,pic(--,---,---.##),pos 67,c 1,pic(--,---,---.##),skip redir
	total=0
	total2=0
	gosub L1250
	gosub L1490
	gosub L1320
	goto L530
 
L1000: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=49-sp-1
	pr #255,using L920: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow L1480
	gosub L1250
	gosub L1490
	gosub L1320
	goto L530
 
L1110: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1320
	goto L530
 
L1160: if foot1=1 then goto L1220
	tabnote=sp
	foot1=1
	foot$=d$
	goto L530
 
L1220: foot$=rtrm$(foot$)&d$
	goto L530
 
L1250: for j=1 to 9
		if ac(j)=0 then goto L1290
		accum(j,1)=0
		accum(j,2)=0
L1290: next j
return
 
L1320: if ls=0 then goto L1460
	if ls=99 then goto L1370
	pr #255,using L1350: " "
L1350: form pos 1,c 1,skip ls
	goto L1460
L1370: fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1420: rtrm$(foot$),"Page "&str$(pt1)
L1420: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
	if eofcode=1 then goto L1460
	pr #255: newpage
	gosub L1620
L1460: return
 
L1480: gosub L1370: continue
L1490: if ul=0 then goto L1580
	if ul=1 then goto L1550
	underlin$="=============="
	pr #255,using L1530: underlin$,underlin$
L1530: form skip 1,pos 49,c 14,pos 67,c 14,skip redir
	goto L1580
L1550: underlin$="______________"
	pr #255,using L1570: underlin$,underlin$
L1570: form pos 49,c 14,pos 67,c 14,skip redir
L1580: if redir=0 then pr #255,using L1590: " "
L1590: form skip 1,c 1,skip 0
return
 
L1620: heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	on error goto L1780
	a=len(rtrm$(fnpedat$))
	b=val(rtrm$(fnpedat$(a-4:a)))
	c=b-1
	pr #255,using L1750: b,c
L1750: form pos 52,pic(-----),pos 71,pic(-----),skip 2
	on error goto Ertn
	goto L1810
L1780: pr #255: tab(49);"CURRENT YEAR";tab(68);"PRIOR YEAR"
	on error goto Ertn
	pr #255:
L1810: return
 
L1830: eofcode=1
	gosub L1370
 
	fncloseprn
	goto Xit
 
L1890: total=income
	total2=pincome
	goto L840
 
Xit: fnXit
 
include: ertn
