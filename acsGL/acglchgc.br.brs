! Replace S:\acsGL\ACGLCHGC
! STATEMENT OF CHANGES IN FINANCIAL POSITION FOR 8 1/2 * 11 PAPER WITH            COMPARSION
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,cogl$(3)*12,acct$*12,bp(13),by(13),udf$*256
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
	dim b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,2),by(13),cap$*128
 
	fnTop(program$,cap$="Comparative Change Amount")
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit
	udf$=env$('temp')&'\'
	open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative: read #20,using 'Form Pos 152,3*C 12',rec=1: mat cogl$ : close #20:
	actpd=fnactpd : fscode=fnfscode : priorcd=fnpriorcd
	on fkey 5 goto L2060
	mp1=75
	if fnps=2 then mp1=mp1+3
	fl1$="Name=[Q]\GLmstr\ACGLFNSF.h[cno],KFName=[Q]\GLmstr\agfsidx5.h[cno],Shr"
	if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSG.h[cno],KFName=[Q]\GLmstr\agfsidx6.h[cno],Shr"
	flo$(1)="8,3,C 50,N" : _
	flo$(2)="8,55,N 10.2,N" : _
	flo$(3)="8,69,N 10.2,N"
	fli$(1)="08,03,C 50,UT,N" : _
	fli$(2)="08,55,N 10.2,UT,N" : _
	fli$(3)="08,69,N 10.2,UT,N"
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,input,keyed
L250: read #1,using L260: acct$,cb,mat by,mat bp eof L350
L260: form pos 1,c 12,pos 87,27*pd 6.2
	if acct$>cogl$(3) then goto L350
	if priorcd=2 then income=income-bp(fscode) else goto L310
	pincome=0
	goto L330
L310: if fscode<=0 or fscode>12 then : _
		income=income-cb else income=income-by(fscode)
	if fscode<=0 or fscode>12 then : _
		pincome=pincome-bp(actpd) else pincome=pincome-bp(fscode)
L330: goto L250
! 
L350: close #1:
	open #1: fl1$,internal,input,keyed
	if fnprocess=1 or fnUseDeptNo=0 then goto L480
! 
	fnTos
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
L480: fnopenprn : _
	if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
	report$="Statement of Changes in Financial Position"
	if fnps=2 then goto L530 ! secondary
	execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 75 3 Replace DupKeys -N"
	goto L540
L530: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 78 3 Replace DupKeys -N"
L540: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed
L550: read #1,using L590: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2060
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L550
	if costcntr=0 then goto L590
	if costcntr><fc then goto L550
L590: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	if te$="S" or te$="F" then goto L620
	if heading=0 and te$><"R" then gosub L1840
L620: on pos ("RFHDTSP",te$,1) goto L1310,L1360,L630,L680,L1200,L1310,L2110 none L550
L630: pr #255,using L640: d$
L640: form pos sp,c 50,skip 1
	gosub L1510
	gosub L1440
	goto L550
L680: if notrans=1 then goto L1020
	if fr=val(r$) and val(r$)><0 then goto L780
	if fr>val(r$) then goto L780
L710: ! read amounts from g/ master file
L720: read #3,using L740: fr,bb,cb,mat by,mat bp,pbp eof L860
	if fr=0 then goto L720
L740: form pos mp1,pd 3,pos 81,28*pd 6.2,pos 327,pd 6.2
	if fscode=0 then goto L780
	if fscode<1 or fscode>12 then fscode=1
	if priorcd=2 then cb=bp(fscode) else cb=by(fscode)
L780: if fr=val(r$) then goto L790 else goto L840
L790: if priorcd=2 then total=total+(cb-pbp) : _
	else total=total+(cb-bp(12))
	if priorcd=2 then total2=0 else goto L820
	goto L830
L820: if fscode<=0 or fscode>12 then total2=total2+(bp(actpd)-pbp) else : _
		total2=total2+(bp(fscode)-pbp)
L830: goto L710
L840: if fr<val(r$) then goto L710
	if fr>val(r$) then goto L870
L860: notrans=1
L870: fnTos(sn$="ACglchgs2") : _
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Description:",mylen,right)
	fnTxt(1,mypos,50,0,right,"",0,"Enter the description if not accurate.",0 ) : _
	resp$(1)=d$
	fnLbl(2,1,"Total Year to Date:",mylen,right)
	fnTxt(2,mypos,12,0,right,"10",0,"Enter the total for the year.",0 ) : _
	resp$(2)=str$(total)
	fnLbl(3,1,"Total Last Year to Date:",mylen,right)
	fnTxt(3,mypos,12,0,right,"10",0,"Enter the total for last year.",0 ) : _
	resp$(3)=str$(total2)
	fnCmdKey("&Next",1,1,0,"Accept the answer.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	d$=resp$(1)
	total=val(resp$(2))
	total2=val(resp$(3))
 
L1020: for j=1 to 9
		accum(j,1)=accum(j,1)+total
		accum(j,2)=accum(j,2)+total2
	next j
	if rs=1 then total=-total else goto L1080
	total2=-total2
L1080: if ds=1 then dollar$="$" else dollar$=" "
	if total><0 or total2><0 then goto L1110
	if ls+ul+ds+ic>0 then goto L1110 else goto L550
L1110: sp2=49-sp-1
	pr #255,using L1130: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow L1680
L1130: form pos sp,c sp2,pos 49,c 1,pic(--,---,---.##),pos 67,c 1,pic(--,---,---.##),skip redir
	total=0
	total2=0
	gosub L1440
	gosub L1700
	gosub L1510
	goto L550
L1200: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	sp2=49-sp-1
	if ds=1 then dollar$="$" else dollar$=" "
	pr #255,using L1130: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow L1680
	gosub L1440
	gosub L1700
	gosub L1510
	goto L550
 
L1310: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1510
	goto L550
 
L1360: if foot1=1 then goto L1420
	tabnote=sp
	foot1=1
	foot$=d$
	goto L550
 
L1420: foot$=rtrm$(foot$)&d$
	goto L550
L1440: for j=1 to 9
		if ac(j)=0 then goto L1480
		accum(j,1)=0
		accum(j,2)=0
L1480: next j
return
 
L1510: if ls=0 then goto L1660
	if ls=99 then goto L1570
	pr #255,using L1540: " "
L1540: form pos 1,c 1,skip ls
	goto L1660
 
L1570: fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1620: rtrm$(foot$),"Page "&str$(pt1)
L1620: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
	if eofcode=1 then goto L1660
	pr #255: newpage
	gosub L1840
L1660: return
 
L1680: gosub L1570: continue
 
L1700: if ul=0 then goto L1800
	if ul=1 then goto L1770
	underlin$="=============="
	pr #255,using L1740: underlin$,underlin$
L1740: form skip 1,pos 49,c 14,pos 67,c 14,skip redir
	goto L1800
 
L1770: underlin$="______________"
	pr #255,using L1790: underlin$,underlin$
L1790: form pos 49,c 14,pos 67,c 14,skip redir
L1800: if redir=0 then pr #255,using L1810: " "
L1810: form skip 1,c 1,skip 0
return
 
L1840: heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255: ""
	on error goto L2010
	a=len(rtrm$(fnpedat$))
	b=val(rtrm$(fnpedat$(a-4:a)))
	c=b-1
	pr #255,using L1970: b,c
L1970: form pos 52,pic(-----),pos 71,pic(-----),skip 2
	on error goto Ertn
	goto L2040
 
L2010: pr #255: tab(49);"Current Year";tab(68);"Prior Year"
	on error goto Ertn
	pr #255:
L2040: return
 
L2060: eofcode=1
	gosub L1570
	fncloseprn
	goto Xit
 
L2110: total=income
	total2=pincome
	goto L1020
 
Xit: fnXit
 
ERTN: fnerror(program$,err,line,act$,"Xit")
	if lwrc$(act$)<>"pause" then goto L2210
	execute "list -"&str$(line) : _
	pause  : _
	goto L2210
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause
L2210: execute act$
	goto ERTN
