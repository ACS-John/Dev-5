! Replace S:\acsGL\acglCasB
! CASH FLOW STATEMENT  WITH BUDGET

	autoLibrary
	on error goto Ertn

	dim bm(13),bp(13),by(13),sc1$(2)*20,fl1$*256,in3$(4),p$(20)*50
	dim b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7),cch$*15
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14

	fnTop(program$,"Cash Flow Statement With Budget")
	actpd$=fnactpd$
	actpd=fnactpd
	fnfscode
	fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	fscode=fnfscode
	priorcd=fnpriorcd
	open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative
	read #20,using 'Form Pos 384,n 2',rec=1: nap
	close #20:
	actpd=fnactpd
	fscode=fnfscode
	if nap<12 or nap> 13 then nap=12
	pors=1
	fnopenprn
	if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
	on fkey 5 goto L2170
	in3$(1)="8,5,N 12.2,UT,N"
	in3$(2)="8,25,N 12.2,UT,N"
	in3$(3)="8,45,N 12.2,UT,N"
	in3$(4)="8,65,N 12.2,UT,N"
	mp1=75
	if fnps=2 then mp1=mp1+3
	fl1$="Name=[Q]\GLmstr\ACGLFNSF.h[cno],KFName=[Q]\GLmstr\agfsidx5.h[cno],Shr"
	if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSG.h[cno],KFName=[Q]\GLmstr\agfsidx6.h[cno],Shr"
	form c 7,skip 0
	open #1: fl1$,internal,input,keyed
	if fnprocess=1 or fnUseDeptNo=0 then goto L410
	fnTos
	mylen=30 : mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) : _
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
	L410: report$=env$('program_caption')
	L420: read #1,using L460: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2170
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L420
	if costcntr=0 then goto L460
	if costcntr><fc then goto L420
	L460: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	if te$="S" or te$="F" then goto L490
	if heading=0 and te$><"R" then gosub L2040
	L490: on pos ("RFHDTSBC",te$,1) goto L1480,L1530,L500,L550,L1320,L1480,L550,L2240 none L420
L500: !
	pr #255,using L510: d$(1:40)
	L510: form pos sp,c 40,skip 1
	gosub L1730
	gosub L1610
goto L420
L550: !
	if te$="B" and ap>0 then goto L1320 ! ENDING BANK BALANCE
	if notrans=1 then goto L940
	if ir>=val(r$) and val(r$)><0 then goto L740
	close #3: ioerr ignore
	if fnps=2 then
		fnIndex('[Q]\GLmstr\GLmstr.h[cno]','[temp]\fsindex.h[cno]','78 3')
	else
		fnIndex('[Q]\GLmstr\GLmstr.h[cno]','[temp]\fsindex.h[cno]','75 3')
	end if
	open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Temp]\fsindex.h[cno],Shr",internal,input,keyed
	L630: ! read amounts from gl master file
	L640: !
	read #3,using L730: ir,bb,cb,mat by,mat bp,mat bm eof L940
	if ir=0 then goto L640
	if fscode=0 or (fscode=actpd and priorcd=1) then goto L730
	if fscode<1 or fscode>13 then fscode=1
	if fnpriorcd=1 then cb=by(fscode) else cb=bp(fscode)
	if fnpriorcd=2 then goto L720
	if fscode>1 then bb=by(fscode-1) else bb=0
goto L730
L720: if fscode>1 then bb=bp(fscode-1) else bb=0
L730: form pos mp1,pd 3,pos 81,41*pd 6.2
L740: if ir=val(r$) then total=total+(cb-bb) else goto L900
	if te$="B" then total=total-(cb-bb): total=total - bb: total2=total2-bp(nap) : goto L770
	total2=total2+cb
L770: for z=1 to 13
		annualb=annualb+bm(z)
	next z
	if fscode=0 then monthb=monthb+bm(actpd) else monthb=monthb+bm(fscode)
	if fscode=0 then goto L820 else goto L860
L820: for j=1 to actpd
		ytdb=ytdb+bm(j)
	next j
	goto L630
L860: for j=1 to fscode
		ytdb=ytdb+bm(j)
	next j
	goto L630
L900: if ir<val(r$) then goto L630
	if ir>val(r$) then goto L940
	notrans=1
	gosub L2240
L940: overundr=ytdb-total2
	unexpend=annualb-total2
	for j=1 to 9
		if ac(j)=9 then goto L1050
		accum(j,1)+=total
		accum(j,2)+=total2
		accum(j,3)=accum(j,3)+annualb
		accum(j,4)=accum(j,4)+monthb
		accum(j,5)=accum(j,5)+ytdb
		accum(j,6)=accum(j,6)+overundr
		accum(j,7)=accum(j,7)+unexpend
L1050: next j
	if rs=1 then total=-total else goto L1130
	total2=-total2
	annualb=-annualb
	monthb=-monthb
	ytdb=-ytdb
	overundr=overundr
	unexpend=unexpend
L1130: if ds=1 then dollar$="$" else dollar$=" "
	if annualb><0 or total2><0 then goto L1170
	if total<>0 then goto L1170
	if ls+ds+ul+ic>0 then goto L1170 else goto L420
L1170: sp2=24-sp-1
	if te$="B" then total=-total: total2=-total2 ! REVERSE SIGN ON BEGINNING BANK BALANCE
	if ul=1 then pr #255,using L1201: d$(1:sp2),dollar$,"{\ul ",monthb,"}",dollar$,"{\ul ",total,"}",dollar$,"{\ul ",total2,"}",dollar$,"{\ul ",annualb,"}" pageoflow L1900 : goto L1200
	pr #255,using L1200: d$(1:sp2),dollar$,monthb,dollar$,total,dollar$,total2,dollar$,annualb pageoflow L1900
L1200: form pos sp,c sp2,pos 24,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),skip redir
L1201: form pos sp,c sp2,pos 24,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,skip redir
	total=0
	total2=0
	annualb=0
	monthb=0
	ytdb=0
	overundr=0
	unexpend=0
	gosub L1610
	if ul=1 then goto L1300
	gosub L1910
L1300: gosub L1730
	goto L420
L1320: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
	if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
	if rs=1 then accum5=-accum(ap,5) else accum5=accum(ap,5)
	if rs=1 then accum6=accum(ap,6) else accum6=accum(ap,6)
	if rs=1 then accum7=accum(ap,7) else accum7=accum(ap,7)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=24-sp-1
	if te$="B" then accum2=accum3=accum4=0
	if ul=1 then pr #255,using L1201: d$(1:sp2),dollar$,"{\ul ",accum4,"}",dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}",dollar$,"{\ul ",accum3,"}" pageoflow L1900 : goto L1440
	pr #255,using L1200: d$(1:sp2),dollar$,accum4,dollar$,accum1,dollar$,accum2,dollar$,accum3 pageoflow L1900
L1440: gosub L1610
	if ul=1 then goto L1460
	gosub L1910
L1460: gosub L1730
	goto L420
L1480: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1730
	goto L420

L1530: if foot1=1 then goto L1590
	tabnote=sp
	foot1=1
	foot$=d$
	goto L420

L1590: foot$=rtrm$(foot$)&d$ : goto L420

L1610: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1700
		accum(j,1)=0
		accum(j,2)=0
		accum(j,3)=0
		accum(j,4)=0
		accum(j,5)=0
		accum(j,6)=0
		accum(j,7)=0
L1700: next j
return

L1730: if ls=0 then goto L1880
	if ls=99 then goto L1790
	pr #255,using L1760: " "
L1760: form pos 1,c 1,skip ls
	goto L1880

L1790: fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1840: rtrm$(foot$),"Page "&str$(pt1)
L1840: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
	if eofcode=1 then goto L1880
	pr #255: newpage
	gosub L2040
L1880: return

L1900: gosub L1790: continue
L1910: if ul=0 then goto L2000
	if ul=1 then goto L1970
	underlin$="=============="
	goto L1980
	goto L2000
L1970: underlin$="______________"
L1980: pr #255,using L1990: underlin$,underlin$,underlin$,underlin$
L1990: form pos 24,4*c 15,skip redir
L2000: ! If REDIR=0 Then pr #255,Using 2010: " "
	form c 1,skip 1
return

L2040: heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	cch$=lpad$(rtrm$(fncch$),15)
	pr #255: tab(31);"MONTHLY";tab(38);cch$;tab(61);"YEAR TO";tab(77);"ANNUAL"
	pr #255: tab(32);"BUDGET";tab(44);"       ";tab(62);"DATE";tab(77);"BUDGET"
	pr #255:
return

L2170: eofcode=1
	gosub L1790
	fnfscode(actpd)
	fnpriorcd(1)
	fncloseprn
	goto Xit

L2240: !
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,d$,mylen,right)
	fnLbl(3,1,"Monthy Budget:",mylen,right)
	fnTxt(3,mypos,12,0,right,"10",0,"Enter the monthly budget.",0 )
	resp$(1)=str$(monthb)
	fnLbl(4,1,"Total for the Month:",mylen,right)
	fnTxt(4,mypos,12,0,right,"10",0,"Enter the total for the month.",0 )
	resp$(2)=str$(total)
	fnLbl(5,1,"Total Year to Date:",mylen,right)
	fnTxt(5,mypos,12,0,right,"10",0,"Enter the total for the year.",0 )
	resp$(3)=str$(total2)
	fnLbl(6,1,"Total Budget for the Year:",mylen,right)
	fnTxt(6,mypos,12,0,right,"10",0,"Enter the total budget for the  year.",0 )
	resp$(4)=str$(annualb)
	fnCmdKey("&Next",1,1,0,"Accept the answer.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	monthb=val(resp$(1))
	total=val(resp$(2))
	total2=val(resp$(3))
	annualb=val(resp$(4))
	goto L940
Xit: fnXit
include: ertn
