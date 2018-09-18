! formerly S:\acsGL\BalanceSheet
! Balance Sheet - Standard 8.5x11
! r: setup
	library 'S:\Core\Library': fntop,fnxit,fnopenprn,fncloseprn,fnerror,fnprocess,fnpedat$,fnpriorcd,fnps,fnfscode,fnUseDeptNo,fnglfs,fnpglen,fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs,fnactpd,fnactpd$,fnindex_it
	on error goto ERTN

	dim b$*3
	dim a$(8)*30
	dim oldtrans$*16
	dim g(8)
	dim d(2)
	dim by(13)
	dim bp(13)
	dim r$*5
	dim d$*50,te$*1,ac(9)
	dim report$*50
	dim secondr$*50
	dim foot$*132
	dim underlin$*14
! /r
	fntop(program$)
	actpd$=fnactpd$ 
	actpd=fnactpd
	fnfscode(actpd)
	fnpriorcd
	if fnglfs=5 then goto XIT           ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	fnfscode
	fnpriorcd
	if fnps=2 then 
		mp1=66
		open #1:"Name=[Q]\GLmstr\acglFnSC.h[cno],KFName=[Q]\GLmstr\fnSCIndx.h[cno],Shr",internal,input,keyed 
	else
		mp1=63
		open #1:"Name=[Q]\GLmstr\ACGLFNSB.h[cno],KFName=[Q]\GLmstr\FNSBIndx.h[cno],Shr",internal,input,keyed 
	end if
	if fnprocess=1 or fnUseDeptNo=0 then goto GetStarted else goto Screen1 
! /r
Screen1: ! r:
	fnTos(sn$="GLInput") 
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 )
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto XIT
	costcntr=val(resp$(1)) 
goto GetStarted ! /r
GetStarted: ! r:
	if fnps=2 then 
		! secondary
		fnindex_it("[Q]\GLmstr\GLmstr.h[cno]","[Q]\GLmstr\fsindex.H[cno]","66 3")
	else
		fnindex_it("[Q]\GLmstr\GLmstr.h[cno]","[Q]\GLmstr\fsindex.H[cno]","63 3")
	end if
	open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\fsindex.h[cno],Shr",internal,input,keyed 
	fnopenprn
	report$="Balance Sheet"
	READ_TOP: ! 
	read #1,using L380: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof DONE
	L380: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto READ_TOP
	if costcntr=0 then goto L420
	if costcntr><fc then goto READ_TOP
	L420: !
	if te$="S" or te$="F" then goto L440
	if heading=0 and te$><"R" then gosub HEADER
	L440: !
	on pos ("RFHDTSPE",te$,1) goto L970,L1010,L460,L520,L840,L970,L840,L520 none READ_TOP
! /r
L460: ! r:
	pr #255,using L470: d$
	L470: form pos sp,c 50,skip 1
	gosub FOOTER
	gosub SET_ACCUM
goto READ_TOP ! /r
L520: ! r:
	if notrans=1 then goto L660
	if br>=val(r$) and val(r$)><0 then goto L610
	L540: ! read general ledger master file for amounts
	L560: !
	read #3,using 'Form POS MP1,PD 3,POS 87,27*PD 6.2': br,cb,mat by,mat bp eof L650
	if br=0 then goto L560
	if fnfscode=0 or (fnfscode=actpd and fnpriorcd=1) then goto L610
	if fnfscode<1 or fnfscode>12 then let fnfscode(1)
	if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
	L610: !
	if br=val(r$) then 
		total=total+cb 
	else 
		goto L630
	end if
	goto L540
	L630: !
	if br<val(r$) then goto L540
	if br>val(r$) then goto L660
	L650: !
	notrans=1
	
	L660: !
	if te$="E" then total=-accum(ap)
	for j=1 to 9
		if ac(j)=9 then 
			goto L690 
		else 
			accum(j)=accum(j)+total
		end if
		L690: !
	next j
	if rs=1 then total=-total
	if ds=1 then dollar$="$" else dollar$=" "
	dollar=24+14*bc ! If CP=1 Then dOLLAR=50+14*BC Else dOLLAR=24+14*BC
	if total><0 then goto L750
	if ls+ul+ds+ic>0 then 
		goto L750 
	else 
		goto READ_TOP
	end if
	
	L750: !
	sp2=dollar-sp-1
	if ul=1 then 
		pr #255,using L761: d$(1:sp2),dollar$,"{\ul ",total,"}" pageoflow PGOF  ! atlantis underline
		L761: form pos sp,c sp2,pos dollar,c 1,c 5,pic(---,---,---.##),c 2,skip 0  ! ! atlantis underline
	else
		pr #255,using L770: d$(1:sp2),dollar$,total pageoflow PGOF
		L770: form pos sp,c sp2,pos dollar,c 1,pic(---,---,---.##),skip 0
	end if
	total=0
	gosub SET_ACCUM
	if ul<>1 then ! atlantis underline
		gosub UNDERLINE
	end if
	gosub FOOTER
goto READ_TOP ! /r
L840: ! r:
	if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap) else accum1=accum(ap)
	if ds=1 then dollar$="$" else dollar$=" "
	dollar=24+14*bc ! if  CP=1 Then dOLLAR=50+14*BC Else dOLLAR=24+14*BC
	sp2=dollar-sp-1
	if ul=1 then pr #255,using L761: d$(1:sp2),dollar$,"{\ul ",accum1,"}" pageoflow PGOF : goto L900
	pr #255,using L770: d$(1:sp2),dollar$,accum1 pageoflow PGOF
	L900: !
	gosub SET_ACCUM
	if ul<>1 then ! atlantis underline
		gosub UNDERLINE
	end if
	gosub FOOTER
	if te$><"P" then goto L950
	for j=1 to 9
		accum(j)=accum(j)-accum(ap) 
	next j
	L950: !
goto READ_TOP ! /r
L970: ! r:
	if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub FOOTER
goto READ_TOP ! /r
L1010: ! r:
	if foot1=1 then goto L1070
	tabnote=sp
	foot1=1
	foot$=d$
goto READ_TOP ! /r
L1070: ! r:
	foot$=rtrm$(foot$)&d$
goto READ_TOP ! /r
SET_ACCUM: ! r:
	for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1130 else accum(j)=0
	L1130: !
	next j
return ! /r
FOOTER: ! r:
	if ls=0 then goto EO_FOOTER
	if ls=99 then goto L1220
	pr #255,using L1200: " "
	L1200: form pos 1,c 1,skip ls
	goto EO_FOOTER
	L1220: fnpglen(pglen)
	! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
	! If PGLEN=42 Then sK+=1
	pr #255,using L1270: rtrm$(foot$)
	L1270: form skip sk,pos tabnote,c fl,skip 1
	if eofcode=1 then goto EO_FOOTER
	pr #255: newpage
	gosub HEADER
	EO_FOOTER: !
return ! /r
PGOF: ! r:
	gosub L1220
continue ! /r
UNDERLINE: ! r: requires ul,bc
	if ul then
		underlin=25+14*bc ! if CP=1 Then uNDERLIN=51+14*BC Else uNDERLIN=25+14*BC
		if ul<>1 then
			underlin$="=============="
			pr #255,using L1430: underlin$
			L1430: form pos underlin,c 14,skip 0  ! atlantis underline
		else
			underlin$="______________"
			pr #255,using L1470: underlin$
			L1470: form pos underlin,c 14,skip 0
		end if
	end if
	pr #255,using 'form skip 1,c 1,skip 0': " "
return ! /r
HEADER: ! r:
	heading=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(fnpedat$)&"}"
	pr #255: "\ql "
return ! /r
DONE: ! r:
	eofcode=1
	gosub L1220
	fnfscode(actpd)
	fnpriorcd(1)
	if pors<>2 then let fncloseprn
goto XIT ! /r
XIT: fnxit
include: ertn