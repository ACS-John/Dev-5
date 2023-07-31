! formerly acsgl\acglbalf  !  G/L BALANCE SHEET -  STANDARD FOR 8 1/2 * 11 PAPER
autoLibrary
on error goto Ertn

fnTop(program$)
actpd$=fnactpd$
actpd=fnactpd
fnfscode
fnpriorcd
if fnGlAskFormatPriorCdPeriod=5 then goto Xit
	! sets fnPs,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
fnfscode
fnpriorcd
if fnProcess=0 then gosub AskFund
mp1=63 : if fnPs=2 then mp1+=3
if fnPs=2 then
	open #hReport=fnH: "Name=[Q]\GLmstr\AcGLFnSc.h[cno],KFName=[Q]\GLmstr\agfsidx1.h[cno],Shr",i,i,k
else
	open #hReport=fnH: "Name=[Q]\GLmstr\ACGLFNSB.h[cno],KFName=[Q]\GLmstr\agfsidx4.h[cno],Shr",i,i,k
end if
Freport: form pos 1,n 3,n 6,n 3,pos mp1,pd 3,pos 87,27*pd 6.2

fnOpenPrn
fnFsIndexBalSht
open #hAcct=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[temp]\fsindex.h[cno],Shr",i,i,k
if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
! on fkey 5 goto Finis
dim report$*50
report$=env$('program_caption')
goto PrTeLoopTop

PrTeLoopTop: ! r:
	dim r$*5
	dim d$*50
	dim te$*1
	dim ac(9)
	read #hReport,using 'form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3': r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof Finis
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto PrTeLoopTop
	if costcntr=0 then goto L480
	if costcntr><fc then goto PrTeLoopTop
	L480: !
	if te$="S" or te$="F" then goto L510
	if heading=0 and te$><"R" then gosub PrHead1
	L510: !
on pos ("RFHDTSPE",te$,1) goto PrTeRs,PrTeF,PrTeH,PrTeDe,PrTeTp,PrTeRs,PrTeTp,PrTeDe none PrTeLoopTop ! /r
PrTeH: ! r:
	pr #255,using L530: d$
	L530: form pos sp,c 50,skip 1
	gosub PrFooterAndHeader1
	gosub AddToAccum
goto PrTeLoopTop ! /r
PrTeDe: ! r:
	if notrans=1 then goto L750
	if br>=val(r$) and val(r$)><0 then goto L660
	ReadAcct: ! read gl master file for amounts
	dim by(13)
	dim bp(13)
	read #hAcct,using Freport: dno,ano,sno,br,cb,mat by,mat bp eof L740
	if br=0 then goto ReadAcct
	if fnfscode=0 or (fnfscode=actpd and priorcd=1) then goto L660
	if fnfscode<1 or fnfscode>12 then let fnfscode(1)
	if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
	L660: !
	for x=1 to 10
		if dno=fundnum(x) then fund=x ! put in certain column
		if fundnum(x)>0 then totcol=x ! total columns needed
	next x
	dim total(10)
	if br=val(r$) then total(fund)+=cb else goto L720
	goto ReadAcct
	L720: !
	if br<val(r$) then goto ReadAcct
	if br>val(r$) then goto L750
	L740: !
	notrans=1
	L750: !
	dim accum(10,9,2)
	for j=1 to 10
		if te$="E" then total(j)=-accum(j,ap,1)
	next j
	for j=1 to 9
		if ac(j)=9 then goto L830 ! 10/14/87
		for j2= 1 to 10
			accum(j2,j,1)=accum(j2,j,1)+total(j2)
		next j2
		L830: !
	next j
	if rs=1 then goto L850 else goto L880
		L850: !
		for j=1 to 10
		total(j)=-total(j)
	next j
	L880: !
	if ds=1 then dollar$="$" else dollar$=" "
	! If CP=1 Then dOLLAR=50+14*BC Else dOLLAR=24+14*BC
	if sum(total)><0 then goto L920
	if ls+ul+ds+ic>0 then goto L920 else goto PrTeLoopTop
	L920: sp2=49-sp-1
	! pr #255,Using 990: D$(1:SP2),DOLLAR$,TOTAL(FUND) Pageoflow 1470
	dim dolcol$*300
	dolcol$=""
	for j=1 to totcol
		if ul=1 then dolcol$=dolcol$&" "&dollar$&"{\ul "&cnvrt$("PIC(-,---,---.##)",total(j))&" }" : goto L970
		dolcol$=dolcol$&" "&dollar$&cnvrt$("PIC(-,---,---.##)",total(j))
		L970: !
	next j
	if ul=1 then pr #255,using L991: d$(1:sp2),rtrm$(dolcol$) pageoflow PgOf : goto L990
	pr #255,using L990: d$(1:sp2),rtrm$(dolcol$) pageoflow PgOf
	L990: form pos sp,c sp2,pos 49,c big,skip 1
	L991: form pos sp,c sp2,pos 49,c 150,skip 1
	form pos sp,c sp2,pos dollar,c 1,pic(--,---,---.##),skip redir
	form pos sp,c sp2,pos 49,c big,skip redir
	mat total=(0)
	gosub AddToAccum
	if ul=1 then goto L1050
	gosub PrUnderline
	L1050: !
	gosub PrFooterAndHeader1
goto PrTeLoopTop ! /r
PrTeTp: ! r:
	if ap=0 then ap=1
	dim accumcol$*300
	accumcol$=""
	for j=1 to totcol
		if rs=1 then accum1=-accum(j,ap,1) else accum1=accum(j,ap,1)
		if ds=1 then dollar$="$" else dollar$=" "
		if ul=1 then accumcol$=accumcol$&" "&dollar$&"{\ul "&cnvrt$("pic(-,---,---.##)",accum1)&"}" : goto L1140
		accumcol$=accumcol$&" "&dollar$&cnvrt$("pic(-,---,---.##)",accum1)
		L1140: !
	next j
	sp2=49-sp-1
	if ul=1 then pr #255,using L991: d$(1:sp2),rtrm$(accumcol$) pageoflow PgOf : goto L1170
	pr #255,using L1170: d$(1:sp2),rtrm$(accumcol$) pageoflow PgOf
	L1170: form pos sp,c sp2,pos 49,c big,skip redir
	gosub AddToAccum
	if ul=1 then goto L1200
	gosub PrUnderline
	L1200: !
	gosub PrFooterAndHeader1
	if te$><"P" then goto L1270
	for j=1 to 9
		for j2=1 to 10
			accum(j2,j,1)=accum(j2,j,1)-accum(j2,ap,1)
		next j2
	next j
	L1270: !
goto PrTeLoopTop ! /r
PrTeRs: ! r:
	if te$="R" then report$=d$
	dim secondr$*50
	if te$="S" then secondr$=d$
	gosub PrFooterAndHeader1
goto PrTeLoopTop ! /r
PrTeF: ! r:
	dim foot$*132
	if foot1=1 then
		foot$=rtrm$(foot$)&d$
	else
		tabnote=sp
		foot1=1
		foot$=d$
	end if
goto PrTeLoopTop ! /r

AddToAccum: ! r:
	for j=1 to 9
		if ac(j)<>0 and ac(j)<>9 then 
			for j2=1 to 10
				accum(j2,j,1)=0
			next j2
		end if
	next j
return ! /r
PrFooterAndHeader1: ! r:
	if ls=0 then goto L1630
	if ls=99 then goto PrFooterAndHeader2
	pr #255,using L1510: " "
	L1510: form pos 1,c 1,skip ls
goto L1630
PrFooterAndHeader2: !
	fnPgLen(pglen)
	! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
	! If PGLEN=42 Then sK=SK+1
	pr #255,using L1590: rtrm$(foot$)
	L1590: form skip sk,pos tabnote,c fl,skip 1
	if eofcode=1 then goto L1630
	pr #255: newpage
	gosub PrHead1
	L1630: !
return ! /r
PgOf: ! r:
	gosub PrFooterAndHeader2
continue ! /r
PrUnderline: ! r:
	dim underlin$*14
	if ul then
		if ul=1 then
			underlin$="  ____________"
		else
			underlin$="  ============"
		end if
		dim bigul$*140
		bigul$=""
		for j=1 to totcol : bigul$=bigul$&underlin$ : next j
		if ul=1 then pr #255,using L1760: bigul$
		L1760: form skip redir,pos 49,c big,skip redir
		if ul=2 then pr #255,using L1770: bigul$
		L1770: form pos 49,c big,skip redir
		if redir=0 then pr #255,using L1790: " "
		L1790: form skip 1,c 1,skip redir
	end if
return ! /r
PrHead1: ! r:
	heading=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	pr #255:
	pr #255,using L1910: heading$
	L1910: form pos 49,c big,skip 2
return ! /r

Finis: ! r:
	eofcode=1
	gosub PrFooterAndHeader2
	fnfscode(actpd)
	fnpriorcd(1)
	fnClosePrn
goto Xit ! /r
Xit: fnXit

AskFund: ! r:
	! pr newpage ! determine fund #s
	dim fundnum(10)
	dim funddesc$(10)*20
	open #5: "Name=[Q]\GLmstr\GLfund.h[cno],RecL=230,use",i,outi,r
	read #5,using L2070: mat fundnum,mat funddesc$ ioerr ignore
	L2070: form pos 1,10*n 3,10*c 20

	L2080: !
	fnTos
	dim resp$(80)*128
	mylen=1: mypos=mylen+3
	fnLbl(1,4,"Fund                 Description ")
	for j=1 to 10
		fnTxt(j+1,mypos,3,0,right,'30',0,"Enter the fund number.")
		resp$(j*2-1)=str$(fundnum(j))
		fnTxt(j+1,mypos+10,20,0,0,"",0,"Enter the fund description.")
		resp$(j*2)=funddesc$(j)
	next j
	fnCmdKey("&Next",1,1,0,"Continues with financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	for j=1 to 10
		fundnum(j)=val(resp$(j*2-1))
		funddesc$(j)=resp$(j*2)
	next j
	rewrite #5,using L2070: mat fundnum,mat funddesc$ ioerr L2240
	goto L2250
	L2240: !
	write #5,using L2070: mat fundnum,mat funddesc$
	L2250: !
	close #5:
	dim heading$*140
	for j=1 to 10
		if fundnum(j)>0 then
			heading$&=" "&lpad$(rtrm$(funddesc$(j)(1:13)),13)
			totcol+=1
		end if
	next j
	big=totcol*14
return ! /r

include: ertn
