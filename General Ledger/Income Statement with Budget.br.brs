! formerly S:\acsGL\AcGlIncB
! -- INCOME STATEMENT WITH BUDGET

	autoLibrary
	on error goto Ertn

	dim foot$*132
	dim underlin$*14
	dim b$*3
	dim a$(8)*30
	dim oldtrans$*16
	dim g(8)
	
	dim actpd$*6
	dim bm(13)
	dim bp(13)
	dim by(13)

	fnTop(program$)
	on fkey 5 goto Finis
	fncno(cno)
	actpd=fnactpd
	actpd$=fnactpd$
	fnfscode
	fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit : _
		! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	fnfscode
	fnpriorcd

	pr newpage
	pors=1
	mp1=69
	if fnps=2 then mp1=mp1+3
	dim fl1$*256
	fl1$="Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr"
	if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr"

	open #1: fl1$,internal,input,keyed
	if fnprocess=1 or fnUseDeptNo=0 then goto L390
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) : _
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
L390: !
costcntr=val(resp$(1))
report$="STATEMENT OF INCOME AND EXPENSES"
fnopenprn
redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
if fnps=2 then 
	fnIndex('[Q]\GLmstr\GLmstr.h[cno]','[temp]\fsindex.H[cno]','72 3') ! secondary
else 
	fnIndex('[Q]\GLmstr\GLmstr.h[cno]','[temp]\fsindex.H[cno]','69 3')
end if 

open #hGl:=fnGetHandle: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Temp]\fsindex.h[cno],Shr",internal,input,keyed

MainLoopTop: ! r:
	dim r$*5
	dim d$*50
	dim te$*1
	dim ac(9)

	read #1,using L550: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof Finis
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto MainLoopTop
	if costcntr=0 then goto L550
	if fc=0 and te$="F" then goto L560 ! 5/08/1989
	if costcntr><fc then goto MainLoopTop
	L550: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	L560: !
	if te$="S" or te$="F" then goto L580
	if heading=0 and te$><"R" then gosub PrHeading
	L580: !
on pos ("RFHDTS",te$,1) goto Te_RS,Te_F,Te_H,Te_D,Te_T,Te_RS none MainLoopTop
! /r
Te_H: ! r:
	pr #255,using L600: d$(1:40)
	L600: form pos sp,c 40,skip 1
	gosub PrMain
	gosub ResetAccum
goto MainLoopTop ! /r
Te_D: ! r:
	if notrans=1 then goto L950
	if ir>=val(r$) and val(r$)><0 then goto L770
	L660: ! read amounts from gl master file
	L670: !
	read #hGl,using F3: ir,bb,cb,mat by,mat bp,mat bm eof Eo3
	F3: form pos mp1,pd 3,pos 81,41*pd 6.2
	if ir=0 then goto L670 ! skip accounts with no income reference #
	if fnfscode=0 or (fnfscode=actpd and fnpriorcd=1) then goto L760
	if fnfscode<1 or fnfscode>13 then let fnfscode=1
	if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
	if fnpriorcd=2 then goto L750
	if fnfscode>1 then bb=by(fnfscode-1) else bb=0
	goto L760
	L750: !
	if fnfscode>1 then bb=bp(fnfscode-1) else bb=0
	L760: !
	L770: !
	if ir=val(r$) then total=total+(cb-bb) else goto L920
	total2=total2+cb
	for z=1 to 13
		annualb=annualb+bm(z)
	next z
	if fnfscode=0 then monthb=monthb+bm(actpd) else monthb=monthb+bm(fnfscode) ! 11/24/86
	if fnfscode=0 then goto L840 else goto L880 ! 11/24/86
	L840: !
	for j=1 to actpd
		ytdb=ytdb+bm(j)
	next j
	goto L660
	L880: !
	for j=1 to fnfscode ! 11/24/86
		ytdb=ytdb+bm(j) ! 11/24/86
	next j ! 11/24/86
	goto L660 ! 11/24/86
	L920: !
	if ir<val(r$) then goto L660
	if ir>val(r$) then goto L950
	Eo3: !
	notrans=1
	L950: !
	overundr=ytdb-total2
	unexpend=annualb-total2
	for j=1 to 9
		if ac(j)=9 then goto L1060 ! 10/14/87
		accum(j,1)=accum(j,1)+total
		accum(j,2)=accum(j,2)+total2
		accum(j,3)=accum(j,3)+annualb
		accum(j,4)=accum(j,4)+monthb
		accum(j,5)=accum(j,5)+ytdb
		accum(j,6)=accum(j,6)+overundr
		accum(j,7)=accum(j,7)+unexpend
		L1060: !
	next j
	if rs=1 then total=-total else goto L1140
	total2=-total2
	annualb=-annualb
	monthb=-monthb
	ytdb=-ytdb
	overundr=overundr
	unexpend=unexpend
	L1140: !
	if ds=1 then dollar$="$" else dollar$=" "
	goto L1190 ! pr all accounts even if zero balance  (if budget ever nets to zero, it messes the monthly budget column up
	if annualb><0 or total2><0 then goto L1190
	if total<>0 then goto L1190
	if ls+ds+ul+ic>0 then goto L1190 else goto MainLoopTop
	L1190: !
	sp2=26-sp-1
	if ul=1 then pr #255,using L1211: d$(1:sp2),dollar$,"{\UL ",annualb,"}",dollar$,"{\UL ",total,"}",dollar$,"{\UL ",monthb,"}",dollar$,"{\UL ",total2,"}",dollar$,"{\UL ",ytdb,"}",dollar$,"{\UL ",overundr,"}",dollar$,"{\UL ",unexpend,"}" pageoflow PgOf : goto L1210
	pr #255,using L1210: d$(1:sp2),dollar$,annualb,dollar$,total,dollar$,monthb,dollar$,total2,dollar$,ytdb,dollar$,overundr,dollar$,unexpend pageoflow PgOf
	L1210: form pos sp,c sp2,pos 26,c 1,n 13.2,x 1,c 1,n 11.2,x 1,c 1,n 11.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,x 1,c 1,n 13.2,skip redir
	L1211: form pos sp,c sp2,pos 26,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 11.2,c 1,x 1,c 1,c 5,n 11.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 13.2,c 1,x 1,c 1,c 5,n 13.2,c 1,skip redir
	total=0
	total2=0
	annualb=0
	monthb=0
	ytdb=0
	overundr=0
	unexpend=0
	gosub ResetAccum
	if ul=1 then goto L1310
	gosub PrSubTotals
	L1310: !
	gosub PrMain
goto MainLoopTop ! /r
Te_T: ! r:
	if ap=0 then ap=1
	dim accum(9,7)
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
	if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
	if rs=1 then accum5=-accum(ap,5) else accum5=accum(ap,5)
	if rs=1 then accum6= accum(ap,6) else accum6=accum(ap,6)
	if rs=1 then accum7= accum(ap,7) else accum7=accum(ap,7)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=26-sp-1
	if ul=1 then pr #255,using L1211: d$(1:sp2),dollar$,"{\UL ",accum3,"}",dollar$,"{\UL ",accum1,"}",dollar$,"{\UL ",accum4,"}",dollar$,"{\UL ",accum2,"}",dollar$,"{\UL ",accum5,"}",dollar$,"{\UL ",accum6,"}",dollar$,"{\UL ",accum7,"}" pageoflow PgOf : goto L1440
	pr #255,using L1210: d$(1:sp2),dollar$,accum3,dollar$,accum1,dollar$,accum4,dollar$,accum2,dollar$,accum5,dollar$,accum6,dollar$,accum7 pageoflow PgOf
	L1440: !
	ft1=0
	gosub ResetAccum
	if ul<>1 then
		gosub PrSubTotals
	end if
	gosub PrMain
goto MainLoopTop ! /r
Te_RS: ! r:
	if te$="R" then
		dim report$*50
		report$=d$
	else if te$="S" then
		dim secondr$*50
		secondr$=d$
	end if
	gosub PrMain
goto MainLoopTop ! /r
Te_F: ! r:
	if foot1=1 then goto L1580
	tabnote=sp
	foot1=1
	foot$=d$
	goto MainLoopTop
	L1580: !
	foot$=rtrm$(foot$)&d$
goto MainLoopTop ! /r
ResetAccum: ! r:
	for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1690 ! 10/14/87
		accum(j,1)=0
		accum(j,2)=0
		accum(j,3)=0
		accum(j,4)=0
		accum(j,5)=0
		accum(j,6)=0
		accum(j,7)=0
		L1690: !
	next j
return ! /r
PrMain: ! r:
	if ls=0 then goto L1870
	if ls=99 then goto L1760
	pr #255,using L1740: " "
	L1740: form pos 1,c 1,skip ls
	goto L1870
	L1760: ! If FT1=1 Then Goto L1870
	fnpglen(pglen)
	! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
	! If PGLEN=42 Then sK=SK+1
	pr #255,using L1820: rtrm$(foot$),"Page "&str$(pt1)
	L1820: form skip sk,pos tabnote,c fl,pos 115,c 8,skip 1
	! ft1=1
	if eofcode=1 then goto L1870
	pr #255: newpage
	gosub PrHeading
	L1870: !
return ! /r
PgOf: gosub L1760: continue
PrSubTotals: ! r: i think i named this routine properly
	if ul=0 then goto L1990
	if ul=1 then goto L1960
	underlin$="=============="
	pr #255:
	goto L1970
	goto L1990
	L1960: !
	underlin$="______________"
	L1970: !
	pr #255,using L1980: underlin$,underlin$(1:12),underlin$(1:12),underlin$,underlin$,underlin$,underlin$
	L1980: form pos 26,c 15,2*c 13,4*c 15,skip redir
	L1990: !
	if redir=0 then pr #255,using 'form skip 1,c 1,skip 0': " " pageoflow PgOf
return ! /r
PrHeading: ! r:
	heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	pr #255: tab(33);"ANNUAL";tab(40);"<--";fncch$;"-->";tab(66);" <--     YEAR TO DATE      -->";tab(97);"<--     BUDGET TO DATE    -->"
	pr #255: tab(33);"BUDGET";tab(45);"BALANCE";tab(60);"BUDGET";tab(73);"BALANCE";tab(90);"BUDGET";tab(101);"OVER/UNDER";tab(116);"UNEXPENDED"
	pr #255:
return ! /r
Finis: ! r:
	eofcode=1
	gosub L1760
	fnfscode(actpd)
	fnpriorcd(1)
	close #hGl:
	fncloseprn
goto Xit ! /r
Xit: fnXit
include: Ertn
