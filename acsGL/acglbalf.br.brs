! G/L BALANCE SHEET -  STANDARD FOR 8 1/2 * 11 PAPER
 
	autoLibrary
	on error goto Ertn
 
	dim sc1$(2)*20,bigul$*140,heading$*140,cch$*20,by(13),bp(13)
	dim p$(20)*50,accum(10,9,2),total(10),fl1$*256,pedat$*20,cap$*128,udf$*256
	dim fundnum(10),funddesc$(10)*20,io1$(20),dolcol$*300,accumcol$*300
	dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8)
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
	dim resp$(80)*50
 
	fnTop(program$,cap$="Fund Comparison Balance Sheet")
	fncno(cno,cnam$)
	udf$=env$('temp')&'\'
	actpd$=fnactpd$ : _
	actpd=fnactpd : _
	fnfscode : _
	fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit : _
		! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	fnfscode : _
	fnpriorcd
	pors=1
	if fnprocess=0 then gosub L2010
	mp1=63 : if fnps=2 then mp1=mp1+3
	if fnps=2 then : _
		fl1$="Name=[Q]\GLmstr\AcGLFnSc.h[cno]," : _
		fl1$=fl1$&"KFName=[Q]\GLmstr\agfsidx1.h[cno],Shr" else : _
		fl1$="Name=[Q]\GLmstr\ACGLFNSB.h[cno]," : _
		fl1$=fl1$&"KFName=[Q]\GLmstr\agfsidx4.h[cno],Shr"
L210: form pos 1,n 3,n 6,n 3,pos mp1,pd 3,pos 87,27*pd 6.2
	open #1: fl1$,internal,input,keyed
	if fnprocess=1 or fnUseDeptNo=0 then goto L320
	goto L350 ! pr NEWPAGE
	close #101: ioerr L260
L260: open #101: "SROW=9,SCOL=4,EROW=12,ECOL=75,BORDER=DR,CAPTION=PRINT BALANCE SHEET",display,outIn
	pr f "13,32,C 16,B,5": "Cancel (F5)"
	pr f "10,5,c 70,n": "ENTER THE COST CENTER OR DEPT # IF YOU WISH TO ONLY pr A STATEMENT"
L290: pr f "11,5,c 65,n": "ON ONE DEPARTMENT; ELSE ENTER 0 TO pr ALL DEPARTMENTS"
	input fields "11,70,N 3,eu,N": costcntr conv L290
	if cmdkey=5 then goto Xit
L320: pr newpage
	pr f "10,20,c 34,h,n": " BALANCE SHEET IN PROCESS"
	pr f "12,2,C 18,B,5": " Press F5 to stop"
L350: fnopenprn
	if fnps=2 then goto L390 ! secondary
	execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 63 3 Replace DupKeys -N"
	goto L400
L390: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 66 3 Replace DupKeys -N"
L400: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed
	if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
	on fkey 5 goto L1940
	report$=cap$
L440: read #1,using L480: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1940
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L440
	if costcntr=0 then goto L480
	if costcntr><fc then goto L440
L480: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	if te$="S" or te$="F" then goto L510
	if heading=0 and te$><"R" then gosub L1820
L510: on pos ("RFHDTSPE",te$,1) goto L1290,L1340,L520,L570,L1080,L1290,L1080,L570 none L440
L520: pr #255,using L530: d$
L530: form pos sp,c 50,skip 1
	gosub L1480
	gosub L1410
	goto L440
L570: if notrans=1 then goto L750
	if br>=val(r$) and val(r$)><0 then goto L660
L590: ! read gl master file for amounts
L600: read #3,using L210: dno,ano,sno,br,cb,mat by,mat bp eof L740
	if br=0 then goto L600
	if fnfscode=0 or (fnfscode=actpd and priorcd=1) then goto L660
	if fnfscode<1 or fnfscode>12 then let fnfscode(1)
	if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
L660: for x=1 to 10
		if dno=fundnum(x) then fund=x ! put in certain column
		if fundnum(x)>0 then totcol=x ! total columns needed
	next x
	if br=val(r$) then total(fund)=total(fund)+cb else goto L720
	goto L590
L720: if br<val(r$) then goto L590
	if br>val(r$) then goto L750
L740: notrans=1
L750: for j=1 to 10
		if te$="E" then total(j)=-accum(j,ap,1)
	next j
	for j=1 to 9
		if ac(j)=9 then goto L830 ! 10/14/87
		for j2= 1 to 10
			accum(j2,j,1)=accum(j2,j,1)+total(j2)
		next j2
L830: next j
	if rs=1 then goto L850 else goto L880
L850: for j=1 to 10
		total(j)=-total(j)
	next j
L880: if ds=1 then dollar$="$" else dollar$=" "
! If CP=1 Then dOLLAR=50+14*BC Else dOLLAR=24+14*BC
	if sum(total)><0 then goto L920
	if ls+ul+ds+ic>0 then goto L920 else goto L440
L920: sp2=49-sp-1
! pr #255,Using 990: D$(1:SP2),DOLLAR$,TOTAL(FUND) Pageoflow 1470
	dolcol$=""
	for j=1 to totcol
		if ul=1 then dolcol$=dolcol$&" "&dollar$&"{\ul "&cnvrt$("PIC(-,---,---.##)",total(j))&" }" : goto L970
		dolcol$=dolcol$&" "&dollar$&cnvrt$("PIC(-,---,---.##)",total(j))
L970: next j
	if ul=1 then pr #255,using L991: d$(1:sp2),rtrm$(dolcol$) pageoflow L1640 : goto L990
	pr #255,using L990: d$(1:sp2),rtrm$(dolcol$) pageoflow L1640
L990: form pos sp,c sp2,pos 49,c big,skip 1
L991: form pos sp,c sp2,pos 49,c 150,skip 1
	form pos sp,c sp2,pos dollar,c 1,pic(--,---,---.##),skip redir
	form pos sp,c sp2,pos 49,c big,skip redir
	mat total=(0)
	gosub L1410
	if ul=1 then goto L1050
	gosub L1670
L1050: gosub L1480
	goto L440
 
L1080: if ap=0 then ap=1
	accumcol$=""
	for j=1 to totcol
		if rs=1 then accum1=-accum(j,ap,1) else accum1=accum(j,ap,1)
		if ds=1 then dollar$="$" else dollar$=" "
		if ul=1 then accumcol$=accumcol$&" "&dollar$&"{\ul "&cnvrt$("pic(-,---,---.##)",accum1)&"}" : goto L1140
		accumcol$=accumcol$&" "&dollar$&cnvrt$("pic(-,---,---.##)",accum1)
L1140: next j
	sp2=49-sp-1
	if ul=1 then pr #255,using L991: d$(1:sp2),rtrm$(accumcol$) pageoflow L1640 : goto L1170
	pr #255,using L1170: d$(1:sp2),rtrm$(accumcol$) pageoflow L1640
L1170: form pos sp,c sp2,pos 49,c big,skip redir
	gosub L1410
	if ul=1 then goto L1200
	gosub L1670
L1200: gosub L1480
	if te$><"P" then goto L1270
	for j=1 to 9
		for j2=1 to 10
			accum(j2,j,1)=accum(j2,j,1)-accum(j2,ap,1)
		next j2
	next j
L1270: goto L440
 
L1290: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1480
	goto L440
 
L1340: if foot1=1 then goto L1390
	tabnote=sp
	foot1=1
	foot$=d$
	goto L440
L1390: foot$=rtrm$(foot$)&d$
	goto L440
L1410: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1460 ! 10/14/87
		for j2=1 to 10
			accum(j2,j,1)=0
		next j2
L1460: next j
return
L1480: if ls=0 then goto L1630
	if ls=99 then goto L1540
	pr #255,using L1510: " "
L1510: form pos 1,c 1,skip ls
	goto L1630
 
L1540: fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1590: rtrm$(foot$)
L1590: form skip sk,pos tabnote,c fl,skip 1
	if eofcode=1 then goto L1630
	pr #255: newpage
	gosub L1820
L1630: return
L1640: gosub L1540
	continue
 
L1670: if ul=0 then goto L1800
	if ul=1 then goto L1710
	underlin$="  ============"
	goto L1720
L1710: underlin$="  ____________"
L1720: bigul$=""
	for j=1 to totcol : bigul$=bigul$&underlin$ : next j
	if ul=1 then pr #255,using L1760: bigul$
	if ul=2 then pr #255,using L1770: bigul$
L1760: form skip redir,pos 49,c big,skip redir
L1770: form pos 49,c big,skip redir
	if redir=0 then pr #255,using L1790: " "
L1790: form skip 1,c 1,skip redir
L1800: return
 
L1820: heading=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	pr #255:
	pr #255,using L1910: heading$
L1910: form pos 49,c big,skip 2
return
 
L1940: eofcode=1
	gosub L1540
	if pors=2 then goto Xit else goto DONE
Xit: fnXit
DONE: !
	fnfscode(actpd)
	fnpriorcd(1)
	fncloseprn
	goto Xit
 
L2010: pr newpage ! determine fund #s
	for j=1 to 10
		io1$(j*2-1)=str$(j+4)&",22,NZ 3,UT,n" : _
		io1$(j*2)=str$(j+4)&",28,C 20,UT,N"
	next j
	open #5: "Name=[Q]\GLmstr\GLfund.h[cno],RecL=230,use",internal,outIn,relative
	read #5,using L2070: mat fundnum,mat funddesc$ ioerr L2080
L2070: form pos 1,10*n 3,10*c 20
L2080: fnTos
	mylen=1: mypos=mylen+3
	fnLbl(1,4,"Fund                 Description ")
	for j=1 to 10
		fnTxt(j+1,mypos,3,0,right,"30",0,"Enter the fund number.") : _
		resp$(j*2-1)=str$(fundnum(j))
		fnTxt(j+1,mypos+10,20,0,0,"",0,"Enter the fund description.") : _
		resp$(j*2)=funddesc$(j)
	next j
	fnCmdKey("&Next",1,1,0,"Continues with financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	for j=1 to 10
		fundnum(j)=val(resp$(j*2-1))
		funddesc$(j)=resp$(j*2)
	next j
	rewrite #5,using L2070: mat fundnum,mat funddesc$ ioerr L2240
	goto L2250
L2240: write #5,using L2070: mat fundnum,mat funddesc$
L2250: close #5:
	for j=1 to 10
		if fundnum(j)>0 then : _
			heading$=heading$&" "&lpad$(rtrm$(funddesc$(j)(1:13)),13) : _
			totcol+=1
	next j
	big=totcol*14
return
 
include: ertn
