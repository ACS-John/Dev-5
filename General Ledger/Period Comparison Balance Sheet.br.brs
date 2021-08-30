! formerly S:\acsGL\acglBalY
! G/L BALANCE SHEET with comparison on months
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,pedat$*20,m1$(13)*9,m2$(13)*8,total(13),p$(20)*50
	dim cch$*20,by(13),bp(13),sc1$(2)*20
	dim b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,13)
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*12
 
	fnTop(program$)
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit
	actpd$=fnactpd$ : _
	actpd=fnactpd
	! fnfscode
	! fnpriorcd
	! if fnGlAskFormatPriorCdPeriod=5 then goto Xit : _
		! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,             period to print)
	! fnfscode
	! fnpriorcd
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,i,r: read #1,using "Form pos 384,N 2",rec=1: nap : close #1:
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,i,r: read #1,using "Form pos 296,N 2",rec=1: lmu : close #1:
	m1$(1)="  January" : m1$(2)=" February" : _
	m1$(3)="    March" : m1$(4)="    April" : _
	m1$(5)="      May" : m1$(6)="     June" : _
	m1$(7)="     July" : m1$(8)="   August" : _
	m1$(9)="September" : m1$(10)="  October" : _
	m1$(11)=" November" : m1$(12)=" December" : _
	m1$(13)=""
	m2$(1)="     One" : m2$(2)="     Two" : m2$(3)="   Three" : _
	m2$(4)="    Four" : m2$(5)="    Five": m2$(6)="     Six" : _
	m2$(7)="   Seven": m2$(8)="   Eight": m2$(9)="    Nine" : _
	m2$(10)="     Ten": m2$(11)="  Eleven": m2$(12)="  Twelve" : _
	m2$(13)="Thirteen"
	mp1=63
	if fnps=2 then mp1=mp1+3
	fl1$="Name=[Q]\GLmstr\ACGLFNSB.h[cno],KFName=[Q]\GLmstr\agfsidx4.h[cno],Shr"
	if fnps=2 then fl1$="Name=[Q]\GLmstr\AcGLFnSc.h[cno],KFName=[Q]\GLmstr\agfsidx1.h[cno],Shr"
	open #1: fl1$,i,i,k
	if fnprocess=1 or fnUseDeptNo=0 then goto L330
	goto L370 ! pr NEWPAGE
	close #101: ioerr L280
L280: open #101: "SROW=9,SCOL=4,EROW=12,ECOL=75,BORDER=DR,CAPTION=PRINT BALANCE SHEET",display,outIn
	pr f "13,32,C 16,R,N": "Press F5 to stop"
	pr f "10,5,c 70,n": "ENTER THE COST CENTER OR DEPT # IF YOU WISH TO ONLY pr A STATEMENT"
L310: pr f "11,5,c 65,n": "ON ONE DEPARTMENT; ELSE ENTER 0 TO pr ALL DEPARTMENTS"
	input fields "11,70,N 3,eu,N": costcntr conv L310
L330: ! pr NEWPAGE
	pr f "10,10,Cc 60,n": " BALANCE SHEET IN PROCESS"
	pr f "12,34,C 11,B,5": "Cancel (F5)"
	on fkey 5 goto L2120
L370: fnopenprn : _
	if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
	if fnps=2 then goto L410 ! secondary
	execute "Index [Q]\GLmstr\GLmstr.h[cno] "&env$('temp')&'\'&"fsindex.h[cno] 63 3 Replace DupKeys -N"
	goto L420
L410: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&env$('temp')&'\'&"fsindex.h[cno] 66 3 Replace DupKeys -N"
L420: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&env$('temp')&'\'&"fsindex.h[cno],Shr",i,i,k
	report$=env$('program_caption')
L440: read #1,using L480: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2120
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L440
	if costcntr=0 then goto L480
	if costcntr><fc then goto L440
L480: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	if te$="S" or te$="F" then goto L510
	if heading=0 and te$><"R" then gosub L1970
L510: on pos ("RFHDTSPE",te$,1) goto L1380,L1430,L520,L580,L1080,L1380,L1080,L580 none L440 ! 8/4/88
L520: pr #255,using L530: d$
L530: form pos sp,c 50,skip 1
	gosub L1600
	gosub L1520
	goto L440
 
L580: if notrans=1 then goto L820 ! 8/4/88
	if br>=val(r$) and val(r$)><0 then goto L630
L600: ! READ GENERAL LEDGER MASTER FILE FOR AMOUNTS
L610: read #3,using 'Form POS MP1,PD 3,POS 87,27*PD 6.2': br,cb,mat by,mat bp eof L810
	if br=0 then goto L610 ! SKIP IF NO REFERENCE #
L630: if br=val(r$) then goto L640 else goto L790
L640: if fnpriorcd=2 then goto L770
	for j=1 to 13
		if j=1 and actpd=1 then total(j)+=cb else goto L680
		goto L740 ! 2/15/89
L680: if j=1 then total(j)=total(j)+by(j) else goto L700
		goto L740
L700: if j>nap then goto L740 ! 7/21/88
		if j<=lmu then total(j)=total(j)+by(j) else goto L730
		goto L740
L730: if actpd<>lmu and j=actpd then : _
			total(j)+=cb
L740: next j
	goto L600
 
L770: for j=1 to 13 : total(j)+=bp(j) : next j
	goto L600
L790: if br<val(r$) then goto L600
	if br>val(r$) then goto L860
L810: notrans=1
L820: if te$="E" then goto L830 else goto L860 ! 8/4/88
L830: for k=1 to 13 ! 8/4/88
		total(k)=-accum(ap,k) ! 8/4/88
	next k ! 8/4/88
L860: for j=1 to 9
		if ac(j)<>9 then : _
			for k=1 to 13 : accum(j,k)=accum(j,k)+total(k) : next k
	next j
	for j=1 to 13
		if rs=1 then total(j)=-total(j)
	next j
	if ds=1 then dollar$="$" else dollar$=" "
	dollar=27+14*bc
	goto L960
	if ls+ul+ds+ic>0 then goto L960 else goto L440
L960: sp2=dollar-sp-1
	if nap=13 then goto L1010
	if ul=1 then pr #255,using L991: d$(1:sp2),"{\ul ",total(1),"}","{\ul ",total(2),"}","{\ul ",total(3),"}","{\ul ",total(4),"}","{\ul ",total(5),"}","{\ul ",total(6),"}","{\ul ",total(7),"}","{\ul ",total(8),"}","{\ul ",total(9),"}","{\ul ",total(10),"}","{\ul ",total(11),"}","{\ul",total(12),"}" pageoflow L1760 : goto L990
	pr #255,using L990: d$(1:sp2),total(1),total(2),total(3),total(4),total(5),total(6),total(7),total(8),total(9),total(10),total(11),total(12) pageoflow L1760
L990: form pos sp,c sp2,pos 39,12*n 12.2,skip redir
L991: form pos sp,c sp2,pos 39,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,c 5,n 12.2,c 1,skip redir
	goto L1030
L1010: pr #255,using L1020: d$(1:sp2),mat total pageoflow L1760
L1020: form pos sp,c sp2,pos 39,13*n 12.2,skip redir
L1030: mat total=(0) ! 6/03/88
	gosub L1520
	if ul=1 then goto L1060
	gosub L1780
L1060: gosub L1600
	goto L440
L1080: if ap=0 then ap=1
	dollar=27+14*bc
	sp2=dollar-sp-1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
	if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
	if rs=1 then accum5=-accum(ap,5) else accum5=accum(ap,5)
	if rs=1 then accum6=-accum(ap,6) else accum6=accum(ap,6)
	if rs=1 then accum7=-accum(ap,7) else accum7=accum(ap,7)
	if rs=1 then accum8=-accum(ap,8) else accum8=accum(ap,8)
	if rs=1 then accum9=-accum(ap,9) else accum9=accum(ap,9)
	if rs=1 then accum10=-accum(ap,10) else accum10=accum(ap,10)
	if rs=1 then accum11=-accum(ap,11) else accum11=accum(ap,11)
	if rs=1 then accum12=-accum(ap,12) else accum12=accum(ap,12)
	if rs=1 then accum13=-accum(ap,13) else accum13=accum(ap,13)
	if nap=13 then goto L1270
	if ul=1 then pr #255,using L991: d$(1:sp2),"{\ul ",accum1,"}","{\ul ",m2,"}","{\ul ",m3,"}","{\ul ",m4,"}","{\ul ",m5,"}","{\ul ",m6,"}","{\ul ",m7,"}","{\ul ",m8,"}","{\ul ",um9,"}","{\ul ",accum10,"}","{\ul ",accum11,"}","{\ul ",accum12,"}" pageoflow L1760 : goto L1260
	pr #255,using L990: d$(1:sp2),accum1,accum2,accum3,accum4,accum5,accum6,accum7,accum8,accum9,accum10,accum11,accum12 pageoflow L1760
L1260: goto L1280
L1270: pr #255,using L1020: d$(1:sp2),accum1,accum2,accum3,accum4,accum5,accum6,accum7,accum8,accum9,accum10,accum11,accum12,accum13 pageoflow L1760
L1280: gosub L1520
	if ul=1 then goto L1300
	gosub L1780
L1300: gosub L1600
	if te$><"P" then goto L1370
	for j=1 to 9
		for k=1 to 13
			accum(j,k)=accum(j,k)-accum(ap,k)
		next k
	next j
L1370: goto L440
L1380: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1600
	goto L440
 
L1430: if foot1=1 then goto L1490
	tabnote=sp
	foot1=1
	foot$=d$
	goto L440
 
L1490: foot$=rtrm$(foot$)&d$
	goto L440
 
L1520: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1570 ! 10/14/87
		for k=1 to 13
			accum(j,k)=0
		next k
L1570: next j
return
 
L1600: if ls=0 then goto L1740
	if ls=99 then goto L1650
	pr #255,using L1630: " "
L1630: form pos 1,c 1,skip ls
	goto L1740
L1650: fnpglen(pglen)
	if pglen<>42 then pglen=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
	if pglen=42 then sk=sk+1
	pr #255,using L1700: rtrm$(foot$)
L1700: form skip sk,pos tabnote,c fl,skip 1
	if eofcode=1 then goto L1740
	pr #255: newpage
	gosub L1970
L1740: return
 
L1760: gosub L1650: continue
 
L1780: if ul=0 then goto L1930
	if ul=1 then goto L1870
	underlin$=" ==========="
	if nap=13 then goto L1840
	pr #255,using L1850: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,""
	goto L1930
L1840: pr #255,using L1850: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$
L1850: form pos 39,13*c 12,skip redir
	goto L1930
L1870: underlin$=" ___________"
	if nap=13 then goto L1910
	pr #255,using L1920: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,""
	goto L1930
L1910: pr #255,using L1920: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$
L1920: form skip 0,pos 39,13*c 12,skip redir
L1930: if redir=0 then pr #255,using L1940: " "
L1940: form skip 1,c 1,skip 0
return
 
L1970: heading=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	pr #255:
	if nap=13 then goto L2080
	pr #255,using L2090: mat m1$
	goto L2100
L2080: pr #255,using L2090: mat m2$
L2090: form pos 42,13*c 12,skip 2
L2100: return
 
L2120: eofcode=1
	gosub L1650
	fnfscode(actpd)
	fnpriorcd(1)
	fncloseprn
	goto Xit
 
Xit: fnXit
 
include: ertn
 
