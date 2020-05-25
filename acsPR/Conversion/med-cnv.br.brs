!  Replace S:\acsPR\Conversion\Med-Cnv
! Split out Medicare if converted to new payroll in 2000
	on error goto Ertn

	dim em$(3)*30
	dim ss$*11
	dim ty(21)
	dim w(13)
	dim a$(3)*40,b$*12,g$*12,d$(10)*8,tty(10),e$(10)*12
	dim dedfed(10),dedcode(10)

	open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input 
	read #1,using 'Form POS 1,3*C 40,C 12,POS 150,10*C 8,N 2,POS 317,10*C 12,POS 618,10*N 1,POS 638,10*N 1,POS 133,PD 6.3,PD 6.2,POS 236,PD 3.3,PD 4.2': mat a$,b$,mat d$,loccode,mat e$,mat dedcode,mat dedfed,mcrate,mcmax,ssrate,ssmax 
	close #1: ! company was prcoinfo before conversion
	mcmax=9999999
	for j=1 to 3: a$(j)=a$(j)(1:30): next j
	open #1: "Name=[Q]\PRmstr\RPMstr.h[cno],KFName=[Q]\PRmstr\RPIndex.h[cno],Shr",internal,input,keyed 
	open #2: "Name=[Q]\PRmstr\RPTRAIL.h[cno],Shr",internal,outIn,relative 
L220: read #1,using L230: eno,mat em$,ss$,em6,ta eof Xit
L230: form pos 1,n 8,3*c 30,c 11,pos 122,n 2,pos 173,pd 3
L240: read #2,using L250,rec=ta: teno,tcd,mat ty,ta
L250: form pos 1,n 8,pos 48,n 2,pos 168,21*pd 5.2,pos 468,pd 3
	if tcd<1 or tcd>10 then tcd=1
	dedfica=0
	dedret=0
	for j=1 to 10
		if dedfed(j)>=1 and dedcode(j)=1 then dedret+=ty(j+3)
! IF DEDFED(J)=1 AND DEDCODE(J)=1 THEN tDEDRET+=TY(J+3)
		if dedfed(j)=2 and dedcode(j)=1 then dedfica+=ty(j+3)
	next j
	w(2)=w(2)+ty(21)-dedret                                   ! TOTAL TAXABLE WAGES
	w3=w3+ty(2)+ty(15)                                        ! FICA W/H YTD
	w(11)=w(11)+ty(21)-dedfica                                ! TOTAL MC WAGES & TIPS
	if em6=1 then w(11)=0                                     ! NO MC
	w(3)=round(min(w3/(ssrate+mcrate)*ssrate,ssmax*ssrate),2) ! SS WH               ! change to seperate medicare
	w(12)=w3-w(3)                                             ! MEDICARE WITHHELD   ! change to seperate medicare
	if em6=1 then w(12)=0 : w(3)=w3                           ! NO MC ALL SS        ! change to seperate medicare
	if em6=2 then w(3)=0 : w(12)=w3                           ! NO SS ALL MC        ! change to seperate medicare
	if em6=9 then w(3)=w(5)=w(11)=w(12)=0                     ! NO SS OR MC
	ty(2)=w(3)
	ty(15)=w3-w(3)
	rewrite #2,using 'Form POS 173,PD 5.2,POS 238,PD 5.2': ty(2),ty(15)
	mat w=(0)
	nqp=dcb=w3=0
	if ta>0 then goto L240 else goto L220

Xit: stop 
include: Ertn