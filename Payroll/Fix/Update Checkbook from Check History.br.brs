	library 'S:\Core\Library': fntop,fnxit, fnss_employee,fnss_employer,fnDedNames
	on error goto Ertn

	dim em$(3)*30,tdc(10),tcp(32)
	dim a$(3)*40,d$(10)*8,io1$(11),i$*62,j$*70,gln$(15)*12 ! old S(7,8),
	dim tad(29),tradesc$*30,lcn$*8,tr$(5)*35
	dim rpnames2$(10)*6,dept(6),dedcode(10),bankgl$*12,gl$*12,bn$*30
	fntop(program$)

	gosub READ_COMPANY_INFO
	mcr=mcr*.01
	ssrate1=fnss_employee*.01
	ssrate2=fnss_employer*.01
	open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,outIn,keyed 
	open #3: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr",internal,input,keyed 
	open #4: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],Shr",internal,outIn,keyed 
	open #7: "Name=[Q]\PRmstr\MGLMSTR.h[cno],KFName=[Q]\PRmstr\MGLIDX1.h[cno],Shr",internal,input,keyed 
	gosub CHECK_ACSCL
	do 
		read #4,using F_HIST: heno,tdn,prd,ckno,mat tdc,mat tcp eof END_HIST
F_HIST: form pos 1,n 8,n 3,pd 6,n 7,5*pd 3.2,37*pd 5.2
		if tcp(2)=round(tcp(31)*ssrate1,2) then tdc(7)=tcp(31) else tdc(7)=round(tcp(2)/ssrate1,2)
		if tcp(3)=round(tcp(31)*mcr,2) then tdc(8)=tcp(31) else tdc(8)=round(tcp(3)/mcr,2) ! calculate medicare wages
		if prd=20120224 then gosub BUILD_CHECK_RECORD
	loop 
CHECK_ACSCL: ! r:
	open #12: "Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr",internal,outIn,keyed 
	open #15: "Name=[Q]\CLmstr\Company.h[cno],Shr",internal,outIn,relative 
	open #8: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr",internal,outIn,keyed 
	open #22: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr",internal,outIn,keyed 
	open #tralloc:=23: "Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno],Shr",internal,outIn,keyed 
	read #15,using L3830,rec=1,release: bankcode,prenum,port
L3830: form pos 152,n 2,pos 406,n 1,pos 788,n 1
	read #12,using FM_BANK,key=lpad$(str$(bankcode),2),release: bn$,bal,upi,nckno nokey L3870
FM_BANK: form pos 3,c 30,pos 45,pd 6.2,pd 6.2,g 8
L3870: return  ! /r
BUILD_CHECK_RECORD: ! 
	eno$=lpad$(str$(heno),8)
	em$(1)=''
	read #1,using FM_PRMSTR,key=eno$: em$(1) nokey ignore
FM_PRMSTR: form pos 9,c 30
	tr$(1)=cnvrt$("n 8",ckno)
	dat$=str$(prd)
	dat=val(dat$(5:6)&dat$(7:8)&dat$(3:4))
	tdn$=cnvrt$("n 3",tdn)
	read #3,using FM_DEPT,key=eno$&tdn$: pgl$
FM_DEPT: form pos 12,c 12
DELETE_ACSCL: ! delete old check records
	clk$=lpad$(str$(bankcode),2)&"1"&tr$(1)
	read #8,using F_TRMSTR,key=clk$: bc$,tcde$,otr1$,otr2$,otr3 nokey WRITE_ACSCL
	bal=bal-otr3
	delete #8,key=clk$: 
	restore #tralloc,key>=clk$: nokey WRITE_ACSCL
RD_TRALLOC: read #tralloc,using 'Form Pos 1,C 11': newkey$ eof WRITE_ACSCL
	if newkey$=clk$ then delete #tralloc: : goto RD_TRALLOC
WRITE_ACSCL: ! 
	tr$(2)=lpad$(str$(dat),6)
	tr3=tcp(32)
	tr$(4)=eno$
	tr$(5)=em$(1)
	write #8,using F_TRMSTR: bankcode,1,tr$(1),tr$(2),tr3,tr$(4),tr$(5),0,0,4
F_TRMSTR: form pos 1,g 2,g 1,c 8,g 6,pd 10.2,c 8,c 35,n 1,n 6,n 1
	bal=bal+tr3
	tragl$=pgl$
	traamt=tcp(31)
	tradesc$="Gross Pay"
	traivd$=str$(dat)
	write #tralloc,using FM_TRALLOC: clk$,tragl$,traamt,tradesc$,traivd$,"",trapos$,tragde
WRITE_TRALLOC: ! 
	for j=1 to 25
		if j=1 then tragl$=gln$(1): tradesc$="Federal WH"
		if j=2 then tragl$=gln$(2): tradesc$="Soc-Sec WH"
		if j=3 then tragl$=gln$(2): tradesc$="Medicare WH"
		if j=4 then tragl$=gln$(3): tradesc$="State WH"
		if j>4 and j<25 then tragl$=gl$(j-4): tradesc$=fullname$(j-4)
		if j=25 then tragl$=gln$(14) : tradesc$="EIC"
		traamt=-tcp(j)
		if j>4 and j<25 and newdedcode(j-4)>1 then traamt=-traamt
		if traamt=0 then goto NXTJ
		write #tralloc,using FM_TRALLOC: clk$,tragl$,traamt,tradesc$,traivd$,"",trapos$,tragde
FM_TRALLOC: form pos 1,c 11,c 12,pd 5.2,c 30,g 6,c 3,c 12,n 1
NXTJ: next j
	tragl$=gln$(2): tradesc$="Employer's Soc-Sec Match" : traamt=-round(tdc(7)*ssrate2,2)
	write #tralloc,using FM_TRALLOC: clk$,tragl$,traamt,tradesc$,traivd$,"",trapos$,tragde
	traamt2=traamt
	tragl$=gln$(2): tradesc$="Employer's Medicare Match" : traamt=-round(tdc(8)*mcr,2)
	write #tralloc,using FM_TRALLOC: clk$,tragl$,traamt,tradesc$,traivd$,"",trapos$,tragde
	traamt2=-(traamt2+traamt)
	tragl$=gln$(2): tradesc$="Off Set SS & Medicare Match" : traamt=traamt2
	read #7,using FM_MGLMSTR,key=tdn$: tragl$ nokey WR_SSMATCH
FM_MGLMSTR: form pos 4,11*c 12
WR_SSMATCH: write #tralloc,using FM_TRALLOC: clk$,tragl$,traamt,tradesc$,traivd$,"",trapos$,tragde
	rewrite #12,using FM_BANK,rec=1: bn$,bal
END_CHECK_RECORD: return 
READ_COMPANY_INFO: ! 
	dim fullname$(20)*20,abrevname$(20)*8,newcalcode(20),newdedfed(20),dedfica(20),dedst(20),deduc(20),newdedcode(20),gl$(20)*12
	open #1: "Name=[Q]\PRmstr\Company.h[cno]",internal,outIn,relative 
	read #1,using F_company,rec=1: mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$
	F_company: form pos 1,3*c 40,c 12,pd 6.3,pd 6.2,pd 5.2,10*c 8,n 2,pd 4.2,pd 3.3,12*pd 4.2,10*pd 3.3,25*c 12,31*n 1,10*c 6,3*pd 4.3,3*pd 3.2,4*pd 4.2,n 1,2*c 6,n 2
	close #1: 
READNAMES: ! 
	fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc,mat gl$)
FM_DNAM: form pos 1,20*c 20,20*c 8,120*n 1,20*c 12
	close #2: 
return 
END_HIST: stop 
XIT: stop  ! fnxit
include: Ertn