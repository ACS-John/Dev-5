! Replace S:\acsGL\OtherDeductioins
! -- Other deductions

	autoLibrary
	on error goto Ertn

	dim miscname$(10)*20,dedcode(10),totalded(10)
	dim k(1),k$(3)*25,l$(1)*11,d(22),m(36),r$*10,n$*5,n(2),empd(22)
	dim adr(2)
	dim deposit(31,2)

	fnTop(program$,"Other Deductions Registers")
	dim dat$*20
	fndat(dat$)

	fnTos
	rc=cf=0: mylen=22: mypos=mylen+3: frameno=1
	fnFra(1,1,3,40,"Date Range for Report","Enter the date range for the payrolls to be included in this report.")
	fnLbl(1,1,"Beginning Date:",mylen,1,0,frameno)
	fnTxt(1,mypos,12,0,1,"3",0,"Enter the date of the first payroll to be included in this report. ",frameno) : _
	resp$(rc+=1)=str$(beg_date)
	fnLbl(2,1,"Ending Date:",mylen,1,0,frameno)
	fnTxt(2,mypos,12,0,1,"3",0,"Enter the last payroll date that should be included in this report. ",frameno) : _
	resp$(rc+=1)=str$(end_date)
	fnCmdKey("Next",1,1,0,"Print report.")
	fnCmdKey("Cancel",5,0,1,"Returns to menu without printing.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	beg_date=val(resp$(1))
	end_date=val(resp$(2))
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,outi,r: read #1,using 'form pos 386,PD 5.3,PD 5.2,PD 5.3,PD 5.2,pos 407,PD 5.3,PD 5.2,pos 418,10*C 20,10*N 1',rec=1: ficarate,ficawage,feducrat,feducwag,mcr,mcm,mat miscname$,mat dedcode : _
	close #1:
	for j=1 to 10
		miscname$(j)=lpad$(rtrm$(miscname$(j)(1:9)),9)
	next j
	nametab=66-int(len(env$('program_caption'))/2)
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",i,outIn,k
	open #2: "Name=[Q]\GLmstr\AcPrCks.h[cno],Shr",i,outi,r
	fnOpenPrn
	gosub L620
	L350: !
	if d(1)>0 and sum(empd) then
		pr #255,using L700: eno,"Total",empd(9),empd(10),empd(11),empd(12),empd(13),empd(14),empd(15),empd(16),empd(17),empd(18) pageoflow L950
		pr #255:
		mat empd=(0)
	end if
	L400: !
	read #1,using 'form pos 1,N 4,3*C 25,pos 271,2*N 5': eno,mat k$,mat adr eof PrTotals
	mat empd=(0)
	if adr(1)=0 then goto L400
	ca=adr(1)
	L440: !
	read #2,using 'form N 4,2*PD 4,19*PD 5.2,PD 3',rec=ca: mat d,nca conv L400
	if fndate_mmddyy_to_ccyymmdd(d(2))<beg_date or fndate_mmddyy_to_ccyymmdd(d(2))>end_date then goto L480
	gosub L670
	gosub L790
	L480: !
	if nca=0 then goto L350
	ca=nca
goto L440
L620: ! r:
	pr #255,using L540: date$('mm/dd/yy'),time$,env$('cnam')
	L540: form skip 1,pos 1,c 8,skip 1,pos 1,c 8,pos nametab,c 40
	p1=66-int(len(rtrm$(env$('program_caption')))/2)
	pr #255,using 'form pos p1,c 50': rtrm$(env$('program_caption'))
	p1=66-int(len(rtrm$(dat$))/2)
	pr #255,using 'form pos p1,c 50': rtrm$(fnpedat$)
	pr #255:
	pr #255,using 'form pos 1,c 6,c 21,10*c 10': "Emp #","Name",miscname$(1)(1:9) ,miscname$(2)(1:9),miscname$(3)(1:9),miscname$(4)(1:9),miscname$(5)(1:9),miscname$(6)(1:9),miscname$(7)(1:9),miscname$(8)(1:9),miscname$(9)(1:9),miscname$(10)(1:9)
return ! /r
L670: ! r: pr details
	pr #255,using L700: eno,k$(1)(1:20),d(9),d(10),d(11),d(12),d(13),d(14),d(15),d(16),d(17),d(18)
	mat empd=empd+d
L700: form pos 1,n 5,x 1,c 20,10*n 10.2
return ! /r
PrTotals: ! r:
	pr #255,using L740: "---------","---------","---------","---------","---------","---------","---------","---------","---------","---------"
L740: form pos 28,10 *c 10
	pr #255,using L770: "","Totals",mat totalded
	form pos 1,c 6,c 20,10*n 10.2
L770: form pos 1,c 6,c 20,10*n 10.2
	fnClosePrn : goto Xit
L790: ! ACCUMULATE TOTALS
	for j=1 to 10
		totalded(j)+=d(j+8)
	next j
return ! /r
L950: ! r:
	pr #255: newpage
	gosub L620
continue ! /r
Xit: fnXit
include: ertn
