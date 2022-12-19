! Replace S:\acsGL\OtherDeductionsYTD
! -- Other deductions Ytd/Qtd
 
	autoLibrary
	on error goto Ertn
 
	dim miscname$(10)*20,dedcode(10),totalytd(10)
	dim totalqtd(10)
	dim k(1),k$(3)*25,l$(1)*11,d(22),m(36),r$*10,n$*5,n(2),dat$*20
	dim fa$(2),sa$(2)*40,fb$(2),ext(2),adr(2),report$*35,deposit(31,2)
 
	fnTop(program$,"Other Deductions Registers-YTD QTD")
	fndat(dat$)
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,outi,r: read #1,using 'form pos 386,PD 5.3,PD 5.2,PD 5.3,PD 5.2,pos 407,PD 5.3,PD 5.2,pos 418,10*C 20,10*N 1',rec=1: ficarate,ficawage,feducrat,feducwag,mcr,mcm,mat miscname$,mat dedcode : _
	close #1:
	for j=1 to 10
		miscname$(j)=lpad$(rtrm$(miscname$(j)(1:9)),9)
	next j
	nametab=66-int(len(rtrm$(env$('cnam')))/2)
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",i,outIn,k
	open #2: "Name=[Q]\GLmstr\ACPRCKS.h[cno],Shr",i,outi,r
	report$="Other Deductions Register-YTD QTD"
	fnOpenPrn
	gosub L390
L240: read #1,using 'form pos 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5': eno,mat k$,ss$,mat m eof L490
	gosub L440
	gosub L570
	goto L240
 
HEADER: !
	pr #255,using L310: date$('mm/dd/yy'),time$,env$('cnam')
L310: form skip 1,pos 1,c 8,skip 1,pos 1,c 8,pos nametab,c 40,skip 1
	p1=66-int(len(rtrm$(report$))/2)
	pr #255,using L340: rtrm$(report$)
L340: form pos p1,c 50
	p1=66-int(len(rtrm$(dat$))/2)
	pr #255,using L340: rtrm$(fnpedat$)
return
 
L390: gosub HEADER
	pr #255:
	pr #255,using L420: "Emp #","Name",miscname$(1)(1:9) ,miscname$(2)(1:9),miscname$(3)(1:9),miscname$(4)(1:9),miscname$(5)(1:9),miscname$(6)(1:9),miscname$(7)(1:9),miscname$(8)(1:9),miscname$(9)(1:9),miscname$(10)(1:9)
L420: form pos 1,c 6,c 21,10*c 10
return
L440: ! pr details
	pr #255,using L470: eno,k$(1)(1:16)&"YTD",m(11),m(13),m(15),m(17),m(19),m(21),m(23),m(25),m(27),m(29)
	pr #255,using L470: eno,"                QTD ",m(12),m(14),m(16),m(18),m(20),m(22),m(24),m(26),m(28),m(30)
L470: form pos 1,n 5,x 1,c 20,10*n 10.2
return
L490: ! pr TOTALS
	pr #255,using L510: "---------","---------","---------","---------","---------","---------","---------","---------","---------","---------"
L510: form pos 28,10 *c 10
	pr #255,using L550: "","Totals-YTD",mat totalytd
	pr #255,using L550: "","Totals-QTD",mat totalqtd
	form pos 1,c 6,c 20,10*n 10.2
L550: form pos 1,c 6,c 20,10*n 10.2
	fnClosePrn : goto Xit
L570: ! ACCUMULATE TOTALS
	for j=1 to 10
		totalytd(j)+=m(j*2+9)
		totalqtd(j)+=m(j*2+10)
	next j
return
	pr #255: newpage
	gosub L390
	continue
Xit: fnXit
 
include: ertn
