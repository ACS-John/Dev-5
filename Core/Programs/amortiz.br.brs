! Replace S:\Core\Programs\Amortiz
! Amortization Program
	autoLibrary
	on error goto Ertn
 
	dim eX(50,2),io2$(6),io3$(2),io4$(2),wd4$(2),pfm$*200
	dim lor$*30,ln$*12,lee$*30,pfm2$*200,text$*60, mask$*25, dv$*40
	dim response$(20)*40
 
	y=12 : n=15 : ir=.05 : la=45000.
 
	wd4$(1)="1. Regular"
	wd4$(2)="2. Condensed"
	pfm$="FORM POS 1,N 6,x 3,pic(zz),""/"",pic(##),""/"",pic(####),"
	pfm2$="FORM POS 1,c 6,x 3,pic(zz/##/####),"
	for j=1 to 6
		pfm$=pfm$&"PIC(-----,---,---.##),"
		pfm2$=pfm2$&"PIC(-----,---,---.##),"
	next j
	pfm$=pfm$&"skip 1"
	pfm2$=pfm2$&"skip 1"
 
MENU1: close #101: ioerr ignore
	fnTos
	lc=0: rc=0
	fnLbl(lc+=1,1,"Loan Amount:",18,1)
	fnTxt(lc,20,20,0,0,"10"): response$(rc+=1)=str$(la)
	fnLbl(lc+=1,1,"Interest Rate (ie: .15):",18,1)
	fnTxt(lc,20,20,0,0,"43") : response$(rc+=1)=str$(ir)
	fnLbl(lc+=1,1,"Years Loan is for:",18,1)
	fnTxt(lc,20,2,0,0,"30"): response$(rc+=1)=str$(n)
	fnLbl(lc+=1,1,"Payments Per Year:",18,1)
	fnTxt(lc,20,2,0,0,"30"): response$(rc+=1)=str$(y)
	fnLbl(lc+=2,1,"Note: An Interest Rate of 15% should be entered as 0.15",57,2)
! tEXT$="Payment:"
! fnPRF(PFX,6,1,22,1,TEXT$)
! tEXT$=STR$(P)
! fnPRF(PFX,6,30,20,0,TEXT$)
	fnCmdSet(2)
	fnAcs("Amort1",0,mat response$,ckey)
	rc=0
	s=la=val(response$(rc+=1))
	ir=val(response$(rc+=1))
	n=val(response$(rc+=1))
	y=val(response$(rc+=1))
 
L330: ! Rinput Fields MAT IO1$: S,R,N,Y
	if ckey=99 or ckey=5 then goto Xit
	if ckey=2 then goto L1870
	if s=0 then goto Xit
	la=s
! ir=r
	p=la*(ir/y)/(1-(1/(1+ir/y)**(n*y))) ! that's the magic
	p=fn_a(p+.009)
	pr f "11,47,NZ 8.2,N": p
	if ckey=4 then goto L440 else goto L330
L440: close #101: ioerr L450
L450: open #101: "SRow=03,SCol=14,ERow=12,ECol=65,Border=SR,Caption=<Print Amortization Schedule",display,outIn
	pr #101: newpage
	i2=p2=0
	on fkey 5 goto L1830
	pr #101,fields "01,15,C 20,N": "            Lendor: "
	pr #101,fields "02,15,C 20,N": "          Borrower: "
	pr #101,fields "03,15,C 20,N": "       Loan Number: "
	pr #101,fields "04,15,C 20,N": "         Loan Date: "
	pr #101,fields "05,15,C 20,N": " First Payment Due: "
	pr #101,fields "06,15,C 20,N": "   Monthly Payment: "
	pr #101,fields "07,20,C 39,H,N": " NOTE: Enter dates in MMDDYYYY format."
	pr #101,fields "08,30,Cc 09,B,1": "Next (F1)"
	pr #101,fields "09,41,Cc 09,B,5": "Stop (F5)"
L590: !
	rinput #101,fields mat io2$: lor$,lee$,ln$,ld,fpd,p conv L590
	if cmdkey=5 then goto MENU1
	d=fpd
	d$=str$(d)
	d$=lpad$(rtrm$(d$),8)
	d1=val(d$(1:2))
	d2=val(d$(3:4))
	d3=val(d$(5:8))
	d5=d3*10000+d2+d1*100
L680: open #101: "SRow=8,SCol=21,ERow=14,ECol=58,Border=SR,Caption=<Amortization Payments",display,outIn
	pr #101: newpage
	pr f "09,22,C 36,N": "Enter any extra payments made toward"
	pr f "10,22,C 36,N": "  principal (blank when completed)"
	pr f "12,26,C 17,N": "Date (MMDDYYYY): "
	pr f "13,26,C 17,N": " Payment Amount: "
	pr f "15,30,Cc 09,B,1": "Next (F1)"
	pr f "15,41,Cc 09,B,5": "Stop (F5)"
	input fields mat io3$: eX(j6+1,1),eX(j6+1,2)
	if cmdkey=5 then goto MENU1
	eX(j6+1,1)=val(str$(eX(j6+1,1))(5:8)&str$(eX(j6+1,1))(1:4))
	if eX(j6+1,1)=0 then goto L820
	j6=j6+1
	goto L680
L820: close #101: ioerr ignore
	open #101: "SROW=11,SCOL=30,EROW=14,ECOL=50,BORDER=SR,CAPTION=<Select pr Type",display,outIn
	pr #101: newpage
	pr f mat io4$: mat wd4$
	pr f "15,34,Cc 11,B,5": "Cancel (F5)"
	input select mat io4$,attr "R": mat wd4$
	typ=curfld
	if cmdkey=5 then goto MENU1
	close #101: ioerr L910
L910: open #101: "SROW=9,SCOL=9,EROW=13,ECOL=71,BORDER=SR,CAPTION=<Print Amortization Schedule",display,outIn
	pr #101: newpage
	pr f "10,27,C 26,H,N": " Printing: Please wait..."
	pr f "12,30,C 15,N": "Printing Page: "
	pr f "14,34,Cc 11,B,5": "Cancel (F5)"
	pge=0
	fnopenprn
	gosub PR_HDR
	goto L1200
 
NPG: pr #255: newpage
	gosub PR_HDR
	continue
 
PR_HDR: pge=pge+1
	pr f "12,45,CL 5,N": str$(pge)
	pr #255,using L1080: "* Amortization Schedule *",pge
L1080: form pos 38,c 70,"Page",n 4
	pr #255: ""
	pr #255: "Lendor: ";lor$;"   Borrower: ";lee$;"   Loan No.: ";ln$
	pr #255,using L1120: "Loan Date: ",ld,"Amount of Loan: ",la,"Interest Rate: ",str$(r*100)&"%","Monthly Payment: ",p
L1120: form pos 1,c 12,pic(zz/zz/zzzz),x 3,c 17,pic(----,---,---.##),x 3,c 16,c 11,c 17,pic(----,---.##)
	pr #255: ""
	pr #255: "Payment                   Beginning        Interest       Principal          Ending        Interest       Principal"
	pr #255,using L1160: " Number   Due Date          Balance         Payment         Payment        Balance         To  Date       To   Date"
L1160: form pos 1,c 119
	pr #255: "_______   ________        _________        ________       _________        ________        ________       _________"
return
 
L1200: for j=1 to n*y
		b1=s
		s9=0
		i1=0
		eq=0
		if j6=0 then goto L1440
		for j1=1 to j6+1
			if eX(j1,1)=0 then goto L1440
			if eX(j1,1)>d4 and eX(j1,1)<d5 then goto L1320
			if eX(j1,1)=d5 then eq=j1
			if eX(j1,1)=d5 then goto L1440
		next j1
L1320: d9=(eX(j1,1)-int(eX(j1,1)*.0001)*10000)*10000+int(eX(j1,1)*.0001)
		d8=val(lpad$(str$(d9),8)(3:4))
		i1=fn_a(s*r/365*max(d2-d8,d8-d2))
		i2=i2+i1
		p2=p2+eX(j1,2)-i1
		i3=i3+i1
		p3=p3+eX(j1,2)-i1
		s=s+i1
		s=s-eX(j1,2)
		s9=eX(j1,2)
		pr #255,using pfm2$: "Extra",d9,b1,i1,eX(j1,2)-i1,s,i2,p2 pageoflow NPG
		b1=s
L1440: i=fn_a(s*r/y-i1)
		d4=d5
		s=s+i
		if s>p then goto L1500
		x9=9
		p=s
L1500: s=s-p
		i2=i2+i
		p2=p2+p-i
		i3=i3+i
		p3=p3+p-i
		pr #255,using pfm$: j,d1,d2,d3,b1,i,p-i,s,i2,p2 pageoflow NPG
		if eq=0 then goto L1640
		b1=s
		d9=(eX(eq,1)-int(eX(eq,1)*.0001)*10000)*10000+int(eX(eq,1)*.0001)
		s=b1-eX(eq,2)
		p2=p2+eX(eq,2)
		p3=p3+eX(eq,2)
		pr #255,using pfm2$: "Extra",d9,b1,0,eX(eq,2),s,i2,p2 pageoflow NPG
		b1=s
L1640: if y=1 then goto L1720
		d1=fn_a(d1+12/y)
		if d1<13 then goto L1730
		pr #255,using L1680: "Year End:",d3,"  Totals: ",i3,p3 pageoflow NPG
L1680: form pos 10,c 10,pic(zzzz),c 12,pos 37,pic(----,---,---.##),pic(-----,---,---.##),skip 2
		d1=1
		i3=0
		p3=0
L1720: d3=d3+1
L1730: if x9=9 then goto L1760
		d5=fn_a(d3*10000+d2+d1*100)
	next j
L1760: pr #255,using L1680: "Year End: ",d3,"  Totals:",i3,p3 pageoflow NPG
	pr #255,using L1790: "Last Payment Amount: ",p
	pr #255,using L1790: "Total Payments: ",i2+p2
L1790: form skip 2,c 24,pic(---,---,---.##)
	pr #255,using L1790: "Total Interest Paid: ",i2
	pr #255,using L1790: "Total Principal Paid: ",p2
	pr #255,using L1790: "Balance Remaining: ",s
L1830: fncloseprn
	on fkey 5 ignore
	x9=0
goto MENU1
L1870: lor$=" "
	lee$=" "
	ln$=" "
	ld=fpd=p=s=r=n=y=i2=p2=i3=p3=0
goto MENU1
 
Xit: end  ! fnXit("")
def fn_a(r)
		fn_a=int(r*100+.5)/100
fnend
include: Ertn
 
 
