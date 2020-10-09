! r: setup
	autoLibrary
	on error goto Ertn
	fnTop(program$)
	dim fl1$(8)
	dim io1$(60)
	
	dim xinp(3)
	dim invoiceNumber$*12,a1$*30
	dim pt(4),fl2$(8),scr2$(4)
	dim ot2$(4)
	dim nam$*25
	dim bk$(20)*30
	dim cde$(30)*6,ct(30),sc(30)
	DIM id$(30)*128
	DIM da(30),gl$(30)*12,gl(3)
	dim billto$(3)*30,cdk$*6,des$*60,bc$(3)*18
	bc$(1)="PARTIAL BILL"
	bc$(2)="FINAL BILL"
	bc$(3)="WRITE OFF"
 
	fncreg_read('Last Invoice Number',tmp$) : iv1=val(tmp$)
 
	fl1$(5)="1,10,c 60,h,n"
	fl1$(6)="2,10,c 60,h,n"
	fl1$(7)="9,1,c 80,h,n"
	fl1$(8)="24,2,c 60,h,n"
	fl2$(5)="2,10,c 60,h,n"
	fl2$(6)="14,10,c 60,h,n"
	fl2$(7)="15,10,c 60,h,n"
	fl2$(8)="24,2,c 60,h,n"
	io1$(1)="4,25,N 5,UE,N"
	io1$(2)="5,25,N 1,UE,N"
	io1$(3)="6,25,N 6,UE,N"
	io1$(4)="7,25,C 12,UE,N"
	for j=1 to 10
		io1$(4+j)=str$(j+9)&",1,C 6,UE,N"
		io1$(14+j)=str$(j+9)&",8,C 55,UE,N"
		io1$(24+j)=str$(j+9)&",64,N 10.2,UE,N"
		io1$(34+j)=str$(j+9)&",75,N 2,UE,N"
		io1$(44+j)=str$(j+9)&",78,N 2,UE,N"
	next j
	for j=1 to 4
		fl1$(j)=str$(j+3)&",8,c 20,N"
		ot2$(j)=str$(j+3)&",25,n 10.2,ut,n"
		fl2$(j)=fl1$(j)
	next j
	dim scr1$(4)
	scr1$(1)="Client"
	scr1$(2)="Billing Code"
	scr1$(3)="Date"
	scr1$(4)="Invoice"
	enableEbilling=fnClient_has('EM')
	open #1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,input,keyed
	open #32: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndx2.h[cno],Shr",internal,input,keyed
! /r
REGULAR_ENTRY: ! r:
	open #hTmpInvoice=fnH: "Name=S:\Core\Data\acsllc\tmpInvoice.h[cno],RecL=4675,Replace",internal,outIn,relative
	F_TMWK2: form pos 1,n 5,n 1,n 6,c 12,30*c 6,30*c 128,30*pd 5.2,30*n 2,30*n 2,30*c 12
	open #3: "Name=S:\Core\Data\acsllc\IVDesc.h[cno],KFName=S:\Core\Data\acsllc\IVDIndex.h[cno],Shr",internal,input,keyed
	fnopenprn
	ScreenEntry: !
	L950: !
	inp3=xinp(3)
	mat xinp=(0)
	xinp(3)=inp3
	invoiceNumber$=str$(iv1+1)
	! invoiceNumber$=str$(iv1+1) conv ignore
	mat id$=(" ")
	mat da=(0)
	mat cde$=("")
	mat ct=(0)
	mat sc=(0)
	L1080: !
	pr newpage
	dim scrid$(4)*80
	scrid$(1)="Time Management Input of Invoices"
	scrid$(2)="Enter CLIENT # as 0 when completed."
	scrid$(3)="-Code- ---------Invoice Descriptions-------------------------  --Amount-- CT SC"
	scrid$(4)="  Press F1 when completed with this screen"

	pr f mat fl1$: mat scr1$,mat scrid$
	pr f "24,2,Cc 70,R,N": "[F1] Continue   [F5] Stop"
	pr f mat io1$: mat xinp,invoiceNumber$,mat cde$(1:10),mat id$(1:10),mat da(1:10),mat ct(1:10),mat sc(1:10)
	pr f "1,72,C 8,R,N": date$
	pr f "2,72,C 8,R,N": time$
	pr f io1$(2): 2
	do
		input fields mat io1$,attr "R": mat xinp,invoiceNumber$,mat cde$(1:10),mat id$(1:10),mat da(1:10),mat ct(1:10),mat sc(1:10) conv CONV1
		cp1=currow
		if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
		if cmdkey>0 then goto L1260 else ce=curfld
		goto L1260
		L1200: !
		do
			ce+=1
			if ce>udim(io1$) then ce=1
			CT1: !
			io1$(ce)=rtrm$(uprc$(io1$(ce)))
			ce1=pos(io1$(ce),"U",9)
		loop while ce1=0
		ce2=ce1+1
		io1$(ce)(ce1:ce1)="UC"
	loop
	CONV1: !
	if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
	ERR1: !
	pr f "24,78,C 1": bell
	goto CT1
	L1260: !
	de=cp1-9
	if de<1 or de>10 then de=1
	if chg=2 and xinp(1)=0 then goto L1720
	if cmdkey=1 or xinp(1)=0 then goto L1710
	if ce><1 then goto L1370
	k$=lpad$(str$(xinp(1)),5)
	read #1,using 'form pos 6,c 30',key=k$: a1$ nokey ERR1
	pr f "4,35,C 40,H,N": a1$
	goto L1200
	L1370: !
	if ce=2 and xinp(2)<1 or xinp(2)>2 then goto ERR1
	if ce=2 then pr f "5,35,C 20,H,N": bc$(xinp(2))
	if ce=3 and xinp(3)<10100 or xinp(3)>123199 then goto ERR1
	if ce<5 then goto L1200
	if ce>4 and ce<15 then goto L1610
	if ce<15 or ce>24 then goto L1450
	if rtrm$(id$(de))="" then goto ERR1
	ce=ce+10 : goto CT1
	L1450: !
	if ce<25 or ce>34 then goto L1480
	if da(de)=0 then goto ERR1
	ce+=10 : goto CT1
	L1480: !
	if ce<35 or ce>44 then goto L1510
	if ct(de)<1 or ct(de)>30 then goto ERR1
	ce=ce+10 : goto CT1
	L1510: !
	if ce<45 or ce>55 then goto L1540
	if sc(de)<0 or sc(de)>25 then goto ERR1
	ce=ce-39
	L1540: !
	gl(1)=val(gl$(de)(1:3))
	gl(2)=val(gl$(de)(4:9))
	gl(3)=val(gl$(de)(10:12))
	pr f "22,40,C 20,N": "General Ledger #"
	dim fli4$(3)
	fli4$(1)="22,58,N 3,ut,N"
	fli4$(2)="22,62,N 6,ut,N"
	fli4$(3)="22,69,N 3,ut,N"
	rinput fields mat fli4$,attr "R": mat gl
	gl$(de)=lpad$(str$(gl(1)),3)&lpad$(str$(gl(2)),6)&lpad$(str$(gl(3)),3)
	pr f "22,40,C 40": ""
	goto CT1
	L1610: !
	if rtrm$(cde$(de))="" then goto L1700
	cde$(de)=uprc$(cde$(de))
	cdk$=lpad$(rtrm$(cde$(de)),6)
	read #3,using 'form pos 1,c 6,c 55,pd 5.2,c 12',key=cdk$: cdk$,des$,da,gl$(de) nokey ERR1
	pr f io1$(ce+10): des$
	pr f io1$(ce+20): da
	if cmdkey<>6 then ce+=20
	goto CT1
	L1700: !
	ce=ce+10 : goto CT1
	L1710: !
	if xinp(1)=0 and chg><2 then goto ScreenFinal
	L1720: !
	if xinp(1)=0 then mat xinp=(0)
	if xinp(1)=0 then goto L1900
	pt(1)=pt(1)+xinp(1)
	for j=1 to 10
		pt(2)+=da(j)
		pt(3)+=ct(j)
		pt(4)+=sc(j)
	next j
	if chg=2 then goto L1900
	! rw=lrec(hTmpInvoice)+1
	write #hTmpInvoice,using F_TMWK2: mat xinp,invoiceNumber$,mat cde$,mat id$,mat da,mat ct,mat sc,mat gl$
	iv1=val(invoiceNumber$)
	fncreg_write('Last Invoice Number',iv1$)
	L1850: !
	if x9=0 then goto L950
	xinp(3)=0
	xinp(5)=0
	xinp(6)=0
	goto L1080
	L1900: !
	rewrite #hTmpInvoice,using F_TMWK2,rec=rr: mat xinp,invoiceNumber$,mat cde$,mat id$,mat da,mat ct,mat sc,mat gl$
	SCR_ADDEDIT: !
	pr newpage
	pr f "10,10,c 60": "Enter ref # to correct; enter 0 when completed"
	L1930: !
	input f "10,60,N 5,UE,N": rr conv L1930
	if rr=0 then goto ScreenFinal
	read #hTmpInvoice,using F_TMWK2,rec=rr: mat xinp,invoiceNumber$,mat cde$,mat id$,mat da,mat ct,mat sc,mat gl$ noRec SCR_ADDEDIT
	pt(1)=pt(1)-xinp(1)
	for j=1 to 10
		pt(2)=pt(2)-da(j)
		pt(3)=pt(3)-ct(j)
		pt(4)=pt(4)-sc(j)
	next j
goto L1080 ! /r
 
ScreenFinal: ! r:
	pr newpage
	scrid$(1)="TIME MANAGEMENT INPUT PROOF TOTALS"
	scrid$(2)="1 for listing, 2 for corrections, 3 for additional entries,"
	scrid$(3)=" 4 to pr invoices entered, or 5 to merge and email."
	scrid$(4)=""
	pr f mat fl2$: mat scr2$,mat scrid$
	pr f mat ot2$: mat pt
	do
		L2110: !
		input fields "16,30,N 1,UE,N": chg conv L2110
		
		if chg=1 then
			gosub PrProof
			goto ScreenFinal
		else if chg=2 then
			goto ScreenCorrection
		else if chg=3 then
			goto ScreenEntry
		else if chg=4 then
			gosub ScreenPrintInvoices
			goto ScreenFinal
		else if chg=5 then
			! fnEmailQueuedInvoices(email_date$)  this seems like a good idea to add here, perhaps a question like int S:\Time Management\ACS Invoices.br.brs
			close #1:
			close #32:
			close #hTmpInvoice:
			fnMergeInvoices
			goto Xit
		end if
	loop
! /r
PrProof: ! r:
	fnopenprn
	gosub PrProofHead
	for j=1 to lrec(hTmpInvoice)
		read #hTmpInvoice,using F_TMWK2,rec=j: mat xinp,invoiceNumber$,mat cde$,mat id$,mat da,mat ct,mat sc,mat gl$
		if xinp(1)<>0 then
			pr #255: "Ref    Client    Billing-Code     Date      Invoice  "
			pr #255,using 'form pos 1,n 4,n 8,n 10,n 12,x 2,c 12': j,mat xinp,invoiceNumber$ pageoflow PrProofPgOf
			pr #255: ''
			pr #255: "-Code-  -------Description-------------------------------------  --Amount--      Cat     Sub  -GL--Number-"
			for j1=1 to 30
				if rtrm$(id$(j1))<>"" then
					pr #255,using 'form pos 1,c 8,c 56,n 11.2,2*n 8,x 3,c 12': cde$(j1),id$(j1),da(j1),ct(j1),sc(j1),gl$(j1) pageoflow PrProofPgOf
				end if
			next j1
			pr #255: rpt$('_',106)
			pr #255:
		end if
	next j
	fncloseprn
return
PrProofHead: !
	pr #255: ''
	pr #255,using 'form pos 1,c 8,pos 44,cc 44,': date$,env$('cnam')
	pr #255,using 'form pos 1,c 8,pos 44,c 44': time$,"Enter and  Print Invoices - Proof Listing"
	pr #255: ''
return
PrProofPgOf: !
	pr #255: newpage
	gosub PrProofHead
continue
! /r

ScreenCorrection: ! r:
	scrid$(1)="Input Correction Screen"
	scrid$(2)="Enter client number as 0 to delete this entry"
	scrid$(3)="  Desc/Code   Invoice Descriptions"
	scrid$(4)="  Press F1 when completed with this screen"
goto SCR_ADDEDIT ! /r
ScreenPrintInvoices: ! r:
	select_invoices_to_print=0
	fnInvoiceOpen
	align=0
	restore #hTmpInvoice:
	do
		PR_SELECTED_INVOICE: !
 
		read #hTmpInvoice,using F_TMWK2: mat xinp,invoiceNumber$,mat cde$,mat id$,mat da,mat ct,mat sc,mat gl$ eof PRI_EOF
		if xinp(1) then
			k$=lpad$(str$(xinp(1)),5)
			read #1,using 'form pos 6,3*c 30',key=k$: mat billto$
			fnInvoiceAdd(k$,mat billto$,invoiceNumber$,xinp(3),mat id$,mat da,0)
		end if
		! if select_invoices_to_print=1 then goto SCR_SELECT_INVOICE
	loop 
	PRI_EOF: !
	fnInvoiceClose(xinp(3), 'Enter and Print')
	L2870: !
return ! /r
Xit: fnXit
include: ertn
