! r: setup
	autoLibrary
	on error goto Ertn
	fnTop(program$)

	dim bc$(3)*18
	bc$(1)='PARTIAL BILL'
	bc$(2)='FINAL BILL'
	bc$(3)='WRITE OFF'

	fncreg_read('Last Invoice Number',invoiceNumber$)
	invoiceNumber$=str$(val(invoiceNumber$)+1)
	if invoiceNumber$='1' then invoiceNumber$=date$(days(date$)-20,'yymm')&'01'
	! r: screen arrays

	dim io1$(60)
	io1$(1)='4,25,N 5,UE,N'
	io1$(2)='5,25,N 1,UE,N'
	io1$(3)='6,25,N 6,UE,N'
	io1$(4)='7,25,C 12,UE,N'
	for j=1 to 10
		io1$(4+j)=str$(j+9)&',1,C 6,UE,N'
		io1$(14+j)=str$(j+9)&',8,C 55,UE,N'
		io1$(24+j)=str$(j+9)&',64,N 10.2,UE,N'
		io1$(34+j)=str$(j+9)&',75,N 2,UE,N'
		io1$(44+j)=str$(j+9)&',78,N 2,UE,N'
	next j
	dim ot2$(4)
	for j=1 to 4
		ot2$(j)=str$(j+3)&',25,n 10.2,ut,n'
	next j
	! /r
	enableEbilling=fnClientHas('EM')
	open #hClient=fnH: 'Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\Client-Idx.h[cno],Shr',i,i,k ! 1
	! open #hClient2=fnH: 'Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\CLIndx2.h[cno],Shr',i,i,k ! 32
! /r
! r: Regular Entry (setup stage 2)
	if ~setupRegularEntry then ! r:
		setupRegularEntry=1
		dim fl1$(8)
		dim scr1$(4)
		fl1$(1)=' 4, 8,Cr 15' : scr1$(1)='Client:'
		fl1$(2)=' 5, 8,Cr 15' : scr1$(2)='Billing Code:'
		fl1$(3)=' 6, 8,Cr 15' : scr1$(3)='Date:'
		fl1$(4)=' 7, 8,Cr 15' : scr1$(4)='Invoice:'
		dim scrid$(4)*80
		fl1$(5)=' 1,10,C  60' : scrid$(1)='Client Billing Input of Invoices'
		fl1$(6)=' 2,10,C  60' : scrid$(2)='Enter CLIENT # as 0 when completed.'
		fl1$(7)=' 9, 1,C  80' : scrid$(3)='-Code- ---------Invoice Descriptions-------------------------  --Amount-- Cat SC'
		fl1$(8)='24, 2,Cc 60' : scrid$(4)='[F1] Complete'
	end if ! /r
	open #hTmpInvoice=fnH: 'Name=S:\Core\Data\acsllc\tmpInvoice.h[cno],RecL=4675,Replace',i,outi,r
	! write #hTmpInvoice,using FtmpInv: mat xInp,invoiceNumber$,mat invDataCde$,
	!														mat invDataId$,mat invDataAmount,
	!														mat invDataCategory,mat invDataSystemCode,mat gl$
	FtmpInv: form pos 1,n 5,n 1,n 6,c 12,30*c 6,30*c 128,30*pd 5.2,30*n 2,30*n 2,30*c 12
	dim xInp(3)
	dim invDataId$(30)*128
	dim invDataAmount(30)
	dim invDataCategory(30)
	dim invDataCde$(30)*6
	dim invDataSystemCode(30)
	open #hIvDesc=fnH: 'Name=S:\Core\Data\acsllc\IVDesc.h[cno],KFName=S:\Core\Data\acsllc\IVDIndex.h[cno],Shr',i,i,k
	fnOpenPrn
	gosub ClearVar
goto ScrFm ! /r
ClearVar: ! r:
	inp3=xInp(3)
	mat xInp=(0)
	xInp(3)=inp3
	invoiceNumber$=str$(val(invoiceNumber$)+1)
	mat invDataId$=(' ')
	mat invDataAmount=(0)
	mat invDataCde$=('')
	mat invDataCategory=(0)
	mat invDataSystemCode=(0)
return ! /r
ScrFm: ! r: (mat fl1$: mat scr1$,mat scrid$
	pr newpage

	pr f mat fl1$: mat scr1$,mat scrid$
	pr f '24,2,Cc 70,R,N': '[F1] Continue   [F5] Stop'
	pr f mat io1$: mat xInp,invoiceNumber$,mat invDataCde$(1:10),mat invDataId$(1:10),mat invDataAmount(1:10),mat invDataCategory(1:10),mat invDataSystemCode(1:10)
	pr f '1,72,C 8,R,N': date$
	pr f '2,72,C 8,R,N': time$
	pr f io1$(2): 2
	do
		input fields mat io1$,attr 'R': mat xInp,invoiceNumber$,mat invDataCde$(1:10),mat invDataId$(1:10),mat invDataAmount(1:10),mat invDataCategory(1:10),mat invDataSystemCode(1:10) conv CONV1

		currentPosition=currow ! r: basic U/UC io1$ processing
		if ce>0 then io1$(ce)(ce1:ce2)='U': ce=0
		if cmdkey>0 then goto L1260 else ce=curfld
		goto L1260
		L1200: !
		do
			ce+=1
			if ce>udim(io1$) then ce=1
			CT1: !
			io1$(ce)=rtrm$(uprc$(io1$(ce)))
			ce1=pos(io1$(ce),'U',9)
		loop while ce1=0
		ce2=ce1+1
		io1$(ce)(ce1:ce1)='UC'
	loop
	CONV1: !
	if ce>0 then io1$(ce)(ce1:ce2)='U'
	ce=cnt+1
	ERR1: !
	pr f '24,78,C 1': bell
	goto CT1 ! /r
	L1260: !
	relativePosition=currentPosition-9
	if relativePosition<1 or relativePosition>10 then relativePosition=1
	if chg=2 and xInp(1)=0 then goto L1720
	if cmdkey=1 or xInp(1)=0 then goto L1710
	if ce><1 then goto L1370
	clientBilled$=rpad$(str$(xInp(1)),5)
	dim a1$*30
	read #hClient,using 'form pos 6,c 30',key=clientBilled$: a1$ nokey ERR1
	pr f '4,35,C 40,H,N': a1$
	goto L1200

	L1370: !
	if ce=2 and xInp(2)<1 or xInp(2)>2 then goto ERR1
	if ce=2 then pr f '5,35,C 20,H,N': bc$(xInp(2))
	if ce=3 and xInp(3)<10100 or xInp(3)>123199 then goto ERR1
	if ce<5 then goto L1200
	if ce>4 and ce<15 then goto L1610
	if ce<15 or ce>24 then goto L1450
	if rtrm$(invDataId$(relativePosition))='' then goto ERR1
	ce=ce+10 : goto CT1

	L1450: !
	if ce<25 or ce>34 then goto L1480
	if invDataAmount(relativePosition)=0 then goto ERR1
	ce+=10 : goto CT1

	L1480: !
	if ce<35 or ce>44 then goto L1510
	if invDataCategory(relativePosition)<1 or invDataCategory(relativePosition)>30 then goto ERR1
	ce=ce+10 : goto CT1

	L1510: !
	if ce<45 or ce>55 then goto L1540
	if invDataSystemCode(relativePosition)<0 or invDataSystemCode(relativePosition)>25 then goto ERR1
	ce=ce-39
	L1540: !

	dim gl(3)
	gl(1)=val(gl$(relativePosition)(1:3))
	gl(2)=val(gl$(relativePosition)(4:9))
	gl(3)=val(gl$(relativePosition)(10:12))
	pr f '22,40,C 20,N': 'General Ledger #'
	dim fli4$(3)
	fli4$(1)='22,58,N 3,ut,N'
	fli4$(2)='22,62,N 6,ut,N'
	fli4$(3)='22,69,N 3,ut,N'
	rinput fields mat fli4$,attr 'R': mat gl
	dim gl$(30)*12
	gl$(relativePosition)=lpad$(str$(gl(1)),3)&lpad$(str$(gl(2)),6)&lpad$(str$(gl(3)),3)
	pr f '22,40,C 40': ''
	goto CT1

	L1610: !
	if rtrm$(invDataCde$(relativePosition))='' then goto L1700
	invDataCde$(relativePosition)=uprc$(invDataCde$(relativePosition))
	dim cdk$*6
	cdk$=lpad$(rtrm$(invDataCde$(relativePosition)),6)
	dim des$*60
	read #hIvDesc,using 'form pos 1,c 6,c 55,pd 5.2,c 12',key=cdk$: cdk$,des$,invDataAmount,gl$(relativePosition) nokey ERR1
	pr f io1$(ce+10): des$
	pr f io1$(ce+20): invDataAmount
	if cmdkey<>6 then ce+=20
	goto CT1

	L1700: !
	ce+=10 : goto CT1

	L1710: !
	if xInp(1)=0 and chg><2 then goto ScrProofTotals
	L1720: !
	if xInp(1)=0 then mat xInp=(0)
	if xInp(1)=0 then goto DoRewriteInvoice
	
	dim pt(4)
	pt(1)=pt(1)+xInp(1)
	for j=1 to 10
		pt(2)+=invDataAmount(j)
		pt(3)+=invDataCategory(j)
		pt(4)+=invDataSystemCode(j)
	next j
	if chg=2 then goto DoRewriteInvoice
	! rw=lrec(hTmpInvoice)+1
	write #hTmpInvoice,using FtmpInv: mat xInp,invoiceNumber$,mat invDataCde$,mat invDataId$,mat invDataAmount,mat invDataCategory,mat invDataSystemCode,mat gl$
	fncreg_write('Last Invoice Number',invoiceNumber$)
	L1850: !
	if x9=0 then gosub ClearVar : goto ScrFm
	xInp(3)=0
	xInp(5)=0
	xInp(6)=0
goto ScrFm ! /r

	DoRewriteInvoice: ! r:
		rewrite #hTmpInvoice,using FtmpInv,rec=rr: mat xInp,invoiceNumber$,mat invDataCde$,mat invDataId$,mat invDataAmount,mat invDataCategory,mat invDataSystemCode,mat gl$
	goto ScrEditSelectRef ! /r
	
	ScrEditSelectRef: ! r:
		pr newpage
		pr f '10,10,c 60': 'Enter ref # to correct; enter 0 when completed'
		input f '10,60,N 5,UE,N': rr
		if rr=0 then goto ScrProofTotals
		read #hTmpInvoice,using FtmpInv,rec=rr: mat xInp,invoiceNumber$,mat invDataCde$,mat invDataId$,mat invDataAmount,mat invDataCategory,mat invDataSystemCode,mat gl$ noRec ScrEditSelectRef
		pt(1)=pt(1)-xInp(1)
		for j=1 to 10
			pt(2)=pt(2)-invDataAmount(j)
			pt(3)=pt(3)-invDataCategory(j)
			pt(4)=pt(4)-invDataSystemCode(j)
		next j
	goto ScrFm ! /r

ScrProofTotals: ! r:
	pr newpage
	pr f '2,10,c 60,h,n' :'INPUT PROOF TOTALS'
	pr f '14,10,c 60,h,n':'1 for listing, 2 for corrections, 3 for additional entries,'
	pr f '15,10,c 60,h,n':' 4 to pr invoices entered, or 5 to merge and email.'
	pr f '24,2,c 60,h,n' :''
	pr f mat ot2$: mat pt
	do
		input fields '16,30,N 1,UE,N': chg conv ignore
		if chg=1 then
			gosub PrProofList
			goto ScrProofTotals
		else if chg=2 then
			scrid$(1)='Input Correction Screen'
			scrid$(2)='Enter client number as 0 to delete this entry'
			scrid$(3)='  Desc/Code   Invoice Descriptions'
			scrid$(4)='[F1] Complete'
			goto ScrEditSelectRef
		else if chg=3 then
			gosub ClearVar
			goto ScrFm
		else if chg=4 then
			gosub ScreenPrintInvoices
			goto ScrProofTotals
		else if chg=5 then
			! fnEmailQueuedInvoices(email_date$)  this seems like a good idea to add here, perhaps a question like int S:\Client Billing\ACS Invoices.br.brs
			close #hClient:
			! close #hClient2:
			close #hTmpInvoice:
			fnMergeInvoices
			goto Xit
		end if
	loop
! /r
PrProofList: ! r:
	fnOpenPrn
	gosub PrProofListHead
	for j=1 to lrec(hTmpInvoice)
		read #hTmpInvoice,using FtmpInv,rec=j: mat xInp,invoiceNumber$,mat invDataCde$,mat invDataId$,mat invDataAmount,mat invDataCategory,mat invDataSystemCode,mat gl$
		if xInp(1)<>0 then
			pr #255: 'Ref    Client    Billing-Code     Date      Invoice  '
			pr #255,using 'form pos 1,n 4,n 8,n 10,n 12,x 2,c 12': j,mat xInp,invoiceNumber$ pageoflow PrProofListPgOf
			pr #255: ''
			pr #255: '-Code-  -------Description-------------------------------------  --Amount--      Cat     Sub  -GL--Number-'
			for j1=1 to 30
				if rtrm$(invDataId$(j1))<>'' then
					pr #255,using 'form pos 1,c 8,c 56,n 11.2,2*n 8,x 3,c 12': invDataCde$(j1),rtrm$(invDataId$(j1))(1:56),invDataAmount(j1),invDataCategory(j1),invDataSystemCode(j1),gl$(j1) pageoflow PrProofListPgOf
				end if
			next j1
			pr #255: rpt$('_',106)
			pr #255:
		end if
	next j
	fnClosePrn
return
	PrProofListHead: ! r:
		pr #255: ''
		pr #255,using 'form pos 1,c 8,pos 44,cc 44,': date$,env$('cnam')
		pr #255,using 'form pos 1,c 8,pos 44,c 44': time$,env$('program_caption')&' - Proof Listing'
		pr #255: ''
	return ! /r
	PrProofListPgOf: ! r:
		pr #255: newpage
		gosub PrProofListHead
	continue ! /r

! /r

ScreenPrintInvoices: ! r:
	select_invoices_to_print=0
	fnInvoiceOpen
	restore #hTmpInvoice:
	do
		read #hTmpInvoice,using FtmpInv: mat xInp,invoiceNumber$,mat invDataCde$,mat invDataId$,mat invDataAmount,mat invDataCategory,mat invDataSystemCode,mat gl$ eof PRI_EOF
		! xInp(1) = client id
		! xInp(3) = invoice date
		if xInp(1) then
			clientBilled$=rpad$(str$(xInp(1)),5)
			dim billingAddress$(3)*30
			read #hClient,using 'form pos 6,3*c 30',key=clientBilled$: mat billingAddress$
			pr ' adding invoice ' : pause
			fnInvoiceAdd(clientBilled$,mat billingAddress$,invoiceNumber$,xInp(3),mat invDataId$,mat invDataAmount,0)
		end if
	loop
	PRI_EOF: !
	fnInvoiceClose(xInp(3), 'Enter and Print')
	execute 'sy -c -w explorer "'&fnReportCacheFolderCurrent$&'\Ebilling"'
	execute 'sy -c -w explorer "'&fnReportCacheFolderCurrent$&'\Invoice\Archive"'
	execute 'sy -c -w explorer "'&fnReportCacheFolderCurrent$&'\Invoice\Print"'
return ! /r

Xit: fnXit
include: ertn
