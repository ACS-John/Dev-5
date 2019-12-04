! r: setup
	library 'S:\Core\Library': fntop,fnxit
	library 'S:\Core\Library': fnopenprn,fncloseprn
	library 'S:\Core\Library': fncreg_read,fncreg_write
	library 'S:\Core\Library': fngethandle
	library 'S:\Core\Library': fnmakesurepathexists,fnprint_file_name$,fnCustomerHasEbilling
	library 'S:\Core\Library': fnPrintInvoice
	library 'S:\Core\Library': fnClient_has
	library 'S:\Core\Library': fnreport_cache_folder_current$
	library 'S:\Core\Library': fnCopy
	on error goto ERTN
	fntop(program$)
	dim fl1$(8),io1$(60),scrid$(4)*80
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
		fl1$(j)=str$(j+3)&",8,c 20"
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
	open #hTmpInvoice:=fngethandle: "Name=S:\Core\Data\acsllc\tmpInvoice.h[cno],RecL=4675,Replace",internal,outIn,relative
	F_TMWK2: form pos 1,n 5,n 1,n 6,c 12,30*c 6,30*c 128,30*pd 5.2,30*n 2,30*n 2,30*c 12
	open #3: "Name=S:\Core\Data\acsllc\IVDesc.h[cno],KFName=S:\Core\Data\acsllc\IVDIndex.h[cno],Shr",internal,input,keyed
	fnopenprn
	L910: !
	scrid$(1)="Time Management Input Of Invoices"
	scrid$(2)="Enter CLIENT # as 0 when completed."
	scrid$(3)="-Code- ---------Invoice Descriptions-------------------------  --Amount-- CT SC"
	scrid$(4)="  Press F1 when completed with this screen"
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
	pr f mat fl1$: mat scr1$,mat scrid$
	pr f "24,2,C 70,R,N": "  Press F1 to Continue; F5 to Stop; F6 for Search"
	pr f mat io1$: mat xinp,invoiceNumber$,mat cde$(1:10),mat id$(1:10),mat da(1:10),mat ct(1:10),mat sc(1:10)
	pr f "1,72,C 8,R,N": date$
	pr f "2,72,C 8,R,N": time$
	pr f io1$(2): 2
	L1150: !
	input fields mat io1$,attr "R": mat xinp,invoiceNumber$,mat cde$(1:10),mat id$(1:10),mat da(1:10),mat ct(1:10),mat sc(1:10) conv CONV1
	cp1=currow
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L1260 else ce=curfld
	goto L1260
	L1200: !
	ce=ce+1
	if ce>udim(io1$) then ce=1
	CT1: !
	io1$(ce)=rtrm$(uprc$(io1$(ce)))
	ce1=pos(io1$(ce),"U",9)
	if ce1=0 then goto L1200
	ce2=ce1+1
	io1$(ce)(ce1:ce1)="UC"
	goto L1150
	CONV1: !
	if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
	ERR1: !
	pr f "24,78,C 1": bell
	goto CT1
	L1260: !
	de=cp1-9
	if de<1 or de>10 then de=1
	if cmdkey=6 then goto HELP1
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
	ce=ce+10 : goto CT1
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
	if cmdkey <> 6 then ce=ce+20
	goto CT1
	L1700: !
	ce=ce+10 : goto CT1
	L1710: !
	if xinp(1)=0 and chg><2 then goto SCR_FINAL
	L1720: !
	if xinp(1)=0 then mat xinp=(0)
	if xinp(1)=0 then goto L1900
	pt(1)=pt(1)+xinp(1)
	for j=1 to 10
		pt(2)=pt(2)+da(j)
		pt(3)=pt(3)+ct(j)
		pt(4)=pt(4)+sc(j)
	next j
	if chg=2 then goto L1900
	rw=lrec(hTmpInvoice)+1
	write #hTmpInvoice,using F_TMWK2: mat xinp,invoiceNumber$,mat cde$,mat id$,mat da,mat ct,mat sc,mat gl$
	iv1=val(invoiceNumber$) ! conv L1850  <--- removed conv on 5/11/18 - want it to error - why isn't my invoice number getting updated after printing invoices
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
	input fields "10,60,N 5,UE,N": rr conv L1930
	if rr=0 then goto SCR_FINAL
	read #hTmpInvoice,using F_TMWK2,rec=rr: mat xinp,invoiceNumber$,mat cde$,mat id$,mat da,mat ct,mat sc,mat gl$ noRec SCR_ADDEDIT
	pt(1)=pt(1)-xinp(1)
	for j=1 to 10
		pt(2)=pt(2)-da(j)
		pt(3)=pt(3)-ct(j)
		pt(4)=pt(4)-sc(j)
	next j
goto L1080 ! /r

SCR_FINAL: ! r:
	pr newpage
	scrid$(1)="TIME MANAGEMENT INPUT PROOF TOTALS"
	scrid$(2)="1 for listing, 2 for corrections, 3 for additional entries,"
	scrid$(3)=" 4 to pr invoices entered, or 5 to merge and email."
	scrid$(4)=""
	pr f mat fl2$: mat scr2$,mat scrid$
	pr f mat ot2$: mat pt
	L2110: input fields "16,30,N 1,UE,N": chg conv L2110
on chg goto PR_PROOF,SCR_CORRECTION,L910,SCR_PRINT_INVOICES,GO_MERGE none L2110
! /r
PR_PROOF: ! r:
	pr newpage
	pr f "13,30,Cc 20,B,5": "Cancel (F5)"
	pr f "10,10,c 60,h,n": "TIME MANAGEMENT CORRECTION LISTING IN PROCESS"
	gosub PR_PROOF_HEAD
	for j=1 to lrec(hTmpInvoice)
		read #hTmpInvoice,using F_TMWK2,rec=j: mat xinp,invoiceNumber$,mat cde$,mat id$,mat da,mat ct,mat sc,mat gl$
		if xinp(1)<>0 then
			pr #255: "Ref    Client    Billing-Code     Date      Invoice  "
			pr #255,using 'form pos 1,n 4,n 8,n 10,n 12,x 2,c 12': j,mat xinp,invoiceNumber$ pageoflow PR_PROOF_PGOF
			pr #255: ''
			pr #255: "-Code-  -------Description-------------------------------------  --Amount--      Cat     Sub  -GL--Number-"
			for j1=1 to 30
				if rtrm$(id$(j1))<>"" then
					pr #255,using 'form pos 1,c 8,c 56,n 11.2,2*n 8,x 3,c 12': cde$(j1),id$(j1),da(j1),ct(j1),sc(j1),gl$(j1) pageoflow PR_PROOF_PGOF
				end if
			next j1
			pr #255: "----------------------------------------------------------------------------------------------------------"
			pr #255:
		end if
	next j
	fncloseprn
goto SCR_FINAL ! /r
PR_PROOF_HEAD: ! r:
	pr #255,using F_Head: date$,env$('cnam'),time$,"Enter and  Print Invoices - Proof Listing"
	F_Head: form skip 1,pos 1,c 8,pos 44,cc 44,skip 1,pos 1,c 8,pos 44,c 44,skip 2
return  ! /r
PR_PROOF_PGOF: ! r:
	pr #255: newpage
	gosub PR_PROOF_HEAD
continue  ! /r
SCR_CORRECTION: ! r:
	scrid$(1)="TIme Management Input Correction Screen"
	scrid$(2)="Enter client # as 0 to delete this entry"
	scrid$(3)="  Desc/Code   Invoice Descriptions"
	scrid$(4)="  Press F1 when completed with this screen"
goto SCR_ADDEDIT ! /r
GO_MERGE: ! r:
	! fnEmailQueuedInvoices(email_date$)  this seems like a good idea to add here, perhaps a question like int S:\Time Management\ACS Invoices.br.brs
	close #1:
	close #hTmpInvoice:
chain "S:\acsTM\TMMRGINV" ! /r
SCR_PRINT_INVOICES: ! r:
	select_invoices_to_print=0
	! pr newpage
	! pr f "10,20,c 30,h,n": "position invoices in printer"
	! pr f "11,20,c 38,n": "Enter 1 to pr selected invoices,"
	! pr f "12,20,c 38,n": "   Or 0 to pr all invoices entered."
	! L2590: input fields "13,26,N 1,UE,N": select_invoices_to_print conv L2590
	! if select_invoices_to_print=1 then goto SCR_SELECT_INVOICE
	! if select_invoices_to_print><0 then goto L2590
	! pr newpage
	! pr f "10,10,Cc 60": "Printing invoices in process"
	fnopenprn
	align=0
	restore #hTmpInvoice:
	do  ! for j=1 to lrec(hTmpInvoice)
		PR_SELECTED_INVOICE: !

		read #hTmpInvoice,using F_TMWK2: mat xinp,invoiceNumber$,mat cde$,mat id$,mat da,mat ct,mat sc,mat gl$ eof PRI_EOF
		if xinp(1)=0 then goto L2840
		k$=lpad$(str$(xinp(1)),5)
		read #1,using 'form pos 6,3*c 30',key=k$: mat billto$
		if ebilling and fnCustomerHasEbilling(client_id$) then
			! open pdf
			pdf_filename_final$=fnprint_file_name$(client_id$,'pdf')
			open #PdfOut:=fngethandle: 'Name=PDF:,PrintFile='&env$('at')&pdf_filename_final$&',Replace,RecL=5000',Display,Output
			! print pdf
			fnPrintInvoice(pdfout,align,client_id$, mat client_addr$,invoiceNumber$,inv_date,mat inv_item$,mat inv_amt,pbal,1)
			! close pdf 
			close #pdfout: 
			! move to Send folder 
			fnmakesurepathexists(fnreport_cache_folder_current$&"\Ebilling\")
			fnCopy(os_filename$(env$('at')&pdf_filename_final$),fnreport_cache_folder_current$&'\Ebilling\ACS Invoice.'&trim$(client_id$)&'.'&date$("mmddyy")&'.pdf')
		else
		   fnPrintInvoice(255,align, k$, mat billto$, invoiceNumber$, xinp(3),mat id$, mat da,0,0)
		end if 
		 L2840: !
		! if select_invoices_to_print=1 then goto SCR_SELECT_INVOICE
	loop  ! next j
	PRI_EOF: !
	fncloseprn
	L2870: !
goto SCR_FINAL ! /r
! SCR_SELECT_INVOICE: ! r:
! 	pr newpage
! 	pr f "10,10,c 60": "Ref # of invoice to print, 0 when finished"
! 	L2900: !
! 	input fields "10,70,N 5,UE,N": j conv L2900
! 	if j=0 then goto SCR_FINAL
! 	if j<1 or j>rw then goto SCR_SELECT_INVOICE
! goto PR_SELECTED_INVOICE ! /r
REPR_PREV_INV: ! r:
	mat pt=(0)
	for rw=1 to lrec(hTmpInvoice)
		read #hTmpInvoice,using F_TMWK2,rec=rw: mat xinp,invoiceNumber$,mat cde$,mat id$,mat da,mat ct,mat sc noRec L3050
		if xinp(1)=0 then goto L2840
		pt(1)=pt(1)+xinp(1)
		for j=1 to 10
			pt(2)=pt(2)+da(j)
			pt(3)=pt(3)+ct(j)
			pt(4)=pt(4)+sc(j)
		next j
		L3050: !
	next rw
return ! /r
XIT: fnxit
SRCH1: ! r: name search
	s1=1
	open #127: "SROW=1,SCOL=1,EROW=24,ECOL=80",display,outIn  ! SAVE SCREEN
	L3230: !
	pr #127: newpage
	close #101: ioerr ignore
	open #101: "SROW=6,SCOL=3,EROW=08,ECOL=78,BORDER=DR,CAPTION=BUsiness Name Search",display,outIn
	prtall=0
	pr f "7,4,C 55,H,N": "Enter beginning search info. or blank for all:"
	pr f "9,32,C 16,R,N": "Press F5 to stop"
	L3290: !
	input fields "7,50,C 25,UE,N": nam$
	if cmdkey=5 then goto SRCHEND
	nam$=rtrm$(nam$)
	l1=len(nam$)
	restore #32,search>=nam$: nokey L3290
	close #101: ioerr ignore
	L3350: !
	pr newpage
	pr f "1,10,C 5,R,N": "Acct#"
	pr f "1,17,C 30,R,N": "Company Name"
	cde=0
	for j=1 to 20
		read #32,using L3410,release: k$,a1$ eof L3510
		L3410: form pos 1,c 5,c 30
		if a1$(1:l1)=nam$ or prtall=1 then goto L3430 else goto L3510
		L3430: !
		cde=1
		pr f str$(j+1)&",10,C 5,ut,N": k$
		pr f str$(j+1)&",17,C 30,ut,N": a1$
		if j>1 then goto L3500
		bk=bk+1
		if bk>20 then bk=1
		bk$(bk)=a1$
		L3500: !
	next j
	L3510: !
	if j>1 then j=j-1
	mat in2$(j)
	pr f "24,08,C 60,R,N": "Enter to continue; F5 to stop or enter ACCOUNT #:"
	L3540: !
	input fields "24,58,C 14,RE,N": k$
	alp=0
	if cmdkey=5 then goto SRCHEND
	if rtrm$(k$)><"" then xinp(1)=val(k$) conv L3540 : goto SRCHEND
	if cmdkey><2 then goto L3630
	bk=bk-1
	if bk<1 then goto L3650
	restore #32,key>=bk$(bk): nokey L3650
	bk=bk-1
	L3630: !
	selclp=1
	goto L3350
	L3650: !
	selclp=0
	goto L3230
	SRCHEND: !
	close #101: ioerr ignore
	close #127: ioerr L3720
	if rtrm$(k$)="" then goto L3720
	if s1=1 then pr f io1$(1): xinp(1)
	if s1=2 then pr f io1$(ce): k$
	L3720: !
return  ! /r
HELP1: ! r:
	ce=curfld
	if ce=1 then gosub SRCH1 : goto CT1
	if ce>4 and ce<16 then gosub SRCH2 : goto CT1
	goto CT1
	SRCH2: !
	s1=2 ! CODE SEARCH
	open #127: "SROW=1,SCOL=1,EROW=24,ECOL=80",display,outIn
	L3990: !
	pr #127: newpage
	close #101: ioerr ignore
	open #101: "SROW=6,SCOL=3,EROW=08,ECOL=78,BORDER=DR,CAPTION=CODE SEARCH",display,outIn
	prtall=0
	pr f "7,4,C 55,H,N": "Enter beginning search info. or blank for all:"
	pr f "9,32,C 16,R,N": "Press F5 to stop"
	L4050: !
	input fields "7,50,C 6,UE,N": nam$
	if cmdkey=5 then goto SRCHEND
	nam$=lpad$(rtrm$(nam$),6)
	restore #3,search>=nam$: nokey L4050
	close #101: ioerr ignore
	L4100: !
	pr newpage
	pr f "1,2,C 6,ut,N": " Code"
	pr f "1,9,C 46,ut,N": "Description"
	pr f "1,56,C 10,ut,N": "  Amount"
	pr f "1,67,C 12,ut,N": "  GL Number"
	cde=0
	for j=1 to 20
		read #3,using 'form pos 1,c 6,c 55,pd 5.2,c 12',release: cdk$,des$,da,gl$ eof L4290
		cde=1
		pr f str$(j+1)&",2,C 6,ut,N": cdk$
		pr f str$(j+1)&",9,C 46,ut,N": des$(1:46)
		pr f str$(j+1)&",56,N 10.2,ut,N": da
		pr f str$(j+1)&",67,C 12,ut,N": gl$
		if j>1 then goto L4280
		bk=bk+1
		if bk>20 then bk=1
		bk$(bk)=cdk$
		L4280: !
	next j
	L4290: !
	if j>1 then j=j-1
	mat in2$(j)
	pr f "24,08,C 60,R,N": "Enter to continue; F5 to stop or enter ACCOUNT #:"
	L4320: !
	input fields "24,58,C 6,RE,N": k$
	alp=0
	if cmdkey=5 then goto SRCHEND
	if rtrm$(k$)><"" then xinp(1)=val(k$) conv L4320 : goto SRCHEND
	if cmdkey><2 then goto L4410
	bk=bk-1
	if bk<1 then goto L4430
	restore #32,key>=bk$(bk): nokey L4430
	bk-=1
	L4410: !
	selclp=1
	goto L4100
	L4430: !
	selclp=0
goto L3990 ! /r
include: ertn
