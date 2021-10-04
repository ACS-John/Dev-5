if env$('acsDeveloper')<>'' then
	! r: test zone
	fn_setup
	testActNum$='ajj'
	dim testBillTo$(3)*60
	testBillTo$(1)='American Jiu Jitsu of Maplewood'
	testBillTo$(2)='something Valley St'
	testBillTo$(3)='Maplewood, NJ number'
	testInvNum$='123456'
	testInvDate=010101
	dim testDesc$(2)*128,testAmt(2)
	testDesc$(1)='something number one' : testAmt(1)=100.00
	testDesc$(2)='something number two' : testAmt(2)=200.00
	testPbal=12345.67
	
	fn_printInvoice(testActNum$,mat testBillTo$,testInvNum$,testInvDate,mat testDesc$,mat testAmt,testPbal)
	fn_invoiceClose(invDate, 'test')
	
	pr 'NOTE: This test makes an email invoice which is not displayed. You can find it here:'
	pr 'D:\ACS\Dev-5_Data\ACS\Report Cache\Client Billing\Advanced Computer Services LLC (420)\Ebilling\'
	! /r
else
	pr 'program not indended to be run directly'
end if
end

def library fnInvoiceOpen
	if ~setup then fn_setup
	fnInvoiceOpen=fn_invoiceOpen
fnend
def fn_invoiceOpen
	! this function does not seem to be necessary, but we'll keep it in place, because i feel like it
fnend
def library fnInvoiceClose(invDate; filenameAddOn$*128)
	if ~setup then fn_setup
	fnInvoiceClose=fn_invoiceClose(invDate, filenameAddOn$)
fnend
def fn_invoiceClose(invDate; filenameAddOn$*128,___,invoiceFilenameBase$*64)
	close #hClient:
	close #hProvider:
	close #hCollection:
	close #hPrintCollection:
	hClient=hProvider=hCollection=hPrintCollection=0
	setup_printInvoice=0

	invoiceFilenameBase$='ACS Invoice '
	invoiceFilenameBase$&=date$(days(invDate,'mmddyy'),'ccyy-mm')
	if trim$(filenameAddOn$)<>'' then 
		invoiceFilenameBase$&=' - '&trim$(filenameAddOn$)
	end if
	invoiceFilenameBase$&='.pdf'
	
	fnCopy(tmpCollectionFile$,'[at]'&fnReportCacheFolderCurrent$&'\'&invoiceFilenameBase$)
	fnCopy(tmpCollectionFile$,'[at]'&fnReportCacheFolderCurrent$&'\Invoice\Archive\'&invoiceFilenameBase$)
	fnCopy(tmpPrintCollectionFile$,'[at]'&fnReportCacheFolderCurrent$&'\Invoice\Print\(print only) '&invoiceFilenameBase$)
	if env$('acsDeveloper')<>'' then ! ='John' then
		fnCopy(tmpCollectionFile$,'[at]D:\ACS\Doc\Invoices\'&invoiceFilenameBase$)
	end if
	collectionPageCount=printCollectionPageCount=0
fnend

def library fnInvoiceAdd(actNum$,mat billTo$,invNum$,invDate,mat desc$,mat amt,pbal)
	if ~setup then fn_setup
	fnInvoiceAdd=fn_printInvoice(actNum$,mat billTo$,invNum$,invDate,mat desc$,mat amt,pbal)
fnend
def fn_printInvoice(actNum$,mat billTo$,invNum$,invDate,mat desc$,mat amt,pbal; ___,totalAmt)
	if ~setup_printInvoice then
		setup_printInvoice=1
		dim c$(0)*256
		dim cN(0)
		hClient=fn_openFio('CO Client',mat c$,mat cN, 1)
		dim p$(0)*256
		dim pN(0)
		hProvider=fn_openFio('CO Provider',mat p$,mat pN, 1)
	end if
	! forcePrintAcePdf=0
	! disableRtf=1
	! r: set cnam$ and cLogo$
	actNum$=trim$(actNum$)
	
	read #hClient,using form$(hClient),key=rpad$(actNum$,kln(hClient)): mat c$,mat cN 
	read #hProvider,using form$(hProvider),key=c$(client_provider): mat p$,mat pN
	dim cnam$*128
	cnam$=rtrm$(p$(provider_name))
	dim cLogo$*128
	cLogo$=rtrm$(p$(provider_logo))
	c$(client_provider)=trim$(c$(client_provider))
	! if c$(client_provider)='css' then  ! Stern and Stern, Recoveries Unlimited and Peter Engler Designs
	! 	cnam$='Commercial Software Solutions LLC'
	! 	cLogo$='S:\Client Billing\resource\cssLogo.png'
	! else if c$(client_provider)='acs' then 
	! 	cnam$='Advanced Computer Services LLC'
	! 	cLogo$='S:\Core\Icon\bwLogo.jpg' ! 's:\Client Billing\Legacy\bwlogo2.jpg'
	! else if c$(client_provider)='jb' then 
	! 	cnam$='John Bowman'
	! 	cLogo$='S:\Core\Icon\John.png'
	! end if
	! /r
	!	gosub LauraStyleInvoiceBody
	! LauraStyleInvoiceBody: ! r:
	customerHasEbilling=fnCustomerHasEbilling(actNum$)
	dim tmpFile$*16
	tmpFile$='tmp[session].pdf'
	open #out=fnH: 'Name=PDF:,PrintFile=[at]'&tmpFile$&',Replace,RecL=5000',d,o
	if ~hCollection then
		dim tmpCollectionFile$*32
		tmpCollectionFile$='tmpCollection[session].pdf'
		open #hCollection=fnH: 'Name=PDF:,PrintFile=[at]'&tmpCollectionFile$&',Replace,RecL=5000',d,o
		collectionPageCount=printCollectionPageCount=0

		dim tmpPrintCollectionFile$*64
		tmpPrintCollectionFile$='tmpPrintCollection[session].pdf'
		open #hPrintCollection=fnH: 'Name=PDF:,PrintFile=[at]'&tmpPrintCollectionFile$&',Replace,RecL=5000',d,o

	end if

	! make the individual file
	fn_lauraStyleInvoiceBody(out,cnam$,cLogo$,invNum$,actNum$,mat billTo$,pbal,mat desc$,mat amt)

	! make the collection files
	if collectionPageCount then
		pr #hCollection: newpage
	end if
	if printCollectionPageCount and ~customerHasEbilling then
		pr #hPrintCollection: newpage
	end if

	! archive (gets everything)
	fn_lauraStyleInvoiceBody(hCollection,cnam$,cLogo$,invNum$,actNum$,mat billTo$,pbal,mat desc$,mat amt)
	collectionPageCount+=1

	! print collection (only stuff that needs to be printed this month)
	if ~customerHasEbilling then
		fn_lauraStyleInvoiceBody(hPrintCollection,cnam$,cLogo$,invNum$,actNum$,mat billTo$,pbal,mat desc$,mat amt)
		printCollectionPageCount+=1
	end if
	close #out:

	! r: copy created temp pdf to it's places
	dim invoiceFilenameBase$*64
	invoiceFilenameBase$='Invoice '
	invoiceFilenameBase$&=date$(days(invDate,'mmddyy'),'ccyy-mm')
	invoiceFilenameBase$&=' inv '&trim$(invNum$)
	invoiceFilenameBase$&=' act '&trim$(actNum$)
	invoiceFilenameBase$&='.pdf'

	if customerHasEbilling then
		fnCopy(tmpFile$,'[at]'&fnReportCacheFolderCurrent$&'\Ebilling\'&invoiceFilenameBase$)
	end if
	! /r
	! return ! /r

fnend

def fn_lauraStyleInvoiceBody(out,cnam$*128,cLogo$*128,invNum$*12,actNum$,mat billTo$,pbal,mat desc$,mat amt; ___, totalAmt,pdfline$*151)
	
	staticSize=0
	
	pdfline$="[pos(+0,+7)][SETSIZE(14)][FONT TIMES][Bold]"&rpt$('_',67)&"[/BOLD][SETSIZE(8)][SETFONT(Lucida Sans)]"

	! pr #out: '[BOLD][FONT TIMES][SETSIZE(8)][pos(+0,+6)][8LPI][LEFT]';
	pr #out: '[pos(+0,+62)][pic(1,1,'&cLogo$&')]'
	pr #out: '[FONT TIMES][SETSIZE(11)][pos(+0,+6)][6LPI][LEFT]'

	pr #out: ''
	pr #out: ''
	pr #out: '[pos(+0,+7)][BOLD]'&trim$(cnam$)&'[/BOLD]'
	pr #out: '[pos(+0,+7)]4 Syme Ave'
	pr #out: '[pos(+0,+7)]West Orange, NJ  07052'

	pr #out: ''
	pr #out: ''

	pr #out: '[LEFT][pos(+4,+7)][BOLD]'&trim$(billTo$(1))&'[/BOLD]'
	pr #out: '[pos(+0,+7)]'&trim$(billTo$(2))
	pr #out: '[pos(+0,+7)]'&trim$(billTo$(3))
	pr #out: '[8LPI]'
	pr #out: ''
	pr #out: '[SETSIZE(36)][BOLD][CENTER]'
	pr #out: '[pos(+0,+40)]Invoice'
	pr #out: '[SETSIZE(8)][/BOLD][LEFT] [SETFONT(Lucida Sans)]'
	pr #out: pdfline$
	pr #out: ''
	pr #out: '[RIGHT][pos(+0,+4)]                 Invoice Number:[LEFT]  [BOLD]'&trim$(invNum$)&'[/BOLD]'
	pr #out: '[RIGHT][pos(+0,+4)]                 Account Number:[LEFT]  [BOLD]'&trim$(actNum$)&'[/BOLD]'
	pr #out: '[RIGHT][pos(+0,+5)]                  Invoice Date:[LEFT]  [BOLD]'&cnvrt$('pic(##/##/##)',invDate)&'[/BOLD]'
	pr #out: ''
	pr #out: ''
	pr #out: '[pos(+0,+7)][SETSIZE(10)][Bold]Description [pos(+0,+50)]Amount[/BOLD]'
	pr #out: pdfline$
	pr #out: ''

	for j1=1 to udim(mat desc$)
		if amt(j1) then
			pr #out: '[pos(+0,+7)][PUSH][LEFT]'&desc$(j1)&'[POP][RIGHT][pos(+0,+55)]'&cnvrt$('pic(ZZZ,ZZ#.##)',amt(j1))
			totalAmt+=amt(j1)
		else if staticSize then
			pr #out: ''
		end if
	next j1
	if pbal then
		pr #out: '[pos(+0,+7)]Previous Balance [pos(+0,+34)][right]'&cnvrt$('pic(---,---,--#.##)',pbal)
		totalAmt+=pbal
	end if
	pr #out: ''
	pr #out: '[LEFT][bold][pos(+0,+47)] Total: [/bold][RIGHT][pos(+0,+6)]'&cnvrt$('pic($---,---,--#.##)',totalAmt)
	pr #out: ''
	pr #out: '[pos(+0,-2)]'&pdfline$

fnend
include: fn_open
include: fn_setup
