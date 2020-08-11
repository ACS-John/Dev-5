fn_setup
fn_printInvoice
end

def library fnInvoiceOpen
	if ~setup then fn_setup
	fnInvoiceOpen=fn_invoiceOpen
fnend
def fn_invoiceOpen
	! this function does not seem to be necessary, but we'll keep it in place, because i feel like it
fnend
def library fnInvoiceClose(invDate)
	if ~setup then fn_setup
	fnInvoiceClose=fn_invoiceClose(invDate)
fnend
def fn_invoiceClose(invDate; ___,invoiceFilenameBase$*64)
	close #hCollection:
	close #hPrintCollection:
	hCollection=hPrintCollection=0

	invoiceFilenameBase$='ACS Invoice '
	invoiceFilenameBase$&=date$(days(invDate,'mmddyy'),'ccyy-mm')
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
def fn_printInvoice(actNum$,mat billTo$,invNum$,invDate,mat desc$,mat amt,pbal; ___,isCss,totalAmt)

	! forcePrintAcePdf=0
	! disableRtf=1
	! r: set cnam$ and cLogo$
	if fnval(actNum$)=4132 then  ! Stern and Stern
		isCss=1
		dim cnam$*40
		dim cLogo$*128
		cnam$='Commercial Software Solutions LLC'
		cLogo$='S:\Time Management\resource\cssLogo.png'
	else
		cnam$='Advanced Computer Services LLC'
		cLogo$='S:\Core\Icon\bwLogo.jpg' ! 's:\acsTM\bwlogo2.jpg'
	end if
	! /r
	!	gosub LauraStyleInvoiceBody
	! LauraStyleInvoiceBody: ! r:
	customerHasEbilling=fnCustomerHasEbilling(actNum$)
	dim tmpFile$*16
	tmpFile$='tmp[session].pdf'
	open #out=fngethandle: 'Name=PDF:,PrintFile=[at]'&tmpFile$&',Replace,RecL=5000',Display,Output
	if ~hCollection then
		dim tmpCollectionFile$*32
		tmpCollectionFile$='tmpCollection[session].pdf'
		open #hCollection=fngethandle: 'Name=PDF:,PrintFile=[at]'&tmpCollectionFile$&',Replace,RecL=5000',Display,Output
		collectionPageCount=printCollectionPageCount=0

		dim tmpPrintCollectionFile$*64
		tmpPrintCollectionFile$='tmpPrintCollection[session].pdf'
		open #hPrintCollection=fngethandle: 'Name=PDF:,PrintFile=[at]'&tmpPrintCollectionFile$&',Replace,RecL=5000',Display,Output

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

def fn_lauraStyleInvoiceBody(out,cnam$*40,cLogo$*128,invNum$*12,actNum$,mat billTo$,pbal,mat desc$,mat amt; ___, totalAmt,pdfline$*151)
	
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
		pr #out: '[pos(+0,+7)]Previous Balance [pos(+0,+34)][right]'&cnvrt$('pic(zzz,zzz,zz#.##)',pbal)
		totalAmt+=pbal
	end if
	pr #out: ''
	pr #out: '[LEFT][bold][pos(+0,+47)] Total: [/bold][RIGHT][pos(+0,+6)]'&cnvrt$('pic($zzz,zzz,zz#.##)',totalAmt)
	pr #out: ''
	pr #out: '[pos(+0,-2)]'&pdfline$

fnend
include: fn_setup
