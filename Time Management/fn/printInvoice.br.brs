fn_setup
fn_printInvoice
end
def library fnPrintInvoice(align,&actnum$,mat billto$,inv_num$,inv_date,mat desc$,mat amt,pbal)
	if ~setup then fn_setup
	fnPrintInvoice=fn_printInvoice(align,actnum$,mat billto$,inv_num$,inv_date,mat desc$,mat amt,pbal)
fnend
def fn_printInvoice(align,&actnum$,mat billto$,inv_num$,inv_date,mat desc$,mat amt,pbal; ___,isCss,total_amt)

	dim pdfFileName$*512

	forcePrintAcePdf=0
	disableRtf=1
	! r: set cnam$ and cLogo$
	if fnval(actnum$)=4132 then  ! Stern and Stern
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

	if fnCustomerHasEbilling(actnum$) then
		! r: create ebill

		if forcePrintAcePdf then ! r: incomplete.
			out=fnpa_open( 'Portrait',' - inv no '&inv_num$&' - acct '&actnum$,'PDF')
			! pr 'just after fnpa_open' : pause
			lh=7 ! rough line height
			lc=20 ! lineCount (X corrdionates in milimeters)
			ml=30 !  margin left
			fnpa_fontbold(1) : fnpa_font('MS Sans Serif')
			fnpa_fontSize(14) : lh=6
			fnpa_txt(cnam$                               ,ml,lc+=lh)
			fnpa_fontSize(12)
			fnpa_txt('4 Syme Ave'                        ,ml,lc+=lh)
			fnpa_txt('West Orange, NJ  07052'            ,ml,lc+=lh)
			fnpa_pic(cLogo$,160,20,30,30) ! ; imgWidth,imgHeight,style$)
			fnpa_fontSize(10) : lh=5
			lc+=lh
			fnpa_fontbold(1)
			fnpa_txt(trim$(billto$(1)),ml,lc+=lh)
			fnpa_fontbold(0)
			fnpa_txt(trim$(billto$(2)),ml,lc+=lh)
			fnpa_txt(trim$(billto$(3)),ml,lc+=lh)
			fnpa_fontSize(20) : 	fnpa_fontbold(1)
			fnpa_txt('Invoice',25,lc+=lh)
			! pr #out: "[pos(+0,+40)]Invoice"
			lh=30
			fnpa_txt('______________________________________________________',ml,lc+=lh)
			fnpa_txt('______________________________________________________',ml,lc+=lh)

			fnpa_fontbold			:		fnpa_fontSize(10)
			fnpa_font('Lucida Sans')
			! fnpa_line(lc+=lh,25,25, 1,1)
			! fnpa_line(lc+=lh,25,25, 1,1)
			! fnpa_line(lc+=lh,25,25, 1,1)
			! fnpa_line(lc+=lh,25,25, 1,1)
			! pr #out: pdfline$
			lc+=20
			!			fnpa_txt('Invoice Number:',
			!			pr #out: "[RIGHT][pos(+0,+4)]                 Invoice Number:[LEFT]  [BOLD]"&trim$(inv_num$)&"[/BOLD]"
			!			pr #out: "[RIGHT][pos(+0,+4)]                 Account Number:[LEFT]  [BOLD]"&trim$(actnum$)&"[/BOLD]"
			!			pr #out: "[RIGHT][pos(+0,+5)]                  Invoice Date:[LEFT]  [BOLD]"&cnvrt$("pic(##/##/##)",inv_date)&"[/BOLD]"
			!			lc+=20
			!			lc+=20
			!			pr #out: "[pos(+0,+7)][SETSIZE(10)][Bold]Description [pos(+0,+50)]Amount[/BOLD]"
			!			pr #out: pdfline$
			!			lc+=20
			!
			!			for j1=1 to udim(mat desc$)
			!				if amt(j1) then
			!					pr #out: "[pos(+0,+7)][PUSH][LEFT]"&desc$(j1)&"[POP][RIGHT][pos(+0,+55)]"&cnvrt$("pic(ZZZ,ZZ#.##)",amt(j1))
			!					total_amt+=amt(j1)
			!				else
			!					pr #out: ''
			!				end if
			!			next j1
			!			if pbal then
			!				pr #out: "[pos(+0,+7)]Previous Balance [pos(+0,+34)][right]"&cnvrt$("pic(zzz,zzz,zz#.##)",pbal)
			!				total_amt+=pbal
			!			end if
			!			pr #out: '' ! using "Form POS 1,c 100" : "[PIC(1,1,S:\acsTM\black line - six inch.rtf.txt)]"
			!			pr #out: "[LEFT][bold][pos(+0,+47)] Total: [/bold][RIGHT][pos(+0,+6)]"&cnvrt$("pic($zzz,zzz,zz#.##)",total_amt)
			!			pr #out: '' ! ,using "Form POS 1,c 100" : "[PIC(1,1,S:\acsTM\black line - six inch.rtf.txt)]"
			!			pr #out: "[pos(+0,-2)]"&pdfline$
			!			! pr #out: ''
			dim tmpFilename$*2048
			tmpFilename$=fnpa_filename$
			pr 'tmpFilename$=';tmpFilename$ : pause
			fnpa_finis
			fnCopy(tmpFilename$,env$('at')&pdfFileName$)

			! /r
		else
			gosub LauraStyleInvoiceBody
		end if
		fnmakesurepathexists(fnReportCacheFolderCurrent$&"\Ebilling\")
		fnCopy('[at]'&os_filename$(pdfFileName$),'[at]'&fnReportCacheFolderCurrent$&"\Ebilling\ACS Invoice."&trim$(client_id$)&'.'&date$("mmddyy")&'.pdf')
		exec 'sy -c "'&fnReportCacheFolderCurrent$&'\Ebilling\ACS Invoice.'&trim$(client_id$)&'.'&date$("mmddyy")&'.pdf"'
		execute 'sy -c -w explorer "'&fnReportCacheFolderCurrent$&'\Ebilling"'
		pause
		! /r
	else
		if disableRtf then
			gosub LauraStyleInvoiceBody
		else
			if file(255)=0 then
				fnopenprn
			end if
			if align<>0 and align<>1 then
				pr #out: newpage
			end if
			! r: create regular RTF invoice
			pr #out: "*INSERT FILE:S:\Time Management\ACS_Logo2.rtf" ! "*INSERT FILE:S:\Time Management\ACS_Logo3.rtf" ! "*INSERT FILE:S:\Time Management\ACS_Logo2.rtf" ! "*INSERT FILE:S:\acsTM\acs_logo.rtf"
			pr #out: ''
			pr #out: ''
			pr #out: ''
			pr #out: '\ql {\f181 \b                '&cnam$&'}'
			pr #out: "\ql {\f181                4 Syme Ave}"
			pr #out: "\ql {\f181                West Orange, NJ  07052}"
			! execute "config option 32 ON" ! Supress notification of error 6245, which indicates an invalid or unsupported (by BR) escape sequence has been printed during Native Windows Printing.
	
			pr #out: ''
			pr #out: ''
			pr #out: "\ql {\f181 \b                "&billto$(1)&"}"
			pr #out: "\ql {\f181                "&billto$(2)&"}"
			pr #out: "\ql {\f181                "&billto$(3)&"}"
			pr #out: "\qc {\f181 \fs72 \b Invoice}"
			pr #out: ''
			pr #out: "*INSERT FILE:S:\acsTM\black line - six inch.rtf.txt"
			pr #out: "\ql             Invoice Number:  {\b "&trim$(inv_num$)&"}"
			pr #out: "\ql             Account Number:  {\b "&trim$(actnum$)&"}"
			pr #out: "\ql               Invoice Date:  {\b "&cnvrt$("pic(##/##/##)",inv_date)&"}"
			pr #out: ''
			pr #out: ''
			pr #out,using "Form pos 1,C 73,C 12": "\qc {\b Description","Amount}"
			pr #out: "*INSERT FILE:S:\acsTM\black line - six inch.rtf.txt"
			for j1=1 to udim(mat desc$)
				if amt(j1) then
					pr #out,using "Form POS 1,C 58,PIC(---,---,---.--)": rtrm$(desc$(j1))(1:58),amt(j1)
					total_amt+=amt(j1)
				else
					pr #out: ''
				end if
			next j1
	
			if pbal then
				pr #out,using "Form POS 1,C 55,X 3,PIC(---,---,---.--)": "Previous Balance",pbal
				total_amt+=pbal
			end if
			pr #out,using "Form POS 59,C 28": "{\strike             }"
			pr #out,using "Form POS 51,Cr 13,PIC($-,---,---.##)": "{\b Total:}",total_amt
			pr #out,using "Form POS 59,C 28": "{\ul \strike "&rpt$(" ",12)&"}"
			pr #out: "*INSERT FILE:S:\acsTM\black line - six inch.rtf.txt"
			! /r
		end if
	end if
fnend
LauraStyleInvoiceBody: ! r:
	! pdfFileName$=fnPrintFileName$( trim$(inv_num$)&' - '&trim$(actnum$),'pdf','Invoice\')
	! pr '1=';pdfFileName$
	pdfFileName$=fnReportCacheFolderCurrent$&'\Invoice\'&trim$(inv_num$)&' - '&trim$(actnum$)&'.pdf'
	! pr '2=';pdfFileName$
	! pause

	pr 'pdfFileName$="'&pdfFileName$&'"'
			open #out=fngethandle: 'Name=PDF:,PrintFile=[at]'&pdfFileName$&',Replace,RecL=5000',Display,Output
			fn_lauraStyleInvoiceBody(out,cnam$,cLogo$,inv_num$,actnum$,mat billto$,pbal,mat desc$,mat amt)
			close #out:
return ! /r
def fn_lauraStyleInvoiceBody(out,cnam$*40,cLogo$*128,inv_num$*12,actnum$,mat billto$,pbal,mat desc$,mat amt; ___, total_amt,pdfline$*151)

	pdfline$="[pos(+0,+7)][SETSIZE(14)][FONT TIMES][Bold]"&rpt$('_',67)&"[/BOLD][SETSIZE(8)][SETFONT(Lucida Sans)]"

	pr #out: '[BOLD][FONT TIMES][SETSIZE(8)][pos(+0,+6)][8LPI]';
	pr #out: '     '&cnam$;
	pr #out: '[/BOLD]'
	pr #out,using 'form pos 27,C': '4 Syme Ave'
	pr #out,using 'form pos 27,C': 'West Orange, NJ  07052'
	pr #out: '[pos(+0,+62)][pic(1,1,'&cLogo$&')]'
	! pr #out: '[pos(+0,+67)][pic(.5,.5,'&cLogo$&')]'   ! "[PIC(1,1,S:\Time Management\ACS_Logo2.rtf)]"
	pr #out: ''
	pr #out: ''
	pr #out: ''
	pr #out: ''

	pr #out: "[LEFT][pos(+4,+7)][BOLD]"&trim$(billto$(1))&"[/BOLD]"
	pr #out: "[pos(+0,+7)]"&trim$(billto$(2))
	pr #out: "[pos(+0,+7)]"&trim$(billto$(3))
	pr #out: ''
	pr #out: ''
	pr #out: "[SETSIZE(36)][BOLD][CENTER]"
	pr #out: "[pos(+0,+40)]Invoice"
	pr #out: "[SETSIZE(8)][/BOLD][LEFT] [SETFONT(Lucida Sans)]"
	pr #out: pdfline$
	pr #out: ''
	pr #out: "[RIGHT][pos(+0,+4)]                 Invoice Number:[LEFT]  [BOLD]"&trim$(inv_num$)&"[/BOLD]"
	pr #out: "[RIGHT][pos(+0,+4)]                 Account Number:[LEFT]  [BOLD]"&trim$(actnum$)&"[/BOLD]"
	pr #out: "[RIGHT][pos(+0,+5)]                  Invoice Date:[LEFT]  [BOLD]"&cnvrt$("pic(##/##/##)",inv_date)&"[/BOLD]"
	pr #out: ''
	pr #out: ''
	pr #out: "[pos(+0,+7)][SETSIZE(10)][Bold]Description [pos(+0,+50)]Amount[/BOLD]"
	pr #out: pdfline$
	pr #out: ''

	for j1=1 to udim(mat desc$)
		if amt(j1) then
			pr #out: "[pos(+0,+7)][PUSH][LEFT]"&desc$(j1)&"[POP][RIGHT][pos(+0,+55)]"&cnvrt$("pic(ZZZ,ZZ#.##)",amt(j1))
			total_amt+=amt(j1)
		else
			pr #out: ''
		end if
	next j1
	if pbal then
		pr #out: "[pos(+0,+7)]Previous Balance [pos(+0,+34)][right]"&cnvrt$("pic(zzz,zzz,zz#.##)",pbal)
		total_amt+=pbal
	end if
	pr #out: '' ! using "Form POS 1,c 100" : "[PIC(1,1,S:\acsTM\black line - six inch.rtf.txt)]"
	pr #out: "[LEFT][bold][pos(+0,+47)] Total: [/bold][RIGHT][pos(+0,+6)]"&cnvrt$("pic($zzz,zzz,zz#.##)",total_amt)
	pr #out: '' ! ,using "Form POS 1,c 100" : "[PIC(1,1,S:\acsTM\black line - six inch.rtf.txt)]"
	pr #out: "[pos(+0,-2)]"&pdfline$

fnend
include: fn_setup
