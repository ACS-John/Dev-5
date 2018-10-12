! Replace S:\acsTM\Print_Invoice.br
! PROGRAMS THAT USE THIS LIBRARY:   C:\ACS\Dev-5\Time Management\ACS Invoices.br.brs   S:\Time Management\Enter and Print Invoices
def library fnprint_invoice(align, &actnum$, mat billto$, inv_num$, inv_date, mat desc$, mat amt,pbal)
	library 'S:\Core\Library': fnopenprn
	if file(255)=0 then let fnopenprn
	if align<>0 and align<>1 then 
		pr #255: newpage
	end if 
	! pr #255,using "Form POS 12,C 50": "\ql {\f181 \b Advanced Computer Services LLC}"
	! pr #255,using "Form POS 12,C 50": "\ql {\f181 4 Syme Ave}"
	! pr #255,using "Form POS 12,C 50": "\ql {\f181 West Orange, NJ  07052}"
	pr #255,using "Form POS 1,C 50": "\ql {\f181 \b Advanced Computer Services LLC}"
	pr #255,using "Form POS 1,C 50": "\ql {\f181 4 Syme Ave}"
	pr #255,using "Form POS 1,C 50": "\ql {\f181 West Orange, NJ  07052}"
	! pr #255: "*INSERT FILE:S:\acsTM\acs_logo.rtf"
	pr #255: "*INSERT FILE:S:\Time Management\ACS_Logo2.rtf"
	pr #255: ""
	pr #255: ''
	pr #255: ''
	pr #255: ''
	! pr #255,using "Form POS 12,C 50": "\ql {\f181 \b "&billto$(1)&"}"
	! pr #255,using "Form POS 12,C 50": "\ql {\f181 "&billto$(2)&"}"
	! pr #255,using "Form POS 12,C 50": "\ql {\f181 "&billto$(3)&"}"
	pr #255,using "Form POS 1,C 50": "\ql {\f181 \b "&billto$(1)&"}"
	pr #255,using "Form POS 1,C 50": "\ql {\f181 "&billto$(2)&"}"
	pr #255,using "Form POS 1,C 50": "\ql {\f181 "&billto$(3)&"}"
	pr #255: ""
	pr #255: ""
	pr #255: "\qc {\f181 \fs72 \b Invoice}"
	pr #255: ""
	pr #255: "*INSERT FILE:S:\acsTM\black line - six inch.rtf.txt"
	pr #255: "\ql             Invoice Number:  {\b "&trim$(inv_num$)&"}"
	pr #255: "\ql             Account Number:  {\b "&trim$(actnum$)&"}"
	pr #255: "\ql               Invoice Date:  {\b "&cnvrt$("pic(##/##/##)",inv_date)&"}"
	pr #255: ""
	total_amt=0
	pr #255: ""
	pr #255,using "Form pos 1,C 73,C 12": "\qc {\b Description","Amount}"
	pr #255: "*INSERT FILE:S:\acsTM\black line - six inch.rtf.txt"
	for j1=1 to udim(mat desc$)
		pr #255,using "Form POS 1,C 55,X 3,PIC(---,---,---.--)": desc$(j1),amt(j1)
		total_amt=total_amt+amt(j1)
	next j1
	if pbal=0 then goto P_TOTAL
	pr #255,using "Form POS 1,C 55,X 3,PIC(---,---,---.--)": "Previous Balance",pbal
	total_amt=total_amt+pbal
	P_TOTAL: !
	pr #255,using "Form POS 59,C 28": "{\strike             }"
	pr #255,using "Form POS 51,Cr 13,PIC($-,---,---.##)": "{\b Total:}",total_amt
	pr #255,using "Form POS 59,C 28": "{\ul \strike "&rpt$(" ",12)&"}"
	pr #255: "*INSERT FILE:S:\acsTM\black line - six inch.rtf.txt"
	total_amt=0
fnend 
