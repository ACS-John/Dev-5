! Replace S:\acsTM\Print_Invoice.br
! PROGRAMS THAT USE THIS LIBRARY:   C:\ACS\Dev-5\Time Management\ACS Invoices.br.brs   S:\Time Management\Enter and Print Invoices
def library fnprint_invoice(out,align, &actnum$, mat billto$, inv_num$, inv_date, mat desc$, mat amt,pbal,ebilling;pdfline$*255)
	autoLibrary
	if file(255)=0 then let fnopenprn
	if align<>0 and align<>1 then 
		pr #out: newpage
	end if 
	! pr #out,using "Form POS 12,C 50": "\ql {\f181 \b Advanced Computer Services LLC}"
	! pr #out,using "Form POS 12,C 50": "\ql {\f181 4 Syme Ave}"
	! pr #out,using "Form POS 12,C 50": "\ql {\f181 West Orange, NJ  07052}"
	! if ebilling=1 then  execute "CONFIG OPTION 31 OFF" 
	! execute "config option 32 ON"
	! if ebilling=1 then pr #out: ""
	if ebilling=0 then pr #out,using "Form POS 1,C 50": "\ql {\f181 \b Advanced Computer Services LLC}" else pr #out,using "Form POS 1,c 100": "[BOLD][FONT TIMES][SETSIZE(8)][pos(+0,+6)][8LPI]     Advanced Computer Services LLC [/BOLD]" ! 
	if ebilling=0 then pr #out,using "Form POS 1,C 50": "\ql {\f181 4 Syme Ave}" else pr #out,using "Form POS 27,c 100" : "4 Syme Ave"
	if ebilling=0 then pr #out,using "Form POS 1,C 50": "\ql {\f181 West Orange, NJ  07052}" else pr #out,using "Form POS 27,c 100" : "West Orange, NJ  07052" 
	! pr #out: "*INSERT FILE:S:\acsTM\acs_logo.rtf"
	! pause 
	if ebilling=0 then pr #out: "*INSERT FILE:S:\Time Management\ACS_Logo2.rtf" else pr #out: "[pos(+0,+67)][pic(.5,.5,s:\acsTM\bwlogo2.jpg)]"   ! "[PIC(1,1,S:\Time Management\ACS_Logo2.rtf)]"
	pr #out: ''
	pr #out: ''
	pr #out: ''
	pr #out: ''
	! pr #out,using "Form POS 12,C 50": "\ql {\f181 \b "&billto$(1)&"}"
	! pr #out,using "Form POS 12,C 50": "\ql {\f181 "&billto$(2)&"}"
	! pr #out,using "Form POS 12,C 50": "\ql {\f181 "&billto$(3)&"}"
	if ebilling=0 then pr #out,using "Form POS 1,C 50": "\ql {\f181 \b "&billto$(1)&"}" else pr #out: "[LEFT][pos(+4,+7)][BOLD]"&trim$(billto$(1))&"[/BOLD]"
	if ebilling=0 then pr #out,using "Form POS 1,C 50": "\ql {\f181 "&billto$(2)&"}" else pr #out: "[pos(+0,+7)]"&trim$(billto$(2)) ! 
	if ebilling=0 then pr #out,using "Form POS 1,C 50": "\ql {\f181 "&billto$(3)&"}" else pr #out: "[pos(+0,+7)]"&trim$(billto$(3)) ! 
	pr #out: ""
	pr #out: ""
	let pdfline$="[pos(+0,+7)][SETSIZE(14)][FONT TIMES][Bold]"&lpad$("_",67,"_")&"[/BOLD][SETSIZE(8)][SETFONT(Lucida Sans)]"
	if ebilling=1 then pr #out: "[SETSIZE(36)][BOLD][CENTER]"
	if ebilling=0 then pr #out: "\qc {\f181 \fs72 \b Invoice}" else pr #out: "[pos(+0,+40)]Invoice"
	if ebilling=0 then pr #out: "" else print #out: "[SETSIZE(8)][/BOLD][LEFT] [SETFONT(Lucida Sans)]"
	if ebilling=0 then pr #out: "*INSERT FILE:S:\acsTM\black line - six inch.rtf.txt" else pr #out: pdfline$ : print #out: ""
	if ebilling=0 then pr #out: "\ql             Invoice Number:  {\b "&trim$(inv_num$)&"}" else pr #out: "[RIGHT][pos(+0,+4)]                 Invoice Number:[LEFT]  [BOLD]"&trim$(inv_num$)&"[/BOLD]"
	if ebilling=0 then pr #out: "\ql             Account Number:  {\b "&trim$(actnum$)&"}" else pr #out: "[RIGHT][pos(+0,+4)]                 Account Number:[LEFT]  [BOLD]"&trim$(actnum$)&"[/BOLD]"
	if ebilling=0 then pr #out: "\ql               Invoice Date:  {\b "&cnvrt$("pic(##/##/##)",inv_date)&"}" else pr #out: "[RIGHT][pos(+0,+5)]                  Invoice Date:[LEFT]  [BOLD]"&cnvrt$("pic(##/##/##)",inv_date)&"[/BOLD]"
	pr #out: ""
	total_amt=0
	pr #out: ""
	if ebilling=0 then pr #out,using "Form pos 1,C 73,C 12": "\qc {\b Description","Amount}" else pr #out: "[pos(+0,+7)][SETSIZE(10)][Bold]Description [pos(+0,+50)]Amount[/BOLD]"
	if ebilling=0 then pr #out: "*INSERT FILE:S:\acsTM\black line - six inch.rtf.txt" else pr #out: pdfline$
	if ebilling=1 then pr #out: ""
	for j1=1 to udim(mat desc$)
		if ebilling=0 then 
			pr #out,using "Form POS 1,C 55,X 3,PIC(---,---,---.--)": desc$(j1),amt(j1) 
		else 
			if amt(j1)<>0 then pr #out: "[pos(+0,+7)][PUSH][LEFT]"&desc$(j1)&"[POP][RIGHT][pos(+0,+55)]"&cnvrt$("pic(ZZZ,ZZ#.##)",amt(j1)) else pr #out: ""
		end if 
		total_amt=total_amt+amt(j1)
	next j1
	if pbal=0 then goto P_TOTAL
	if ebilling=0 then pr #out,using "Form POS 1,C 55,X 3,PIC(---,---,---.--)": "Previous Balance",pbal else pr #out: "[pos(+0,+7)]Previous Balance [pos(+0,+34)][right]"&cnvrt$("pic(zzz,zzz,zz#.##)",pbal)
	total_amt=total_amt+pbal
	P_TOTAL: !
	if ebilling=0 then pr #out,using "Form POS 59,C 28": "{\strike             }" else pr #out: "" ! using "Form POS 1,c 100" : "[PIC(1,1,S:\acsTM\black line - six inch.rtf.txt)]"
	if ebilling=0 then pr #out,using "Form POS 51,Cr 13,PIC($-,---,---.##)": "{\b Total:}",total_amt else pr #out: "[LEFT][bold][pos(+0,+47)] Total: [/bold][RIGHT][pos(+0,+6)]"&cnvrt$("pic($zzz,zzz,zz#.##)",total_amt)
	if ebilling=0 then pr #out,using "Form POS 59,C 28": "{\ul \strike "&rpt$(" ",12)&"}" else pr #out: "" ! ,using "Form POS 1,c 100" : "[PIC(1,1,S:\acsTM\black line - six inch.rtf.txt)]"
	if ebilling=0 then pr #out: "*INSERT FILE:S:\acsTM\black line - six inch.rtf.txt" else pr #out: "[pos(+0,-2)]"&pdfline$
	! if ebilling=1 then pr #out: ""
	total_amt=0
fnend 
