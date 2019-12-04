def library fnPrintInvoice(out,align, &actnum$, mat billto$, inv_num$, inv_date, mat desc$, mat amt,pbal,ebilling; pdfline$*255,___,isCss)
	library 'S:\Core\Library': fnopenprn
	if file(255)=0 then let fnopenprn
	if align<>0 and align<>1 then
		pr #out: newpage
	end if
	if ebilling then
		library 'S:\Core\Library': fnval
		if fnval(actnum$)=4132 then ! Stern and Stern
			! pause
			isCss=1
		end if
		! execute "CONFIG OPTION 31 OFF"
		! pr #out: ""
		if isCss then
			pr #out,using "Form POS 1,c 100": "[BOLD][FONT TIMES][SETSIZE(8)][pos(+0,+6)][8LPI]     Commercial Software Solutions LLC [/BOLD]"
			pr #out,using "Form POS 27,c 100" : "4 Syme Ave"
			pr #out,using "Form POS 27,c 100" : "West Orange, NJ  07052"
			pr #out: "[pos(+0,+67)][pic(.5,.5,S:\Time Management\resource\cssLogo.png)]"   ! "[PIC(1,1,S:\Time Management\ACS_Logo2.rtf)]"
		else
			pr #out,using "Form POS 1,c 100": "[BOLD][FONT TIMES][SETSIZE(8)][pos(+0,+6)][8LPI]     Advanced Computer Services LLC [/BOLD]"
			pr #out,using "Form POS 27,c 100" : "4 Syme Ave"
			pr #out,using "Form POS 27,c 100" : "West Orange, NJ  07052"
			pr #out: "[pos(+0,+67)][pic(.5,.5,s:\acsTM\bwlogo2.jpg)]"   ! "[PIC(1,1,S:\Time Management\ACS_Logo2.rtf)]"
		end if
	else
		if isCss then
			pr #out,using "Form POS 1,C 50": "\ql {\f181 \b Commercial Software Solutions LLC}"
		else
			pr #out,using "Form POS 1,C 50": "\ql {\f181 \b Advanced Computer Services LLC}"
		end if
		! pr #out,using "Form POS 12,C 50": "\ql {\f181 \b Advanced Computer Services LLC}"
		pr #out,using "Form POS 12,C 50": "\ql {\f181 4 Syme Ave}"
		pr #out,using "Form POS 12,C 50": "\ql {\f181 West Orange, NJ  07052}"
		! execute "config option 32 ON"
		! pr #out: "*INSERT FILE:S:\acsTM\acs_logo.rtf"
		pr #out: "*INSERT FILE:S:\Time Management\ACS_Logo2.rtf"
	end if
	pr #out: ''
	pr #out: ''
	pr #out: ''
	pr #out: ''
	! pr #out,using "Form POS 12,C 50": "\ql {\f181 \b "&billto$(1)&"}"
	! pr #out,using "Form POS 12,C 50": "\ql {\f181 "&billto$(2)&"}"
	! pr #out,using "Form POS 12,C 50": "\ql {\f181 "&billto$(3)&"}"
	if ebilling then
		pr #out: "[LEFT][pos(+4,+7)][BOLD]"&trim$(billto$(1))&"[/BOLD]"
		pr #out: "[pos(+0,+7)]"&trim$(billto$(2))
		pr #out: "[pos(+0,+7)]"&trim$(billto$(3))
		pr #out: ""
		pr #out: ""
	else
		pr #out,using "Form POS 1,C 50": "\ql {\f181 \b "&billto$(1)&"}"
		pr #out,using "Form POS 1,C 50": "\ql {\f181 "&billto$(2)&"}"
		pr #out,using "Form POS 1,C 50": "\ql {\f181 "&billto$(3)&"}"
	end if
	pdfline$="[pos(+0,+7)][SETSIZE(14)][FONT TIMES][Bold]"&lpad$("_",67,"_")&"[/BOLD][SETSIZE(8)][SETFONT(Lucida Sans)]"

	if ebilling then
		pr #out: "[SETSIZE(36)][BOLD][CENTER]"
		pr #out: "[pos(+0,+40)]Invoice"
		pr #out: "[SETSIZE(8)][/BOLD][LEFT] [SETFONT(Lucida Sans)]"
		pr #out: pdfline$
		pr #out: ""
		pr #out: "[RIGHT][pos(+0,+4)]                 Invoice Number:[LEFT]  [BOLD]"&trim$(inv_num$)&"[/BOLD]"
		pr #out: "[RIGHT][pos(+0,+4)]                 Account Number:[LEFT]  [BOLD]"&trim$(actnum$)&"[/BOLD]"
		pr #out: "[RIGHT][pos(+0,+5)]                  Invoice Date:[LEFT]  [BOLD]"&cnvrt$("pic(##/##/##)",inv_date)&"[/BOLD]"
	else
		pr #out: "\qc {\f181 \fs72 \b Invoice}"
		pr #out: ""
		pr #out: "*INSERT FILE:S:\acsTM\black line - six inch.rtf.txt"
		pr #out: "\ql             Invoice Number:  {\b "&trim$(inv_num$)&"}"
		pr #out: "\ql             Account Number:  {\b "&trim$(actnum$)&"}"
		pr #out: "\ql               Invoice Date:  {\b "&cnvrt$("pic(##/##/##)",inv_date)&"}"
	end if
	pr #out: ""
	total_amt=0
	pr #out: ""
	if ebilling then
		pr #out: "[pos(+0,+7)][SETSIZE(10)][Bold]Description [pos(+0,+50)]Amount[/BOLD]"
		pr #out: pdfline$
		pr #out: ""
	else
		pr #out,using "Form pos 1,C 73,C 12": "\qc {\b Description","Amount}"
		pr #out: "*INSERT FILE:S:\acsTM\black line - six inch.rtf.txt"
	end if
	for j1=1 to udim(mat desc$)
		if amt(j1) then
			if ebilling then
				pr #out: "[pos(+0,+7)][PUSH][LEFT]"&desc$(j1)&"[POP][RIGHT][pos(+0,+55)]"&cnvrt$("pic(ZZZ,ZZ#.##)",amt(j1))
			else
				pr #out,using "Form POS 1,C 58,PIC(---,---,---.--)": rtrm$(desc$(j1)),amt(j1)
			end if
			total_amt+=amt(j1)
		else
			pr #out: ""
		end if
	next j1
	if pbal then
		if ebilling then
			pr #out: "[pos(+0,+7)]Previous Balance [pos(+0,+34)][right]"&cnvrt$("pic(zzz,zzz,zz#.##)",pbal)
		else
			pr #out,using "Form POS 1,C 55,X 3,PIC(---,---,---.--)": "Previous Balance",pbal
		end if
		total_amt+=pbal
	end if
	if ebilling then
		pr #out: "" ! using "Form POS 1,c 100" : "[PIC(1,1,S:\acsTM\black line - six inch.rtf.txt)]"
		pr #out: "[LEFT][bold][pos(+0,+47)] Total: [/bold][RIGHT][pos(+0,+6)]"&cnvrt$("pic($zzz,zzz,zz#.##)",total_amt)
		pr #out: "" ! ,using "Form POS 1,c 100" : "[PIC(1,1,S:\acsTM\black line - six inch.rtf.txt)]"
		pr #out: "[pos(+0,-2)]"&pdfline$
		! pr #out: ""
	else
		pr #out,using "Form POS 59,C 28": "{\strike             }"
		pr #out,using "Form POS 51,Cr 13,PIC($-,---,---.##)": "{\b Total:}",total_amt
		pr #out,using "Form POS 59,C 28": "{\ul \strike "&rpt$(" ",12)&"}"
		pr #out: "*INSERT FILE:S:\acsTM\black line - six inch.rtf.txt"
	end if
	total_amt=0
fnend
