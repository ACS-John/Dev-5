10000 ! Replace S:\acsTM\Print_Invoice.br
10100 ! PROGRAMS THAT USE THIS LIBRARY:   S:\acsTM\MaintInv   S:\acsTM\tmPrtInv
10200   def library fnprint_invoice(align, &actnum$, mat billto$, inv_num$, inv_date, mat desc$, mat amt,pbal)
10300 ! ______________________________________________________________________
10400     library 'S:\Core\Library': fnopenprn
10500 ! Dim inv_num$*12
10600 ! Dim amt(10)
10700 ! Dim desc$(10)*55
10800 ! Dim billto$(3)*30
10900     if file(255)=0 then let fnopenprn
11000     if align<>0 and align<>1 then 
11100       print #255: newpage
11200     end if 
11300     print #255,using "Form POS 12,C 50": "\ql {\f181 \b Advanced Computer Services LLC}"
11400     print #255,using "Form POS 12,C 50": "\ql {\f181 4 Syme Ave}"
11500     print #255,using "Form POS 12,C 50": "\ql {\f181 West Orange, NJ  07052}"
11600     print #255: "*INSERT FILE:S:\acsTM\acs_logo.rtf"
11700     print #255: ""
11800     print #255,using "Form POS 12,C 50": "\ql {\f181 \b "&billto$(1)&"}"
11900     print #255,using "Form POS 12,C 50": "\ql {\f181 "&billto$(2)&"}"
12000     print #255,using "Form POS 12,C 50": "\ql {\f181 "&billto$(3)&"}"
12100     print #255: ""
12200     print #255: ""
12300     print #255: "\qc {\f181 \fs72 \b Invoice}"
12400     print #255: ""
12500     print #255: "*INSERT FILE:S:\acsTM\black line - six inch.rtf.txt"
12600     print #255: "\ql             Invoice Number:  {\b "&trim$(inv_num$)&"}"
12700     print #255: "\ql             Account Number:  {\b "&trim$(actnum$)&"}"
12800     print #255: "\ql               Invoice Date:  {\b "&cnvrt$("pic(##/##/##)",inv_date)&"}"
12900     print #255: ""
13000     let total_amt=0
13100     print #255: ""
13200     print #255,using "Form pos 1,C 73,C 12": "\qc {\b Description","Amount}"
13300     print #255: "*INSERT FILE:S:\acsTM\black line - six inch.rtf.txt"
13400     for j1=1 to udim(mat desc$)
13500       print #255,using "Form POS 1,C 55,X 3,PIC(---,---,---.--)": desc$(j1),amt(j1)
13600       let total_amt=total_amt+amt(j1)
13700     next j1
13800     if pbal=0 then goto P_TOTAL
13900     print #255,using "Form POS 1,C 55,X 3,PIC(---,---,---.--)": "Previous Balance",pbal
14000     let total_amt=total_amt+pbal
14100 P_TOTAL: print #255,using "Form POS 59,C 28": "{\strike             }"
14200     print #255,using "Form POS 51,Cr 13,PIC($-,---,---.##)": "{\b Total:}",total_amt
14300     print #255,using "Form POS 59,C 28": "{\ul \strike "&rpt$(" ",12)&"}"
14400     print #255: "*INSERT FILE:S:\acsTM\black line - six inch.rtf.txt"
14500     let total_amt=0
14600   fnend 
