00010 ! formerly acsTM\moInvoice
20020   library "S:\acsTM\print_invoice": fnprint_invoice
20040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fngethandle,fnautomatedsavepoint
20060   let fntop(program$,cap$="ACS Invoices")
20082   let client_id_sage_ax=3811
20084   let client_id_brc=90
20120   dim sys_name$(40)*55,client_id$*5,b(8),iv$*12,skln$*80 ! co$(4)*40,
20150   dim client_addr$(3)*30,cap$*128
20160   dim in1$(3)
20180   dim inp(7),wo_desc$*30
20200   let fn_get_system_list(mat sys_name$)
20220   open #h_ivnum:=fngethandle: "Name="&env$('Q')&"\TMmstr\IVNUM.h"&env$('cno')&",Use,RecL=8,Shr",internal,outin,relative 
20260   read #h_ivnum,using "Form POS 1,N 8",rec=1: invoice_number
20270   close #h_ivnum: 
20280   let invoice_number=invoice_number+1
20420   print newpage
20440   print fields "5,2,cr 43": "Invoice Date (mmddyy):"
20460   print fields "4,30,c": "Invoice Date MUST match expiration date of Annual Support contracts!"
20480   print fields "6,2,cr 43": "Starting Invoice Number:"
20500   print fields "7,2,Cr 43": "Starting Account Number:"
20520   let in1$(1)="5,46,Nz 6,U"
20540   let in1$(2)="6,46,N 12,U"
20560   let in1$(3)="7,46,N 5,U"
20600 ! let inv_date=date('mmddyy')
20620   print fields "10,30,c 20,,B99": "Cancel (Esc)"
20640 SCREEN1_ASK: ! 
20650   rinput fields mat in1$: inv_date,invoice_number,starting_acct_no conv SCREEN1_ASK
20660   if cmdkey=5 or fkey=99 then goto XIT
20680   let b4=date(days(inv_date,"mmddyy"),"ccyymmdd")
