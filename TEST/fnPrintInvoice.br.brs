autoLibrary
fntop(program$)
! fnOpenPrn
dim client_id$(2)*5
client_id$(1)='2070'
client_id$(2)='440'
dim client_addr$(3)*30
client_addr$(1)='Client Name --+----|----+----|'
client_addr$(2)='Address -|----+----|----+----|'
client_addr$(3)='City, State and Zip ----+----|'
dim inv_num$(2)*12
inv_num$(1)='i23456789012'
inv_num$(2)='i34567890123'
pbal=99999.99
inv_date=101112
dim inv_item$(0)*128
dim inv_amt(0)
fnAddOneC(mat inv_item$,'invoice item number 1') : fnAddOneN(mat inv_amt,  100.11)
fnAddOneC(mat inv_item$,'invoice item number 2') : fnAddOneN(mat inv_amt, 2200.22)
fnAddOneC(mat inv_item$,'invoice item number 3') : fnAddOneN(mat inv_amt,33300.33)
dim pdfFileName$*255
! fnpa_open
! fnPrintInvoiceClose
for inv=1 to 1
	pdfFileName$=fnPrintFileName$(client_id$(inv),'pdf','invoice\individual\*')
	fnPrintInvoice(255,0,client_id$(inv),mat client_addr$,inv_num$(inv),inv_date,mat inv_item$,mat inv_amt,pbal, pdfFileName$)
	exec 'sy -c "'&pdfFileName$&'"'
nex inv
! fnpa_finis
fnXit
