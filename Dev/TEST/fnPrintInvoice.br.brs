autoLibrary
fntop(program$)
! fnOpenPrn
dim clientId$(2)*5
clientId$(1)='2070'
clientId$(2)='440'
dim clientAddr$(3)*30
clientAddr$(1)='Client Name --+----|----+----|'
clientAddr$(2)='Address -|----+----|----+----|'
clientAddr$(3)='City, State and Zip ----+----|'
dim invNum$(2)*12
invNum$(1)='i23456789012'
invNum$(2)='i34567890123'
pbal=99999.99
inv_date=101112
dim invItem$(0)*128
dim invAmt(0)
fnAddOneC(mat invItem$,'invoice item number 1') : fnAddOneN(mat invAmt,  100.11)
fnAddOneC(mat invItem$,'invoice item number 2') : fnAddOneN(mat invAmt, 2200.22)
fnAddOneC(mat invItem$,'invoice item number 3') : fnAddOneN(mat invAmt,33300.33)
dim pdfFileName$*255
fnInvoiceOpen
for inv=1 to 2
	fnInvoiceAdd(clientId$(inv),mat clientAddr$,invNum$(inv),inv_date,mat invItem$,mat invAmt,pbal)
nex inv
fnInvoiceClose
fnXit
