library 'Library\openFile.wb': fnopen_invoice
library 'Library\SQL.wb': fnsql_setup$
dim inv$(0)*60,invN(0)
dim invFieldsC$(0)*20,invFieldsN$(0)*20
dim invFormAll$*2048
execute 'copy Invoice.Int//6 Invoice-'&date$('ccyymmdd')&'-'&srep$(time$,':','')&'.dat//6'
execute "*SubProc "&fnsql_setup$('INVOICE',mat inv$,mat invN,mat invFieldsC$,mat invFieldsN$,invFormAll$)

hInv=fnopen_invoice(mat hInvoice)
do
	read #hInv,using invFormAll$: mat inv$,mat invN eof InEoInvoice
	if pos(inv$(invoice_inv_no),'-000000')>0 then
		pr count+=1,inv$(invoice_fileno),inv$(invoice_inv_no)
		! if count/30=int(count/30) then pause
		delete #hInv:
	end if
loop
InEoInvoice: !
close #hInv:
pr 'Delete Count=';count