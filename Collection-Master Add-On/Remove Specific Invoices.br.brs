library 'Library\openFile.wb': fnopen_invoice
library 'Library\SQL.wb': fnsql_setup$
library 'Library\clsutil.wb': fncom
dim inv$(0)*60,invN(0)
dim invFieldsC$(0)*20,invFieldsN$(0)*20
dim invFormAll$*2048
dim backupFile$*256
backupFile$='Invoice-'&date$('ccyymmdd')&'.dat//6'
if ~exists(backupFile$) th
	execute 'copy Invoice.Int//6 '&backupFile$
end if
exe "*SubProc "&fnsql_setup$('INVOICE',mat inv$,mat invN,mat invFieldsC$,mat invFieldsN$,invFormAll$)

hInv=fnopen_invoice(mat hInvoice)
readCount=0
total=lrec(hInv)
do
	read #hInv,using invFormAll$: mat inv$,mat invN eof InEoInvoice
	readCount+=1
	fncom(readCount,total, 5)
	if inv$(invoice_fileno)(1:4)='PM16' and trim$(inv$(invoice_inv_date))='20/05/26' t
		count+=1
		pr inv$(invoice_fileno)&' '&inv$(invoice_inv_date)
		delete #hInv:
		! pau
	en if
	! if pos(inv$(invoice_inv_no),'-000000')>0 then
	! 	pr count+=1,inv$(invoice_fileno),inv$(invoice_inv_no)
	! 	! if count/30=int(count/30) then pause
	! end if
loop
InEoInvoice: !
close #hInv:
pr 'read Count=';readCount
pr 'Delete Count=';count
end