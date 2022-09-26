autoLibrary
dim mesg$(6)*80

mat mesg$(6)
mesg$(1)="Unposted Collections File Exists!" 
mesg$(2)="Do you wish to save previous input?" 
mesg$(3)="" 
mesg$(4)="Yes:  Allows you to edit previous unposted input and make new entries" 
mesg$(5)="No:  Erases previous unposted entries and allows you to make new entries" 
mesg$(6)="Cancel:  Exits this program without any changes" 
fnMsgBox(mat mesg$, resp$, cap$, 3)

mat mesg$(6)
mesg$(1)="This is your last chance!" 
mesg$(2)="" 
mesg$(3)="Do you want to pr a" 
mesg$(4)="Cash Receipts Journal" 
mesg$(5)="or a " 
mesg$(6)="Deposit List?" 
fnMsgBox(mat mesg$,resp$,cap$,52)
