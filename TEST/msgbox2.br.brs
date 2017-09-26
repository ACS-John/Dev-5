00030   library 'S:\Core\Library': fnmsgbox
00660   let mesg$(1)="Unposted Collections File Exists!" !:
        let mesg$(2)="Do you wish to save previous input?" !:
        let mesg$(3)="" !:
        let mesg$(4)="Yes:  Allows you to edit previous unposted input and make new entries" !:
        let mesg$(5)="No:  Erases previous unposted entries and allows you to make new entries" !:
        let mesg$(6)="Cancel:  Exits this program without any changes" !:
        let fnmsgbox(mat mesg$, resp$, cap$, 3)
02000   dim mesg$(6)*80
02150   let mesg$(1)="This is your last chance!" !:
        let mesg$(2)="" !:
        let mesg$(3)="Do you want to print a" !:
        let mesg$(4)="Cash Receipts Journal" !:
        let mesg$(5)="or a " !:
        let mesg$(6)="Deposit List?" !:
        let fnmsgbox(mat mesg$,resp$,cap$,52)
