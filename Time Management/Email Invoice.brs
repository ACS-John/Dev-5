! Email Invoice
! This program emails printed invoices as attachments
fn_setup
fntop(program$)
dim unused_gb(10),toname$*50,econtact$(1)*255,econtact(1),form$(1)*1000
fnEmailEntryScreen
!
! Screen to get the date of the pdf's created
! print a list ? to hit ok
! send via email
! fnEmail_Invoice
! copy to server 'sent' folder
!
XIT: fnxit
stop ! 
def fn_setup
	if ~setup then
		setup=1
		library : fnGet_Email_Info,fnEmail_Invoice,FnSendEmail
		library 'S:\Core\Library': fntop,fnerror,fngethandle,fnmakesurepathexists
		library 'S:\Core\Library': fnTos,fnAcs,fnCmdKey,fnFra,fnButton,fnChk,fnxit,fnlbl,fntxt
		on error goto ERTN
	end if 
fnend
def fnEmailEntryScreen
	dim resp$(1)*80
	let fnTos(sn$="Email Invoices")
	let rc=cf=pdfdate=0
	cf+=1 : fradate=cf : mylen=26 : mypos=mylen+2
	fnLbl(1,1,"Email Invoices Created On:",mylen,1,0,pdfdate)
	fnTxt(1,mypos,10,0,1,"3",0,today$,pdfdate)
	if use_date=0 then use_date=date('mmddyy') ! *10000+100+date('yy')
	resp$(rc+=1)=str$(use_date)
	! if trim$(use_date$)="" then let use_date$=date("mm/dd/yy")
	! resp$(rc+=1)=use_date$
	fnCmdKey("Next",1,1,0,"Emails all PDF invoices created on this date")
	fnCmdKey("Cancel",5,0,1,"Returns to main menu")
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then 
		goto Tf_XIT
	else if ckey=1 then 
		fnEmail_Invoice(resp$(1))
	end if 
	!
	Tf_XIT: ! continue
fnend
def library fnGet_Email_Info
	library 'S:\Core\Library': fnreg_read
	dim em_emailFrom$*255,em_emailReplyTo$*255,em_smtpServer$*100
	fnreg_read('email.ReplyTo',em_emailReplyTo$)
	fnreg_read('email.smtpServer',em_smtpServer$)
	fnreg_read('email.From',em_emailFrom$,'acs-billing@utilitybilling.us')
	fnreg_read('email.FromPassword',em_emailFromPassword$)
	fnreg_read('email.Port',em_emailPort$,'25')
	if trim$(em_emailReplyTo$)="" or trim$(em_smtpServer$)="" or trim$(em_emailFrom$)="" or trim$(em_emailFromPassword$)="" then 
		let msgbox("Email account not configured. Please enter the appropriate information in Preferences.")
	end if 
fnend 
def library fnEmail_Invoice(email_date$;pdfname$*255,pdfline$*1000,ppos,ppos2,testday$)
	! this sends the emails that were printed as PDF's earlier
	! read log 
	fnmakesurepathexists("s:\Time Management\Ebilling\Sent\")
	execute "dir 's:\Time Management\Ebilling' >'s:\Time Management\Ebilling\sendingnow.txt' -B" 
	open #elist:=fngethandle: "name=s:\Time Management\Ebilling\sendingnow.txt",display,input
	let econtact=fn_open("TM Contact",mat econtact$,mat econtact,mat form$,1,1)
	!open #hcontact:=fngethandle: "Name=S:\Core\Data\acsllc\TM Contact.h[cno],KFName=S:\Core\Data\acsllc\TM Contact.h[cno],Shr",internal,input,keyed
	do while file(elist)=0
		linput #elist: pdfline$ eof donesend
		! if it exists then look up customer to information
		! pause 
		if pdfline$(1:7)="ACS Inv" then 
			let pdfname$=pdfline$(1:len(pdfline$))
			let ppos=pos(pdfname$,".")
			let ppos2=pos(pdfname$,".",ppos+1)
			let clientno$=trim$(pdfname$(ppos+1:ppos2-1))
			let testday$=pdfname$(ppos2+1:pos(pdfname$,".",ppos2+1)-1)
			! if on selected date
			! print testday$ : print email_date$ : pause 
			if days(testday$,"mmddyy")=days(email_date$,"ccyymmdd") then 
				! print clientno$ : pause ! send emails
				restore #econtact,key=rpad$(Clientno$,5," "): nokey skipthis
				do while file(econtact)=0
					read #econtact,using form$(econtact): mat econtact$,mat econtact eof ignore
					if rpad$(clientno$,5," ")=rpad$(econtact$(con_clientid),5," ") and econtact(con_emailbilling)=1 then 
						let goodemail=fnSendEmail(trim$(econtact$(con_bemail)),trim$(econtact$(con_name))&", please find ACS invoice attach. Thanks for opting for ebilling, from John at the ACS team!","ACS Invoice ","s:\Time Management\Ebilling\"&trim$(pdfname$),1)
						! print econtact$(con_name): pr goodemail : pause
						if goodemail=1 then 
							execute "copy 's:\Time Management\Ebilling\"&trim$(pdfname$)&"' 's:\Time Management\Ebilling\Sent\"&trim$(pdfname$)&"'"
							execute "free 's:\Time Management\Ebilling\"&trim$(pdfname$)&"'" ! remove from ebilling folde r
						else
							le msgbox("Email not sent to "&triM$(econtact$(con_name))&", Client #"&econtact$(con_clientid)&", check email on file "&trim$(econtact$(con_bemail))&". Please send manually.","Email not sent","OK","EXCL")
						end if 
					end if 
				loop while rpad$(clientno$,5," ")=rpad$(econtact$(con_clientid),5," ")
				skipthis: ! no key 
			end if 
		end if 
	loop 
	donesend: ! close and done 
	close #elist: 
	execute "copy 's:\Time Management\Ebilling\sendingnow.txt' 's:\Time Management\Ebilling\Sent\sent"&date$("mmddyy")&time$(1:2)&time$(4:5)&time$(7:8)&".txt'"
	execute "free 's:\Time Management\Ebilling\sendingnow.txt'"
fnend
def library FnSendEmail(Emailaddress$*255,EmailMessage$*10000;Subject$*255,Invoicefile$*255,noprompt,BCCEmail$*255,mat CCEmails$,CCAsTo,___,Resultfile,Success,Outputstring$*1024,Index,emailserver$*255,EmailAddressString$*255)
! this is a copy of fileios' fnSendEmail, modified to use fnreg_read values
	library : fnOpenFile, fnGetFileNumber, fnDoesLayoutExist
	if exists("sendemail.exe") then
		let fnGet_Email_Info
		if Exists("EmailLog.[WSID]") then execute "*free EmailLog.[WSID]"
		if EmailMessage$="" then let EmailMessage$=" "
		let EmailMessage$=srep$(EmailMessage$,hex$("0D"),hex$("0A"))
		let EmailMessage$=srep$(EmailMessage$,hex$("0A0A"),hex$("0A"))
		let EmailMessage$=srep$(EmailMessage$,hex$("0A"),"\n")
		if len(BCCEmail$) then let BCCEmail$(1:0)=" -bcc "
		let CCEmailString$=""
		for Index=1 to udim(mat CCEmails$)
				if len(trim$(CCEmails$(Index))) then
					if len(trim$(CCEmailString$)) then let CCEmailString$=CCEmailString$&" "
					let CCEmailString$=CCEmailString$&CCEmails$(Index)
				end if
		next Index
		let EmailAddressString$=Emailaddress$
		if CCAsTo then
				if len(trim$(EmailAddressString$)) then let EmailAddressString$=EmailAddressString$&" "
				let EmailAddressString$=EmailAddressString$&CCEmailString$
				let CCEmailString$=""
		end if
		if len(trim$(CCEmailString$)) then let CCEmailString$=" -cc "&CCEmailString$
		let emailserver$=em_smtpServer$&":"&em_emailPort$
		if trim$(em_emailReplyTo$)="" then let em_emailReplyTo$=em_emailFrom$
		if len(trim$(InvoiceFile$)) then
				execute 'sy -M -s sendemail -s '&emailserver$&' -t '&EmailAddressString$&' -f '&em_emailFrom$&' -xu '&em_emailFrom$&' -xp '&em_emailFromPassword$&' -u "'&Subject$&'" -m "'&EmailMessage$&'" -a "'&os_filename$(InvoiceFile$)&'"'&CCEmailSTring$&BCCEmail$&' -o reply-to='&em_emailReplyTo$&' -v -q -l EmailLog.'&Wsid$
		else
				execute 'sy -M -s sendemail -s '&emailserver$&' -t '&EmailAddressString$&' -f '&em_emailFrom$&' -xu '&em_emailFrom$&' -xp '&em_emailFromPassword$&' -u "'&Subject$&'" -m "'&EmailMessage$&'"'&CCEmailSTring$&BCCEmail$&' -o reply-to='&em_emailReplyTo$&' -v -q -l EmailLog.'&Wsid$
		end if
		execute 'type EmailLog.[WSID] >>EmailLog.txt'
		open #(Resultfile:=Fngetfilenumber): "name=EmailLog.[WSID],recl=512",display,input
		do Until File(Resultfile)
				linput #Resultfile: Outputstring$ eof IGNORE
				if Pos(Outputstring$,"Email was sent successfully!") then let Success=1
		loop
		close #Resultfile:
		execute '*free EmailLog.[WSID]'
		let FnSendEmail=Success
		if Success and ~noprompt then LET MSGBOX("Email was sent successfully!","Email Sent")
	else
			let msgbox("SendEmail.exe program not found. Please include it with your software. This program can be downloaded for free from the internet.")
	end if
fnend
include: ertn
include: fn_open