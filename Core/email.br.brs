! r: test zone
	fn_setup
	dim tstSubject$*256
	tstSubject$='fnSendEmail Test '&date$('month, day, ccyy')
	dim tstBody$*10000
	tstBody$='<html><body>This is a test<br>This is line two testing<br></body></html>'
	dim tstBccEmail$(0)*256
	mat tstBccEmail$(0)
	dim tstCcEmails$(0)*128

	! mat tstCcEmails$(0)
	! fnAddOneC(mat tstCcEmails$,'niceguywinning@gmail.com')
	! ! fnAddOneC(mat tstCcEmails$,'john@ajjmaplewood.com')
	! dim tstToEmail$(0)*64
	! fnAddOneC(mat tstToEmail$,'john@advancedcomputer.services')
	! fnAddOneC(mat tstToEmail$,'john@ajjmaplewood.com')
	! fn_sendEmail(mat tstToEmail$,tstBody$, tstSubject$,'',mat tstCcEmails$)

	dim tstToEmailSingle$*64
	tstToEmailSingle$='niceguywinning@gmail.com'
	fn_sendEmail(tstToEmailSingle$,tstBody$, tstSubject$&' with attachment',program$)

	! fn_sendEmail('john@advancedcomputer.services',tstBody$, tstSubject$&' withOUT attachment')
	! fn_sendEmail('niceguywinning@gmail.com',tstBody$, tstSubject$&' withOUT attachment')
	! fnSendEmail('niceguywinning@gmail.com',tstBody$, tstSubject$,program$,mat ccEmails$,mat bccEmail$)
end ! /r

def library fnSendEmail(mat toEmail$,emailMessage$*10000; subject$*256,attachFile$*256,mat ccEmail$,mat bccEmail$)
	if ~setup then fn_setup
	fnSendEmail=fn_sendEmail(mat toEmail$,emailMessage$, subject$,attachFile$,mat ccEmail$,mat bccEmail$)
fnend
def fn_sendEmail(mat toEmail$,emailMessage$*10000; _
		subject$*256,attachFile$*256,mat ccEmail$,mat bccEmail$, ___, _
		hResult,line$*1024,toPart$*512,ccPart$*512,bccPart$*512,attachFilePart$*512, _
		shellFlag$,returnN)
	if env$('acsDeveloper')<>'' then debug=1
	if ~setup_sendEmail then ! r:
		setup_sendEmail=1
		dim em_emailFrom$*256
		dim em_emailReplyTo$*256
		dim em_smtpServer$*100
		fnreg_read('email.ReplyTo',em_emailReplyTo$) ! ,'noreply@@utilitybilling.us')
		fnreg_read('email.smtpServer',em_smtpServer$,'smtp.office365.com:587')
		fnreg_read('email.From',em_emailFrom$,'acs-billing@utilitybilling.us')
		fnreg_read('email.FromPassword',em_emailFromPassword$,'ACSbilling1224.')
		fnreg_read('email.Port',em_emailPort$,'25')
		if trim$(em_smtpServer$)="" or trim$(em_emailFrom$)="" or trim$(em_emailFromPassword$)="" then
			dim txt$(0)*256
			mat txt$(0)
			fnAddOneC(mat txt$,'Email account not configured adaquately.')
			if trim$(em_smtpServer$)="" then
				fnAddOneC(mat txt$,'SMTP Server is blank.')
			end if
			if trim$(em_emailFrom$)="" then
				fnAddOneC(mat txt$,'From Email Address is blank.')
			end if
			if trim$(em_emailFromPassword$)="" then
				fnAddOneC(mat txt$,'From Email Password is blank.')
			end if
			fnmsgbox(mat txt$, respUnused$,'',mb_stop+mb_okonly)
			setup_sendEmail=0
			goto Xit
			! msgbox("Email account not configured. Please enter the appropriate information in Preferences.")
		end if
		if ~exists("S:\Core\sendEmail.exe") then
			pr os_filename$('S:\Core\sendEmail.exe')&' utility not found. Please call ACS technical support at 1-800-643-6318.'
			pause
			setup_sendEmail=0
			goto Xit
		end if
	end if ! /r
	fnFree('EmailLog.[session]')
	if emailMessage$="" then emailMessage$=" "
	emailMessage$=srep$(emailMessage$,cr$,lf$)
	emailMessage$=srep$(emailMessage$,lf$&lf$,lf$)
	emailMessage$=srep$(emailMessage$,lf$,"\n")

	mat2str(mat toEmail$,toPart$,' ','trim')
	toPart$=' -t '&toPart$&' '

	if fnArrayWasPassedC(mat ccEmail$) then
		mat2str(mat ccEmail$,ccPart$,' ','trim')
		ccPart$=' -cc '&ccPart$&' '
	end if

	if fnArrayWasPassedC(mat bccEmail$) then
		mat2str(mat bccEmail$,bccPart$,' ','trim')
		bccPart$=' -bcc '&bccPart$&' '
	end if

	! if trim$(em_emailReplyTo$)="" then em_emailReplyTo$=em_emailFrom$

	if len(trim$(attachFile$)) then
		attachFilePart$=' -a "'&os_filename$(attachFile$)&'"'
	end if
	open #hCmd=fnH: 'name=sendEmail_[session].cmd,recl=2048,replace',d,o
	pr #hCmd: 'prompt $p$g'
	pr #hCmd: '@echo Sending Email...'
	pr #hCmd: '"'&os_filename$('S:\Core\sendEmail.exe')&'"';
	pr #hCmd: ' -s '&em_smtpServer$&':'&em_emailPort$;
	pr #hCmd: toPart$;
	pr #hCmd: ' -f '&em_emailFrom$;
	pr #hCmd: ' -xu '&em_emailFrom$;
	pr #hCmd: ' -xp '&em_emailFromPassword$;
	pr #hCmd: ' -u "'&subject$&'"';
	pr #hCmd: ' -m "'&emailMessage$&'"';
	pr #hCmd: attachFilePart$;
	pr #hCmd: ccPart$;
	pr #hCmd: bccPart$;
	if em_emailReplyTo$<>'' then
		pr #hCmd: ' -o reply-to='&em_emailReplyTo$;
	end if
	pr #hCmd: ' -v';
	pr #hCmd: ' -q';
	pr #hCmd: ' -l EmailLog.'&session$
	if debug then
		pr #hCmd: 'pause'
		shellFlag$='-s '
	else
		shellFlag$='-M -s '
	end if
	close #hCmd:
	execute 'sy '&shellFlag$&'sendEmail_'&session$&'.cmd'
	if debug then pr 'after shell call' : spause
	if fnCopy('EmailLog.[session]',fnReportCacheFolderCurrent$&'\Emails Sent - '&date$('ccyy-mm-dd')&' '&fnSafeFilename$(time$)&'.txt') then

		! execute 'type EmailLog.[session] >>EmailLog.txt'
		open #hResult=fnH: "name=EmailLog.[session],recl=512",display,input
		do until file(hResult)
			linput #hResult: line$ eof ignore
			if pos(line$,"Email was sent successfully!") then returnN=1
		loop
		close #hResult,free:
	else
		mat txt$(0)
		fnAddOneC(mat txt$,'Email Send failed.  No log file to parse.')
		fnmsgbox(mat txt$, respUnused$,'',mb_stop+mb_okonly)
	end if
	fn_sendEmail=returnN
	Xit: !
fnend
include: fn_setup
