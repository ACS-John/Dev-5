	! r: test zone
		! library 'S:\Core\Library.br': fntop
		library program$: fnSendEmail
		! fntop(program$)
		dim subject$*255
		subject$='fnSendEmail Test'
		dim emailMessage$*10000
		emailMessage$='This is a test<br>This is line two testing<br>'
		dim attachFile$*255
		attachFile$=program$
		prompt=0
		dim bccEmail$*256
		bccEmail$=''
		mat ccEmails$(0)
		CCAsTo=0
		fnSendEmail('john@advancedcomputer.services',emailMessage$, subject$&' with attachment',attachFile$,prompt,bccEmail$,mat ccEmails$,CCAsTo)
		fnSendEmail('niceguywinning@gmail.com',emailMessage$, subject$&' with attachment',attachFile$,prompt,bccEmail$,mat ccEmails$,CCAsTo)
		fnSendEmail('john@advancedcomputer.services',emailMessage$, subject$&' withOUT attachment',attachFile$,prompt,bccEmail$,mat ccEmails$,CCAsTo)
		fnSendEmail('niceguywinning@gmail.com',emailMessage$, subject$&' withOUT attachment',attachFile$,prompt,bccEmail$,mat ccEmails$,CCAsTo)
		! fnSendEmail('niceguywinning@gmail.com',emailMessage$, subject$,attachFile$,prompt,bccEmail$,mat ccEmails$,CCAsTo)
	! /r
end
def library fnSendEmail(toEmail$*256,emailMessage$*10000; subject$*256,attachFile$*1024,prompt,BCCEmail$*256,mat CCEmails$,CCAsTo,___,hResult,success,line$*1024,item,emailAddr$*1024,returnN)
	library 'S:\Core\Library.br': fngethandle
	library 'S:\Core\Library.br': fnFree
	if ~setupSendEmail then
		setupSendEmail=1
		dim sePathBr$*256
		sePathBr$='S:\Core\sendEmail.exe'
		dim sePathOs$*256
		sePathOs$=os_filename$(sePathBr$)
	end if
	if exists(sePathOs$) then
		dim userAccount$*128
		dim fromEmail$*128
		if env$('client')='ACS' then
			! server$='smtp.gmail.com'
			! fromEmail$='john@advancedcomputer.services' 
			! userAccount$='john@advancedcomputer.services'
			! password$='jiujitsu42!'
			server$=' smtp.mail.com:587'
			fromEmail$='acs.report@deliveryman.com' 
			userAccount$='acs.report@deliveryman.com'
			password$='ACSken1@#'
		else if env$('client')='Brumbaugh' then ! r:
			! server$='10.20.129.12:25' <--- that would use port 25
			server$='10.20.129.21'
			dim fromEmail$*128
			fromEmail$='bqreportservices@bqlaw.com'
			userAccount$=''
			password$='' ! /r
		else
			pr 'smtp server address, fromEmail, userAccount, and password are not set up for client '&env$('client')&'.'
			pr 'Please contact support to setup automated emails for this facility.'
			pr 'Type GO and press Enter to skip sending of this email and continue'
			pause
			goto XitSendEmail
		end if
		fnFree('EmailLog.[WSID]')
		if emailMessage$="" then emailMessage$=" "
		emailMessage$=srep$(emailMessage$,hex$("0D"),hex$("0A"))
		emailMessage$=srep$(emailMessage$,hex$("0A0A"),hex$("0A"))
		emailMessage$=srep$(emailMessage$,hex$("0A"),"\n")
		if len(BCCEmail$) then BCCEmail$(1:0)=" -bcc "
		CCEmailString$=""
		for item=1 to udim(mat CCEmails$)
			if len(trim$(CCEmails$(item))) then
					if len(trim$(CCEmailString$)) then CCEmailString$=CCEmailString$&" "
					CCEmailString$=CCEmailString$&CCEmails$(item)
			end if
		next item

		emailAddr$=toEmail$

		if CCAsTo then
			if len(trim$(emailAddr$)) then emailAddr$=emailAddr$&" "
			emailAddr$=emailAddr$&CCEmailString$
			CCEmailString$=""
		end if

		if len(trim$(CCEmailString$)) then CCEmailString$=" -cc "&CCEmailString$

		
		
		if len(trim$(attachFile$)) then
			execute 'sy -M -s "'&sePathOs$&'" -s '&server$&' -t '&emailAddr$&' -f '&fromEmail$&' -xu '&userAccount$&' -xp '&password$&' -u "'&subject$&'" -m "'&emailMessage$&'" -a "'&os_filename$(attachFile$)&'"'&CCEmailSTring$&BCCEmail$&' -v -q -l EmailLog.'&Wsid$
		else
			execute 'sy -M -s "'&sePathOs$&'" -s '&server$&' -t '&emailAddr$&' -f '&fromEmail$&' -xu '&userAccount$&' -xp '&password$&' -u "'&subject$&'" -m "'&emailMessage$&'"'&CCEmailSTring$&BCCEmail$&' -v -q -l EmailLog.'&Wsid$
		end if
		execute 'type EmailLog.[WSID] >>EmailLog.txt'
		open #(hResult:=fngethandle): "name=EmailLog.[WSID],recl=512",display,input
		do until file(hResult)
			linput #hResult:line$ eof IGNORE
			if pos(line$,"Email was sent successfully!") then success=1
		loop
		close #hResult:
		execute '*free EmailLog.[WSID]'
		returnN=success
		if prompt then 
			if success then 
				msgbox("Email success.","Email")
			else
				msgbox("Email failed.","Email")
			end if
		end if
	else
		msgbox('"'&sePathOs$&'" utility not found. Call 1-800-643-6318 for support.')
	end if
	XitSendEmail: !
	fnSendEmail=returnN
fnend