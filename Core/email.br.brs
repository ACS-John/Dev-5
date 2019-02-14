!		! r: test zone
!			! library 'S:\Core\Library.br': fntop
!			library program$: fnSendEmail
!			! fntop(program$)
!			dim subject$*255
!			subject$='fnSendEmail Test'
!			dim emailMessage$*10000
!			emailMessage$='This is a test<br>This is line two testing<br>'
!			dim attachFile$*255
!			attachFile$=''
!			prompt=0
!			dim bccEmail$*256
!			bccEmail$=''
!			mat ccEmails$(0)
!			CCAsTo=0
!			fnSendEmail('niceguywinning@gmail.com',emailMessage$, subject$,attachFile$,prompt,bccEmail$,mat ccEmails$,CCAsTo)
!		! /r
end
def library fnSendEmail(toEmail$*256,emailMessage$*10000; subject$*256,attachFile$*1024,prompt,BCCEmail$*256,mat CCEmails$,CCAsTo,___,hResult,success,line$*1024,item,emailAddr$*1024,returnN)
	library 'S:\Core\Library.br': fngethandle
	library 'S:\Core\Library.br': fnFree
	if exists("Core\sendEmail.exe") then
		if env$('client')='Brumbaugh' then
			! server$='10.20.129.12:25' <--- that would use port 25
			server$='10.20.129.12'
			fromEmail$='noreply@bqlaw.com'
			userAccount$=''
			password$=''
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
			execute 'sy -M -s Core\sendEmail.exe -s '&server$&' -t '&emailAddr$&' -f '&fromEmail$&' -xu '&userAccount$&' -xp '&password$&' -u "'&subject$&'" -m "'&emailMessage$&'" -a "'&os_filename$(attachFile$)&'"'&CCEmailSTring$&BCCEmail$&' -v -q -l EmailLog.'&Wsid$
		else
			execute 'sy -M -s Core\sendEmail.exe -s '&server$&' -t '&emailAddr$&' -f '&fromEmail$&' -xu '&userAccount$&' -xp '&password$&' -u "'&subject$&'" -m "'&emailMessage$&'"'&CCEmailSTring$&BCCEmail$&' -v -q -l EmailLog.'&Wsid$
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
		msgbox("Core\sendEmail.exe program not found. Please include it with your software. This program can be downloaded for free from the internet.")
	end if
	XitSendEmail: !
	fnSendEmail=returnN
fnend