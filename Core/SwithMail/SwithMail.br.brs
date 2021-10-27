autoLibrary
fn_setup
fnTop(program$)
on error goto Ertn

dim testToEmail$(0)*64
fnAddOneC(mat testToEmail$,'niceguywinning@gmail.com')
fnAddOneC(mat testToEmail$,'niceguywinning@pm.me')
dim testEmailMessage$*256
testEmailMessage$='&lt;p&gt;this is my &lt;b&gt;first&lt;/b&gt; email.&lt;/p&gt;'
dim testSubject$*256
testSubject$='Test Email'
dim testAttachFile$(0)*256
fnAddOneC(mat testAttachFile$,'C:\ACS\Dev-5\Core\SwithMail\readme.txt')
fnAddOneC(mat testAttachFile$,'C:\ACS\Dev-5\Core\xit.br')


if fn_swithMail(mat testToEmail$,testEmailMessage$, testSubject$,mat testAttachFile$) then
	pr 'test emails sent successfully.'
else 
	pr 'test failed.'
end if


end

def library fnSwithMail(mat toEmail$,emailMessage$*10000; subject$*256,mat attachFile$,mat ccEmail$,mat bccEmail$)
	fn_setup
	fnSwithMail=fn_swithMail(mat toEmail$,emailMessage$, subject$,mat attachFile$,mat ccEmail$,mat bccEmail$)
fnend
def fn_swithMail(mat toEmail$,emailMessage$*10000; subject$*256,mat attachFile$,mat ccEmail$,mat bccEmail$, ___, _
     settingsName$*256,batchName$*256,replyName$*256,returnN,line$*256,x)
	if ~setup_swithMail then ! r:
		setup_swithMail=1
		dim emailFrom$*256
		dim emailUsername$*256
		dim emailReplyTo$*256
		dim smtpServer$*100
		dim emailFromPassword$*64
		fnreg_read('email.ReplyTo'    	,emailReplyTo$)
		fnreg_read('email.smtpServer' 	,smtpServer$        	,'smtp.gmail.com') ! smtp.office365.com
		fnreg_read('email.From'        	,emailFrom$         	,'billing@advancedcomputer.services')
		fnreg_read('email.FromName'    	,emailFromName$     	,'ACS Billing')                         	! TODO: ADD TO PREFERENCES
		fnreg_read('email.Username'   	,emailUsername$     	,'john@advancedcomputer.services')    	! TODO: ADD TO PREFERENCES
		fnreg_read('email.FromPassword'	,emailFromPassword$)	! ,'asdfghjkl;qwErtyuiopop[zxcvbnm')
		fnreg_read('email.Port'        	,emailPort$         	,'587')
		if trim$(smtpServer$)='' or trim$(emailFrom$)='' or trim$(emailFromPassword$)='' then
			dim txt$(0)*256
			mat txt$(0)
			fnAddOneC(mat txt$,'Email account not configured adaquately.')
			if trim$(smtpServer$        	)='' then fnAddOneC(mat txt$,'SMTP Server is blank.')
			if trim$(emailFrom$         	)='' then fnAddOneC(mat txt$,'From Email Address is blank.')
			if trim$(emailFromPassword$	)='' then fnAddOneC(mat txt$,'From Email Password is blank.')
			fnmsgbox(mat txt$, respUnused$,'',mb_stop+mb_okonly)
			setup_swithMail=0
			goto Xit
			! msgbox('Email account not configured. Please enter the appropriate information in Preferences.')
		end if
	end if ! /r

	! r: make settings file
	if env$('acsDeveloper')<>'' then
		settingsName$='S:\Core\SwithMail\acsDev SwithMail Settings [acsuserid].xml'
	else
		settingsName$='[temp]\acsSwithMail[acsuserid].xml'
	end if
	open #hSet=fnH: 'Name='&settingsName$&',RecL=256,replace',d,o
	pr #hSet: '<?xml version="1.0" encoding="utf-8" standalone="yes"?>'
	pr #hSet: '<SwithMailSettings>'
	pr #hSet: '  <ServerSettings>'
	pr #hSet: '    <FromName>'&emailFromName$&'</FromName>'
	pr #hSet: '    <FromAddress>'&emailFrom$&'</FromAddress>'
	pr #hSet: '    <Username>'&emailUsername$&'</Username>'
	pr #hSet: '    <Password>'&emailFromPassword$&'</Password>'
	pr #hSet: '    <ObscurePassword>False</ObscurePassword>'
	pr #hSet: '    <MailServer>'&smtpServer$&'</MailServer>'
	pr #hSet: '    <MailServerPort>'&emailPort$&'</MailServerPort>'
	pr #hSet: '    <SSL>True</SSL>'
	pr #hSet: '    <Encoding />'
	pr #hSet: '    <RequestReceipt>False</RequestReceipt>'
	pr #hSet: '    <MessageID>False</MessageID>'
	pr #hSet: '  </ServerSettings>'
	pr #hSet: '  <EmailAddresses>'
	pr #hSet: '    <To>';
		for x=1 to udim(mat toEmail$)
		  if x>1 then pr #hSet: ',';
			pr #hSet: toEmail$(x);
		nex x
		pr #hSet: '</To>'
	pr #hSet: '    <CC />'
	pr #hSet: '    <BCC />'
	pr #hSet: '    <ReplyTo />'
	pr #hSet: '  </EmailAddresses>'
	if udim(mat attachFile$)>0 then
		pr #hSet: '  <Attachments>'
			for x=1 to udim(mat attachFile$)
				pr #hSet: '    <AttachmentPath>'&attachFile$(x)&'</AttachmentPath>'
			nex x
		pr #hSet: '  </Attachments>'
	end if
	pr #hSet: '  <EmailContent>'
	pr #hSet: '    <HTML>True</HTML>'
	pr #hSet: '    <DontReplaceNewLine>True</DontReplaceNewLine>'
	pr #hSet: '    <Subject>'&testSubject$&'</Subject>'
	pr #hSet: '    <Body>'&emailMessage$
	pr #hSet: '</Body>'
	pr #hSet: '    <BodyTxtFile />'
	pr #hSet: '  </EmailContent>'
	! pr #hSet: '<Logging>'
	! pr #hSet: '  <Log>True</Log>'
	! pr #hSet: '</Logging>'
	pr #hSet: '</SwithMailSettings>'
	close #hSet:
	! /r

	! r: make batch file
	batchName$='[temp]\acsSwithMail[acsuserid].cmd'
	replyName$='[temp]\acsSwithMail[acsuserid].txt'
	fnFree(replyName$)
	open #hCmd=fnH: 'Name='&batchName$&',RecL=256,replace',d,o
	pr #hCmd: 'prompt $p$g'
	pr #hCmd: 'echo off'
	pr #hCmd: '@type '&os_filename$('S:\Core\SwithMail\logo.txt')&'"'
	! pr #hCmd: '"'&os_filename$('S:\Core\SwithMail\SwithMail.exe')&'" /x "'&os_filename$(settingsName$)&'"'
	pr #hCmd: '"'&os_filename$('S:\Core\SwithMail\SwithMail.exe')&'" /s /x "'&os_filename$(settingsName$)&'"'
	pr #hCmd: ''
	pr #hCmd: 'if %errorlevel% ==0 goto Success'
	pr #hCmd: 'if %errorlevel% ==1 goto Error'
	pr #hCmd: ''
	pr #hCmd: ':Success'
	pr #hCmd: 'echo Success >"'&os_filename$(replyName$)&'"'
	pr #hCmd: 'goto End'
	pr #hCmd: ''
	pr #hCmd: ':Error'
	pr #hCmd: 'echo Error >"'&os_filename$(replyName$)&'"'
	pr #hCmd: 'goto End'
	pr #hCmd: ''
	pr #hCmd: ':End'
	! pr #hCmd: 'pause'
	close #hCmd:
	! /r
	! pr batchName$ : pause
	execute 'sy "'&os_filename$(batchName$)&'"'
	! execute 'sy -m "'&os_filename$(batchName$)&'"'

	! r: evaluate response and set returnN
	open #hReply=fnH: 'name='&replyName$,d,i
	do
		linput #hReply: line$ eof EoReply
		if trim$(line$)='Success' then returnN=1
	loop until returnN
	EoReply: !
	close #hReply:
	! /r
	Xit: !
	fn_swithMail=returnN
fnend
include: fn_setup
