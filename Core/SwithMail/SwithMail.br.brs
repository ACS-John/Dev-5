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


pr 'fn returns ';fn_swithMail(mat testToEmail$,testEmailMessage$, testSubject$,mat testAttachFile$)


end 

def library fnSwithMail(mat toEmail$,emailMessage$*10000; subject$*256,mat attachFile$,mat ccEmail$,mat bccEmail$)
	fn_setup
	fnSwithMail=fn_swithMail(mat toEmail$,emailMessage$, subject$,mat attachFile$,mat ccEmail$,mat bccEmail$)
fnend
def fn_swithMail(mat toEmail$,emailMessage$*10000; subject$*256,mat attachFile$,mat ccEmail$,mat bccEmail$, ___, _
     settingsName$*256,batchName$*256)
	if ~setup_swithMail then ! r: 
		setup_swithMail=1
		dim emailFrom$*256
		dim emailReplyTo$*256
		dim smtpServer$*100
		dim emailFromPassword$*64
		fnreg_read('email.ReplyTo'    	,emailReplyTo$) ! ,'noreply@utilitybilling.us'
		fnreg_read('email.smtpServer' 	,smtpServer$        	,'smtp.office365.com') ! 'smtp.gmail.com') ! 'smtp.office365.com:587'
		fnreg_read('email.From'        	,emailFrom$         	,'billing@advancedcomputer.services') ! 'acs-billing@utilitybilling.us'
		fnreg_read('email.FromPassword'	,emailFromPassword$	,'asdfghjkl;qwErtyuiopop[zxcvbnm') ! '76HLUJCaa%' ! old: 'ACSbilling1224.'
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
	settingsName$='[temp]\acsSwithMail[acsuserid].xml'
	open #hSettings=fnH: 'Name='&settingsName$&',RecL=256,replace',d,o
	pr #hSettings: '<?xml version="1.0" encoding="utf-8" standalone="yes"?>'
	pr #hSettings: '<SwithMailSettings>'
	pr #hSettings: '  <ServerSettings>'
	pr #hSettings: '    <FromName>ACS Billing</FromName>'
	pr #hSettings: '    <FromAddress>billing@advancedcomputer.services</FromAddress>'
	pr #hSettings: '    <Username>john@advancedcomputer.services</Username>'
	pr #hSettings: '    <Password>/mW/PMRZfu4o5Xgj403LWmAEDrsz0jhDFx4Ac52Tqig=</Password>'
	pr #hSettings: '    <ObscurePassword>True</ObscurePassword>'
	pr #hSettings: '    <MailServer>smtp.gmail.com</MailServer>'
	pr #hSettings: '    <MailServerPort>'&emailPort$&'</MailServerPort>'
	pr #hSettings: '    <SSL>True</SSL>'
	pr #hSettings: '    <Encoding />'
	pr #hSettings: '    <RequestReceipt>False</RequestReceipt>'
	pr #hSettings: '    <MessageID>False</MessageID>'
	pr #hSettings: '  </ServerSettings>'
	pr #hSettings: '  <EmailAddresses>'
	pr #hSettings: '    <To>';
		for x=1 to udim(mat toEmail$)
		  if x>1 then pr #hSettings: ',';
			pr #hSettings: toEmail$(x);
		nex x
		pr #hSettings: '</To>'
	pr #hSettings: '    <CC />'
	pr #hSettings: '    <BCC />'
	pr #hSettings: '    <ReplyTo />'
	pr #hSettings: '  </EmailAddresses>'
	pr #hSettings: '  <Attachments>'
	pr #hSettings: '    <AttachmentPath>C:\ACS\Dev-5\Core\SwithMail\readme.txt</AttachmentPath>'
	pr #hSettings: '    <AttachmentPath>C:\ACS\Dev-5\Core\xit.br</AttachmentPath>'
	pr #hSettings: '  </Attachments>'
	pr #hSettings: '  <EmailContent>'
	pr #hSettings: '    <HTML>True</HTML>'
	pr #hSettings: '    <DontReplaceNewLine>True</DontReplaceNewLine>'
	pr #hSettings: '    <Subject>'&testSubject$&'</Subject>'
	pr #hSettings: '    <Body>'&emailMessage$
	pr #hSettings: '</Body>'
	pr #hSettings: '    <BodyTxtFile />'
	pr #hSettings: '  </EmailContent>'
	pr #hSettings: '</SwithMailSettings>'
	close #hSettings:
	! /r

	! r: make batch file
	batchName$='[temp]\acsSwithMail[acsuserid].cmd'
	
	open #h_batch=fnH: 'Name='&batchName$&',RecL=256,replace',d,o
	pr #h_batch: 'prompt $p$g'
	pr #h_batch: '"'&os_filename$('S:\Core\SwithMail\SwithMail.exe')&'" /s /x "'&os_filename$(settingsName$)&'"'
	pr #h_batch: 'pause'
	close #h_batch:
	! /r
	execute 'sy -m "'&os_filename$(batchName$)&'"'
	Xit: !
fnend
include: fn_setup
