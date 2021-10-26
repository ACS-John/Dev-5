autoLibrary
fnTop(program$)
on error goto Ertn

! def library fnSendEmail(mat toEmail$,emailMessage$*10000; subject$*256,attachFile$*256,mat ccEmail$,mat bccEmail$)
! fnend


	! r: make settings file
	dim settingsName$*256
	settingsName$='[temp]\acsSwithMail[acsuserid].xml'
	open #hSettings=fnH: 'Name='&batchName$&',RecL=256,replace',d,o
	pr #hSettings: '<?xml version="1.0" encoding="utf-8" standalone="yes"?>'
	pr #hSettings: '<SwithMailSettings>'
	pr #hSettings: '  <ServerSettings>'
	pr #hSettings: '    <FromName>ACS Billing</FromName>'
	pr #hSettings: '    <FromAddress>billing@advancedcomputer.services</FromAddress>'
	pr #hSettings: '    <Username>john@advancedcomputer.services</Username>'
	pr #hSettings: '    <Password>/mW/PMRZfu4o5Xgj403LWmAEDrsz0jhDFx4Ac52Tqig=</Password>'
	pr #hSettings: '    <ObscurePassword>True</ObscurePassword>'
	pr #hSettings: '    <MailServer>smtp.gmail.com</MailServer>'
	pr #hSettings: '    <MailServerPort>587</MailServerPort>'
	pr #hSettings: '    <SSL>True</SSL>'
	pr #hSettings: '    <Encoding />'
	pr #hSettings: '    <RequestReceipt>False</RequestReceipt>'
	pr #hSettings: '    <MessageID>False</MessageID>'
	pr #hSettings: '  </ServerSettings>'
	pr #hSettings: '  <EmailAddresses>'
	pr #hSettings: '    <To>niceguywinning@gmail.com</To>'
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
	pr #hSettings: '    <Subject>test subject</Subject>'
	pr #hSettings: '    <Body>&lt;p&gt;this is my &lt;b&gt;first&lt;/b&gt; email.&lt;/p&gt;'
	pr #hSettings: '</Body>'
	pr #hSettings: '    <BodyTxtFile />'
	pr #hSettings: '  </EmailContent>'
	pr #hSettings: '</SwithMailSettings>'
	close #hSettings:
	! /r

	! r: make batch file
	dim batchName$*256
	batchName$='[temp]\acsSwithMail[acsuserid].cmd'
	open #h_batch=fnH: 'Name='&batchName$&',RecL=256,replace',d,o
	pr #h_batch: 'prompt $p$g'
	pr #h_batch: '"'&os_filename$('S:\Core\SwithMail\SwithMail.exe')&'" /s /x "'&os_filename$(settingsName$)&'"'
	pr #h_batch: 'pause'
	close #h_batch:
	! /r
	execute 'sy -m "'&os_filename$(batchName$)&'"'
Xit: end ! fnXit
include: Ertn
