! after installing it just says I don't have a license for it.  lame.

fn_setup


library program$: fnSmtpMail
		open #hMsg:=fngethandle: "Name=[Temp]E-[SESSION].htm,EoL=None,Replace",DISPLAY,OUTPUT 



		pr #hMsg: '<form action="mailto:user@example.com" method="get">'
		pr #hMsg: '<input name="subject" type="hidden" value="Message Title">'
		pr #hMsg: ''
		pr #hMsg: 'Feedback:<br/>'
		pr #hMsg: '<textarea name=body cols="40">'
		pr #hMsg: 'Please share your thoughts here'
		pr #hMsg: 'and then choose Send Feedback.'
		pr #hMsg: '</textarea>'
		pr #hMsg: ''
		pr #hMsg: '<input type="submit" value="Send Feedback">'
		pr #hMsg: '</form>'


		! pr #hMsg: '<html>'
		! pr #hMsg: '  <head>'
		! pr #hMsg: '  </head>'
		! pr #hMsg: '  <body>'
		! pr #hMsg: '  <pre>'
		! pr #hMsg: '    preformatted text test'
		! pr #hMsg: '    This is some text     (five spaces)     and more text.'
		! pr #hMsg: '    This is other text.'
		! pr #hMsg: '  </pre>'
		! pr #hMsg: '    <hr>'
		! pr #hMsg: '    <p>that probably should have been a line</p>'
		! pr #hMsg: '  </body>'
		! pr #hMsg: '</html>'
		dim msgNotehtml$*128
		msgNotehtml$=fnClient_Os_Path$(File$(hMsg))
		pr 'msgNotehtml$="'&msgNotehtml$&'"'
		close #hMsg: 

fnSmtpMail('info@advancedcomputer.services','Test Subject',msgNotehtml$)
end
def fn_setup
	if ~setup then
		setup=1
		library "CLSUtil/Library": fnClient_System32$
		library "CLSUtil/Library": fnMessageBox
		library "CLSUtil/Library": fnExe
		library "CLSUtil/Library": fnGetinf$
		library "CLSUtil/Library": fnList_print
		library "CLSUtil/Library": fnGetHandle
		library "CLSUtil/Library": fnClient_Os_Path$
	end if
fnend
def library fnSmtpMail(recip$*128,subject$*128,msgNotehtml$*128;senderDisplayName$*128,recip_cc$*128)
	! SEND_EMAIL: !
	if ~setup then fn_setup
	if ~setupSmtpMail then
		setupSmtpMail=1
		dim smtp$*80
		dim bcc$*80
		dim reply$*80
		fn_readClsSetup(smtp$,bcc$,reply$)
		if smtp$='' then 
			smtp$='10.20.129.12'
			! smtp$='smtp.gmail.com'
			
			! port=25
			
			bcc$='niceguywinning@gmail.com'
			reply$='noreply@papernapkin.net'
		end if
	end if
	if ~exists(fnClient_System32$&"\ossmtp.ocx") Or ~Exists(fnClient_System32$&"\ossmtp.dll") then 
		fnMessageBox("Please Stand By, Automatically Installing & Registering the SMTP email Windows Control.\nThis should only happen 1 time per machine.",16,"Installing SMTPMAIL Email")
		fnExe('-w QUOTE=NONE','VBSetup\Install_SMTPMail.exe','')
	end if
	fnList_print("Sending email to "&recip$)

		open #19: "name=[Temp]E-[SESSION].INI,replace,recl=1024",DISPLAY,OUTPUT 
		pr #19: "ServerName = "&Smtp$ 
		pr #19: "RecipDisplayName = "&recip$
		pr #19: "RecipAddress = "&recip$
		pr #19: "MsgSubject = "&subject$
		pr #19: "MsgNoteHTML=";msgNotehtml$
		pr #19: "SenderDisplayName = "&senderDisplayName$ 
		pr #19: "SenderAddress = "&reply$
		pr #19: "BCCAddress = "&Bcc$ 
		pr #19: "CCAddress = "&recip_cc$
		dim iniFile$*128
		iniFile$=file$(19) 
		close #19: 

		fnExe('-w QUOTE=NONE','vb32\smtpmail.exe',iniFile$) 
	goto SmtpMailXit
	EmailReject: ! Reject EMAIL NO - TO ADDR
		pr 'EmailReject'
		pause
	goto SmtpMailXit
	SmtpMailXit: !
fnend
def fn_readClsSetup(&smtp$,&bcc$,&reply$)
	open #hClsSetup:=fngethandle: "name=clssetup//8,shr",internal,outin,relative ioerr ChainClsSetup
	read #hClsSetup,using 'form pos 316,v 60,v 60,v 60',rec=11: smtp$,bcc$,reply$
	close #hClsSetup: 
	goto ReadClsSetupFinis
	ChainClsSetup: !
		pr 'Failed Opening clsSetup//8. err='&str$(err)
		pr 'sending you to clsSetup 1-S-4-9 is where to set this stuff up.'
		pause
	chain "CLSSETUP/PROG1"
	ReadClsSetupFinis: !
fnend
