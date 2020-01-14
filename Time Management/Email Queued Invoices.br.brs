! Email Invoice
! This program emails queued pdf invoices as attachments
fn_setup
fntop(program$)
fn_emailEntryScreen
XIT: fnxit
def fn_emailEntryScreen
	dim resp$(1)*80
	fnTos
	rc=cf=pdfdate=0
	cf+=1 : fradate=cf : mylen=26 : mypos=mylen+2
	fnLbl(1,1,"Email Invoices Created On:",mylen,1,0,pdfdate)
	fnTxt(1,mypos,10,0,1,"3",0,today$,pdfdate)
	if use_date=0 then use_date=date('mmddyy') ! *10000+100+date('yy')
	resp$(rc+=1)=str$(use_date)
	! if trim$(use_date$)="" then use_date$=date("mm/dd/yy")
	! resp$(rc+=1)=use_date$
	fnCmdKey("Next",1,1,0,"Emails all PDF invoices created on this date")
	fnCmdKey("Cancel",5,0,1,"Returns to main menu")
	fnAcs2(mat resp$,ckey)
	if ckey=5 then 
		goto Tf_XIT
	else if ckey=1 then 
		fn_emailQueuedInvoices(resp$(1))
	end if 
	!
	Tf_XIT: ! continue
fnend
def library fnEmailQueuedInvoices(email_date$)
	if ~setup then let fn_setup
	fnEmailQueuedInvoices=fn_emailQueuedInvoices(email_date$)
fnend
def fn_emailQueuedInvoices(email_date$; ___,pdfname$*255,pdfline$*1000,ppos,ppos2,testday$)
	! this sends the emails that were printed as PDF's earlier
	! read log 
	fnmakesurepathexists(fnreport_cache_folder_current$&'\Ebilling\Sent\')
	execute "dir '"&fnreport_cache_folder_current$&"\Ebilling' >'"&fnreport_cache_folder_current$&"\Ebilling\sendingnow.txt' -B" 
	open #hList:=fngethandle: "name="&fnreport_cache_folder_current$&"\Ebilling\sendingnow.txt",display,input
	dim contact$(0)*255
	dim contactN(0)
	hContact=fn_open("TM Contact",mat contact$,mat contactN,mat form$, 1,1)
	
	! todo: implement fnGetDir2(dir$*256,mat filename$; option$,filter$*40,mat fileDate$,mat fileTime$,forceFullPath,mat fileSize)
	
	do while file(hList)=0
		linput #hList: pdfline$ eof EmailInvoiceFinis
		! if it exists then look up customer to information
		! pause 
		if pdfline$(1:7)="ACS Inv" then 
			pdfname$=pdfline$(1:len(pdfline$))
			ppos=pos(pdfname$,".")
			ppos2=pos(pdfname$,".",ppos+1)
			clientno$=trim$(pdfname$(ppos+1:ppos2-1))
			testday$=pdfname$(ppos2+1:pos(pdfname$,".",ppos2+1)-1)
			! if on selected date
			! print testday$ : print email_date$ : pause 
			if days(testday$,"mmddyy")=days(email_date$,"ccyymmdd") then 
				! print clientno$ : pause ! send emails
				restore #hContact,key=rpad$(Clientno$,5," "): nokey skipthis
				do while file(hContact)=0
					read #hContact,using form$(hContact): mat contact$,mat contactN eof ignore
					if trim$(clientno$)=trim$(contact$(con_clientid)) and contactN(con_emailbilling)=1 then
					
						dim emailBody$*1024
						emailBody$='<p>'
						emailBody$&=trim$(contact$(con_name))&",<br>Your invoice is attached to this email.</p>"
						emailBody$&='<p>Thanks for choosing for ebilling.  If you have any problems accessing or viewing your invoice, please call us.</p>'
						emailBody$&='<p>'
						emailBody$&='Sincerely,<br>'
						emailBody$&='Your ACS team!<br>'
						emailBody$&='<a href="http://advancedcomputer.services">Advanced Computer Services LLC</a><br>'
						emailBody$&='4 Syme Ave<br>'
						emailBody$&='West Orange, NJ 07052<br>'
						emailBody$&='1-800-643-6318</p>'
						
						dim attachment$*1024
						attachment$=fnreport_cache_folder_current$&'\'&trim$(pdfname$)
						dim tmpTo$*512
						tmpTo$=trim$(contact$(con_bemail))
						if fnSendEmail(tmpTo$,emailBody$,"ACS Invoice ",attachment$)>0 then 
							fnRename(attachment$,fnreport_cache_folder_current$&'\Sent\'&trim$(pdfname$))
						else
							dim mg$(0)*128
							mat mg$(0)
							fnAddOneC(mat mg$,'Email failed to send.')
							fnAddOneC(mat mg$,'Contact Name:'&tab$&triM$(contact$(con_name)))
							fnAddOneC(mat mg$,'Client ID:'&tab$&triM$(contact$(con_clientid)))
							fnAddOneC(mat mg$,'Billing Email:'&tab$&triM$(contact$(con_bemail)))
							fnAddOneC(mat mg$,'Attachment:')
							fnAddOneC(mat mg$,attachment$)
							fnAddOneC(mat mg$,'Please send manually.')
							fnMsgBox(mat mg$, mbResp$,'',mb_ok+mb_exclamation)
						end if 
					end if 
				loop while rpad$(clientno$,5," ")=rpad$(contact$(con_clientid),5," ")
				skipthis: ! no key 
			end if 
		end if 
	loop 
	EmailInvoiceFinis: ! close and done 
	close #hList: 
	fnRename(fnreport_cache_folder_current$&'\Ebilling\sendingnow.txt',fnreport_cache_folder_current$&'\Ebilling\Sent\sent'&date$("mmddyy")&time$(1:2)&time$(4:5)&time$(7:8)&'.txt')
fnend
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fnSendEmail
		library 'S:\Core\Library': fntop,fngethandle,fnmakesurepathexists
		library 'S:\Core\Library': fnTos,fnAcs2,fnCmdKey,fnFra,fnButton,fnChk,fnxit,fnlbl,fntxt
		library 'S:\Core\Library': fnAddOneC
		library 'S:\Core\Library': fnMsgBox
		library 'S:\Core\Library': fnreport_cache_folder_current$
		library 'S:\Core\Library': fnRename
		on error goto Ertn
		gosub Enum
	end if 
fnend
include: ertn
include: fn_open
include: enum
