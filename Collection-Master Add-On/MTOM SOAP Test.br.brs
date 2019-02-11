fn_setup
! r: highly subjective setup
	dim mtomSoapUtil$*256
	mtomSoapUtil$=os_filename$(program$(1:pos(program$,'\',-1)-1))&'\mtomSoap.cmd'	! .exe'
	
	! bar number that can be used for test filing from the court: 25623
	filerBarNumber=25623  ! Kirk is 16805
	! filerBarNumber=16805 <-- Kirk 
	
	dim notificationEmail$*128
	notificationEmail$='jbowman@bqlaw.com'
	dim optNotificationEmail1$*128
	optNotificationEmail1$=''
	dim optNotificationEmail2$*128
	optNotificationEmail2$=''

	court$='County' ! or 'District'
	enableReplevin=0
! /r
fntop(program$)

! r: Menu
	do
		pk$='A-M'

		dim menuOption$(0)*128
		mat menuOption$(0)
		fnAddOneC(mat menuOption$,' List<Municipalities> listFilers();')
		fnAddOneC(mat menuOption$,' New      Non-Criminal Flow (4-8)') ! Always Civil cases
		fnAddOneC(mat menuOption$,' Existing Non-Criminal Flow (3,5-8)') ! Always Civil cases
		fnAddOneC(mat menuOption$,' >  loadExistingCase') ! Always Civil cases
		fnAddOneC(mat menuOption$,' >  CreateNewFiling')
		fnAddOneC(mat menuOption$,' >  addParty Plantiff')
		fnAddOneC(mat menuOption$,' >  addParty Defendant')
		fnAddOneC(mat menuOption$,' >  streamDocument')
		fnAddOneC(mat menuOption$,' >  submitFiling')

		choice=fnMenu(env$('program_caption'),Mat menuOption$, pk$,'John Bowman Services LLC') ! ,0,1) ! ; &Pk$, Title$*80, Footer$*80, Nnp, Autonumber,Mstart,Mat Custom_Menubar$, Mat Custom_Menuprg$, Mat Custom_Menustatus$,&Menu_Startup$,Menu_Offset,M_Timeout,M_Trust$*30,M_F93_Enable)

		choiceWalker=0
		if choice=(choiceWalker+=1)  then ! list<municipalities>
			fn_listMunicipalities
		else if choice=(choiceWalker+=1) then ! New      Non-Criminal Flow (4-8)
			fn_createNewFiling
			fn_addParty_Plantiff
			fn_addParty_Defendant
			fn_streamDocument
			fn_submitFiling
		else if choice=(choiceWalker+=1) then ! Existing Non-Criminal Flow (3,5-8)
			fn_loadExistingCase
			fn_createNewFiling
			fn_addParty_Plantiff
			fn_addParty_Defendant
			fn_streamDocument
			fn_submitFiling
		else if choice=(choiceWalker+=1)  then ! loadExistingCase
			fn_loadExistingCase
		else if choice=(choiceWalker+=1)  then ! CreateNewFiling
			fn_createNewFiling
		else if choice=(choiceWalker+=1)  then ! addParty Plantiff
			fn_addParty_Plantiff
		else if choice=(choiceWalker+=1)  then ! addParty Defendant
			fn_addParty_Defendant
		else if choice=(choiceWalker+=1)  then ! streamDocument
			fn_streamDocument
		else if choice=(choiceWalker+=1)  then ! submitFiling
			fn_submitFiling
		else
			fnXit
		end if
	loop
! /r
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library.br': fngethandle
		library 'S:\Core\Library.br': fnCountMatchesC
		library 'S:\Core\Library.br': fnMsgBox
		library 'S:\Core\Library.br': fnAddOneC
		library 'S:\Core\Library.br': fntop
		library 'S:\Core\Library.br': fnXit

		library "library\CLSUtil.wb": fnGetInf$
		library "library\CLSUtil.wb": fncom
		library "library\CLSUtil.wb": fnget_formall$,fnget_formarr
		library "library\CLSUtil.wb": fnreport_path$,fnclaim_path$
		library "library\CLSUtil.wb": fnget_claimfiles,fnclaim_scroll
		library "library\CLSUtil.wb": fnrange_to_array,fnarray_to_range$
		library "library\CLSUtil.wb": fnfix_bh,fnask_payref
		library "library\CLSUtil.wb": fnget_form
		library "library\CLSUtil.wb": fnunpack$
		library "library\CLSUtil.wb": fnStime,fnStime$
		library "library\CLSUtil.wb": fnMessageBox
		library "library\CLSUtil.wb": Fnlist_Print
		library "Prog2\Mast2.wb": fnsql_read

		library "library\CLSUtil.wb": fnfix_bh
		library "Prog2\Mast_SQL.wb": fnmast2_int_cache
		library "library\CLSUtil.wb": fnAsk_file1
		! LIBRARY "Toolbar/Library": Fnsubmenu
		library "library\CLSUtil.wb": fnmenu
	end if
fnend

def fn_mtomSoapOpen(; efilOpen$*128)
	dim outFile$*256
	outFile$=os_filename$(env$('temp'))&'\mtomSoapOut'&session$&'.xml'
	open #hOut:=fngethandle: 'name='&outFile$&',recl=2048,replace',d,o
	pr #hOut: 'Nonce='&str$(days(date$))&srep$(time$,':','')&str$(int(rnd*9999990300))&str$(int(rnd*9999990300))
	if efilOpen$<>'' then 
		fn_prOutXmlEfil_open(efilOpen$)
	end if
fnend
def fn_mtomSoapFinis
	pr #hout: '</efil:'&efilCache$&'>'
	close #hOut:

	! dim mtomSoapSettings$*256
	! mtomSoapSettings$=os_filename$(program$(1:pos(program$,'\',-1)-1))&'\settings.ini'
	dim replyFile$*256
	replyFile$=outFile$(1:pos(outFile$,'.',-1)-1)&'-reply'&outFile$(pos(outFile$,'.',-1):inf)

	dim mtomSoapCallFile$*256
	mtomSoapCallFile$=os_filename$(env$('temp'))&'\call'&session$&'.cmd'
	open #hCall:=fngethandle: 'name='&mtomSoapCallFile$&',recl=4096,replace',d,o
	pr #hCall: 'prompt $p$g'
	! pr #hCall: '"'&mtomSoapUtil$&'" /settings="'&mtomSoapSettings$&'" /in="'&outFile$&'" /out="'&replyFile$&'"'
	pr #hCall: '"'&mtomSoapUtil$&'" /in="'&outFile$&'" /out="'&replyFile$&'"'
	close #hCall:

	execute 'sy "'&mtomSoapCallFile$&'"'
	! execute 'sy -C "'&outFile$&'"'
	! execute 'sy -C "'&mtomSoapSettings$&'"'
	execute 'sy -C "'&replyFile$&'"'

fnend
dim efilCache$*128
def fn_prOutXmlEfil_open(value$*128)
	efilCache$=value$
	pr #hout: '<efil:'&value$&'>'
fnend
def fn_prOutXmlItem(encap$*128,value$*128; optional)
	if ~optional or trim$(value$)<>'' then
		if optional then
			pr #hOut: '<!--Optional:-->'
		end if
		pr #hout: '<'&encap$&'>'&value$&'</'&encap$&'>'
	end if
fnend
! r: APIs('
def fn_listMunicipalities
	pr #hOut: 'List<Municipalities> listFilers();'
	fn_mtomSoapOpen('ListMunicipalities')
	! pr #hOut: 'List<Municipalities> listFilers();'
	! pr #hOut: 'List<Municipalities> listFilers();'
	fn_mtomSoapFinis
fnend
def fn_loadExistingCase
	fn_mtomSoapOpen('loadExistingCase')
	fn_prOutXmlItem('caseNumber'         ,'C99CR160000004')
	fn_prOutXmlItem('filerBarNumber'     ,str$(filerBarNumber))
	fn_prOutXmlItem('hearingDate'        ,'2018-11-05T13:00:00Z', 1)
	fn_prOutXmlItem('notificationEmail'  ,notificationEmail$)
	fn_optNotificationEmails
	fn_prOutXmlItem('custMemo'           ,'This is a sample customer memo', 1)
	fn_prOutXmlItem('dssNumber'          ,'', 1)
	fn_mtomSoapFinis
fnend
def fn_createNewFiling
	fn_mtomSoapOpen('createNewFiling')
	fn_prOutXmlItem('court'              ,court$)
	fn_prOutXmlItem('county'             ,'1')
	fn_prOutXmlItem('caseType'           ,'CI')
	fn_caseSubType
	fn_prOutXmlItem('filerBarNumber'     ,str$(filerBarNumber))
	fn_prOutXmlItem('notificationEmail'  ,notificationEmail$)
	fn_optNotificationEmails
	fn_prOutXmlItem('custMemo'           ,'', 1)
	fn_prOutXmlItem('dssNumber'          ,'', 1)
	fn_mtomSoapFinis
fnend
def fn_addParty_Plantiff
	fn_addParty
fnend
def fn_addParty_Defendant
	fn_addParty
fnend
def fn_addParty
	fn_mtomSoapOpen('addParty')
	fn_prOutXmlItem('filingId'           ,'')
	fn_prOutXmlItem('partyRole'          ,'')
	fn_prOutXmlItem('partyType'          ,'')
	fn_prOutXmlItem('firstName'          ,'')
	fn_prOutXmlItem('lastName'           ,'')
	fn_prOutXmlItem('middleInitial'     ,'')
	fn_prOutXmlItem('suffix'             ,'')
	fn_prOutXmlItem('gender'             ,'')
	fn_prOutXmlItem('businessName'       ,'')
	fn_prOutXmlItem('SSNFTIN'            ,'')
	fn_prOutXmlItem('aka1type'           ,'')
	fn_prOutXmlItem('aka1'               ,'')
	fn_prOutXmlItem('aka2type'           ,'')
	fn_prOutXmlItem('aka2'               ,'')
	fn_prOutXmlItem('address1'           ,'')
	fn_prOutXmlItem('address2'           ,'')
	fn_prOutXmlItem('city'               ,'')
	fn_prOutXmlItem('state'              ,'')
	fn_prOutXmlItem('zipCode'            ,'')
	fn_prOutXmlItem('homePhone'          ,'')
	fn_prOutXmlItem('dateOfBirth'        ,'')
	fn_prOutXmlItem('dateOfDeath'        ,'')
	fn_mtomSoapFinis
fnend
def fn_streamDocument
	fn_mtomSoapOpen('streamDocument')
	fn_prOutXmlItem('filingId'           ,'')
	fn_prOutXmlItem('docType'            ,'')
	! r: add sample PDF file    ! DataHandler pdfFile,
	dim tmpLine$*4096
	open #hTmp:=fngethandle: 'name='&program$(1:pos(program$,'\',-1))&'mtom soap sample 1.pdf,Shr',d,i
	do
		linput #hTmp: tmpLine$ eof SdTmpEof
		pr #hOut: tmpLine$
	loop
	SdTmpEof: !
	close #hTmp: 
	! /r
	fn_prOutXmlItem('filedFor'           ,'')
	fn_prOutXmlItem('docNumber'          ,'')
	fn_prOutXmlItem('serviceMethod'      ,'')
	fn_prOutXmlItem('serviceFees'        ,'')
	fn_prOutXmlItem('serviceInstructions','')
	fn_prOutXmlItem('serviceDate'        ,'')
	fn_prOutXmlItem('judgmentDebto'      ,'')
	fn_mtomSoapFinis
fnend
def fn_submitFiling
	fn_mtomSoapOpen('submitFiling')
	fn_prOutXmlItem('filingId'           ,'')
	fn_prOutXmlItem('appearanceDate'     ,'')
	fn_prOutXmlItem('commentsForCler'    ,'')
	fn_mtomSoapFinis
fnend
! /r
! r: API Parameters
def fn_optNotificationEmails
	fn_prOutXmlItem('optionalNotificationEmail1',optNotificationEmail1$, 1)
	fn_prOutXmlItem('optionalNotificationEmail1',optNotificationEmail1$, 1)
fnend
def fn_caseSubType
	!   for us     CNTRUNSP  99% 1 ! county
	!   for us     CONTRACT  99%   ! district
	!   for us     CNTRPVN    1%   only in cases of replevin
	if enableReplevin then
		fn_prOutXmlItem('caseSubType','CNTRPVN', 1)
	else if court$='County' then
		fn_prOutXmlItem('caseSubType','CNTRUNSP', 1)
	else if court$='District' then
		fn_prOutXmlItem('caseSubType','CONTRACT', 1)
	end if
fnend
! /r
