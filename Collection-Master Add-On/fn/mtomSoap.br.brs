pr 'this program was not intended to be run directly.' : stop
!  fn_setup
!  ! r: highly subjective setup
!  	
!  	! bar number that can be used for test filing from the court: 25623
!  	filerBarNumber=25623  ! Kirk is 16805
!  	! filerBarNumber=16805 <-- Kirk 
!  	
!  	dim notificationEmail$*128
!  	notificationEmail$='jbowman@bqlaw.com'
!  	dim optNotificationEmail1$*128
!  	optNotificationEmail1$=''
!  	dim optNotificationEmail2$*128
!  	optNotificationEmail2$=''
!  
!  	court$='County' ! or 'District'
!  	enableReplevin=0
!  ! /r
!  fntop(program$)
!  
!  ! r: Menu
!  	do
!  		pk$='A-M'
!  
!  		dim menuOption$(0)*128
!  		mat menuOption$(0)
!  		fnAddOneC(mat menuOption$,' List<Municipalities> listFilers();')
!  		fnAddOneC(mat menuOption$,' New      Non-Criminal Flow (4-8)') ! Always Civil cases
!  		fnAddOneC(mat menuOption$,' Existing Non-Criminal Flow (3,5-8)') ! Always Civil cases
!  		fnAddOneC(mat menuOption$,' >  loadExistingCase') ! Always Civil cases
!  		fnAddOneC(mat menuOption$,' >  CreateNewFiling')
!  		fnAddOneC(mat menuOption$,' >  addParty Plantiff')
!  		fnAddOneC(mat menuOption$,' >  addParty Defendant')
!  		fnAddOneC(mat menuOption$,' >  streamDocument')
!  		fnAddOneC(mat menuOption$,' >  submitFiling')
!  
!  		choice=fnMenu(env$('program_caption'),Mat menuOption$, pk$,'John Bowman Services LLC') ! ,0,1) ! ; &Pk$, Title$*80, Footer$*80, Nnp, Autonumber,Mstart,Mat Custom_Menubar$, Mat Custom_Menuprg$, Mat Custom_Menustatus$,&Menu_Startup$,Menu_Offset,M_Timeout,M_Trust$*30,M_F93_Enable)
!  
!  		choiceWalker=0
!  		if choice=(choiceWalker+=1)  then ! list<municipalities>
!  			fn_listMunicipalities
!  		else if choice=(choiceWalker+=1) then ! New      Non-Criminal Flow (4-8)
!  			fn_createNewFiling
!  			fn_addParty ! _Plantiff
!  			fn_addParty ! _Defendant
!  			fn_streamDocument
!  			fn_submitFiling
!  		else if choice=(choiceWalker+=1) then ! Existing Non-Criminal Flow (3,5-8)
!  			fn_loadExistingCase('C99CR160000004',str$(filerBarNumber),'2018-11-05T13:00:00Z',notificationEmail$, 'This is a sample customer memo')
!  			fn_createNewFiling
!  			fn_addParty ! _Plantiff
!  			fn_addParty ! _Defendant
!  			fn_streamDocument
!  			fn_submitFiling
!  		else if choice=(choiceWalker+=1)  then ! loadExistingCase
!  			fn_loadExistingCase('C99CR160000004',str$(filerBarNumber),'2018-11-05T13:00:00Z',notificationEmail$, 'This is a sample customer memo')
!  		else if choice=(choiceWalker+=1)  then ! CreateNewFiling
!  			fn_createNewFiling
!  		else if choice=(choiceWalker+=1)  then ! addParty Plantiff
!  			fn_addParty ! _Plantiff
!  		else if choice=(choiceWalker+=1)  then ! addParty Defendant
!  			fn_addParty ! _Defendant
!  		else if choice=(choiceWalker+=1)  then ! streamDocument
!  			fn_streamDocument
!  		else if choice=(choiceWalker+=1)  then ! submitFiling
!  			fn_submitFiling
!  		else
!  			fnXit
!  		end if
!  	loop
!  ! /r
def fn_setup
	if ~setup then
		setup=1
		
		dim folderIn$*256
		folderIn$='C:\clsinc\MTOM SOAP\In'
		dim folderOut$*256
		folderOut$='C:\clsinc\MTOM SOAP\Out'
		dim mtomSoapUtil$*256
		mtomSoapUtil$=os_filename$(program$(1:pos(program$,'\',-1)-1))&'\mtomSoap.cmd'	! .exe'
		
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

def fn_mtomSoapOpen(method$*128)
	dim reqFile$*256
	reqFile$=folderIn$&'\mtomSoap'&session$&'-Request.xml'
	open #hOut:=fngethandle: 'name='&reqFile$&',recl=2048,replace',d,o
	pr #hout: '<info>'
	pr #hout: '  <method>'
	pr #hout: '    '&method$
	pr #hout: '  </method>'
	pr #hout: '  <parameterList>'
fnend
def fn_mtomSoapFinis
	! pr #hout: '</efil:'&efilCache$&'>'
	pr #hout: '  </parameterList>'
	pr #hout: '</info>'
	close #hOut:

	! dim mtomSoapSettings$*256
	! mtomSoapSettings$=os_filename$(program$(1:pos(program$,'\',-1)-1))&'\settings.ini'
	dim replyFile$*256
	replyFile$=reqFile$(1:pos(reqFile$,'.',-1)-1)&'-reply'&reqFile$(pos(reqFile$,'.',-1):inf)

	dim mtomSoapCallFile$*256
	mtomSoapCallFile$=os_filename$(env$('temp'))&'\call'&session$&'.cmd'
	open #hCall:=fngethandle: 'name='&mtomSoapCallFile$&',recl=4096,replace',d,o
	pr #hCall: 'prompt $p$g'
	! pr #hCall: '"'&mtomSoapUtil$&'" /settings="'&mtomSoapSettings$&'" /in="'&reqFile$&'" /out="'&replyFile$&'"'
	! pr #hCall: '"'&mtomSoapUtil$&'" /in="'&fn_filenameOnly$(reqFile$)&'" /out="'&fn_filenameOnly$(replyFile$)&'"'
	pr #hCall: '"'&mtomSoapUtil$&'" /in="'&reqFile$&'" /out="'&replyFile$&'"'
	close #hCall:
	pause
	execute 'sy "'&mtomSoapCallFile$&'"'
	! execute 'sy -C "'&reqFile$&'"'
	! execute 'sy -C "'&mtomSoapSettings$&'"'
	execute 'sy -C "'&replyFile$&'"'

fnend
def fn_filenameOnly$*128(fullpath$*1024)
	fn_filenameOnly$=fullpath$(pos(fullpath$,'\',-1)+1:inf)
fnend
! dim efilCache$*128
def fn_prOutXmlItem(encap$*128,value$*128; optional)
	if ~optional or trim$(value$)<>'' then
		! if optional then
		! 	pr #hOut: '<!--Optional:-->'
		! end if
		pr #hout: '    <'&encap$&'>'&value$&'</'&encap$&'>'
	end if
fnend
! r: APIs('
def library fnListMunicipalities
	if ~setup then let fn_setup
	fnListMunicipalities=fn_listMunicipalities
fnend
def fn_listMunicipalities
	! pr #hOut: 'List<Municipalities> listFilers();'
	fn_mtomSoapOpen('ListMunicipalities')
	! pr #hOut: 'List<Municipalities> listFilers();'
	! pr #hOut: 'List<Municipalities> listFilers();'
	fn_mtomSoapFinis
fnend

def library fnLoadExistingCase(caseNumber$,fileBarNumber$,hearingDate$,notificationEmail$; custMemo$*256,dssNumber$)
	if ~setup then let fn_setup
	fnLoadExistingCase=fn_loadExistingCase(caseNumber$,fileBarNumber$,hearingDate$,notificationEmail$, custMemo$,dssNumber$)
fnend
def fn_loadExistingCase(caseNumber$,fileBarNumber$,hearingDate$,notificationEmail$*256; custMemo$*256,dssNumber$)
	fn_mtomSoapOpen('loadExistingCase')
	fn_prOutXmlItem('caseNumber'         ,caseNumber$)
	fn_prOutXmlItem('filerBarNumber'     ,fileBarNumber$)
	fn_prOutXmlItem('hearingDate'        ,hearingDate$, 1)
	fn_prOutXmlItem('notificationEmail'  ,notificationEmail$)
	fn_optNotificationEmails
	fn_prOutXmlItem('custMemo'           ,custMemo$, 1)
	fn_prOutXmlItem('dssNumber'          ,dssNumber$, 1)
	fn_mtomSoapFinis
fnend
def library fnCreateNewFiling(court$,countyNumber$,caseType$,fileBarNumber$,notificationEmail$*256; custMemo$*256,dssNumber$)
	if ~setup then let fn_setup
	fnCreateNewFiling=fn_createNewFiling(court$,countyNumber$,caseType$,fileBarNumber$,notificationEmail$, custMemo$,dssNumber$)
fnend
def fn_createNewFiling(court$,countyNumber$,caseType$,fileBarNumber$,notificationEmail$*256; custMemo$*256,dssNumber$)
	fn_mtomSoapOpen('createNewFiling')
	fn_prOutXmlItem('court'              ,court$)
	fn_prOutXmlItem('county'             ,countyNumber$)
	fn_prOutXmlItem('caseType'           ,caseType$)
	fn_caseSubType
	fn_prOutXmlItem('filerBarNumber'     ,fileBarNumber$)
	fn_prOutXmlItem('notificationEmail'  ,notificationEmail$)
	fn_optNotificationEmails
	fn_prOutXmlItem('custMemo'           ,custMemo$, 1)
	fn_prOutXmlItem('dssNumber'          ,dssNumber$, 1)
	fn_mtomSoapFinis
fnend
def library fnAddParty(; filingId$,partyRole$,partyType$,firstName$,lastName$,middleInitial$,suffix$,gender$,businessName$,SSNFTIN$,aka1type$,aka1$,aka2type$,aka2$,address1$,address2$,city$,state$,zipCode$,homePhone$,dateOfBirth$,dateOfDeath$)
	if ~setup then let fn_setup
	fnAddParty=fn_addParty( filingId$,partyRole$,partyType$,firstName$,lastName$,middleInitial$,suffix$,gender$,businessName$,SSNFTIN$,aka1type$,aka1$,aka2type$,aka2$,address1$,address2$,city$,state$,zipCode$,homePhone$,dateOfBirth$,dateOfDeath$)
fnend
def fn_addParty(; filingId$,partyRole$,partyType$,firstName$,lastName$,middleInitial$,suffix$,gender$,businessName$,SSNFTIN$,aka1type$,aka1$,aka2type$,aka2$,address1$,address2$,city$,state$,zipCode$,homePhone$,dateOfBirth$,dateOfDeath$)
	fn_mtomSoapOpen('addParty')
	fn_prOutXmlItem('filingId'          	,filingId$)
	fn_prOutXmlItem('partyRole'         	,partyRole$)
	fn_prOutXmlItem('partyType'         	,partyType$)
	fn_prOutXmlItem('firstName'         	,firstName$)
	fn_prOutXmlItem('lastName'          	,lastName$)
	fn_prOutXmlItem('middleInitial'    	,middleInitial$)
	fn_prOutXmlItem('suffix'            	,suffix$)
	fn_prOutXmlItem('gender'            	,gender$)
	fn_prOutXmlItem('businessName'     	,businessName$)
	fn_prOutXmlItem('SSNFTIN'           	,SSNFTIN$)
	fn_prOutXmlItem('aka1type'          	,aka1type$)
	fn_prOutXmlItem('aka1'              	,aka1$)
	fn_prOutXmlItem('aka2type'          	,aka2type$)
	fn_prOutXmlItem('aka2'              	,aka2$)
	fn_prOutXmlItem('address1'          	,address1$)
	fn_prOutXmlItem('address2'          	,address2$)
	fn_prOutXmlItem('city'              	,city$)
	fn_prOutXmlItem('state'             	,state$)
	fn_prOutXmlItem('zipCode'           	,zipCode$)
	fn_prOutXmlItem('homePhone'         	,homePhone$)
	fn_prOutXmlItem('dateOfBirth'      	,dateOfBirth$)
	fn_prOutXmlItem('dateOfDeath'      	,dateOfDeath$)
	fn_mtomSoapFinis
fnend

def library fnStreamDocument(filingId$,docType$,pdfFile$*1024; filedFor$,docNumber$,serviceMethod$,serviceFees$,serviceInstructions$,serviceDate$,judgmentDebtor$)
	if ~setup then let fn_setup
	fnStreamDocument=fn_streamDocument(filingId$,docType$,pdfFile$, filedFor$,docNumber$,serviceMethod$,serviceFees$,serviceInstructions$,serviceDate$,judgmentDebtor$)
fnend
def fn_streamDocument(filingId$,docType$,pdfFile$*1024; filedFor$,docNumber$,serviceMethod$,serviceFees$,serviceInstructions$,serviceDate$,judgmentDebtor$)
	fn_mtomSoapOpen('streamDocument')
	fn_prOutXmlItem('filingId'            		,filingId$             )
	fn_prOutXmlItem('docType'             		,docType$              )
	pr #hOut: '<pdfFilePath>'&pdfFile$&'</pdfFilePath>'
	fn_prOutXmlItem('filedFor'            		,filedFor$             )
	fn_prOutXmlItem('docNumber'           		,docNumber$            )
	fn_prOutXmlItem('serviceMethod'       		,serviceMethod$       )
	fn_prOutXmlItem('serviceFees'         		,serviceFees$         )
	fn_prOutXmlItem('serviceInstructions'		,serviceInstructions$ )
	fn_prOutXmlItem('serviceDate'         		,serviceDate$         )
	fn_prOutXmlItem('judgmentDebtor'     		,judgmentDebtor$      )
	fn_mtomSoapFinis
fnend

def library fnSubmitFiling(filingI$,appearanceDate$; commentsForCler$*1024)
	if ~setup then let fn_setup
	fnSubmitFiling=fn_submitFiling(filingI$,appearanceDate$, commentsForCler$)
fnend
def fn_submitFiling(filingI$,appearanceDate$; commentsForCler$*1024)
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
