pr 'this program was not intended to be run directly.' : stop
def fn_setup
	if ~setup then
		setup=1
		
		dim folderIn$*256
		folderIn$='C:\clsinc\MTOM SOAP\In'
		dim folderOut$*256
		folderOut$='C:\clsinc\MTOM SOAP\Out'
		dim mtomSoapUtil$*256
		mtomSoapUtil$=os_filename$('Collection-Master Add-On\mtomSoap.cmd') ! os_filename$(program$(1:pos(program$,'\',-1)-1))&'\mtomSoap.cmd'	!  exe'
		
		autoLibrary

	end if
fnend

def fn_mtomSoapOpen(method$*128)
	dim reqFile$*256
	reqFile$=folderIn$&'\mtomSoap'&session$&'-Request.xml'
	open #hOut=fnH: 'name='&reqFile$&',recl=2048,replace',d,o
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
	fnFree(replyFile$)
	dim mtomSoapCallFile$*256
	mtomSoapCallFile$=os_filename$(env$('temp'))&'\call'&session$&'.cmd'
	open #hCall=fnH: 'name='&mtomSoapCallFile$&',recl=4096,replace',d,o
	pr #hCall: 'prompt $p$g'
	! pr #hCall: '"'&mtomSoapUtil$&'" /settings="'&mtomSoapSettings$&'" /in="'&reqFile$&'" /out="'&replyFile$&'"'
	! pr #hCall: '"'&mtomSoapUtil$&'" /in="'&fn_filenameOnly$(reqFile$)&'" /out="'&fn_filenameOnly$(replyFile$)&'"'
	pr #hCall: '"'&mtomSoapUtil$&'" /in="'&reqFile$&'" /out="'&replyFile$&'"'
	close #hCall:
	! exec 'type '&mtomSoapCallFile$ : pause
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
	if ~setup then fn_setup
	fnListMunicipalities=fn_listMunicipalities
fnend
def fn_listMunicipalities
	! pr #hOut: 'List<Municipalities> listFilers();'
	fn_mtomSoapOpen('ListMunicipalities')
	! pr #hOut: 'List<Municipalities> listFilers();'
	! pr #hOut: 'List<Municipalities> listFilers();'
	fn_mtomSoapFinis
fnend

def library fnLoadExistingCase(caseNumber$,fileBarNumber$,hearingDate$*64,notificationEmail$; custMemo$*256,dssNumber$)
	if ~setup then fn_setup
	fnLoadExistingCase=fn_loadExistingCase(caseNumber$,fileBarNumber$,hearingDate$,notificationEmail$, custMemo$,dssNumber$)
fnend
def fn_loadExistingCase(caseNumber$,fileBarNumber$,hearingDate$*64,notificationEmail$*256; custMemo$*256,dssNumber$)
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
	if ~setup then fn_setup
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
	if ~setup then fn_setup
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
	if ~setup then fn_setup
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
	if ~setup then fn_setup
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
	! these are from page 25 under the section (starting on page 24) titled: Appendix 5 Case Subtypes/Jurisdiction Mapping
	!      of the document NebraskaEFilingWEDocs.docx
	if enableReplevin then
		fn_prOutXmlItem('caseSubType','CNTRPVN', 1) ! Contract-Replevin
	else if court$='County' then
		fn_prOutXmlItem('caseSubType','CNTRUNSP', 1) ! Contract-Unspecified
	else if court$='District' then
		fn_prOutXmlItem('caseSubType','CONTRACT', 1)
	end if
fnend
! /r
