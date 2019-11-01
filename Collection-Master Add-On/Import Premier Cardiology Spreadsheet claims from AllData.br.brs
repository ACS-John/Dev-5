on error goto Ertn
fn_setup
fnTop(program$)
fn_premierCardiologyImport
fnXit
def library fnPremierCardiologyImport(; sourceId$)
	if ~setup then let fn_setup
	fnPremierCardiologyImport=fn_premierCardiologyImport( sourceId$)
fnend
def  fn_premierCardiologyImport(; sourceId$)
if sourceId$='' then sourceId$='AllData'
! r: main flow
Screen1: !
	dim csvFile$*512
	fnreg_read(env$('program_caption')&'.csvFile',csvFile$, 'D:\CM\Stern and Stern\New_format_Premier_Cardiology.xlsx')
	fnreg_read(env$('program_caption')&'.starting fileno',sFileNo$,'PCE01001')
	fnreg_read(env$('program_caption')&'.forw no',forwNo$,'1617')
	fnreg_read(env$('program_caption')&'.enableImport',enableImport$) : if enableImport$='True' then enableImport=1 else enableImport=0
	fnreg_read(env$('program_caption')&'.priorityColumn',priorityColumn$)
	fnreg_read(env$('program_caption')&'.priorityText',priorityText$)
	fnreg_read(env$('program_caption')&'.priorityDiaryCode',priorityDiaryCode$)
	if fn_askScreen1(csvFile$,sFileNo$,forwNo$,enableImport,enableImport$,priorityColumn$,priorityText$,priorityDiaryCode$)=99 then
		goto PciXit
	else
		fnreg_write(env$('program_caption')&'.csvFile',csvFile$)
		fnreg_write(env$('program_caption')&'.starting fileno',sFileNo$)
		fnreg_write(env$('program_caption')&'.forw no',forwNo$)
		fnreg_write(env$('program_caption')&'.enableImport',enableImport$)
		fnreg_write(env$('program_caption')&'.priorityColumn',priorityColumn$)
		fnreg_write(env$('program_caption')&'.priorityText',priorityText$)
		fnreg_write(env$('program_caption')&'.priorityDiaryCode',priorityDiaryCode$)



		! r: gather Forwarder data
		if ~hForw or file(hForw)=-1 then
			open #hForw:=fnGetHandle: "name=MASFORW//8,shr",internal,outin,relative ! MASFORW//8 means COMMON\MASFORW
		end if
		read #hForw,using forwFormAll$,rec=val(forwNo$): mat forw$,mat forwN noRec Screen1
		close #hForw:
		str2mat(forw$(forw_city_state),mat cred_cs$, ',')
		if udim(mat cred_cs$)<>2 then 
			pr 'udim(mat cred_cs$)<>2, udim(mat cred_cs$)=';udim(mat cred_cs$);' that is a problem.'
			pause
		end if
		cred_cs$(1)=trim$(cred_cs$(1))&','
		! /r


		dim csvOrigional$*1024
		csvOrigional$=csvFile$
		fnCopy(csvFile$,csvFile$&'.bak')
		csvFile$=fn_removeExcessCRLF$(csvFile$)
		fn_readFileIntoArrays

		hIn=fn_init_csv_in(csvFieldCount,csvFile$)
		if csv_patientId=0 then
			pr 'invalid file.  No "Patient ID" column.';bell
			pause
			goto PciXit
		end if
		
		dim csvPath$*256
		dim csvProg$*256
		dim csvExt$*128
		fnGetPp(csvFile$,csvPath$,csvProg$,csvExt$)
		dim outFile$*256
		outFile$=env$('at')&srep$(csvPath$&csvProg$&'-CM_EDI'&'.csv','@::','')
		hOut=fnGetHandle
include: filenamesPushMixedCase
		open #hOut: 'name='&outFile$&',recl=1024,replace',display,output
include: filenamesPopUpperCase
		fn_pr_hOut('0[tab]H[tab]This file is "'&os_filename$(outFile$)&'"'	)
		fn_pr_hOut('0[tab]H[tab]This file was made by the "'&env$('program_caption')&'" (a Collection-Master Add-On program) on '&date$('mm/dd/ccyy')&' at '&time$&'.'	)
		fn_pr_hOut('0[tab]H[tab]This file was made from the source file: "'&csvFile$&'".'	)
		fn_pr_hOut('0[tab]H[tab]from "'&os_filename$(csvOrigional$)&'"')
		lineCount=0
		do
			dim line$*1024
			linput #hIn: line$ eof Finis
			lineCount+=1
			if line$<>'' and line$<>'"' then
				dim item$(0)*512
				str2mat(line$,mat item$,tab$)
				mat item$(csvFieldCount)
				item$(csv_fileNo)=fn_getFileNo$(forwNo$,item$(csv_patientId))
				! if item$(csv_fileNo)<>list_fileNo$(lineCount) then 
				! 	pr 'item fileno=';item$(csv_fileNo)
				! 	pr 'list fileno=';list_fileNo$(lineCount)
				! 	pause ! 1124
				! end if
				item$(csv_patientName)=srep$(item$(csv_patientName),', ',',')
				item$(csv_respName)=srep$(item$(csv_respName),', ',',')
				item$(csv_patientName)=srep$(item$(csv_patientName),',','/')
				item$(csv_respName)=srep$(item$(csv_respName),',','/')
				
				fn_pr_hOut('0[tab]H[tab] XXX lineCount: '&str$(lineCount)	&' XXX fileNo: '&item$(csv_fileNo)&' XXX')
				item$(csv_invoiceNo)=item$(csv_claimId)&'-'&date$(days(item$(csv_serviceDate),'mm/dd/ccyy'),'ccyymmdd')
	
				fn_writeDemographics(hOut,mat item$)
				fn_writePaperless(hOut,mat item$,lineCount,csvFile$)
				fn_writeInvoices(hOut,mat item$)
				fn_writeInfinity(hOut,mat item$)
				mat2str(mat item$,line$,tab$)
				! print #hOut: line$
			end if
		loop
	end if
! /r
Finis: ! 
	fn_pr_hOut('0[tab]H[tab]lineCount='&str$(lineCount))
	fn_close_csv_in( 1)
	close #hOut:
	if enableImport then
		! r: Generate Automation Files
		open #hIni:=fnGetHandle: 'name=custom\cm_edi_pcs.ini,replace',d,o
		pr #hIni: '||MENUPATH:4-2-1-1'
		pr #hIni: 'EDI NUMBER=326'
		pr #hIni: 'FILE NUMBER='&sFileNo$
		pr #hIni: 'SOURCE FILE='&outFile$
		pr #hIni: 'UNASSIGNED FORW='&forwNo$
		close #hIni:

		open #hCmd:=fnGetHandle: 'name=batch\cm_edi_pcs.cmd,replace',d,o
		pr #hCmd: 'f:'
		pr #hCmd: 'cd \clsinc'
		pr #hCmd: 'set Automate=cm_edi_pcs.ini'
		pr #hCmd: os_filename$(env$('at')&'f:\clsinc\wbwin\br32.exe') ! brclient.exe
		close #hCmd:
		! setenv('Automate','cm_edi_pcs.ini')
		! /r
		! exec 'proc run'
		execute 'sy -c -M F:\clsinc\batch\cm_edi_pcs.cmd'
	else
		dim mbText$*2048
		mbText$='New Claims: '&tab$&str$(countNewClaim) 
		mbText$(inf:inf)=lf$&'Claim Updates: '&tab$&str$(countUpdateClaim) 
		mbText$(inf:inf)=lf$&'Sucessfully created a file for CM EDI Import:'
		mbText$(inf:inf)=lf$&outFile$
		fnMessageBox(mbText$,mb_information+mb_okonly,env$('program_caption'))
		! msgbox('Success on '&csvFile$)
	end if
	goto PciXit ! /r
	PciXit: !
fnend



def fn_askScreen1(&sourceFile$,&sFileNo$,&forwNo$,&enableImport,&enableImport$,&priorityColumn$,&priorityText$,&priorityDiaryCode$; ___,returnN,rc,lc)
	! function returns 99 if Cancel
	IF ~Ask_File1_Setup then
		dim Af1_Data_Fil_Prior$*256
		Screen_Width=Max(80,val(Env$("Session_Cols")))
		Ask_File1_Setup=1
	end if
	Data_Fil_Max_Len=Fnstring_Len_Max(sourceFile$)
	Af1_Data_Fil_Prior$=sourceFile$
	! handled by gosub Enum in fn_setup --> GOSUB SETUP_MESSAGEBOX ! MB_OK=1 : MB_CANCEL=2 : MB_ABORT=3 : MB_RETRY=4 : MB_IGNORE=5 : MB_YES=6 : MB_NO=7 ! fnMessageBox Response Enumerations
	if trim$(defaultFilter$)="" then defaultFilter$="N:*.xlsx"
	if sourceFile$='' then
		sourceFile$=defaultFilter$
	end if

	AskScreen1_ask: !
	fnTos
	dim resp$(64)*256
	col1pos=1
	col1len=36
	col2pos=col1pos+col1len+2
	lc=0 ! line count
	rc=0 ! response counter
	dim resp$(20)*1024
	fnLbl(lc+=1,col1pos,'Source Text (Tab delimited) File:', col1len,1)
	fnTxt(lc,col2pos,42,80,0,'1070',0,'Save the file provided by Premier Cardiology as Text (Tab delimited) and select it here.')
	resp$(resp_sourceFile:=rc+=1)=sourceFile$

	lc+=1
	fnLbl(lc+=1,col1pos,'Starting File Number:', col1len,1)
	fnTxt(lc,col2pos,8,8,0,'1000',0,'Select the starting fileno.  Should be a few letters followed by several digits of numbers with leading zeros.')
	resp$(resp_sFileNo:=rc+=1)=sFileNo$

	lc+=1
	fnLbl (lc+=1,col1pos,'Forwarder Number:', col1len,1)
	fncombof('masforw',lc,col2pos,width,'MASFORW//8',1,3,4,10, '',1,1,'Select the Forwarder to assign to imported claims.',0,0,'BH')
	resp$(resp_forwNo:=rc+=1)=forwNo$

	lc+=1
	fnChk(lc+=1,col2pos+1,'Enable Automated Import:',1)
	resp$(resp_enableImport:=rc+=1)=enableImport$

	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then
		returnN=99
		goto AskScreen1_xit
	else
		sourceFile$	=resp$(resp_sourceFile)
		sFileNo$   	=resp$(resp_sFileNo)
		forwNo$    	=resp$(resp_forwNo)(1:pos(resp$(resp_forwNo),' ')-1)
		enableImport$=resp$(resp_enableImport)
		if enableImport$='True' then let enableImport=1 else enableImport=0
		if sourceFile$(len(sourceFile$):len(sourceFile$))='\' then
			sourceFile$(inf:inf)='*.*'
		end if
		if exists(sourceFile$)=1 then ! note EXISTS=1 if it does exist and it is a Directory
			sourceFile$=sourceFile$&'\*.*'
		end if
		if trim$(sourceFile$)='' then
			sourceFile$=fnOpen$(defaultFilter$)
			goto AskScreen1_ask
		else if pos(sourceFile$,'*')>0 or pos(sourceFile$,'?')>0 then
			sourceFile$=fnOpen$(env$('at')&Trim$(sourceFile$))
			goto AskScreen1_ask
		end if
	end if
	AskScreen1_testFileExist: !
	! sourceFile$=fnBr_filename$(sourceFile$)  this forces all filenames to upper case - let us not do that unless we absolutely have to.
	if ~exists(env$('at')&sourceFile$) then
		tempMessageboxResponse=fnMessageBox('The file you specified does not exist.\n'&sourceFile$, mb_exclamation+mb_ignore,env$('program_caption'))
		if tempMessageboxResponse=Mb_Retry then
			goto AskScreen1_testFileExist
		else
			goto AskScreen1_ask
		end if
	end if
	AskScreen1_xit: !
	prompt$=""
	close #af1_win:
	if returnN=99 then sourceFile$=af1_data_fil_prior$
	fn_askScreen1=returnN
fnend


! functions that format and Write out to the CM EDI file.
def fn_writeDemographics(hOut,mat item$; ___,whichAdk,tmpCity$*64,tmpSt$*64,tmpZip$*64) ! requires local enumerations csv_*,mat cs$,forwNo$, etc
	if ~setup_wd then
		dim alreadyAddedDemographicsKey$(0)
		mat alreadyAddedDemographicsKey$(0)
		setup_wd=1
	end if
	
	whichAdk=srch(mat alreadyAddedDemographicsKey$,item$(csv_fileNo))
	if whichAdk<=0 then
		fnAddOneC(mat alreadyAddedDemographicsKey$,item$(csv_fileNo))
		fn_pr_hOut('0[tab]H[tab]Demographics for '&item$(csv_fileNo))
		! r: CM EDI Record 101 / 102    New placement record. / Placement Record Update
		tmpOrigionalClaimAmount$=str$(fn_origionalClaimAmount(item$(csv_patientId)))
		tmpPatientBalance$=str$(fn_patientBalance(item$(csv_patientId)))
		tmpOrigionalClaimAmount$=tmpPatientBalance$
		! if trim$(item$(csv_patientId))='448658' then pause
		fn_add('FIRM_FILENO' 		,item$(csv_fileNo)		,1	)
		fn_add('Forw_Refno' 		,item$(csv_patientId)			)
		fn_add('Forw_Fileno'		,item$(csv_patientId)			) ! item$(csv_claimId)			) claimId is inconsistent within patient - good for invoice, not for forwarder file number, corrected 10/24/2019
		fn_add('SENDER_ID'  		,forwNo$									)
		fn_add('CRED_NAME'   		,forw$(forw_name)					)
		fn_add('CRED_STREET'		,forw$(forw_addr)					)
		fn_add('CRED_CITY'   		,cred_cs$(1)							)
		fn_add('CRED_ST'    		,cred_cs$(2)							)
		fn_add('CRED_ZIP'   		,forw$(forw_zip))
		fn_add('BALANCE'     		,tmpPatientBalance$      	)
		fn_add('OPENED_AMT'  		,tmpPatientBalance$      	)
		fn_add('DATE_DEBT'   		,item$(csv_serviceDate)  	)
		fn_add('ORGCRD'      		,item$(csv_providerName) 	)
		fn_add('ORIG_CLAIM'  		,tmpOrigionalClaimAmount$	)
		fn_add('SERVICES_AMT'		,tmpOrigionalClaimAmount$	)
		fn_add('ORGACT#'     		,item$(csv_patientId)    	)
		fn_add('CRED_XLINE'  		,item$(csv_facilityName) 	)
		! pr item$(csv_patientId) : pause
		if fn_existingFileNo$(ForwNo$,item$(csv_patientId))='' then
			countNewClaim+=1
			fn_pr(101) ! new placement
		else
			countUpdateClaim+=1
			fn_pr(102) ! update existing
		end if
		! /r

		! r: CM EDI Record 111    Debtor Demographics
		fncsz(item$(csv_respCsz),tmpCity$,tmpSt$,tmpZip$)
		if item$(csv_patientName)=item$(csv_respName) then ! The Patient is Responsible for their own debt
			! r: add Debtor 1 - the patient who is responsible to pay their own bill
			fn_add('FIRM_FILENO' 	,item$(csv_fileNo)           ,1	)
			fn_add('NUMBER'      	,'1'                            	)
			fn_add('RELATION'   	,'MAIN'                         	)
			fn_add('FORW_REFNO' 	,item$(csv_patientId)         	)
			fn_add('NAME'        	,item$(csv_respName)          	)
			fn_add('STREET'      	,item$(csv_respAddress)       	)
			fn_add('CITY'        	,tmpCity$                      	)
			fn_add('ST'          	,tmpSt$                         	)
			fn_add('ZIP'         	,tmpZip$                        	)
			fn_add('SSN'         	,item$(csv_social)             	)
			fn_add('PHONE'       	,item$(csv_respPhone)         	)
			fn_add('PHONE2'      	,item$(csv_respWorkPhone)     	)
			fn_add('RESP_PARTY' 	,'Y'                            	)
			fn_add('MOBIL_PHONE'	,item$(csv_respCellPhone)     	)
			fn_add('EMAIL'       	,item$(csv_respEmail)         	)
			fn_add('BIRTH_DATE' 	,item$(csv_patientDOB)         	)
			fn_pr(111)
			! /r
		else ! The responsible person is not the same person as the patient
			! r: add Debtor 1 - the Responsible person
			fn_add('FIRM_FILENO' 	,item$(csv_fileNo)           ,1	)
			fn_add('NUMBER'      	,'1'                            	)
			fn_add('RELATION'   	,'MAIN'                         	)
			fn_add('FORW_REFNO' 	,item$(csv_patientId)         	)
			fn_add('NAME'        	,item$(csv_respName)          	)
			fn_add('STREET'      	,item$(csv_respAddress)       	)
			fn_add('CITY'        	,tmpCity$                      	)
			fn_add('ST'          	,tmpSt$                         	)
			fn_add('ZIP'         	,tmpZip$                        	)
			fn_add('PHONE'       	,item$(csv_respPhone)         	)
			fn_add('PHONE2'      	,item$(csv_respWorkPhone)     	)
			fn_add('RESP_PARTY' 	,'Y'                            	)
			fn_add('MOBIL_PHONE'	,item$(csv_respCellPhone)     	)
			fn_add('EMAIL'       	,item$(csv_respEmail)         	)
			fn_pr(111)
			! /r
			! r: add Debtor 2 - the Patient
			fn_add('FIRM_FILENO' 	,item$(csv_fileNo)           ,1	)
			fn_add('NUMBER'      	,'2'                            	)
			fn_add('RELATION'   	,'PATIENT'                     	)
			fn_add('FORW_REFNO' 	,item$(csv_patientId)         	)
			fn_add('NAME'        	,item$(csv_patientName)       	)
			fn_add('SSN'         	,item$(csv_social)             	)
			fn_add('RESP_PARTY' 	,'N'                            	)
			fn_add('BIRTH_DATE' 	,item$(csv_patientDOB)         	)
			fn_pr(111, 1)
			! /r
		end if
		

		
		! /r
	end if
fnend
def fn_writeInvoices(hOut,mat item$; ___,invoiceOrigAmt$*64)
	fn_pr_hOut('0[tab]H[tab]Invoices for '&item$(csv_fileNo))
	invoiceOrigAmt$=str$(fnval(item$(csv_balance))+fnval(item$(csv_insurancePaid))+fnval(item$(csv_patientPaid)))
	! CM EDI Record 180 Invoice File
	fn_add('FORW_REFNO'    	,item$(csv_patientId)             ,1)
	fn_add('FORW_REFNO'    	,item$(csv_patientId)             ,1)
	fn_add('FIRM_FILENO'   	,item$(csv_fileNo)                 	)
	fn_add('INV_DATE'      	,item$(csv_serviceDate)           	)
	fn_add('DESCRIPTION'   	,item$(csv_providerName)          	)
	fn_add('INV_DESC'     	,'CPT: '&item$(csv_cpt)             ) ! fnCptCode$(item$(csv_cpt))(1:20) 	)
	fn_add('ACCT_NO'       	,item$(csv_facilityName)          	)
	fn_add('DEBTOR_NO'     	,'1'                                	)
	fn_add('CUR_BAL'       	,item$(csv_balance    )           	)
	fn_add('SERVICE_DATE'  	,item$(csv_serviceDate)           	)
	fn_add('INV_NO'        	,item$(csv_invoiceNo  )           	)
	fn_add('ORIG_AMT'      	,invoiceOrigAmt$                   	)
	fn_pr(180)
fnend
def fn_writePaperless(hOut,mat item$,lineCount,sourceFile$*512)
	fn_writePaperlessOneLine('***From line ',str$(lineCount+1)&' of ') ! +1 to account for uncounted header line
	fn_writePaperlessOneLine(sourceFile$(1:2),sourceFile$(3:inf))
	fn_writePaperlessOneLine('    Facility Name:',item$(csv_facilityName     	))
	fn_writePaperlessOneLine('    Provider Name:',item$(csv_providerName     	))
	fn_writePaperlessOneLine('    Patient Name :',item$(csv_patientName      	))
	fn_writePaperlessOneLine('    Patient DOB  :',item$(csv_patientDOB       	))
	fn_writePaperlessOneLine('       Patient Id:',item$(csv_patientId        	))
	fn_writePaperlessOneLine('     Resp Name   :',item$(csv_respName         	))
	fn_writePaperlessOneLine('     Resp Address:',item$(csv_respAddress      	))
	fn_writePaperlessOneLine('     Resp CSZ    :',item$(csv_respCsz           	))
	fn_writePaperlessOneLine('    Patient Phone:',item$(csv_patientPhone     	))
	fn_writePaperlessOneLine('       Resp Phone:',item$(csv_respPhone        	))
	fn_writePaperlessOneLine('  Resp Cell Phone:',item$(csv_respCellPhone    	))
	fn_writePaperlessOneLine('  Resp Work Phone:',item$(csv_respWorkPhone    	))
	fn_writePaperlessOneLine('       Resp Email:',item$(csv_respEmail        	))
	fn_writePaperlessOneLine('Social Security #:',item$(csv_social            	))
	fn_writePaperlessOneLine('         Claim Id:',item$(csv_claimId           	))
	fn_writePaperlessOneLine('  Primary Carrier:',item$(csv_primaryCarrier   	))
	fn_writePaperlessOneLine(' Insurance Number:',item$(csv_insuranceN        	))
	fn_writePaperlessOneLine('Secondary Carrier:',item$(csv_secondaryCarrier 	))
	fn_writePaperlessOneLine('Sec Insurance Num:',item$(csv_secInsuranceN    	))
	fn_writePaperlessOneLine('     Service Date:',item$(csv_serviceDate      	))
	fn_writePaperlessOneLine('              CPT:',item$(csv_cpt)&': '&fnCptCode$(item$(csv_cpt)))
	fn_writePaperlessOneLine('   Insurance Paid:',item$(csv_insurancePaid    	))
	fn_writePaperlessOneLine('     Patient Paid:',item$(csv_patientPaid      	))
	fn_writePaperlessOneLine('       Total Paid:',item$(csv_totalPaid        	))
	fn_writePaperlessOneLine('          Balance:',item$(csv_balance           	))
	fn_writePaperlessOneLine('BillPatientReason:',item$(csv_billPatientReason	))
	fn_writePaperlessOneLine('      Claim Notes:',item$(csv_claimNotes        	))
	fn_writePaperlessOneLine('           FileNo:',item$(csv_fileNo            	))
	fn_writePaperlessOneLine('       Invoice No:',item$(csv_invoiceNo        	))
fnend
def fn_writePaperlessOneLine(comment$*256,comment2$*2048;___, which)
	dim polWrote$(0)*2048
	which=srch(mat polWrote$,item$(csv_patientId)&item$(csv_fileNo)&comment$&comment2$)
	if which<=0 then
		fnAddOneC(mat polWrote$,item$(csv_patientId)&item$(csv_fileNo)&comment$&comment2$)
		if trim$(comment2$)<>'' and comment2$<>'$0.00' then
			fn_add('FORW_REFNO'   	,item$(csv_patientId)	        ,1)
			fn_add('FIRM_FILENO'   	,item$(csv_fileNo)	          	)
			fn_add('PDATE'         	,date$('mm/dd/ccyy') 	        	)
			fn_add('PTIME'         	,time$                	        	)
			fn_add('INITIALS'     	,''                    	        	)  ! leave blank for EDI and user id 1 as per EDI Help Manual
			fn_add('PCMT'          	,comment$&comment2$  	        	)
			fn_pr(109)
		end if
	end if
	
fnend
def fn_writeInfinity(hOut,mat item$)

	fn_add('FORW_REFNO'    	,item$(csv_patientId)             ,1)
	fn_add('FIRM_FILENO'   	,item$(csv_fileNo)                 	)
	fn_pr(134)
fnend
dim hLine$*2048,dLine$*2408
def fn_add(hLineAdd$*128,dLineAdd$*2048; reset)
	if reset then
		hLine$=hLineAdd$
		dLine$=dLineAdd$
	else
		hLine$(inf:inf)=tab$&hLineAdd$
		dLine$(inf:inf)=tab$&dLineAdd$
	end if
fnend
def fn_pr(num; forceHeader)
	! fn_pr_hOut_header(num,hLine$)
	headerNow$=str$(num)&tab$&'H'
	if forceHeader or headerNow$<>headerPrior$ then
		fn_pr_hOut(headerNow$&tab$&'Date'&tab$&'Time'&tab$&hLine$)
		headerPrior$=headerNow$
	end if
	! fn_pr_hOut_detail(num,dLine$)
	dLine$(0:0)=str$(num)&'[tab]D[tab]'&date$('mm/dd/ccyy')&tab$&time$&tab$
	fn_pr_hOut(dLine$)
fnend
def fn_pr_hOut(x$*2048)
	x$=srep$(x$,'[tab]',tab$)
	pr #hOut: x$&tab$&'#'
fnend

def fn_existingFileNo$(ForwNo$,forwRefNo$; ___,return$,claimKey$*64,which,hClaim)
	if setup_efn<>val(ForwNo$) then
		setup_efn=val(ForwNo$)
		dim claim$(0)*60,claimN(0)
		dim claimFieldsC$(0)*20,claimFieldsN$(0)*20
		dim claimFormAll$*2048
		fnsql_setup$('master',mat claim$,mat claimN,mat claimFieldsC$,mat claimFieldsN$,claimFormAll$)
		gosub enumMaster

		! open #hClaim:=fnGetHandle: "name=master//6,kfname=FOFILIDX//6,shr",internal,input,keyed
		open #hClaim:=fnGetHandle: "name=master//6,shr",internal,input,relative
		! restore #hClaim:
		dim efn_fileno$    		(0)*20
		dim efn_forwFileNo$		(0)*20
		dim efn_forwRefNo$		(0)*20
		
		mat efn_fileno$    		(0)
		mat efn_forwFileNo$		(0)
		mat efn_forwRefNo$		(0)
		do
			mat claim$=('')
			mat claimN=(0)
			read #hClaim,using claimFormAll$: mat claim$,mat claimN eof EfnEoMaster
			if claimN(master_forw_no)=val(ForwNo$) then
				fnAddOneC(mat efn_fileno$    		,trim$(	claim$(master_fileno)      	))
				fnAddOneC(mat efn_forwFileNo$		,trim$(	claim$(master_forw_fileno) 	))
				fnAddOneC(mat efn_forwRefNo$		,trim$(	claim$(master_forw_refno)  	))
			end if
		loop
		EfnEoMaster: !
		close #hClaim:
	end if
	which=srch(mat efn_forwRefNo$,trim$(forwRefNo$))
	if which<=0 then
		return$=''
	else
		return$=efn_fileno$(which)
	end if
	fn_existingFileNo$=return$
fnend

def fn_getFileNo$(forwNo$,uniqueIdentifier$; ___,x,return$,which)
	if ~setup_getFileNo then
		dim gfnKey$(0)*128
		mat gfnKey$(0)
		dim gfnFileNo$(0)*8
		mat gfnFileNo$(0)
		dim number$(10)
		for x=1 to 10 : number$(x)=str$(x-1) : nex x
		firstNumberInSfileNo=fnPosOfAny(sFileNo$,mat number$)
		! pause
		lastNumberInSfileNo=fnPosOfAny(sFileNo$,mat number$, -1)
		firstFileNumber=val(sFileNo$(firstNumberInSfileNo:inf))
		nextFileNumber=firstFileNumber
		dim fileNumberFormat$*64
		fileNumberFormat$='pic('&rpt$('#',lastNumberInSfileNo-firstNumberInSfileNo+1)&')'
		setup_getFileNo=1
	end if
	return$=fn_existingFileNo$(ForwNo$,uniqueIdentifier$)
	if return$='' then
		uniqueIdentifier$=trim$(uniqueIdentifier$)
		which=srch(mat gfnKey$,uniqueIdentifier$)
		if which<=0 then
			which=udim(mat gfnKey$)+1
			mat gfnKey$(which)
			mat gfnFileNo$(which)
			gfnKey$(which)=uniqueIdentifier$
			gfnFileNo$(which)=sFileNo$(1:firstNumberInSfileNo-1)&cnvrt$(fileNumberFormat$,nextFileNumber)
			nextFileNumber+=1
		end if
		return$=gfnFileNo$(which)
	end if
	fn_getFileNo$=return$
fnend
def fn_origionalClaimAmount(patientId$; ___,returnN,x,xStart)
	xStart=srch(mat list_patientId$,patientId$)
	if xStart>0 then
		for x=xStart to udim(mat list_patientId$)
			if list_patientId$(x)=patientId$ then
				returnN+=list_balanceN(x)+list_totalPaidN(x)
			end if
		nex x
	end if
	fn_origionalClaimAmount=returnN
fnend





def fn_patientBalance(patientId$; ___,returnN,x,xStart)
	xStart=srch(mat list_patientId$,patientId$)
	if xStart>0 then
		for x=xStart to udim(mat list_patientId$)
			if list_patientId$(x)=patientId$ then
				returnN+=list_balanceN(x)
			end if
		nex x
	end if
	fn_patientBalance=returnN
fnend
def fn_invoiceNumber$*128
	fn_invoiceNumber$=item$(csv_patientId)&'-'&item$(csv_claimId)&'-'&date$(days(item$(csv_serviceDate),'mm/dd/ccyy'),'ccyymmdd') ! XXX
fnend


def fn_init_csv_in(&csvFieldCount,csvFile$*1024;___,returnN) ! everything here is local.
	if ~init_csv_in then
		init_csv_in=1
		dim Csv_Fields$(0)*128
		dim Csv_Data$(0)*256
		csvFieldCount=Fnopen_Csv(returnN:=fnGetHandle,env$('at')&srep$(csvFile$,'@::',''),Csv_Delimiter$,Mat Csv_Fields$,Mat Csv_Data$)
		lineCount=0
		! r: set csv_*
		csv_facilityName      	=srch(mat Csv_Fields$,uprc$('Facility Name'        	))
		csv_providerName      	=srch(mat Csv_Fields$,uprc$('Provider Name'        	))
		csv_patientName        	=srch(mat Csv_Fields$,uprc$('Patient Name'          	))
		csv_patientDOB         	=srch(mat Csv_Fields$,uprc$('Patient DOB'           	))
		csv_patientId          	=srch(mat Csv_Fields$,uprc$('Patient ID'            	))
		csv_respName           	=srch(mat Csv_Fields$,uprc$('Resp Name'             	))
		csv_respAddress        	=srch(mat Csv_Fields$,uprc$('Resp Address'          	))
		csv_respCsz            	=srch(mat Csv_Fields$,uprc$('Resp City-State-Zip'  	))
		csv_patientPhone      	=srch(mat Csv_Fields$,uprc$('Patient Phone'        	))
		csv_respPhone          	=srch(mat Csv_Fields$,uprc$('Resp Phone'            	))
		csv_respCellPhone     	=srch(mat Csv_Fields$,uprc$('Resp Cell Phone'      	))
		csv_respWorkPhone     	=srch(mat Csv_Fields$,uprc$('Resp Work Phone'      	))
		csv_respEmail          	=srch(mat Csv_Fields$,uprc$('Resp Email'            	))
		csv_social             	=srch(mat Csv_Fields$,uprc$('Social'                	))
		csv_claimId            	=srch(mat Csv_Fields$,uprc$('Claim ID'              	))
		csv_primaryCarrier    	=srch(mat Csv_Fields$,uprc$('Primary Carrier'      	))
		csv_insuranceN        	=srch(mat Csv_Fields$,uprc$('Insurance#'          	))
		csv_secondaryCarrier  	=srch(mat Csv_Fields$,uprc$('Secondary Carrier'    	))
		csv_secInsuranceN     	=srch(mat Csv_Fields$,uprc$('Sec Insurance#'       	))
		csv_serviceDate        	=srch(mat Csv_Fields$,uprc$('Service Date'          	))
		csv_cpt                	=srch(mat Csv_Fields$,uprc$('CPT'                    	))
		csv_insurancePaid     	=srch(mat Csv_Fields$,uprc$('Insurance Paid'       	))
		csv_patientPaid        	=srch(mat Csv_Fields$,uprc$('Patient Paid'          	))
		csv_totalPaid          	=srch(mat Csv_Fields$,uprc$('Total Paid'            	))
		csv_balance            	=srch(mat Csv_Fields$,uprc$('Balance'               	))
		csv_billPatientReason 	=srch(mat Csv_Fields$,uprc$('Bill Patient Reason'  	))
		csv_claimNotes         	=srch(mat Csv_Fields$,uprc$('Claim Notes'           	))
	
		! ***  Add New Invented Columns ***
		csvFieldCount+=1 : csv_fileNo   	=fnAddOneC(mat csv_Fields$,'FileNo')
		csvFieldCount+=1 : csv_invoiceNo	=fnAddOneC(mat csv_Fields$,'InvoiceNo')
		! csvFieldCount=csv_invoiceNo
		! /r
	end if
	fn_init_csv_in=returnN
fnend
def fn_close_csv_in(; delFileIn)
	if delFileIn then
		close #hIn,free:
	else
		close #hIn:
	end if
	init_csv_in=0
fnend
def fn_readFileIntoArrays
	if ~setup_rfia then ! r:
		setup_rfia=1
		dim list_facilityName$(0)*256
		dim list_providerName$(0)*256
		dim list_patientName$(0)*256
		dim list_patientDOB$(0)*256
		dim list_patientId$(0)*256
		dim list_respName$(0)*256
		dim list_respAddress$(0)*256
		dim list_respCsz$(0)*256
		dim list_patientPhone$(0)*256
		dim list_respPhone$(0)*256
		dim list_respCellPhone$(0)*256
		dim list_respWorkPhone$(0)*256
		dim list_respEmail$(0)*256
		dim list_social$(0)*256
		dim list_claimId$(0)*256
		dim list_primaryCarrier$(0)*256
		dim list_insuranceN$(0)*256
		dim list_secondaryCarrier$(0)*256
		dim list_secInsuranceN$(0)*256
		dim list_serviceDate$(0)*256
		dim list_cpt$(0)*256
		dim list_insurancePaidN(0)
		dim list_patientPaidN(0)
		dim list_totalPaidN(0)
		dim list_balanceN(0)
		dim list_billPatientReason$(0)*256
		dim list_claimNotes$(0)*512
		! dim list_fileNo$(0)*512
		! dim list_isNewN(0)
		
		mat list_facilityName$(0)
		mat list_providerName$(0)
		mat list_patientName$(0)
		mat list_patientDOB$(0)
		mat list_patientId$(0)
		mat list_respName$(0)
		mat list_respAddress$(0)
		mat list_respCsz$(0)
		mat list_patientPhone$(0)
		mat list_respPhone$(0)
		mat list_respCellPhone$(0)
		mat list_respWorkPhone$(0)
		mat list_respEmail$(0)
		mat list_social$(0)
		mat list_claimId$(0)
		mat list_primaryCarrier$(0)
		mat list_insuranceN$(0)
		mat list_secondaryCarrier$(0)
		mat list_secInsuranceN$(0)
		mat list_serviceDate$(0)
		mat list_cpt$(0)
		mat list_insurancePaidN(0)
		mat list_patientPaidN(0)
		mat list_totalPaidN(0)
		mat list_balanceN(0)
		mat list_billPatientReason$(0)
		mat list_claimNotes$(0)
		! mat list_fileNo$(0)
		! mat list_isNewN(0)
		
		hIn=fn_init_csv_in(csvFieldCount,csvFile$)
		do
			dim line$*1024
			linput #hIn: line$ eof Rfia_EoF
			lineCount+=1
			if line$<>'' and line$<>'"' then
				dim item$(0)*512
				str2mat(line$,mat item$,tab$)
				mat item$(csvFieldCount)
				item$(csv_fileNo)=fn_getFileNo$(forwNo$,item$(csv_patientId))
				item$(csv_invoiceNo)=fn_invoiceNumber$
				! r: add each item into it's array
				fnAddOneC(mat list_facilityName$,item$(csv_facilityName))
				fnAddOneC(mat list_providerName$,item$(csv_providerName))
				fnAddOneC(mat list_patientName$,item$(csv_patientName))
				fnAddOneC(mat list_patientDOB$,item$(csv_patientDOB))
				fnAddOneC(mat list_patientId$,item$(csv_patientId))
				fnAddOneC(mat list_respName$,item$(csv_respName))
				fnAddOneC(mat list_respAddress$,item$(csv_respAddress))
				fnAddOneC(mat list_respCsz$,item$(csv_respCsz))
				fnAddOneC(mat list_patientPhone$,item$(csv_patientPhone))
				fnAddOneC(mat list_respPhone$,item$(csv_respPhone))
				fnAddOneC(mat list_respCellPhone$,item$(csv_respCellPhone))
				fnAddOneC(mat list_respWorkPhone$,item$(csv_respWorkPhone))
				fnAddOneC(mat list_respEmail$,item$(csv_respEmail))
				fnAddOneC(mat list_social$,item$(csv_social))
				fnAddOneC(mat list_claimId$,item$(csv_claimId))
				fnAddOneC(mat list_primaryCarrier$,item$(csv_primaryCarrier))
				fnAddOneC(mat list_insuranceN$,item$(csv_insuranceN))
				fnAddOneC(mat list_secondaryCarrier$,item$(csv_secondaryCarrier))
				fnAddOneC(mat list_secInsuranceN$,item$(csv_secInsuranceN))
				fnAddOneC(mat list_serviceDate$,item$(csv_serviceDate))
				fnAddOneC(mat list_cpt$,item$(csv_cpt))
				fnAddOneN(mat list_insurancePaidN,fnVal(item$(csv_insurancePaid)))
				fnAddOneN(mat list_patientPaidN,fnVal(item$(csv_patientPaid)))
				fnAddOneN(mat list_totalPaidN,fnVal(item$(csv_totalPaid)))
				fnAddOneN(mat list_balanceN,fnVal(item$(csv_balance)))
				! if list_balanceN(udim(mat list_balanceN))=0 then pr 'fnval('&item$(csv_balance)&') returned 0' : pause
				fnAddOneC(mat list_billPatientReason$,item$(csv_billPatientReason))
				fnAddOneC(mat list_claimNotes$,item$(csv_claimNotes))
				
				! fnAddOneC(mat list_fileNo$,item$(csv_fileNo))
				! if fn_existingFileNo$(ForwNo$,item$(csv_patientId))='' then
				! 	fnAddOneN(mat list_isNewN,1)
				! else
				! 	fnAddOneN(mat list_isNewN,0)
				! end if
				! /r
			end if
		loop
		Rfia_EoF: !
		fn_close_csv_in
	end if ! /r
fnend

def fn_setup
	if ~setup then
		setup=1
		! library 'Library\clsUtil.wb': fnErase_buttons

		library 'Library\clsUtil.wb': fnOpen_Csv
		library 'Library\clsUtil.wb': fnAsci
		! library 'Library\clsUtil.wb': fnBr_filename$
		library 'Library\clsUtil.wb': fnMessageBox
		library 'Library\clsUtil.wb': fnOpen$
		library 'Library\clsUtil.wb': fnString_Len_Max
		library 'Library\openFile.wb': fnOpen_master

		library 'Library\clsUtil.wb': fnGetHandle
		! library 'S:\Core\Library.br': fnGetHandle     removed in favor of clsUtil version to avoid error 2274
		
		library 'S:\Core\Library.br': fnTop
		library 'S:\Core\Library.br': fnGetPp
		library 'S:\Core\Library.br': fnFree
		library 'S:\Core\Library.br': fnCopy
		library 'S:\Core\Library.br': fnXit
		library 'S:\Core\Library.br': fnAddOneC,fnAddOneN
		library 'S:\Core\Library.br': fnCsz
		library 'S:\Core\Library.br': fnTos,fnLbl,fnTxt,fnCmdSet
		library 'S:\Core\Library.br': fnChk
		library 'S:\Core\Library.br': fnCombof
		library 'S:\Core\Library.br': fnAcs2
		library 'S:\Core\Library.br': fnPosOfAny
		library 'S:\Core\Library.br': fnReg_read
		library 'S:\Core\Library.br': fnReg_write
		library 'S:\Collection-Master Add-On\fn\Library.br': fnCptCode$
		library 'S:\Collection-Master Add-On\fn\Library.br': fnVal
		gosub Enum
		! fnErase_buttons

		library 'Library\SQL.wb': fnsql_setup$

		gosub Enum

		dim forw$(0)*60,forwN(0)
		dim forwFieldsC$(0)*20,forwFieldsN$(0)*20
		dim forwFormAll$*2048
		! execute "*SubProc "&     <--- not necessary with include:enum\forw  and  gosub Enumforw
		fnsql_setup$('masforw',mat forw$,mat forwN,mat forwFieldsC$,mat forwFieldsN$,forwFormAll$)
		gosub EnumForw

		
	end if
fnend

def fn_removeExcessCRLF$*256(csvFile$*256; ___,return$*256,hIn,hOut,line$*1024,delim$,lineCount,itemsOnLine,lineCountIn,lineCountOut)
	
	dim Csv_Fields$(0)*128
	dim Csv_Data$(0)*256
	minItemCount=Fnopen_Csv(hIn:=fnGetHandle,env$('at')&csvFile$,delim$,Mat Csv_Fields$,Mat Csv_Data$)
	lineCount=0
	close #hIn: ioerr ignore
	open #hIn:=fnGetHandle:  'name='&env$('at')&csvFile$,display,input
include: filenamesPushMixedCase
	open #hOut:=fnGetHandle: 'name='&env$('at')&csvFile$&'-fixedCrLf,recl=2048,replace',display,output
include: filenamesPopUpperCase
		
	do
		linput #hIn: line$ eof Recrlf_EoF
		lineCountIn+=1
		
		dim ic_item$(0)*512
		str2mat(line$&' ',mat ic_item$,delim$)
		itemsOnLine=udim(mat ic_item$)
		ic_item$(itemsOnLine)=trim$(ic_item$(itemsOnLine))
		! itemsOnLine+=fn_itemCount(line$,delim$)
		
		if ic_item$(itemsOnLine)(1:1)='"' and ic_item$(itemsOnLine)(len(ic_item$(itemsOnLine)):len(ic_item$(itemsOnLine)))<>'"' then
			print #hOut: line$&'    ';
		else
			print #hOut: line$
			lineCountOut+=1
		end if

		! pr #hOut: line$;
		! if itemsOnLine>=minItemCount then
		! 	pr #hOut: ''
		! 	lineCountOut+=1
		! 	itemsOnLine=0
		! end if
	loop
	Recrlf_EoF: !
	return$=file$(hOut)
	! pr 'lineCountIn=';lineCountIn
	! pr 'lineCountOut=';lineCountOut
	! pause
	close #hIn:
	close #hOut:
	fn_removeExcessCRLF$=return$
fnend
def fn_itemCount(line$*2048,delim$; ___,returnN)
	dim ic_item$(0)*512
	str2mat(line$&' ',mat ic_item$,delim$)
	returnN=udim(mat ic_item$)
	fn_itemCount=returnN
fnend
Xit: !
fnXit
include: cm\enum\common
include: cm\enum\forw
include: cm\enum\master
include: cm\err
