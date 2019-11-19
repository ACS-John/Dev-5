fn_setup
fnTop(program$)
fn_premierCardiologyImport('allData')
fnXit
def library fnPremierCardiologyImport(; sourceId$)
	if ~setup then let fn_setup
	fnPremierCardiologyImport=fn_premierCardiologyImport( sourceId$)
fnend
def  fn_premierCardiologyImport(; sourceId$)
	if sourceId$='' then sourceId$='allData'
	if sourceId$<>'allData' and sourceId$<>'direct' then
		pr bell;'unrecognized sourceId$='&sourceId$ : pause
		goto PciXit
	end if

	open #hClaimOpen:=fnGetHandle: "name=MASTER//6,kfname=MASTERX//6,shr",internal,input,keyed
	open #hClaimClosed:=fnGetHandle: "name=HISTORY//1,kfname=HISTORYX//1,shr",internal,input,keyed

	Screen1: !
	dim csvFile$*512
	! r: read screen answers
	if sourceId$='direct' then
		fnreg_read(env$('program_caption')&'.csvFile',csvFile$, 'D:\CM\Stern and Stern\Premier Cardiology (direct)\Copy of Stern Collections sept 10 2019.txt')
		fnreg_read(env$('program_caption')&'.starting fileno',sFileNo$,'PCJ20001')
		fnreg_read(env$('program_caption')&'.forw no',forwNo$,'1617')
		fnreg_read(env$('program_caption')&'.enableImport',enableImport$) : if enableImport$='True' then enableImport=1 else enableImport=0
		fnreg_read(env$('program_caption')&'.priorityColumnN',priorityColumn$,'PRIINSNAME')
		fnreg_read(env$('program_caption')&'.priorityText',priorityText$,'Patient Has Check')
		fnreg_read(env$('program_caption')&'.priorityDiaryCode',priorityDiaryCode$,'222')
	else
		fnreg_read(env$('program_caption')&'.csvFile',csvFile$, 'D:\CM\Stern and Stern\New_format_Premier_Cardiology.xlsx')
		fnreg_read(env$('program_caption')&'.starting fileno',sFileNo$,'PCE01001')
		fnreg_read(env$('program_caption')&'.forw no',forwNo$,'1617')
		fnreg_read(env$('program_caption')&'.enableImport',enableImport$) : if enableImport$='True' then enableImport=1 else enableImport=0
		fnreg_read(env$('program_caption')&'.priorityColumnN',priorityColumn$)
		fnreg_read(env$('program_caption')&'.priorityText',priorityText$)
		fnreg_read(env$('program_caption')&'.priorityDiaryCode',priorityDiaryCode$)
	end if
	! /r
	if fn_askScreen1(csvFile$,sFileNo$,forwNo$,enableImport,enableImport$,priorityColumn$,priorityText$,priorityDiaryCode$)=99 then
		goto PciXit
	else



		! r: write screen answers
		fnreg_write(env$('program_caption')&'.csvFile',csvFile$)
		fnreg_write(env$('program_caption')&'.starting fileno',sFileNo$)
		fnreg_write(env$('program_caption')&'.forw no',forwNo$)
		fnreg_write(env$('program_caption')&'.enableImport',enableImport$)

		fnreg_write(env$('program_caption')&'.priorityColumnN',priorityColumn$)
		fnreg_write(env$('program_caption')&'.priorityText',priorityText$)
		fnreg_write(env$('program_caption')&'.priorityDiaryCode',priorityDiaryCode$)
		! /r

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

		if sourceId$='allData' and csv_KEY=0 then
			pr 'invalid file.  No "Patient ID" column.';bell
			pause
			goto PciXit
		else if sourceId$='direct' and csv_KEY=0 then
			pr 'invalid file.  No "acctNo" column.';bell
			pause
			goto PciXit
		end if
		priorityColumn$=trim$(priorityColumn$)
		if priorityColumn$<>'' then
			priorityColumnN=fnsrch_case_insensitive(mat csv_fields$,priorityColumn$)
			if priorityColumnN<=0 then
				pr 'invalid priority column.';bell
				pause
			end if
		end if


		dim useDate$*10
		useDate$=date$('mm/dd/ccyy') ! '10/25/2019'
		dim useTime$*8
		useTime$=time$ ! '09:58:14'
		enableInvoiceCpt=1
		
		dim csvPath$*256
		dim csvProg$*256
		dim csvExt$*128
		fnGetPp(csvFile$,csvPath$,csvProg$,csvExt$)
		dim outFile$*256
		outFile$=env$('at')&srep$(csvPath$&csvProg$&'-CM_EDI'&'.csv','@::','')
include: filenamesPushMixedCase
		open #hOut:=fnGetHandle: 'name='&outFile$&',recl=1024,replace',display,output
include: filenamesPopUpperCase
		fn_pr_hOut('0[tab]H[tab]This file is "'&os_filename$(outFile$)&'"'	)
		fn_pr_hOut('0[tab]H[tab]This file was made by the "'&env$('program_caption')&'" (a Collection-Master Add-On program) on '&useDate$&' at '&useTime$&'.'	)
		fn_pr_hOut('0[tab]H[tab]This file was made from the source file: "'&csvFile$&'".'	)
		fn_pr_hOut('0[tab]H[tab]from "'&os_filename$(csvOrigional$)&'"')
		fn_pr_hOut('0[tab]H[tab]Detected file layout: '&sourceId$)
		lineCount=0
		do
			dim line$*1024
			linput #hIn: line$ eof Finis
			lineCount+=1
			if line$<>'' and line$<>'"' then
				dim item$(0)*512
				str2mat(line$,mat item$,tab$)
				mat item$(csvFieldCount)
				! item$(csv_FILENO)=fn_getFileNo$(forwNo$,item$(csv_KEY),oc$)
				
				! if item$(csv_FILENO)<>list_FILENO$(lineCount) then ! these things should always match up - we shouldn't even need to read them in again
				! 	pr 'item fileno=';item$(csv_FILENO)
				! 	pr 'list fileno=';list_FILENO$(lineCount)
				! 	pause 
				! end if
				if days(item$(csv_SERVICEDAY),'mm/dd/ccyy')<>val(list_SERVICEDAY$(lineCount)) then ! these things should always match up - we shouldn't even need to read them in again
					pr 'item serviceday=';days(item$(csv_SERVICEDAY),'mm/dd/ccyy')
					pr 'list serviceday=';list_SERVICEDAY$(lineCount)
					pause 
				end if

				
				item$(csv_FILENO)=list_FILENO$(lineCount)
				item$(csv_SERVICEDAY)=list_SERVICEDAY$(lineCount)
				oc$=list_OCN$(lineCount)
				item$(csv_INVOICENO)=list_INVOICENO$(lineCount)
				item$(csv_RESPNAME)=list_RESPNAME$(lineCount)
				
				if csv_patientName>0 then 
					item$(csv_patientName)=srep$(item$(csv_patientName),', ',',')
					item$(csv_patientName)=srep$(item$(csv_patientName),',','/')
				end if
				if csv_RESPNAME>0 then 
					item$(csv_RESPNAME)=srep$(item$(csv_RESPNAME),', ',',')
					item$(csv_RESPNAME)=srep$(item$(csv_RESPNAME),',','/')
				end if
				! pr oc$ : pause
				if oc$='closed' then
					masterWhich=srch(claim_fileno$,list_FILENO$(lineCount))
					claim_forwRefNo$(masterWhich)
					fn_reportClosedEncounter(oc$,list_FILENO$(lineCount),str$(claim_forwNo(masterWhich)),claim_forwRefNo$(masterWhich),str$(claim_balance(masterWhich)),str$(fn_patientBalance(item$(csv_KEY))))
				else
					
					fn_pr_hOut('0[tab]H[tab] XXX lineCount: '&str$(lineCount)	&' XXX fileNo: '&item$(csv_FILENO)&' XXX')
					! item$(csv_INVOICENO)=item$(csv_claimId)&'-'&fn_date$(item$(csv_SERVICEDAY),'ccyymmdd')
	
					fn_writeDemographics(hOut,oc$,mat item$)
					fn_writePaperless(hOut,mat item$,lineCount,csvFile$)
					fn_writeInvoices(hOut,mat item$)
					! fn_writeInfinity(hOut,mat item$)
					if fn_isPriority(mat item$,priorityColumnN,priorityText$) then
							priorityCount+=1
							! priorityColumn$,priorityText$,priorityDiaryCode$
						fn_writeDiary(hOut,mat item$)
					end if
				end if
				! mat2str(mat item$,line$,tab$)
				! print #hOut: line$
			end if
		loop
	end if

	Finis: ! r:
	close #hClaimOpen:
	close #hClaimClosed:

	fn_pr_hOut('0[tab]H[tab]lineCount='&str$(lineCount))
	fn_close_csv_in( 1)
	close #hOut:
	fn_reportClosedEncounter_finis
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
		mbText$(inf:inf)=lf$&'Claim Updates-Open: '&tab$&str$(countUpdateClaimOpen)
		mbText$(inf:inf)=lf$&'Claim Skipped-Closed: '&tab$&str$(udim(mat alreadyReportedFileNo$))
		mbText$(inf:inf)=lf$&'Claim Diaried for Priority: '&tab$&str$(priorityCount)
		mbText$(inf:inf)=lf$&'Sucessfully created a file for CM EDI Import:'
		mbText$(inf:inf)=lf$&outFile$
		fnMessageBox(mbText$,mb_information+mb_okonly,env$('program_caption'))
		! msgbox('Success on '&csvFile$)
	end if
	goto PciXit ! /r
	PciXit: !
	
fnend
def fn_isPriority(mat item$,priorityColumnN,priorityText$)
	if pos(lwrc$(item$(priorityColumnN)),lwrc$(trim$(priorityText$)))>0 then
		returnN=1
	else
		returnN=0
	end if
	fn_isPriority=returnN
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
	lc+=1
	fnLbl(lc+=1,col1pos,'Source Text (Tab delimited) File:', col1len,1)
	fnTxt(lc,col2pos,42,512,0,'1070',0,'Save the file provided by Premier Cardiology as Text (Tab delimited) and select it here.')
	resp$(resp_sourceFile:=rc+=1)=sourceFile$

	lc+=1
	fnLbl(lc+=1,col1pos,'Starting File Number:', col1len,1)
	fnTxt(lc,col2pos,8,8,0,'1000',0,'Select the starting fileno.  Should be a few letters followed by several digits of numbers with leading zeros.')
	resp$(resp_sFileNo:=rc+=1)=sFileNo$

	fnLbl (lc+=1,col1pos,'Forwarder Number:', col1len,1)
	fncombof('masforw',lc,col2pos,width,'MASFORW//8',1,3,4,10, '',1,1,'Select the Forwarder to assign to imported claims.',0,0,'BH')
	resp$(resp_forwNo:=rc+=1)=forwNo$

	lc+=1

	fnLbl (lc+=1,col1pos,'Priority Diary Code:', col1len,1)
	fncombof('DiaryCd',lc,col2pos,width,'SHARE\DIARYCD.INT',151,3,1,50, 'SHARE\DIARYCD.IDX',1,0,'Diary Code to be added if Priority Text found in Priority Column.',0,0,'BH')
	! fnTxt(lc,col2pos,8,0,0,'',0,'Diary Code to be added if Priority Text found in Priority Column.')
	resp$(resp_priDiaryCode:=rc+=1)=priorityDiaryCode$

	fnLbl (lc+=1,col1pos,'Priority Column:', col1len,1)
	fnTxt(lc,col2pos,8,40,20,'',0,'Select the column heading of the importing file which the Text will appear in to trigger the priority diary code.')
	resp$(resp_priDiaryColumn:=rc+=1)=priorityColumn$

	fnLbl (lc+=1,col1pos,'Priority Text:', col1len,1)
	fnTxt(lc,col2pos,8,80,40,'',0,'Select the case insensetive text that, if found in the Priority Column will trigger the addition of the Priority Diary Code.')
	resp$(resp_priDiaryText:=rc+=1)=priorityText$


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
		priorityDiaryCode$	=resp$(resp_priDiaryCode)(1:pos(resp$(resp_priDiaryCode),' ')-1)
		priorityColumn$			=resp$(resp_priDiaryColumn)
		priorityText$				=resp$(resp_priDiaryText)
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
def fn_writeDemographics(hOut,oc$,mat item$; ___,whichAdk,tmpCity$*64,tmpSt$*64,tmpZip$*64) ! requires local enumerations csv_*,mat cs$,forwNo$, etc
	if ~setup_wd then
		dim alreadyAddedDemographicsKey$(0)
		mat alreadyAddedDemographicsKey$(0)
		setup_wd=1
	end if

	whichAdk=srch(mat alreadyAddedDemographicsKey$,item$(csv_FILENO))
	if whichAdk<=0 then
		fnAddOneC(mat alreadyAddedDemographicsKey$,item$(csv_FILENO))
		fn_pr_hOut('0[tab]H[tab]Demographics for '&item$(csv_FILENO))
		! r: CM EDI Record 101 / 102    New placement record. / Placement Record Update
		tmpOrigionalClaimAmount$=str$(fn_origionalClaimAmount(item$(csv_KEY)))
		tmpPatientBalance$=str$(fn_patientBalance(item$(csv_KEY)))
		tmpOrigionalClaimAmount$=tmpPatientBalance$
		! if trim$(item$(csv_KEY))='448658' then pause
		fn_add('FIRM_FILENO' 		,item$(csv_FILENO)	,1)
		fn_add('Forw_Refno' 		,item$(csv_KEY)				)
		fn_add('Forw_Fileno'		,item$(csv_KEY)				) ! item$(csv_claimId)			) claimId is inconsistent within patient - good for invoice, not for forwarder file number, corrected 10/24/2019
		fn_add('SENDER_ID'  		,forwNo$							)
		fn_add('CRED_NAME'   		,forw$(forw_name)			)
		fn_add('CRED_STREET'		,forw$(forw_addr)			)
		fn_add('CRED_CITY'   		,cred_cs$(1)					)
		fn_add('CRED_ST'    		,cred_cs$(2)					)
		fn_add('CRED_ZIP'   		,forw$(forw_zip)			)
		fn_add('BALANCE'     		,tmpPatientBalance$ 	)
		fn_add('OPENED_AMT'  		,tmpPatientBalance$ 	)
		fn_add('DATE_DEBT'   		,fn_date$(item$(csv_SERVICEDAY)))
		fn_add('ORGCRD'      		,item$(csv_PROVIDER) 	)
		fn_add('ORIG_CLAIM'  		,tmpOrigionalClaimAmount$	)
		fn_add('SERVICES_AMT'		,tmpOrigionalClaimAmount$	)
		fn_add('ORGACT#'     		,item$(csv_KEY)    	)
		fn_add('CRED_XLINE'  		,item$(csv_FACILITY) 	)
		! pr item$(csv_KEY) : pause
		if oc$='open' then
			countUpdateClaimOpen+=1
			fn_pr(102) ! update existing
		else if oc$='new' then
			fn_pr(101) ! new placement
			countNewClaim+=1
		else
			pr 'fn_writeDemographics does not handle oc$ type '&oc$
			pause
		end if
		! /r

		! r: CM EDI Record 111    Debtor Demographics
		tmpCity$=tmpSt$=tmpZip$=''
		if csv_respCsz>0 then
			fncsz(item$(csv_respCsz),tmpCity$,tmpSt$,tmpZip$)
		end if
		if csv_patientName<=0 or item$(csv_patientName)=item$(csv_RESPNAME) then ! The Patient is Responsible for their own debt
			! r: add Debtor 1 - the patient who is responsible to pay their own bill
			fn_add('FIRM_FILENO' 	,item$(csv_FILENO)           ,1	)
			fn_add('NUMBER'      	,'1'                          	)
			fn_add('RELATION'   	,'MAIN'                       	)
			fn_add('FORW_REFNO' 	,item$(csv_KEY)               	)
			fn_add('NAME'        	,item$(csv_RESPNAME)          	)
			fn_addItemIfValid('STREET',csv_respAddress)
			if csv_respCsz>0 then
				fn_add('CITY'        	,tmpCity$                   	)
				fn_add('ST'          	,tmpSt$                     	)
				fn_add('ZIP'         	,tmpZip$                    	)
			end if
			fn_addItemIfValid('SSN',csv_social)
			fn_addItemIfValid('PHONE',csv_respPhone)
			fn_addItemIfValid('PHONE2',csv_respWorkPhone)
			fn_add('RESP_PARTY' 	,'Y'                          	)
			fn_addItemIfValid('MOBIL_PHONE',csv_respCellPhone)
			if pos(item$(csv_EMAIL),'@')>0 then
				fn_add('EMAIL'       	,item$(csv_EMAIL)           	)
			end if
			fn_addItemIfValid('BIRTH_DATE',csv_patientDOB)
			fn_pr(111)
			! /r
		else ! The responsible person is not the same person as the patient
			! r: add Debtor 1 - the Responsible person
			fn_add('FIRM_FILENO' 	,item$(csv_FILENO)           ,1	)
			fn_add('NUMBER'      	,'1'                          	)
			fn_add('RELATION'   	,'MAIN'                       	)
			fn_add('FORW_REFNO' 	,item$(csv_KEY)               	)
			fn_add('NAME'        	,item$(csv_RESPNAME)          	)
			fn_addItemIfValid('STREET',csv_respAddress)
			if csv_respCsz>0 then
				fn_add('CITY'        	,tmpCity$                      	)
				fn_add('ST'          	,tmpSt$                         	)
				fn_add('ZIP'         	,tmpZip$                        	)
			end if
			fn_addItemIfValid('PHONE',csv_respPhone)         
			fn_addItemIfValid('PHONE2',csv_respWorkPhone)  
			fn_add('RESP_PARTY' 	,'Y')
			fn_addItemIfValid('MOBIL_PHONE',csv_respCellPhone)  
			if pos(item$(csv_EMAIL),'@')>0 then
				fn_add('EMAIL'       	,item$(csv_EMAIL)         	)
			end if
			fn_pr(111)
			! /r
			! r: add Debtor 2 - the Patient
			fn_add('FIRM_FILENO' 	,item$(csv_FILENO)           ,1	)
			fn_add('NUMBER'      	,'2'                            	)
			fn_add('RELATION'   	,'PATIENT'                     	)
			fn_add('FORW_REFNO' 	,item$(csv_KEY)         	)
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
def fn_date$(day$; format$,___,return$)
	if format$='' then format$='mm/dd/ccyy'
	return$=date$(val(day$),format$)
	! if return$(1:1)='0' then return$(1:1)=''
	! return$=srep$(return$,'/0','/')
	! above lines were for backward compatability to compare vs previous alldata import which did not touch formatting and had dates like 1/1/2019
	fn_date$=return$
fnend
def fn_writeInvoices(hOut,mat item$; ___,invoiceOrigAmt)
	fn_pr_hOut('0[tab]H[tab]Invoices for '&item$(csv_FILENO))
	invoiceOrigAmt=0
	if sourceId$='allData' then
		invoiceOrigAmt=fnval(item$(csv_BALANCE))+fnval(item$(csv_insurancePaid))+fnval(item$(csv_patientPaid))
	else if sourceId$='direct' then
		invoiceOrigAmt=fnval(item$(csv_charge))
	end if
	! CM EDI Record 180 Invoice File
	fn_add('FORW_REFNO'    	,item$(csv_KEY)             ,1)
	fn_add('FIRM_FILENO'   	,item$(csv_FILENO)                 	)
	fn_add('INV_DATE'      	,fn_date$(item$(csv_SERVICEDAY)))
	fn_add('DESCRIPTION'   	,item$(csv_PROVIDER))
	if csv_CPT>0 then
		fn_add('INV_DESC'   	,'CPT: '&item$(csv_CPT)) ! fnCptCode$(item$(csv_CPT))(1:20) 	)
	else if csv_procedureGroup>0 then
		fn_add('INV_DESC'   	,item$(csv_procedureGroup))
	end if
	fn_add('ACCT_NO'       	,item$(csv_FACILITY)          	)
	if enableInvoiceCpt and csv_CPT>0 then
		fn_add('CPT'        	,item$(csv_CPT))
	end if
	fn_add('DEBTOR_NO'     	,'1')
	fn_add('CUR_BAL'       	,item$(csv_BALANCE    )           	)
	fn_add('SERVICE_DATE'  	,fn_date$(item$(csv_SERVICEDAY)))
	fn_add('INV_NO'        	,item$(csv_INVOICENO  )           	)
	fn_add('ORIG_AMT'      	,str$(invoiceOrigAmt))
	fn_pr(180)
fnend
def fn_writePaperless(hOut,mat item$,lineCount,sourceFile$*512)
	fn_writePaperlessOneLine('***From line ',str$(lineCount+1)&' of ') ! +1 to account for uncounted header line
	fn_writePaperlessOneLine(sourceFile$(1:2),sourceFile$(3:inf))
	if sourceId$='allData' then
		fn_writePaperlessOneLine('    Facility Name:',item$(csv_FACILITY     	))
		fn_writePaperlessOneLine('    Provider Name:',item$(csv_PROVIDER     	))
		fn_writePaperlessOneLine('    Patient Name :',item$(csv_patientName      	))
		fn_writePaperlessOneLine('    Patient DOB  :',item$(csv_patientDOB       	))
		fn_writePaperlessOneLine('       Patient Id:',item$(csv_KEY        	))
		fn_writePaperlessOneLine('     Resp Name   :',item$(csv_RESPNAME         	))
		fn_writePaperlessOneLine('     Resp Address:',item$(csv_respAddress      	))
		fn_writePaperlessOneLine('     Resp CSZ    :',item$(csv_respCsz           	))
		fn_writePaperlessOneLine('    Patient Phone:',item$(csv_patientPhone     	))
		fn_writePaperlessOneLine('       Resp Phone:',item$(csv_respPhone        	))
		fn_writePaperlessOneLine('  Resp Cell Phone:',item$(csv_respCellPhone    	))
		fn_writePaperlessOneLine('  Resp Work Phone:',item$(csv_respWorkPhone    	))
		fn_writePaperlessOneLine('       Resp Email:',item$(csv_EMAIL        	))
		fn_writePaperlessOneLine('Social Security #:',item$(csv_social            	))
		fn_writePaperlessOneLine('         Claim Id:',item$(csv_claimId           	))
		fn_writePaperlessOneLine('  Primary Carrier:',item$(csv_primaryCarrier   	))
		fn_writePaperlessOneLine(' Insurance Number:',item$(csv_insuranceN        	))
		fn_writePaperlessOneLine('Secondary Carrier:',item$(csv_secondaryCarrier 	))
		fn_writePaperlessOneLine('Sec Insurance Num:',item$(csv_secInsuranceN    	))
		fn_writePaperlessOneLine('     Service Date:',fn_date$(item$(csv_SERVICEDAY)))
		fn_writePaperlessOneLine('              CPT:',item$(csv_CPT)&': '&fnCptCode$(item$(csv_CPT)))
		fn_writePaperlessOneLine('   Insurance Paid:',item$(csv_insurancePaid    	))
		fn_writePaperlessOneLine('     Patient Paid:',item$(csv_patientPaid      	))
		fn_writePaperlessOneLine('       Total Paid:',item$(csv_totalPaid        	))
		fn_writePaperlessOneLine('          Balance:',item$(csv_BALANCE           	))
		fn_writePaperlessOneLine('BillPatientReason:',item$(csv_billPatientReason	))
		fn_writePaperlessOneLine('      Claim Notes:',item$(csv_claimNotes        	))
	else if sourceId$='direct' then
		fn_writePaperlessOneLine('         AcctNo:',item$(csv_KEY)                   )
		fn_writePaperlessOneLine('          LName:',item$(csv_lName)                 )
		fn_writePaperlessOneLine('          FName:',item$(csv_fName)                 )
		fn_writePaperlessOneLine('             MI:',item$(csv_mi)                    )
		fn_writePaperlessOneLine('          EMail:',item$(csv_EMAIL)                 )
		fn_writePaperlessOneLine('            DoS:',fn_date$(item$(csv_SERVICEDAY))  )
		fn_writePaperlessOneLine('           Code:',item$(csv_code            )      )
		fn_writePaperlessOneLine('         Charge:',item$(csv_charge          )      )
		fn_writePaperlessOneLine('        Balance:',item$(csv_BALANCE         )      )
		fn_writePaperlessOneLine('       WriteOff:',item$(csv_writeOff        )      )
		fn_writePaperlessOneLine('ReceivedPrimary:',item$(csv_receivedPrimary )      )
		fn_writePaperlessOneLine('  ReceivedOther:',item$(csv_receivedOther   )      )
		fn_writePaperlessOneLine('  ReceivedTotal:',item$(csv_receivedTotal   )      )
		fn_writePaperlessOneLine('     NonAllowed:',item$(csv_nonAllowed      )      )
		fn_writePaperlessOneLine('  AppliedFromCB:',item$(csv_appliedFromCb   )      )
		fn_writePaperlessOneLine('      ClaimPhys:',item$(csv_PROVIDER        )      )
		fn_writePaperlessOneLine('            NPI:',item$(csv_npi             )      )
		fn_writePaperlessOneLine('   ClaimRefPhys:',item$(csv_claimrefphys    )      )
		fn_writePaperlessOneLine(' ProcedureGroup:',item$(csv_procedureGroup  )      )
		fn_writePaperlessOneLine('     PriInsName:',item$(csv_priInsName      )      )
		fn_writePaperlessOneLine(' DueFromInsName:',item$(csv_dueFromInsName  )      )
		fn_writePaperlessOneLine('     SecInsName:',item$(csv_secInsName      )      )
		fn_writePaperlessOneLine('        FacName:',item$(csv_FACILITY        )      )
		
		fn_writePaperlessOneLine('*    Resp Name   :',item$(csv_RESPNAME      )      )
	end if
	fn_writePaperlessOneLine('*          FileNo:',item$(csv_FILENO        )      )
	fn_writePaperlessOneLine('*      Invoice No:',item$(csv_INVOICENO     )      )

fnend
def fn_writePaperlessOneLine(comment$*256,comment2$*2048;___, which)
	dim polWrote$(0)*2048
	which=srch(mat polWrote$,item$(csv_KEY)&item$(csv_FILENO)&comment$&comment2$)
	if which<=0 then
		fnAddOneC(mat polWrote$,item$(csv_KEY)&item$(csv_FILENO)&comment$&comment2$)
		if trim$(comment2$)<>'' and comment2$<>'$0.00' then
			fn_add('FORW_REFNO'   	,item$(csv_KEY)	        ,1)
			fn_add('FIRM_FILENO'   	,item$(csv_FILENO)	          	)
			fn_add('PDATE'         	,useDate$        	)
			fn_add('PTIME'         	,useTime$        	)
			fn_add('INITIALS'     	,''              	)  ! leave blank for EDI and user id 1 as per EDI Help Manual
			fn_add('PCMT'          	,comment$&comment2$  	        	)
			fn_pr(109)
		end if
	end if

fnend
def fn_writeInfinity(hOut,mat item$)
	fn_add('FORW_REFNO'    	,item$(csv_KEY)             ,1)
	fn_add('FIRM_FILENO'   	,item$(csv_FILENO)                 	)
	fn_pr(134)
fnend
def fn_writeDiary(hOut,mat item$)
	fn_add('Date',useDate$                   ,1  )
	fn_add('Time',useTime$                       )
	fn_add('FORW_REFNO', item$(csv_KEY)          ) ! Forwarder or Senders internal File # C  
	fn_add('FORW_FILENO',item$(csv_KEY)          ) ! Forwarders file #( Credit Card #) C  
	fn_add('FIRM_FILENO',item$(csv_FILENO)       ) ! Receivers File # (Usually Left Blank) C  
	! fn_add('SENDER_ID',                        ) ! Sender ID Code C  
	! fn_add('RECEIVER_ID',                      ) ! Receiver ID Code C  
	fn_add('DDATE',useDate$                      )
	fn_add('DCODE',priorityDiaryCode$            ) ! Claim Diary Code N REQUIRED  
	fn_add('DCMT','CSS Premier Cardiology Import') ! Claim Diary Comment C  
	fn_add('DQUEUE'   ,'QCSSPCI'                 ) ! Claim Diary Queue C  
	fn_add('DTIME'    ,useTime$                  ) ! Claim Diary Time ##:##  
	fn_add('DPRIORITY','500'                     ) ! Claim Diary Priority --#  
	fn_pr(195)
fnend
dim hLine$*2048,dLine$*2408
def fn_addItemIfValid(hLineAdd$*128,enum)
	if enum>0 then
		fn_add(hLineAdd$,item$(enum))
	end if
fnend
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
	dLine$(0:0)=str$(num)&'[tab]D[tab]'&useDate$&tab$&useTime$&tab$
	fn_pr_hOut(dLine$)
fnend
def fn_pr_hOut(x$*2048)
	x$=srep$(x$,'[tab]',tab$)
	pr #hOut: x$&tab$&'#'
fnend
dim claim_fileno$    		(0)*8
dim claim_forwFileNo$		(0)*20
dim claim_forwRefNo$		(0)*20
dim claim_oc$						(0)*6
dim claim_balance				(0)
dim claim_forwNo				(0)
def fn_existingFileNoWintinForw$(ForwNo$,forwFileNo$,&efn_oc$; ___,return$,claimKey$*64,which,hClaim)
	if setup_efn<>val(ForwNo$) then ! r:
		setup_efn=val(ForwNo$)
		dim claim$(0)*60,claimN(0)
		dim claimFieldsC$(0)*20,claimFieldsN$(0)*20
		dim claimFormAll$*2048
		fnsql_setup$('master',mat claim$,mat claimN,mat claimFieldsC$,mat claimFieldsN$,claimFormAll$)
		gosub enumMaster

		mat claim_fileno$    		(0)
		mat claim_forwFileNo$		(0)
		mat claim_forwRefNo$		(0)
		mat claim_oc$						(0)
		mat claim_balance				(0)
		mat claim_forwNo				(0)
		for oc=1 to 2
			if oc=1 then
				hClaim=hClaimOpen
				efn_oc$='open'
			else
				hClaim=hClaimClosed
				efn_oc$='closed'
			end if
			do
				mat claim$=('')
				mat claimN=(0)
				read #hClaim,using claimFormAll$: mat claim$,mat claimN eof EfnEoMaster
				if claimN(master_forw_no)=val(ForwNo$) then
					fnAddOneC(mat claim_fileno$    		,trim$(	claim$(master_fileno)      	))
					fnAddOneC(mat claim_forwFileNo$		,trim$(	claim$(master_forw_fileno) 	))
					fnAddOneC(mat claim_forwRefNo$		,trim$(	claim$(master_forw_refno)  	))
					fnAddOneC(mat claim_oc$						,efn_oc$)
					fnAddOneN(mat claim_balance				,claimN(master_balance))
					fnAddOneN(mat claim_forwNo				,claimN(master_forw_no))
				end if
			loop
			EfnEoMaster: !
		nex oc
	end if ! /r
	
	which=srch(mat claim_forwFileNo$,trim$(forwFileNo$))
	if which<=0 then
		which2018=srch(mat claim_forwRefNo$,trim$(forwFileNo$)&'-2018')
		if which2018>0 then
			which=which2018
		end if
	end if
	
	if which<=0 then
		return$=''
		efn_oc$='new'
	else
		return$=claim_fileno$(which)
		efn_oc$=claim_oc$(which)
	end if
	! pr 'inside fn_existingFileNoWintinForw$ oc/return$=';efn_oc$&'/'&return$&'/'&forwFileNo$ ! if efn_oc$='closed' then pause
	! if forwFileNo$='3104647' or forwFileNo$='3084666' then efn_oc$='closed' : pr bell;'cheating': pause
	fn_existingFileNoWintinForw$=return$
fnend
def fn_getFileNo$(forwNo$,uniqueIdentifier$,&oc$; ___,x,return$,which,which2018,newFileNo$)
	if ~setup_getFileNo then
		setup_getFileNo=1
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
	end if
	return$=fn_existingFileNoWintinForw$(ForwNo$,uniqueIdentifier$,oc$)
	!		pr 'fileno$/forwNo/key/oc$=';return$&'/'&ForwNo$&'/'&uniqueIdentifier$&'/'&oc$ : pause
	if return$='' then
		uniqueIdentifier$=trim$(uniqueIdentifier$)
		which=srch(mat gfnKey$,uniqueIdentifier$)
		if which<=0 then
			do
				newFileNo$=sFileNo$(1:firstNumberInSfileNo-1)&cnvrt$(fileNumberFormat$,nextFileNumber)
				nextFileNumber+=1
			loop until ~fn_fileNoExist(newFileNo$)
			which=udim(mat gfnKey$)+1
			mat gfnKey$(which)
			mat gfnFileNo$(which)
			gfnKey$(which)=uniqueIdentifier$
			gfnFileNo$(which)=newFileNo$
			! pr 'returning a NEW file number of '&newFileNo$ : pause
		end if
		return$=gfnFileNo$(which)
	end if
	fn_getFileNo$=return$
fnend
def fn_fileNoExist(testFileNo$*8; ___,returnN)
	read #hClaimOpen,key=rpad$(filenNo$,8): nokey Fne_notInOpen
	returnN=1
	goto FneFinis
	Fne_notInOpen: !
	read #hClaimClosed,key=rpad$(filenNo$,8): nokey Fne_notInClosed
	returnN=2
	goto FneFinis
	Fne_notInClosed: !
	goto FneFinis
	FneFinis: !
	fn_fileNoExist=returnN
fnend
def fn_origionalClaimAmount(patientId$; ___,returnN,x,xStart)
	xStart=srch(mat list_KEY$,patientId$)
	if xStart>0 then
		for x=xStart to udim(mat list_KEY$)
			if list_KEY$(x)=patientId$ then
				if sourceId$='allData' then
					returnN+=list_BALANCEN(x)+list_totalPaidN(x)
				else if sourceId$='direct' then
					returnN+=list_BALANCEN(x)
					returnN+=list_receivedTotalN(x)
					returnN+=list_writeOffN(x)
				end if
			end if
		nex x
	end if
	fn_origionalClaimAmount=returnN
fnend

def fn_patientBalance(patientId$; ___,returnN,x,xStart)
	xStart=srch(mat list_KEY$,patientId$)
	if xStart>0 then
		for x=xStart to udim(mat list_KEY$)
			if list_KEY$(x)=patientId$ then
				returnN+=list_BALANCEN(x)
			end if
		nex x
	end if
	fn_patientBalance=returnN
fnend
def fn_invoiceNumber$*128(; ___,return$*128)
	if sourceId$='allData' then
		return$=item$(csv_KEY)&'-'&item$(csv_claimId)&'-'&fn_date$(item$(csv_SERVICEDAY),'ccyymmdd') ! XXX
	else if sourceId$='direct' then
		return$=item$(csv_KEY)&'-'&item$(csv_code)&'-'&fn_date$(item$(csv_SERVICEDAY),'ccyymmdd')
	end if
	fn_invoiceNumber$=return$
fnend

def fn_init_csv_in(&csvFieldCount,csvFile$*1024;___,returnN) ! everything here is local.
	if ~allData_init_csv_in then
		allData_init_csv_in=1
		dim csv_fields$(0)*128
		dim csv_data$(0)*256
		csvFieldCount=Fnopen_Csv(returnN:=fnGetHandle,env$('at')&srep$(csvFile$,'@::',''),csv_delimiter$,Mat csv_fields$,Mat csv_data$)
		lineCount=0
		
		csv_procedureGroup=0
		csv_CPT=0
		if sourceId$='allData' then
			csv_FACILITY          	=srch(mat csv_fields$,uprc$('Facility Name'        	))
			csv_PROVIDER          	=srch(mat csv_fields$,uprc$('Provider Name'        	))
			csv_patientName        	=srch(mat csv_fields$,uprc$('Patient Name'          	))
			csv_patientDOB         	=srch(mat csv_fields$,uprc$('Patient DOB'           	))
			csv_KEY                	=srch(mat csv_fields$,uprc$('Patient ID'            	))
			csv_RESPNAME           	=srch(mat csv_fields$,uprc$('Resp Name'             	))
			csv_respAddress        	=srch(mat csv_fields$,uprc$('Resp Address'          	))
			csv_respCsz            	=srch(mat csv_fields$,uprc$('Resp City-State-Zip'  	))
			csv_patientPhone      	=srch(mat csv_fields$,uprc$('Patient Phone'        	))
			csv_respPhone          	=srch(mat csv_fields$,uprc$('Resp Phone'            	))
			csv_respCellPhone     	=srch(mat csv_fields$,uprc$('Resp Cell Phone'      	))
			csv_respWorkPhone     	=srch(mat csv_fields$,uprc$('Resp Work Phone'      	))
			csv_EMAIL             	=srch(mat csv_fields$,uprc$('Resp Email'            	))
			csv_social             	=srch(mat csv_fields$,uprc$('Social'                	))
			csv_claimId            	=srch(mat csv_fields$,uprc$('Claim ID'              	))
			csv_primaryCarrier    	=srch(mat csv_fields$,uprc$('Primary Carrier'      	))
			csv_insuranceN        	=srch(mat csv_fields$,uprc$('Insurance#'          	))
			csv_secondaryCarrier  	=srch(mat csv_fields$,uprc$('Secondary Carrier'    	))
			csv_secInsuranceN     	=srch(mat csv_fields$,uprc$('Sec Insurance#'       	))
			csv_SERVICEDAY        	=srch(mat csv_fields$,uprc$('Service Date'          	))
			csv_CPT                	=srch(mat csv_fields$,uprc$('CPT'                    	))
			csv_insurancePaid     	=srch(mat csv_fields$,uprc$('Insurance Paid'       	))
			csv_patientPaid        	=srch(mat csv_fields$,uprc$('Patient Paid'          	))
			csv_totalPaid          	=srch(mat csv_fields$,uprc$('Total Paid'            	))
			csv_BALANCE            	=srch(mat csv_fields$,uprc$('Balance'               	))
			csv_billPatientReason 	=srch(mat csv_fields$,uprc$('Bill Patient Reason'  	))
			csv_claimNotes         	=srch(mat csv_fields$,uprc$('Claim Notes'           	))
		else if sourceId$='direct' then
			csv_KEY                =srch(mat csv_fields$,uprc$('acctNo'))
			csv_lName              =srch(mat csv_fields$,uprc$('lName'))
			csv_fName              =srch(mat csv_fields$,uprc$('fName'))
			csv_mi                 =srch(mat csv_fields$,uprc$('mi'))
			csv_EMAIL              =srch(mat csv_fields$,uprc$('email'))
			csv_SERVICEDAY         =srch(mat csv_fields$,uprc$('dos'))
			csv_code               =srch(mat csv_fields$,uprc$('code'))
			csv_charge             =srch(mat csv_fields$,uprc$('charge'))
			csv_BALANCE            =srch(mat csv_fields$,uprc$('balance'))
			csv_writeOff           =srch(mat csv_fields$,uprc$('writeOff'))
			csv_receivedPrimary    =srch(mat csv_fields$,uprc$('receivedPrimary'))
			csv_receivedOther      =srch(mat csv_fields$,uprc$('receivedOther'))
			csv_receivedTotal      =srch(mat csv_fields$,uprc$('receivedTotal'))
			csv_nonAllowed         =srch(mat csv_fields$,uprc$('nonAllowed'))
			csv_appliedFromCb      =srch(mat csv_fields$,uprc$('appliedFromCb'))
			csv_PROVIDER           =srch(mat csv_fields$,uprc$('claimPhys'))
			csv_npi                =srch(mat csv_fields$,uprc$('npi'))
			csv_claimrefphys       =srch(mat csv_fields$,uprc$('claimrefphys'))
			csv_procedureGroup     =srch(mat csv_fields$,uprc$('procedureGroup'))
			csv_priInsName         =srch(mat csv_fields$,uprc$('priInsName'))
			csv_dueFromInsName     =srch(mat csv_fields$,uprc$('dueFromInsName'))
			csv_secInsName         =srch(mat csv_fields$,uprc$('secInsName'))
			csv_FACILITY            =srch(mat csv_fields$,uprc$('facName'))
			
			csvFieldCount+=1 : csv_RESPNAME   	=fnAddOneC(mat csv_fields$,'respName')
		else 
			pr bell;'invalid sourceId$': pause
		end if
		! ***  Add New Invented Columns ***
			csvFieldCount+=1 : csv_FILENO   	=fnAddOneC(mat csv_fields$,'FileNo')
			csvFieldCount+=1 : csv_INVOICENO	=fnAddOneC(mat csv_fields$,'InvoiceNo')


	end if
	fn_init_csv_in=returnN
fnend
def fn_close_csv_in(; delFileIn)
	if delFileIn then
		close #hIn,free:
	else
		close #hIn:
	end if
	allData_init_csv_in=0
fnend
def fn_readFileIntoArrays(;___,oc$,respName$*256)
	! r: common and allData dims
		dim list_KEY$(0)*256
		dim list_FACILITY$(0)*256
		dim list_PROVIDER$(0)*256
		dim list_patientName$(0)*256
		dim list_patientDOB$(0)*256
		dim list_RESPNAME$(0)*256
		dim list_respAddress$(0)*256
		dim list_respCsz$(0)*256
		dim list_patientPhone$(0)*256
		dim list_respPhone$(0)*256
		dim list_respCellPhone$(0)*256
		dim list_respWorkPhone$(0)*256
		dim list_EMAIL$(0)*256
		dim list_social$(0)*256
		dim list_claimId$(0)*256
		dim list_primaryCarrier$(0)*256
		dim list_insuranceN$(0)*256
		dim list_secondaryCarrier$(0)*256
		dim list_secInsuranceN$(0)*256
		dim list_SERVICEDAY$(0)*256
		dim list_CPT$(0)*256
		dim list_insurancePaidN(0)
		dim list_patientPaidN(0)
		dim list_totalPaidN(0)
		dim list_BALANCEN(0)
		dim list_billPatientReason$(0)*256
		dim list_claimNotes$(0)*512
		dim list_INVOICENO$(0)*128
		dim list_FILENO$(0)*512
		dim list_OCN$(0)
	! /r
	
	! r: direct only dims
		dim list_lName$(0)*256
		dim list_fName$(0)*256
		dim list_mi$(0)*256
		dim list_code$(0)*256
		dim list_charge$(0)*256
		dim list_balance$(0)*256
		dim list_writeOffN(0)
		dim list_receivedPrimary$(0)*256
		dim list_receivedOther$(0)*256
		dim list_receivedTotalN(0)
		dim list_nonAllowed$(0)*256
		dim list_appliedFromCb$(0)*256
		dim list_npi$(0)*256
		dim list_claimrefphys$(0)*256
		dim list_procedureGroup$(0)*256
		dim list_priInsName$(0)*256
		dim list_dueFromInsName$(0)*256
		dim list_secInsName$(0)*256
	! /r
	
	! r: common and allData mat (0)s
		mat list_KEY$(0)
		mat list_FACILITY$(0)
		mat list_PROVIDER$(0)
		mat list_patientName$(0)
		mat list_patientDOB$(0)
		mat list_RESPNAME$(0)
		mat list_respAddress$(0)
		mat list_respCsz$(0)
		mat list_patientPhone$(0)
		mat list_respPhone$(0)
		mat list_respCellPhone$(0)
		mat list_respWorkPhone$(0)
		mat list_EMAIL$(0)
		mat list_social$(0)
		mat list_claimId$(0)
		mat list_primaryCarrier$(0)
		mat list_insuranceN$(0)
		mat list_secondaryCarrier$(0)
		mat list_secInsuranceN$(0)
		mat list_SERVICEDAY$(0)
		mat list_CPT$(0)
		mat list_insurancePaidN(0)
		mat list_patientPaidN(0)
		mat list_totalPaidN(0)
		mat list_BALANCEN(0)
		mat list_billPatientReason$(0)
		mat list_claimNotes$(0)
		mat list_INVOICENO$(0)
		mat list_FILENO$(0)
		mat list_OCN$(0)
	! /r
	
	! r: direct mat (0)s
		mat list_lName$(0)
		mat list_fName$(0)
		mat list_mi$(0)
		mat list_EMAIL$(0)
		mat list_SERVICEDAY$(0)
		mat list_code$(0)
		mat list_charge$(0)
		mat list_balance$(0)
		mat list_writeOffN(0)
		mat list_receivedPrimary$(0)
		mat list_receivedOther$(0)
		mat list_receivedTotalN(0)
		mat list_nonAllowed$(0)
		mat list_appliedFromCb$(0)
		mat list_npi$(0)
		mat list_claimrefphys$(0)
		mat list_procedureGroup$(0)
		mat list_priInsName$(0)
		mat list_dueFromInsName$(0)
		mat list_secInsName$(0)
	! /r

	hIn=fn_init_csv_in(csvFieldCount,csvFile$)

	do
		dim line$*1024
		linput #hIn: line$ eof Rafia_EoF
		lineCount+=1
		if line$<>'' and line$<>'"' then
			dim item$(0)*512
			str2mat(line$,mat item$,tab$)
			mat item$(csvFieldCount)
			item$(csv_FILENO)=fn_getFileNo$(forwNo$,item$(csv_KEY),oc$)
			! pr 'fileno$/key/oc$=';item$(csv_FILENO)&'/'&item$(csv_KEY)&'/'&oc$ : pause
			item$(csv_SERVICEDAY)=str$(days(item$(csv_SERVICEDAY),'mm/dd/ccyy'))
			
			fnAddOneC(mat list_FILENO$,item$(csv_FILENO))
			fnAddOneC(mat list_OCN$,oc$)
			fnAddOneC(mat list_KEY$,item$(csv_KEY))
			fn_ifValidAddItemC(mat list_procedureGroup$,csv_procedureGroup)
			fnAddOneC(mat list_FACILITY$,item$(csv_FACILITY))
			fnAddOneC(mat list_PROVIDER$,item$(csv_PROVIDER))
			fn_ifValidAddItemC(mat list_patientName$,csv_patientName)
			fn_ifValidAddItemC(mat list_patientDOB$,csv_patientDOB)
			
			if sourceId$='allData' then
			fn_ifValidAddItemC(mat list_RESPNAME$,csv_RESPNAME)
				fnAddOneC(mat list_RESPNAME$,item$(csv_RESPNAME))
			else if sourceId$='direct' then
				respName$=rtrm$(item$(csv_lName))
				if trim$(item$(csv_fName))<>'' then
					respName$=respName$&', '&rtrm$(item$(csv_fName))
				end if
				if trim$(item$(csv_mi))<>'' then
					respName$=respName$&' '&rtrm$(item$(csv_mi))
				end if
				fnAddOneC(mat list_RESPNAME$,respName$)
			end if
			fn_ifValidAddItemC(mat list_respAddress$,csv_respAddress)
			
			fn_ifValidAddItemC(mat list_respCsz$,csv_respCsz)
			fn_ifValidAddItemC(mat list_patientPhone$,csv_patientPhone)
			fn_ifValidAddItemC(mat list_respPhone$,csv_respPhone)
			fn_ifValidAddItemC(mat list_respCellPhone$,csv_respCellPhone)
			fn_ifValidAddItemC(mat list_respWorkPhone$,csv_respWorkPhone)
			fn_ifValidAddItemC(mat list_EMAIL$,csv_EMAIL)
			fn_ifValidAddItemC(mat list_social$,csv_social)
			fn_ifValidAddItemC(mat list_claimId$,csv_claimId)
			fn_ifValidAddItemC(mat list_primaryCarrier$,csv_primaryCarrier)
			fn_ifValidAddItemC(mat list_insuranceN$,csv_insuranceN)
			fn_ifValidAddItemC(mat list_secondaryCarrier$,csv_secondaryCarrier)
			fn_ifValidAddItemC(mat list_secInsuranceN$,csv_secInsuranceN)
			fnAddOneC(mat list_SERVICEDAY$,item$(csv_SERVICEDAY))
			fn_ifValidAddItemC(mat list_CPT$,csv_CPT)
			fn_ifValidAddItemN(mat list_insurancePaidN,csv_insurancePaid)
			fn_ifValidAddItemN(mat list_patientPaidN,csv_patientPaid)
			fn_ifValidAddItemN(mat list_totalPaidN,csv_totalPaid)
			fn_ifValidAddItemN(mat list_BALANCEN,csv_BALANCE)
			! if list_BALANCEN(udim(mat list_BALANCEN))=0 then pr 'fnval('&item$(csv_BALANCE)&') returned 0' : pause
			fn_ifValidAddItemC(mat list_billPatientReason$,csv_billPatientReason)
			fn_ifValidAddItemC(mat list_claimNotes$,csv_claimNotes)
			fnAddOneC(mat list_INVOICENO$,fn_invoiceNumber$)

			
			
			fn_ifValidAddItemC(mat list_lName$,csv_lName)
			fn_ifValidAddItemC(mat list_fName$,csv_fName)
			fn_ifValidAddItemC(mat list_mi$,csv_mi)
			fn_ifValidAddItemC(mat list_EMAIL$,csv_EMAIL)
			fn_ifValidAddItemC(mat list_code$,csv_code)
			fn_ifValidAddItemC(mat list_charge$,csv_charge)
			fn_ifValidAddItemC(mat list_balance$,csv_BALANCE)
			fn_ifValidAddItemN(mat list_writeOffN,csv_writeOff)
			fn_ifValidAddItemC(mat list_receivedPrimary$,csv_receivedPrimary)
			fn_ifValidAddItemC(mat list_receivedOther$,csv_receivedOther)
			fn_ifValidAddItemN(mat list_receivedTotalN,csv_receivedTotal)
			fn_ifValidAddItemC(mat list_nonAllowed$,csv_nonAllowed)
			fn_ifValidAddItemC(mat list_appliedFromCb$,csv_appliedFromCb)
			fn_ifValidAddItemC(mat list_npi$,csv_npi)
			fn_ifValidAddItemC(mat list_claimrefphys$,csv_claimrefphys)
			fn_ifValidAddItemC(mat list_procedureGroup$,csv_procedureGroup)
			fn_ifValidAddItemC(mat list_priInsName$,csv_priInsName)
			fn_ifValidAddItemC(mat list_dueFromInsName$,csv_dueFromInsName)
			fn_ifValidAddItemC(mat list_secInsName$,csv_secInsName)
		else
			pr 'invalid line encountered:'&line$
			pause
		end if
	loop
	Rafia_EoF: !
	fn_close_csv_in
fnend
def fn_ifValidAddItemC(mat list$,csv_enum)
	if csv_enum>0 then
		fnAddOneC(mat list$,item$(csv_enum))
	end if
fnend
def fn_ifValidAddItemN(mat listN,csv_enum)
	if csv_enum>0 then
		fnAddOneN(mat listN,fnVal(item$(csv_enum)))
	end if
fnend

def fn_setup
	on error goto Ertn
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
		library 'S:\Core\Library.br': fnsrch_case_insensitive
		
		library 'S:\Collection-Master Add-On\fn\Library.br': fnCptCode$
		library 'S:\Collection-Master Add-On\fn\Library.br': fnVal

		library 'Library\SQL.wb': fnsql_setup$

		gosub Enum
		gosub SetupPrint

		dim forw$(0)*60,forwN(0)
		dim forwFieldsC$(0)*20,forwFieldsN$(0)*20
		dim forwFormAll$*2048
		! execute "*SubProc "&     <--- not necessary with include:enum\forw  and  gosub Enumforw
		fnsql_setup$('masforw',mat forw$,mat forwN,mat forwFieldsC$,mat forwFieldsN$,forwFormAll$)
		gosub EnumForw

	end if
fnend

def fn_removeExcessCRLF$*256(csvFile$*256; ___,return$*256,hIn,hOut,line$*1024,delim$,lineCount,itemsOnLine,lineCountIn,lineCountOut)

	dim csv_fields$(0)*128
	dim csv_data$(0)*256
	minItemCount=Fnopen_Csv(hIn:=fnGetHandle,env$('at')&csvFile$,delim$,Mat csv_fields$,Mat csv_data$)
	lineCount=0
	close #hIn: ioerr ignore
	open #hIn:=fnGetHandle:  'name='&env$('at')&csvFile$,display,input
include: filenamesPushMixedCase
	open #hOut:=fnGetHandle: 'name='&env$('at')&csvFile$&'-fixedCrLf,recl=2048,replace',display,output
include: filenamesPopUpperCase

	do
		linput #hIn: line$ eof Recrlf_EoF
		lineCountIn+=1

		dim removeExcess_item$(0)*512
		str2mat(line$&' ',mat removeExcess_item$,delim$)
		itemsOnLine=udim(mat removeExcess_item$)
		removeExcess_item$(itemsOnLine)=trim$(removeExcess_item$(itemsOnLine))
		! itemsOnLine+=fn_itemCount(line$,delim$)

		if removeExcess_item$(itemsOnLine)(1:1)='"' and removeExcess_item$(itemsOnLine)(len(removeExcess_item$(itemsOnLine)):len(removeExcess_item$(itemsOnLine)))<>'"' then
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
	dim itemCount_item$(0)*512
	str2mat(line$&' ',mat itemCount_item$,delim$)
	returnN=udim(mat itemCount_item$)
	fn_itemCount=returnN
fnend
def fn_reportClosedEncounter(oc$,fileno$,forwNo$,forwFileNo$,Balance$,BalanceNew$; ___,whichFileNo)

	if ~reportClosedEncounter_init then
		reportClosedEncounter_init=1
		fnSel(1024, 'Select Output',255, '','HTML')
		if fkey=93 or fkey=99 then pr 'unexpected cancel' : pause
		pr #255: '</pre>'
		pr #255: '<table algin="Center">'
		pr #255: '  <tr><td colspan="2" align="Center"><h2>'&env$('program_caption')&'</h2></td></tr>'
		pr #255: '  <tr><td colspan="2" align="Center"><h3>as of '&date$('month d, ccyy')&' '&useTime$&'</h3></td></tr>'
		pr #255: '  <tr><td colspan="2" align="Center"><h3>These accounts already exist in closed and were skipped. Only open accounts can be updated.</h3></td></tr>'
		pr #255: '  <tr>'
		pr #255: '    <td><h4>OC</h4></td>'
		pr #255: '    <td><h4>FileNo</h4></td>'
		pr #255: '    <td><h4>ForwNo</h4></td>'
		pr #255: '    <td><h4>Forw Ref No</h4></td>'
		pr #255: '    <td><h4>Balance Old</h4></td>'
		pr #255: '    <td><h4>Balance New</h4></td>'
		pr #255: '  </tr>'
		dim alreadyReportedFileNo$(0)
		mat alreadyReportedFileNo$(0)
	end if

	whichFileNo=srch(mat alreadyReportedFileNo$,fileno$)
	if whichFileNo<=0 then
		fnAddOneC(mat alreadyReportedFileNo$,fileno$)
		pr #255: '  <tr>'
		pr #255: '    <td>'&oc$&'</td>'
		pr #255: '    <td>'&fileno$&'</td>'
		pr #255: '    <td>'&forwNo$&'</td>'
		pr #255: '    <td>'&forwRefNo$&'</td>'
		pr #255: '    <td>'&Balance$&'</td>'
		pr #255: '    <td>'&BalanceNew$&'</td>'
		pr #255: '  </tr>'
	end if
fnend
def fn_reportClosedEncounter_finis
	pr #255: '</table>'
	fnclose
	reportClosedEncounter_init=0
fnend
Xit: !
fnXit
include: cm\enum\common
include: cm\enum\forw
include: cm\enum\master
include: cm\print
include: cm\err