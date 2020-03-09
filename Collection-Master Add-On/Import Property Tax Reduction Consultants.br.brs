fn_setup
fnTop(program$)

	cssDebug=1
	if sourceId$='' then sourceId$='allData'
	if sourceId$<>'allData' and sourceId$<>'direct' then
		pr bell;'unrecognized sourceId$='&sourceId$ : pause
		goto PciXit
	end if

	open #hClaimOpen:=fnGetHandle: "name=MASTER//6,kfname=MASTERX//6,shr",internal,input,keyed
	open #hClaimbyForwarderOpen:=fnGetHandle: "name=MASTER//6,kfname=FORWIDXA//6,shr",internal,input,keyed

	open #hClaimClosed:=fnGetHandle: "name=HISTORY//1,kfname=HISTORYX//1,shr",internal,input,keyed
	open #hClaimbyForwarderClosed:=fnGetHandle: "name=HISTORY//1,kfname=FORWIDXA//1,shr",internal,input,keyed
	hInv=Fnopen_Invoice(mat hInvoice)

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
		fnreg_read(env$('program_caption')&'.ocacDiaryCode',ocacDiaryCode$,'222')
	else
		fnreg_read(env$('program_caption')&'.csvFile',csvFile$, 'D:\CM\Stern and Stern\New_format_Premier_Cardiology.xlsx')
		fnreg_read(env$('program_caption')&'.starting fileno',sFileNo$,'PCE01001')
		fnreg_read(env$('program_caption')&'.forw no',forwNo$,'1617')
		fnreg_read(env$('program_caption')&'.enableImport',enableImport$) : if enableImport$='True' then enableImport=1 else enableImport=0
		fnreg_read(env$('program_caption')&'.priorityColumnN',priorityColumn$)
		fnreg_read(env$('program_caption')&'.priorityText',priorityText$)
		fnreg_read(env$('program_caption')&'.priorityDiaryCode',priorityDiaryCode$)
		fnreg_read(env$('program_caption')&'.ocacDiaryCode',ocacDiaryCode$)
	end if
	! /r
	if fn_askScreen1(csvFile$,sFileNo$,forwNo$,enableImport,enableImport$,priorityColumn$,priorityText$,priorityDiaryCode$,ocacDiaryCode$)=99 then
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
		fnreg_write(env$('program_caption')&'.ocacDiaryCode',ocacDiaryCode$)
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
		csvFile$=fnRemoveExcessCRLF$(csvFile$)

		fn_readFileIntoArrays
		fn_writeArraysToTestFile(csvOrigional$&'-debug.txt')

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

				if sourceId$='direct' then
					serviceDayFormat$='mm/dd/yy'
				else
					serviceDayFormat$='mm/dd/ccyy'
				end if
				if days(item$(csv_SERVICEDAY),serviceDayFormat$)<>val(list_SERVICEDAY$(lineCount)) then ! these things should always match up - we shouldn't even need to read them in again
					pr 'item serviceday=';days(item$(csv_SERVICEDAY),serviceDayFormat$)
					pr 'list serviceday=';list_SERVICEDAY$(lineCount)
					pause
				end if

				item$(csv_FILENO)    =list_FILENO$(lineCount)        : fileno$=item$(csv_FILENO)
				item$(csv_SERVICEDAY)=list_SERVICEDAY$(lineCount)
				oc$                  =list_OCN$(lineCount)
				invoiceExist$        =list_INVOICEEXIST$(lineCount)
				invoiceOrigAmt       =list_INVOICEORIGAMTN(lineCount)
				invoiceRateN         =list_InvoiceRateN(lineCount)
				item$(csv_INVOICENO) =list_INVOICENO$(lineCount)
				item$(csv_RESPNAME)  =list_RESPNAME$(lineCount)
				item$(csv_EMAIL)     =list_EMAIL$(lineCount)
				item$(csv_PROVIDER)  =list_PROVIDER$(lineCount)
				if csv_code>0 then
					item$(csv_code)    =list_code$(lineCount)
				end if
				if csv_patientName>0 then
					item$(csv_patientName)=srep$(item$(csv_patientName),', ',',')
					item$(csv_patientName)=srep$(item$(csv_patientName),',','/')
				end if
				item$(csv_RESPNAME)=srep$(item$(csv_RESPNAME),', ',',')
				item$(csv_RESPNAME)=srep$(item$(csv_RESPNAME),',','/')
				! pr oc$ : pause

				if oc$='closed' then
					masterWhich=srch(claim_fileno$,list_FILENO$(lineCount))
					fn_reportClosedEncounter(oc$,list_FILENO$(lineCount),str$(claim_forwNo(masterWhich)),claim_forwFileNo$(masterWhich),str$(claim_balance(masterWhich)),str$(fn_patientBalance(item$(csv_KEY))))
				else

					fn_pr_hOut('0[tab]H[tab] +++ lineCount: '&str$(lineCount)	&' fileNo: '&item$(csv_FILENO)&' +++')
					! pr '0[tab]H[tab] +++ lineCount: '&str$(lineCount)	&' fileNo: '&item$(csv_FILENO)&' +++'
					! item$(csv_INVOICENO)=item$(csv_claimId)&'-'&fn_date$(item$(csv_SERVICEDAY),'ccyymmdd')

					fn_writeDemographics(hOut,oc$,mat item$)
					fn_writePaperless(hOut,mat item$,lineCount,csvFile$)
					fn_writeInvoice(hOut,mat item$,invoiceOrigAmt,invoiceRateN)
					! fn_writeInfinity(hOut,mat item$)
					if fn_isPriority(mat item$,priorityColumnN,priorityText$) then
							priorityCount+=1
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
	close #hClaimbyForwarderOpen:
	close #hClaimClosed:
	close #hClaimbyForwarderClosed:
	close #hInv:
	mat invRecNoReturned(0)
	fn_pr_hOut('0[tab]H[tab]lineCount='&str$(lineCount))
	fn_close_csv_in( 1)
	close #hOut:
	fn_reportClosedEncounter_finis
	if enableImport then
		! r: Generate Automation Files
		open #hIni:=fnGetHandle: 'name=custom\cm_edi_ptrc.ini,replace',d,o
		pr #hIni: '||MENUPATH:4-2-1-1'
		pr #hIni: 'EDI NUMBER=326'
		pr #hIni: 'FILE NUMBER='&sFileNo$
		pr #hIni: 'SOURCE FILE='&outFile$
		pr #hIni: 'UNASSIGNED FORW='&forwNo$
		close #hIni:

		open #hCmd:=fnGetHandle: 'name=batch\cm_edi_ptrc.cmd,replace',d,o
		pr #hCmd: 'f:'
		pr #hCmd: 'cd \clsinc'
		pr #hCmd: 'set Automate=cm_edi_ptrc.ini'
		pr #hCmd: os_filename$(env$('at')&'f:\clsinc\wbwin\br32.exe') ! brclient.exe
		close #hCmd:
		! setenv('Automate','cm_edi_ptrc.ini')
		! /r
		! exec 'proc run'
		execute 'sy -c -M F:\clsinc\batch\cm_edi_ptrc.cmd'
	else
		dim mbText$*2048
		mbText$='New Claims: '&tab$&str$(countNewClaim)
		mbText$&=lf$&'Claim Updates-Open: '&tab$&str$(countUpdateClaimOpen)
		mbText$&=lf$&'Claim Skipped-Closed: '&tab$&str$(udim(mat alreadyReportedFileNo$))
		mbText$&=lf$&'Claim Diaried for Priority: '&tab$&str$(priorityCount)
		if cssDebug then
			mbText$&=lf$&'lineCount: '&tab$&str$(lineCount)
		end if
		mbText$&=lf$&'Sucessfully created a file for CM EDI Import:'
		mbText$&=lf$&outFile$
		fnMessageBox(mbText$,mb_information+mb_okonly,env$('program_caption'))
		! msgbox('Success on '&csvFile$)
	end if
	goto PciXit ! /r
	PciXit: !
goto Xit
def fn_isPriority(mat item$,priorityColumnN,priorityText$)
	if pos(lwrc$(item$(priorityColumnN)),lwrc$(trim$(priorityText$)))>0 then
		returnN=1
	else
		returnN=0
	end if
	fn_isPriority=returnN
fnend


def fn_askScreen1(&sourceFile$,&sFileNo$,&forwNo$,&enableImport,&enableImport$,&priorityColumn$,&priorityText$,&priorityDiaryCode$,&ocacDiaryCode$; ___,returnN,rc,lc)
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
	col1len=42 ! 36
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
	fnLbl (lc+=1,col1pos,'Origional Claim Amount Changed Diary Code:', col1len,1)
	fncombof('DiaryCd',lc,col2pos,width,'SHARE\DIARYCD.INT',151,3,1,50, 'SHARE\DIARYCD.IDX',1,0,'Diary Code to be added if Priority Text found in Priority Column.',0,0,'BH')
	resp$(resp_ocacDiaryCode:=rc+=1)=ocacDiaryCode$

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
		ocacDiaryCode$			=resp$(resp_ocacDiaryCode)(1:pos(resp$(resp_ocacDiaryCode),' ')-1)
		enableImport$				=resp$(resp_enableImport)
		if enableImport$='True' then let enableImport=1 else enableImport=0
		if sourceFile$(len(sourceFile$):len(sourceFile$))='\' then
			sourceFile$&='*.*'
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


def fn_init_csv_in(&csvFieldCount,csvFile$*1024;___,returnN) ! everything here is local.
	if ~allData_init_csv_in then
		allData_init_csv_in=1
		dim csv_fields$(0)*128
		dim csv_data$(0)*256
		csvFieldCount=Fnopen_Csv(returnN:=fnGetHandle,env$('at')&srep$(csvFile$,'@::',''),csv_delimiter$,Mat csv_fields$,Mat csv_data$)
		lineCount=0

		csv_ForwFile            =srch(mat csv_fields$,uprc$('FORW_FILE'             ))
		csv_OrigClaim           =srch(mat csv_fields$,uprc$('ORIG_CLAIM'            ))
		csv_CredName            =srch(mat csv_fields$,uprc$('CRED_NAME'             ))
		csv_CredStreet          =srch(mat csv_fields$,uprc$('CRED_STREET'           ))
		csv_CredCs              =srch(mat csv_fields$,uprc$('CRED_CS'               ))
		csv_CredZip             =srch(mat csv_fields$,uprc$('CRED_ZIP'              ))
		csv_DebtName            =srch(mat csv_fields$,uprc$('DEBT_NAME'             ))
		csv_DebtStree           =srch(mat csv_fields$,uprc$('DEBT_STREE'            ))
		csv_DebtCS              =srch(mat csv_fields$,uprc$('DEBT_CS'               ))
		csv_DebtZip             =srch(mat csv_fields$,uprc$('DEBT_ZIP'              ))
		csv_DebtPhon            =srch(mat csv_fields$,uprc$('DEBT_PHON'             ))
		csv_SBL                 =srch(mat csv_fields$,uprc$('S_B_L'                 ))
		csv_DateGrant           =srch(mat csv_fields$,uprc$('DATE_GRANT'            ))
		csv_TaxYear             =srch(mat csv_fields$,uprc$('TAX_YEAR'              ))
		csv_IdNumb              =srch(mat csv_fields$,uprc$('ID_NUMB'               ))
		csv_OrigBal             =srch(mat csv_fields$,uprc$('ORIG_BAL'              ))
		csv_Int                 =srch(mat csv_fields$,uprc$('INT'                   ))
		csv_Misc                =srch(mat csv_fields$,uprc$('MISC'                  ))
		csv_PtrcRep             =srch(mat csv_fields$,uprc$('PTRC REP'              ))
		csv_TownTrueSavings     =srch(mat csv_fields$,uprc$('TOWN_TrueSavings'      ))
		csv_ColTwnAmtDue        =srch(mat csv_fields$,uprc$('COL_Twn_Amt_Due'       ))
		csv_AllocatedAmt        =srch(mat csv_fields$,uprc$('Allocated Amt'         ))
		csv_COLMiscFee          =srch(mat csv_fields$,uprc$('COL_Misc_Fee'          ))
		csv_AllocatedAmt        =srch(mat csv_fields$,uprc$('Allocated Amt'         ))
		csv_ColLateFee          =srch(mat csv_fields$,uprc$('COL_Late_Fee'          ))
		csv_AllocatedAmt        =srch(mat csv_fields$,uprc$('Allocated Amt'         ))
		csv_TownTFiling_Fee     =srch(mat csv_fields$,uprc$('TOWN_TFiling_Fee'      ))
		csv_AllocatedAmt        =srch(mat csv_fields$,uprc$('Allocated Amt'         ))
		csv_TownTamtDueAndFees  =srch(mat csv_fields$,uprc$('TOWN_TamtDueAndFees'   ))
		csv_VillSavings_Amt     =srch(mat csv_fields$,uprc$('VILL_Savings_Amt'      ))
		csv_ColVilAmtDue        =srch(mat csv_fields$,uprc$('COL_Vil_Amt_Due'       ))
		csv_AllocatedAmt        =srch(mat csv_fields$,uprc$('Allocated Amt'         ))
		csv_VillVillFilingFee   =srch(mat csv_fields$,uprc$('VILL_Vill_Filing_Fee'  ))
		csv_AllocatedAmt        =srch(mat csv_fields$,uprc$('Allocated Amt'         ))
		csv_VillVamtDueAndFees  =srch(mat csv_fields$,uprc$('VILL_VamtDueAndFees'   ))
		csv_ColTotalBilled      =srch(mat csv_fields$,uprc$('COL_Total_Billed'      ))
		csv_ColTotalPaid        =srch(mat csv_fields$,uprc$('COL_Total_Paid'        ))
		csv_ColBilledBalance    =srch(mat csv_fields$,uprc$('COL_Billed_Balance'    ))
		csv_ColBalanceSold      =srch(mat csv_fields$,uprc$('COL_Balance_Sold'      ))



		! ***  Add New Invented Columns ***
			csvFieldCount+=1 : csv_FILENO           =fnAddOneC(mat csv_fields$,'FileNo'        )
			csvFieldCount+=1 : csv_INVOICENO        =fnAddOneC(mat csv_fields$,'InvoiceNo'     )
			csvFieldCount+=1 : csv_DEBTORUNIQUEID   =fnAddOneC(mat csv_fields$,'DebtorUniqueId')
			csvFieldCount+=1 : csv_OCN              =fnAddOneC(mat csv_fields$,'OCN'           )
			csvFieldCount+=1 : csv_INVOICEEXIST     =fnAddOneC(mat csv_fields$,'InvoiceExist'  )
			csvFieldCount+=1 : csv_INVOICEORIGAMT   =fnAddOneC(mat csv_fields$,'InvoiceOrigAmt')
			csvFieldCount+=1 : csv_INVOICERATE      =fnAddOneC(mat csv_fields$,'InvoiceRate'   )

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


def fn_readFileIntoArrays(;___,oc$,respName$*256,tmpServiceDay,invoiceOrigAmt)
	! r: dims
	dim list_ForwFile$(0)*256
	dim list_OrigClaim$(0)*256
	dim list_CredName$(0)*256
	dim list_CredStreet$(0)*256
	dim list_CredCs$(0)*256
	dim list_CredZip$(0)*256
	dim list_DebtName$(0)*256
	dim list_DebtStree$(0)*256
	dim list_DebtCS$(0)*256
	dim list_DebtZip$(0)*256
	dim list_DebtPhon$(0)*256
	dim list_SBL$(0)*256
	dim list_DateGrant$(0)*256
	dim list_TaxYear$(0)*256
	dim list_IdNumb$(0)*256
	dim list_OrigBal$(0)*256
	dim list_Int$(0)*256
	dim list_Misc$(0)*256
	dim list_PtrcRep$(0)*256
	dim list_TownTrueSavings$(0)*256
	dim list_ColTwnAmtDue$(0)*256
	dim list_AllocatedAmt$(0)*256
	dim list_COLMiscFee$(0)*256
	dim list_AllocatedAmt$(0)*256
	dim list_ColLateFee$(0)*256
	dim list_AllocatedAmt$(0)*256
	dim list_TownTFiling_Fee$(0)*256
	dim list_AllocatedAmt$(0)*256
	dim list_TownTamtDueAndFees$(0)*256
	dim list_VillSavings_Amt$(0)*256
	dim list_ColVilAmtDue$(0)*256
	dim list_AllocatedAmt$(0)*256
	dim list_VillVillFilingFee$(0)*256
	dim list_AllocatedAmt$(0)*256
	dim list_VillVamtDueAndFees$(0)*256
	dim list_ColTotalBilled$(0)*256
	dim list_ColTotalPaid$(0)*256
	dim list_ColBilledBalance$(0)*256
	dim list_ColBalanceSold$(0)*256

	dim list_FILENO$(0)*256
	dim list_INVOICENO$(0)*256
	dim list_DEBTORUNIQUEID$(0)*256
	dim list_OCN$(0)*256
	dim list_INVOICEEXIST$(0)*256
	dim list_INVOICEORIGAMT$(0)*256
	dim list_INVOICERATE$(0)*256

	! /r


	! r: common and allData mat (0)s

		mat list_ForwFile$(0)
		mat list_OrigClaim$(0)
		mat list_CredName$(0)
		mat list_CredStreet$(0)
		mat list_CredCs$(0)
		mat list_CredZip$(0)
		mat list_DebtName$(0)
		mat list_DebtStree$(0)
		mat list_DebtCS$(0)
		mat list_DebtZip$(0)
		mat list_DebtPhon$(0)
		mat list_SBL$(0)
		mat list_DateGrant$(0)
		mat list_TaxYear$(0)
		mat list_IdNumb$(0)
		mat list_OrigBal$(0)
		mat list_Int$(0)
		mat list_Misc$(0)
		mat list_PtrcRep$(0)
		mat list_TownTrueSavings$(0)
		mat list_ColTwnAmtDue$(0)
		mat list_AllocatedAmt$(0)
		mat list_COLMiscFee$(0)
		mat list_AllocatedAmt$(0)
		mat list_ColLateFee$(0)
		mat list_AllocatedAmt$(0)
		mat list_TownTFiling_Fee$(0)
		mat list_AllocatedAmt$(0)
		mat list_TownTamtDueAndFees$(0)
		mat list_VillSavings_Amt$(0)
		mat list_ColVilAmtDue$(0)
		mat list_AllocatedAmt$(0)
		mat list_VillVillFilingFee$(0)
		mat list_AllocatedAmt$(0)
		mat list_VillVamtDueAndFees$(0)
		mat list_ColTotalBilled$(0)
		mat list_ColTotalPaid$(0)
		mat list_ColBilledBalance$(0)
		mat list_ColBalanceSold$(0)

		mat list_FILENO$(0)
		mat list_INVOICENO$(0)
		mat list_DEBTORUNIQUEID$(0)
		mat list_OCN$(0)
		mat list_INVOICEEXIST$(0)
		mat list_INVOICEORIGAMT$(0)
		mat list_INVOICERATE$(0)
	! /r


	hIn=fn_init_csv_in(csvFieldCount,csvFile$)

	do
		dim line$*1024
		linput #hIn: line$ eof Rafia_EoF
		lineCount+=1
		dim item$(0)*512
		str2mat(line$,mat item$,tab$,'quotes:trim')
		mat item$(csvFieldCount)
		item$(csv_FILENO)=''
		! pr 'fileno$/key/oc$=';item$(csv_FILENO)&'/'&item$(csv_KEY)&'/'&oc$ : pause
		if sourceId$='direct' then
			tmpServiceDay=days(item$(csv_SERVICEDAY),'mm/dd/yy')
		else
			tmpServiceDay=days(item$(csv_SERVICEDAY),'mm/dd/ccyy')
		end if
		fnAddOneC(mat list_FILENO$,item$(csv_FILENO))
		fnAddOneC(mat list_OCN$,oc$)
		if oc$='new' then
			invoiceExist$='no'
		else
			invoiceExist$='undetermined'
		end if
		fnAddOneC(mat list_INVOICEEXIST$,invoiceExist$)


	if sourceId$='allData' then
		invoiceOrigAmt=fnval(item$(csv_BALANCE))+fnval(item$(csv_insurancePaid))+fnval(item$(csv_patientPaid))
	else if sourceId$='direct' then
		invoiceOrigAmt=fnval(item$(csv_charge))
	end if
	fnAddOneN(mat list_INVOICEORIGAMTN,invoiceOrigAmt)
	fnAddOneN(mat list_InvoiceRateN,invoiceOrigAmt-fnval(item$(csv_BALANCE)))
		! if csv_code>0 then
		! 	item$(csv_code)=srep$(item$(csv_code),'"','')
		! end if
		! if item$(csv_PROVIDER)='"Pierre-Jerome Shoulton, Yardly"' then pause
		! item$(csv_PROVIDER)=srep$(item$(csv_PROVIDER),'"','')

		fnAddOneC(mat list_KEY$,item$(csv_KEY))
		fn_ifValidAddItemC(mat list_procedureGroup$,csv_procedureGroup)
		fnAddOneC(mat list_FACILITY$,item$(csv_FACILITY))
		fnAddOneC(mat list_PROVIDER$,trim$(item$(csv_PROVIDER),'"'))
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
		fn_ifValidAddItemC(mat list_EMAIL$,csv_EMAIL, '@')
		fnAddOneC(mat list_SERVICEDAY$,str$(tmpServiceDay))
		fn_ifValidAddItemN(mat list_BALANCEN,csv_BALANCE)
		fn_ifValidAddItemC(mat list_social$,csv_social)
		fn_ifValidAddItemC(mat list_claimId$,csv_claimId)
		fn_ifValidAddItemC(mat list_primaryCarrier$,csv_primaryCarrier)
		fn_ifValidAddItemC(mat list_insuranceN$,csv_insuranceN)
		fn_ifValidAddItemC(mat list_secondaryCarrier$,csv_secondaryCarrier)
		fn_ifValidAddItemC(mat list_secInsuranceN$,csv_secInsuranceN)
		fn_ifValidAddItemC(mat list_CPT$,csv_CPT)
		fn_ifValidAddItemN(mat list_insurancePaidN,csv_insurancePaid)
		fn_ifValidAddItemN(mat list_patientPaidN,csv_patientPaid)
		fn_ifValidAddItemN(mat list_totalPaidN,csv_totalPaid)
		fn_ifValidAddItemC(mat list_billPatientReason$,csv_billPatientReason)
		fn_ifValidAddItemC(mat list_claimNotes$,csv_claimNotes)
		if sourceId$='allData' then
			fileno$         = item$(csv_FILENO)
			invNoSugPt1$    = item$(csv_KEY)
			invNoSugPt2$    = item$(csv_claimId)
			invNoSugPt3$    = date$(tmpServiceDay,'ccyymmdd')
			invDay          = tmpServiceDay
		else if sourceId$='direct' then
			fileno$         = item$(csv_FILENO)
			invNoSugPt1$    = item$(csv_KEY)
			invNoSugPt2$    = str$(tmpServiceDay+1) ! str$(invDay+1)
			invNoSugPt3$    = item$(csv_code)
			invDay          = tmpServiceDay
		end if
		dim tmpInvoiceNo$*20
		tmpInvoiceNo$=fn_invoiceNumber$(fileno$,invNoSugPt1$,invNoSugPt2$,invNoSugPt3$,item$(csv_PROVIDER),invDay,invoiceOrigAmt,invoiceExist$)
		fnAddOneC(mat list_INVOICENO$,tmpInvoiceNo$)
		list_INVOICEEXIST$(udim(mat list_INVOICEEXIST$))=invoiceExist$


		fn_ifValidAddItemC(mat list_lName$,csv_lName)
		fn_ifValidAddItemC(mat list_fName$,csv_fName)
		fn_ifValidAddItemC(mat list_mi$,csv_mi)
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
		fn_ifValidAddItemC(mat list_priInsName$,csv_priInsName)
		fn_ifValidAddItemC(mat list_dueFromInsName$,csv_dueFromInsName)
		fn_ifValidAddItemC(mat list_secInsName$,csv_secInsName)
	loop
	Rafia_EoF: !
	fn_close_csv_in
fnend
def fn_ifValidAddItemC(mat list$,csv_enum; qualifier$)
	if csv_enum>0 then
		if qualifier$='' or pos(item$(csv_enum),qualifier$)>0 then
			fnAddOneC(mat list$,rtrm$(item$(csv_enum)))
		else
			fnAddOneC(mat list$,'')
		end if
	end if
fnend
def fn_ifValidAddItemN(mat listN,csv_enum)
	if csv_enum>0 then
		fnAddOneN(mat listN,fnVal(item$(csv_enum)))
	end if
fnend


def fn_writeArraysToTestFile(outFile$*1024; ___,x,xLimit,hOut)
include: filenamesPushMixedCase
	open #hOut:=fnGetHandle: 'name='&outFile$&',recl=1024,replace',display,output
include: filenamesPopUpperCase
	xLimit=udim(mat list_KEY$)
	! r: heading
	fn_ifUsedPrintHeaderC('FORW_FILE'                      ,hOut,mat list_ForwFile$                     )
	fn_ifUsedPrintHeaderC('ORIG_CLAIM'                     ,hOut,mat list_OrigClaim$                    )
	fn_ifUsedPrintHeaderC('CRED_NAME'                      ,hOut,mat list_CredName$                     )
	fn_ifUsedPrintHeaderN('CRED_STREET'                    ,hOut,mat list_CredStreet$                   )
	fn_ifUsedPrintHeaderN('CRED_CS'                        ,hOut,mat list_CredCs$                       )
	fn_ifUsedPrintHeaderC('CRED_ZIP'                       ,hOut,mat list_CredZip$                      )
	fn_ifUsedPrintHeaderC('DEBT_NAME'                      ,hOut,mat list_DebtName$                     )
	fn_ifUsedPrintHeaderC('DEBT_STREE'                     ,hOut,mat list_DebtStree$                    )
	fn_ifUsedPrintHeaderC('DEBT_CS'                        ,hOut,mat list_DebtCS$                       )
	fn_ifUsedPrintHeaderC('DEBT_ZIP'                       ,hOut,mat list_DebtZip$                      )
	fn_ifUsedPrintHeaderC('DEBT_PHON'                      ,hOut,mat list_DebtPhon$                     )
	fn_ifUsedPrintHeaderC('S_B_L'                          ,hOut,mat list_SBL$                          )
	fn_ifUsedPrintHeaderN('DATE_GRANT'                     ,hOut,mat list_DateGrant$                    )
	fn_ifUsedPrintHeaderC('TAX_YEAR'                       ,hOut,mat list_TaxYear$                      )
	fn_ifUsedPrintHeaderC('ID_NUMB'                        ,hOut,mat list_IdNumb$                       )
	fn_ifUsedPrintHeaderC('ORIG_BAL'                       ,hOut,mat list_OrigBal$                      )
	fn_ifUsedPrintHeaderC('INT'                            ,hOut,mat list_Int$                          )
	fn_ifUsedPrintHeaderC('MISC'                           ,hOut,mat list_Misc$                         )
	fn_ifUsedPrintHeaderC('PTRC REP'                       ,hOut,mat list_PtrcRep$                      )
	fn_ifUsedPrintHeaderC('TOWN_TrueSavings'               ,hOut,mat list_TownTrueSavings$              )
	fn_ifUsedPrintHeaderC('COL_Twn_Amt_Due'                ,hOut,mat list_ColTwnAmtDue$                 )
	fn_ifUsedPrintHeaderC('Allocated Amt'                  ,hOut,mat list_AllocatedAmt$                 )
	fn_ifUsedPrintHeaderC('COL_Misc_Fee'                   ,hOut,mat list_COLMiscFee$                   )
	fn_ifUsedPrintHeaderC('Allocated Amt'                  ,hOut,mat list_AllocatedAmt$                 )
	fn_ifUsedPrintHeaderC('COL_Late_Fee'                   ,hOut,mat list_ColLateFee$                   )
	fn_ifUsedPrintHeaderC('Allocated Amt'                  ,hOut,mat list_AllocatedAmt$                 )
	fn_ifUsedPrintHeaderC('TOWN_TFiling_Fee'               ,hOut,mat list_TownTFiling_Fee$              )
	fn_ifUsedPrintHeaderC('Allocated Amt'                  ,hOut,mat list_AllocatedAmt$                 )
	fn_ifUsedPrintHeaderN('TOWN_TamtDueAndFees'            ,hOut,mat list_TownTamtDueAndFees$           )
	fn_ifUsedPrintHeaderN('VILL_Savings_Amt'               ,hOut,mat list_VillSavings_Amt$              )
	fn_ifUsedPrintHeaderN('COL_Vil_Amt_Due'                ,hOut,mat list_ColVilAmtDue$                 )
	fn_ifUsedPrintHeaderC('Allocated Amt'                  ,hOut,mat list_AllocatedAmt$                 )
	fn_ifUsedPrintHeaderC('VILL_Vill_Filing_Fee'           ,hOut,mat list_VillVillFilingFee$            )
	fn_ifUsedPrintHeaderC('Allocated Amt'                  ,hOut,mat list_AllocatedAmt$                 )
	fn_ifUsedPrintHeaderC('VILL_VamtDueAndFees'            ,hOut,mat list_VillVamtDueAndFees$           )
	fn_ifUsedPrintHeaderC('COL_Total_Billed'               ,hOut,mat list_ColTotalBilled$               )
	fn_ifUsedPrintHeaderC('COL_Total_Paid'                 ,hOut,mat list_ColTotalPaid$                 )
	fn_ifUsedPrintHeaderC('COL_Billed_Balance'             ,hOut,mat list_ColBilledBalance$             )
	fn_ifUsedPrintHeaderC('COL_Balance_Sold'               ,hOut,mat list_ColBalanceSold$               )
												         
	fn_ifUsedPrintHeaderC('FileNo'                         ,hOut,mat list_FILENO$                       )
	fn_ifUsedPrintHeaderC('InvoiceNo'                      ,hOut,mat list_INVOICENO$                    )
	fn_ifUsedPrintHeaderN('DebtorUniqueId'                 ,hOut,mat list_DEBTORUNIQUEID$               )
	fn_ifUsedPrintHeaderC('OCN'                            ,hOut,mat list_OCN$                          )
	fn_ifUsedPrintHeaderC('InvoiceExist'                   ,hOut,mat list_INVOICEEXIST$                 )
	fn_ifUsedPrintHeaderC('InvoiceOrigAmt'                 ,hOut,mat list_INVOICEORIGAMT$               )
	fn_ifUsedPrintHeaderC('InvoiceRate'                    ,hOut,mat list_INVOICERATE$                  )
	pr #hOut: '' ! crlf
	! /r
	for x=1 to xLimit
		! r: line item
		fn_ifUsedPrintItC(mat list_ForwFile$                                    ,x,hOut)
		fn_ifUsedPrintItC(mat list_OrigClaim$                                   ,x,hOut)
		fn_ifUsedPrintItC(mat list_CredName$                                    ,x,hOut)
		fn_ifUsedPrintItN(mat list_CredStreet$                                  ,x,hOut)
		fn_ifUsedPrintItN(mat list_CredCs$                                      ,x,hOut)
		fn_ifUsedPrintItC(mat list_CredZip$                                     ,x,hOut)
		fn_ifUsedPrintItC(mat list_DebtName$                                    ,x,hOut)
		fn_ifUsedPrintItC(mat list_DebtStree$                                   ,x,hOut)
		fn_ifUsedPrintItC(mat list_DebtCS$                                      ,x,hOut)
		fn_ifUsedPrintItC(mat list_DebtZip$                                     ,x,hOut)
		fn_ifUsedPrintItC(mat list_DebtPhon$                                    ,x,hOut)
		fn_ifUsedPrintItD(mat list_SBL$                                         ,x,hOut)
		fn_ifUsedPrintItN(mat list_DateGrant$                                   ,x,hOut)
		fn_ifUsedPrintItC(mat list_TaxYear$                                     ,x,hOut)
		fn_ifUsedPrintItC(mat list_IdNumb$                                      ,x,hOut)
		fn_ifUsedPrintItC(mat list_OrigBal$                                     ,x,hOut)
		fn_ifUsedPrintItC(mat list_Int$                                         ,x,hOut)
		fn_ifUsedPrintItC(mat list_Misc$                                        ,x,hOut)
		fn_ifUsedPrintItC(mat list_PtrcRep$                                     ,x,hOut)
		fn_ifUsedPrintItC(mat list_TownTrueSavings$                             ,x,hOut)
		fn_ifUsedPrintItC(mat list_ColTwnAmtDue$                                ,x,hOut)
		fn_ifUsedPrintItC(mat list_AllocatedAmt$                                ,x,hOut)
		fn_ifUsedPrintItC(mat list_COLMiscFee$                                  ,x,hOut)
		fn_ifUsedPrintItC(mat list_AllocatedAmt$                                ,x,hOut)
		fn_ifUsedPrintItC(mat list_ColLateFee$                                  ,x,hOut)
		fn_ifUsedPrintItC(mat list_AllocatedAmt$                                ,x,hOut)
		fn_ifUsedPrintItC(mat list_TownTFiling_Fee$                             ,x,hOut)
		fn_ifUsedPrintItC(mat list_AllocatedAmt$                                ,x,hOut)
		fn_ifUsedPrintItN(mat list_TownTamtDueAndFees$                          ,x,hOut)
		fn_ifUsedPrintItN(mat list_VillSavings_Amt$                             ,x,hOut)
		fn_ifUsedPrintItN(mat list_ColVilAmtDue$                                ,x,hOut)
		fn_ifUsedPrintItC(mat list_AllocatedAmt$                                ,x,hOut)
		fn_ifUsedPrintItC(mat list_VillVillFilingFee$                           ,x,hOut)
		fn_ifUsedPrintItC(mat list_AllocatedAmt$                                ,x,hOut)
		fn_ifUsedPrintItC(mat list_VillVamtDueAndFees$                          ,x,hOut)
		fn_ifUsedPrintItC(mat list_ColTotalBilled$                              ,x,hOut)
		fn_ifUsedPrintItC(mat list_ColTotalPaid$                                ,x,hOut)
		fn_ifUsedPrintItC(mat list_ColBilledBalance$                            ,x,hOut)
		fn_ifUsedPrintItC(mat list_ColBalanceSold$                              ,x,hOut)

		fn_ifUsedPrintItC(mat list_FILENO$                                      ,x,hOut)
		fn_ifUsedPrintItC(mat list_INVOICENO$                                   ,x,hOut)
		fn_ifUsedPrintItN(mat list_DEBTORUNIQUEID$                              ,x,hOut)
		fn_ifUsedPrintItC(mat list_OCN$                                         ,x,hOut)
		fn_ifUsedPrintItC(mat list_INVOICEEXIST$                                ,x,hOut)
		fn_ifUsedPrintItC(mat list_INVOICEORIGAMT$                              ,x,hOut)
		fn_ifUsedPrintItC(mat list_INVOICERATE$                                 ,x,hOut)
		! /r

		pr #hOut: '' ! crlf
	nex x
	close #hOut: 
fnend
def fn_ifUsedPrintHeaderC(header$*256,hOut,mat array$)
	if udim(mat array$)>10 then
		pr #hOut: rtrm$(header$)&tab$;
	end if
fnend
def fn_ifUsedPrintHeaderN(header$*256,hOut,mat arrayN)
	if udim(mat arrayN)>10 then
		pr #hOut: rtrm$(header$)&tab$;
	end if
fnend
def fn_ifUsedPrintItD(mat array$,x,hOut)
	if udim(mat array$)>10 then
		pr #hOut: date$(val(array$(x)),'mm/dd/ccyy')&tab$;
	end if
fnend
def fn_ifUsedPrintItC(mat array$,x,hOut)
	if udim(mat array$)>10 then
		pr #hOut: array$(x)&tab$;
	end if
fnend
def fn_ifUsedPrintItN(mat arrayN,x,hOut)
	if udim(mat arrayN)>10 then
		pr #hOut: str$(arrayN(x))&tab$;
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
		library 'Library\openFile.wb': Fnopen_Invoice

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
		library 'S:\Core\Library.br': fnCountMatchesC,fnCountMatchesN

		library 'S:\Collection-Master Add-On\fn\Library.br': fnCptCode$
		library 'S:\Collection-Master Add-On\fn\Library.br': fnVal
		library 'S:\Core\Library.br': fnRemoveExcessCRLF$

		library 'Library\SQL.wb': fnsql_setup$

		gosub Enum
		gosub SetupPrint

		dim forw$(0)*60,forwN(0)
		dim forwFieldsC$(0)*20,forwFieldsN$(0)*20
		dim forwFormAll$*2048
		! execute "*SubProc "&     <--- not necessary with include:enum\forw  and  gosub Enumforw
		fnsql_setup$('masforw',mat forw$,mat forwN,mat forwFieldsC$,mat forwFieldsN$,forwFormAll$)
		gosub EnumForw

		dim inv$(0)*60,invN(0)
		dim invFieldsC$(0)*20,invFieldsN$(0)*20
		dim invFormAll$*2048
		execute "*SubProc "&fnsql_setup$('INVOICE',mat inv$,mat invN,mat invFieldsC$,mat invFieldsN$,invFormAll$)

	end if
fnend


Xit: !
fnXit
include: cm\enum\common
include: cm\enum\forw
include: cm\enum\master
include: cm\print
include: cm\err