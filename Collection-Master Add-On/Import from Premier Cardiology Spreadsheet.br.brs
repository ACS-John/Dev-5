on error goto Ertn
fn_setup
fnTop(program$)

	! dim xlateFile$*256
	! xlateFile$='D:\CM\Stern and Stern\Premier Cardiology crosswalk.txt'
	! dim from$(0)*256
	! dim to$(0)*256
	! fn_readCrossWalk(xlateFile$,mat from$,mat to$)
! r: main flow
	dim csvFile$*512
	fnreg_read(env$('program_caption')&'.csvFile',csvFile$, 'D:\CM\Stern and Stern\New_format_Premier_Cardiology.xlsx')
	fnreg_read(env$('program_caption')&'.starting fileno',sFileNo$,'PCE01001')
	fnreg_read(env$('program_caption')&'.forw no',forwNo$,'1617')
	fnreg_read(env$('program_caption')&'.enableImport',enableImport$) : if enableImport$='True' then enableImport=1 else enableImport=0
	if fn_askScreen1(csvFile$,sFileNo$,forwNo$,enableImport,enableImport$)=99 then
		goto Xit
	else
		fnreg_write(env$('program_caption')&'.csvFile',csvFile$)
		fnreg_write(env$('program_caption')&'.starting fileno',sFileNo$)
		fnreg_write(env$('program_caption')&'.forw no',forwNo$)
		fnreg_write(env$('program_caption')&'.enableImport',enableImport$)

		fnCopy(csvFile$,csvFile$&'.bak')
		dim Csv_Fields$(0)*128
		dim Csv_Data$(0)*256
		csvFieldCount=Fnopen_Csv(hIn:=fnGetHandle,csvFile$,Csv_Delimiter$,Mat Csv_Fields$,Mat Csv_Data$)
		lineCount=0
		! r: set csv_*
		csv_facilityName      	=srch(mat Csv_Fields$,uprc$('Facility Name'        	))
		csv_providerName      	=srch(mat Csv_Fields$,uprc$('Provider Name'        	))
		csv_patientName        	=srch(mat Csv_Fields$,uprc$('Patient Name'         	))
		csv_patientDOB         	=srch(mat Csv_Fields$,uprc$('Patient DOB'          	))
		csv_patientId          	=srch(mat Csv_Fields$,uprc$('Patient ID'           	))
		csv_respName           	=srch(mat Csv_Fields$,uprc$('Resp Name'            	))
		csv_respAddress        	=srch(mat Csv_Fields$,uprc$('Resp Address'         	))
		csv_respCsz            	=srch(mat Csv_Fields$,uprc$('Resp City-State-Zip' 	))
		csv_patientPhone      	=srch(mat Csv_Fields$,uprc$('Patient Phone'        	))
		csv_respPhone          	=srch(mat Csv_Fields$,uprc$('Resp Phone'           	))
		csv_respCellPhone     	=srch(mat Csv_Fields$,uprc$('Resp Cell Phone'     	))
		csv_respWorkPhone     	=srch(mat Csv_Fields$,uprc$('Resp Work Phone'     	))
		csv_respEmail          	=srch(mat Csv_Fields$,uprc$('Resp Email'           	))
		csv_social             	=srch(mat Csv_Fields$,uprc$('Social'               	))
		csv_claimId            	=srch(mat Csv_Fields$,uprc$('Claim ID'             	))
		csv_primaryCarrier    	=srch(mat Csv_Fields$,uprc$('Primary Carrier'     	))
		csv_insuranceN	        	=srch(mat Csv_Fields$,uprc$('Insurance#'           	))
		csv_secondaryCarrier  	=srch(mat Csv_Fields$,uprc$('Secondary Carrier'   	))
		csv_secInsuranceN     	=srch(mat Csv_Fields$,uprc$('Sec Insurance#'      	))
		csv_serviceDate        	=srch(mat Csv_Fields$,uprc$('Service Date'         	))
		csv_cpt                	=srch(mat Csv_Fields$,uprc$('CPT'                   	))
		csv_insurancePaid     	=srch(mat Csv_Fields$,uprc$('Insurance Paid'       	))
		csv_patientPaid        	=srch(mat Csv_Fields$,uprc$('Patient Paid'         	))
		csv_totalPaid          	=srch(mat Csv_Fields$,uprc$('Total Paid'           	))
		csv_balance            	=srch(mat Csv_Fields$,uprc$('Balance'              	))
		csv_billPatientReason 	=srch(mat Csv_Fields$,uprc$('Bill Patient Reason' 	))
		csv_claimNotes         	=srch(mat Csv_Fields$,uprc$('Claim Notes'          	))

		! ***  Add New Invented Columns ***
		csv_fileNo   	=fnAddOneC(mat csv_Fields$,'FileNo')
		csv_invoiceNo	=fnAddOneC(mat csv_Fields$,'InvoiceNo')
		! /r
		if csv_patientId=0 then
			pr 'invalid file.  No "Patient ID" column.';bell
			pause
			goto Xit
		end if
		csvFieldCount=csv_invoiceNo
		dim csvPath$*256
		dim csvProg$*256
		dim csvExt$*128
		fnGetPp(csvFile$,csvPath$,csvProg$,csvExt$)

		dim outFile$*256
		outFile$=csvPath$&'\'&csvProg$&'-CM_EDI'&'.csv'
		open #hOut:=fnGetHandle: 'name='&outFile$&',recl=1024,replace',display,output
		fn_pr_hOut(	'0[tab]H[tab]This file is "'&os_filename$(outFile$)&'"'	)
		fn_pr_hOut(	'0[tab]H[tab]This file was made by the "'&env$('program_caption')&'" (a Collection-Master Add-On program) on '&date$('mm/dd/ccyy')&' at '&time$&'.'	)
		fn_pr_hOut(	'0[tab]H[tab]This file was made from the source file: "'&csvFile$&'".'	)
		fn_pr_hOut(	'0[tab]H[tab]from "'&os_filename$(file$(hIn))&'"'	)
		do
			dim line$*1024
			linput #hIn: line$ eof Finis
			lineCount+=1
			dim item$(0)*512
			str2mat(line$,mat item$,tab$)
			mat item$(csvFieldCount)
			item$(csv_fileNo)=fn_getFileNo$(item$(csv_patientId))
			item$(csv_invoiceNo)=item$(csv_claimId)&'-'&date$(days(item$(csv_serviceDate),'mm/dd/ccyy'),'ccyymmdd')

			fn_writeDemographics(hOut,mat item$)
			fn_writeInvoices     (hOut,mat item$)
			mat2str(mat item$,line$,tab$)
			! print #hOut: line$
		loop
	end if
! /r
Finis: ! r:
	close #hIn:
	close #hOut:
	if enableImport then
		fn_generateAutomationFiles
		! exec 'proc run'
		execute 'sy -c -M F:\clsinc\batch\cm_edi_pcs.cmd'
	else
		dim mbText$*2048
		mbText$='Sucessfully created a file for CM EDI Import:'
		mbText$(inf:inf)=lf$&outFile$
		fnMessageBox(mbText$,mb_information+mb_okonly,env$('program_caption'))
		! msgbox('Success on '&csvFile$)
	end if
goto Xit ! /r
Xit: !
fnXit
def fn_askScreen1(&sourceFile$,&sFileNo$,&forwNo$,&enableImport,&enableImport$; ___,returnN,rc,lc)
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
	fnLbl(lc+=1,col1pos,'Source Premier Cardiology XLSX File:', col1len,1)
	fnTxt(lc,col2pos,42,80,0,'1070',0,'Select the source XLSX file provided by Premier Cardiology.')
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
			sourceFile$=fnopen$(defaultFilter$)
			goto AskScreen1_ask
		else if pos(sourceFile$,'*')>0 or pos(sourceFile$,'?')>0 then
			sourceFile$=fnopen$(Trim$(sourceFile$))
			goto AskScreen1_ask
		end if
	end if
	AskScreen1_testFileExist: !
	sourceFile$=fnBr_filename$(sourceFile$)
	if ~exists(sourceFile$) then
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



def fn_writeDemographics(hOut,mat item$; ___,whichAdk,line$*2048) ! requires local enumerations csv_*
	if ~setup_wd then
		dim alreadyAddedDemographicsKey$(0)
		mat alreadyAddedDemographicsKey$(0)
		setup_wd=1
	end if
	whichAdk=srch(mat alreadyAddedDemographicsKey$,item$(csv_fileNo))
	if whichAdk<=0 then
		! r: CM EDI Record 101     New placement record.
		line$=''
		line$(inf:inf)='FIRM_FILENO'
		line$(inf:inf)='[tab]Forw_Refno'
		line$(inf:inf)='[tab]Forw_Fileno'
		line$(inf:inf)='[tab]ForwNo'
		fn_pr_hOut_header(101,line$)
		line$=''
		line$(inf:inf)=tab$&item$(csv_fileno)
		line$(inf:inf)=item$(csv_patientId)
		line$(inf:inf)=tab$&item$(csv_claimId)
		line$(inf:inf)=tab$&forwNo$
		fn_pr_hOut_detail(101,line$) 
		! /r
		! r: CM EDI Record 111    Debtor Demographics
		line$=''
		line$(inf:inf)='FIRM_FILENO'
		line$(inf:inf)='[tab]FORW_REFNO'
		fn_pr_hOut_header(111,line$)
		line$=''
		line$(inf:inf)=item$(csv_fileno)
		line$(inf:inf)=tab$&item$(csv_patientId)
		fn_pr_hOut_detail(111,line$)
		fnAddOneC(mat alreadyAddedDemographicsKey$,item$(csv_fileNo))
		! /r
	end if
fnend
def fn_writeInvoices(hOut,mat item$; ___,line$*2048)
	! CM EDI Record 180 Invoice File
	line$=''
	line$(inf:inf)='FORW_REFNO'
	line$(inf:inf)='[tab]FIRM_FILENO'
	line$(inf:inf)='[tab]INV_DATE'
	line$(inf:inf)='[tab]DESCRIPTION'
	line$(inf:inf)='[tab]ACCT_NO'
	fn_pr_hOut_header(180,line$)
	line$=''
	line$(inf:inf)=item$(csv_patientId)
	line$(inf:inf)=tab$&item$(csv_fileNo)
	line$(inf:inf)=tab$&item$(csv_serviceDate)
	line$(inf:inf)=tab$&'CPT: '&item$(csv_cpt)
	line$(inf:inf)=tab$&item$(csv_facilityName)
	fn_pr_hOut_detail(180,line$)
fnend

def fn_pr_hOut_header(num,etc$*512)
	headerNow$=str$(num)&tab$&'H'
	if headerNow$<>headerPrior$ then
		fn_pr_hOut(headerNow$&tab$&'Date'&tab$&'Time'&tab$&etc$)
		headerPrior$=headerNow$
	end if
fnend
def fn_pr_hOut_detail(num,etc$*512)
	etc$(0:0)=str$(num)&'[tab]D[tab]'&date$('mm/dd/ccyy')&tab$&time$&tab$
	fn_pr_hOut(etc$)
fnend
def fn_pr_hOut(x$*512)
	x$=srep$(x$,'[tab]',tab$)
	pr #hOut: x$&tab$&'#'
fnend




def fn_getFileNo$(uniqueIdentifier$; ___,x,return$)
	if ~setup_getFileNo then
		dim gfnKey$(0)*128
		mat gfnKey$(0)
		dim gfnFileNo$(0)*8
		mat gfnFileNo$(0)
		gfnCount=0
		dim number$(10)
		for x=1 to 10 : number$(x)=str$(x-1) : nex x

		firstNumberInSfileNo=fnPosOfAny(sFileNo$,mat number$)
		lastNumberInSfileNo=fnPosOfAny(sFileNo$,mat number$, -1)
		firstFileNumber=val(sFileNo$(firstNumberInSfileNo:inf))
		nextFileNumber=firstFileNumber
		dim fileNumberFormat$*64
		fileNumberFormat$='pic('&rpt$('#',lastNumberInSfileNo-firstNumberInSfileNo)&')'
		setup_getFileNo=1
	end if
	uniqueIdentifier$=trim$(uniqueIdentifier$)
	gfnWhich=srch(mat gfnKey$,uniqueIdentifier$)
	if gfnWhich<=0 then
		gfnCount+=1
		mat gfnKey$(gfnCount)
		mat gfnFileNo$(gfnCount)
		gfnKey$(gfnCount)=uniqueIdentifier$
		return$=sFileNo$(1:firstNumberInSfileNo-1)&cnvrt$(fileNumberFormat$,nextFileNumber)
		nextFileNumber+=1
	else
		return$=gfnFileNo$(gfnWhich)
	end if
	fn_getFileNo$=return$
fnend
def fn_readCrossWalk(xlateFile$*256,mat from$,mat to$; ___,x,returnN)
	fnasci(xlateFile$,Mat Asci$)
	returnN=udim(mat asci$)
	mat from$(returnN)
	mat to$(returnN)
	for x=1 to returnN
		from$(x)=asci$(x)(1:pos(asci$(x),tab$)-1)
		to$(x)=asci$(x)(pos(asci$(x),tab$,-1)+1:inf)
	nex x
	fn_readCrossWalk=returnN
fnend


def fn_generateAutomationFiles(;___,hIni,hCmd)
		open #hIni:=fnGetHandle: 'name=F:\clsinc\custom\cm_edi_pcs.ini,replace',d,o
		pr #hIni: '||MENUPATH:4-2-1-1'
		pr #hIni: 'EDI NUMBER=326'
		pr #hIni: 'FILE NUMBER='&sFileNo$
		pr #hIni: 'SOURCE FILE='&outFile$
		pr #hIni: 'UNASSIGNED FORW='&forwNo$
		close #hIni:

		open #hCmd:=fnGetHandle: 'name=F:\clsinc\batch\cm_edi_pcs.cmd,replace',d,o
		pr #hCmd: 'f:'
		pr #hCmd: 'cd \clsinc'
		pr #hCmd: 'set Automate=cm_edi_pcs.ini'
		pr #hCmd: 'f:\clsinc\wbwin\br32.exe' ! brclient.exe
		close #hCmd:
		! setenv('Automate','cm_edi_pcs.ini')
fnend


def fn_setup
	if ~setup then
		setup=1
		! library 'Library\clsUtil.wb': fnErase_buttons
		library 'Library\clsUtil.wb': Fnopen_Csv
		library 'Library\clsUtil.wb': fnasci
		library 'Library\clsUtil.wb': fnBr_filename$
		library 'Library\clsUtil.wb': fnMessageBox
		library 'Library\clsUtil.wb': Fnopen$
		library 'Library\clsUtil.wb': Fnstring_Len_Max

		library 'S:\Core\Library.br': fnTop
		library 'S:\Core\Library.br': fnGetPp
		library 'S:\Core\Library.br': fnGetHandle
		library 'S:\Core\Library.br': fnFree
		library 'S:\Core\Library.br': fnCopy
		library 'S:\Core\Library.br': fnXit
		library 'S:\Core\Library.br': fnAddOneC
		library 'S:\Core\Library.br': fnTos,fnlbl,fntxt,fncmdset
		library 'S:\Core\Library.br': fnchk
		library 'S:\Core\Library.br': fnacs2
		library 'S:\Core\Library.br': fncombof
		library 'S:\Core\Library.br': fnPosOfAny
		library 'S:\Core\Library.br': fnreg_read
		library 'S:\Core\Library.br': fnreg_write
		gosub Enum
		! fnErase_buttons


	end if
fnend
include: cm\enum\common
include: cm\err
