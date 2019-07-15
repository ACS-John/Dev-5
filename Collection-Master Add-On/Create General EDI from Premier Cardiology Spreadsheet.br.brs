on error goto Error_Hanler
fn_setup
fnTop(program$)
do
	dim xlateFile$*256
	xlateFile$='D:\CM\Stern and Stern\Premier Cardiology crosswalk.txt'
	dim from$(0)*256
	dim to$(0)*256
	fn_readCrossWalk(xlateFile$,mat from$,mat to$)

	dim csvFile$*512
	if fn_askScreen1(csvFile$,sFileNo$,forwNo)=99 then
		goto Xit
	else
		fnCopy(csvFile$,csvFile$&'.bak')
		dim Csv_Fields$(0)*128
		dim Csv_Data$(0)*256
		csvFieldCount=Fnopen_Csv(hIn:=fnGetHandle,csvFile$,Csv_Delimiter$,Mat Csv_Fields$,Mat Csv_Data$)
		! open #hIn:= fnGetHandle: 'name='&csvFile$&'.bak',display,input


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
		csv_claimID            	=srch(mat Csv_Fields$,uprc$('Claim ID'             	))
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


		csv_fileNo   	=fnAddOneC(mat csv_Fields$,'FileNo')
		csv_invoiceNo	=fnAddOneC(mat csv_Fields$,'InvoiceNo')
		
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

		dim generalEdiFile$*256
		generalEdiFile$=csvPath$&'\'&csvProg$&'-General_EDI'&'.csv'
		dim cmEdiFile$*256
		cmEdiFile$=csvPath$&'\'&csvProg$&'-CM_EDI'&'.txt'
		open #hOutGeneralEdi:=fnGetHandle: 'name='&generalEdiFile$&',recl=1024,replace',display,output
		open #hOutCmEdi:=fnGetHandle:       'name='&cmEdiFile$&',recl=1024,replace',display,output
		! pause
		lineCount   =0
		colComment  =0
		colCheckDate=0
		do
			dim line$*1024
			linput #hIn: line$ eof ConversionFinis
			lineCount+=1
			dim item$(0)*512
			str2mat(line$,mat item$,',')
			mat item$(csvFieldCount)
			item$(csv_fileNo)=fn_getFileNo$(item$(csv_patientId))
			item$(csv_invoiceNo)=
			if lineCount=1 then ! it's the header
				colComment  =srch(mat item$,'COMMENT' )
				colCheckDate=srch(mat item$,'CHECK_DATE')
				if colComment<=0 or colCheckDate<=0 then
					msgbox('This file does not have the appropriate column headings.Required headings are:'&CHR$(13)&'COMMENT'&CHR$(13)&'CHECK_DATE'&CHR$(13)&'This file will be skipped.')
					close #hIn:
					close #hOutGeneralEdi:
					if fnCopy(csvFile$&'.bak',csvFile$) then
						fnFree(csvFile$&'.bak')
					end if
					goto ConversionXit
				end if
			else
				if udim(mat item$)=>colComment and udim(mat item$)=>colCheckDate and trim$(item$(colCheckDate))<>'' and pos(item$(colComment),'('&trim$(item$(colCheckDate))&')')<=0 then
					item$(colComment)=rtrm$(item$(colComment))&' ('&trim$(item$(colCheckDate))&')'
				end if
				mat2str(mat item$,line$,',')
			end if
			print #hOutGeneralEdi: line$
		loop
	end if
	ConversionFinis: !
	close #hIn:
	close #hOutGeneralEdi:
	close #hOutCmEdi:
	msgbox('Success on '&csvFile$)
	ConversionXit: !
loop
Xit: !
fnXit
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
def fn_setup
	if ~setup then
		setup=1
		! library 'Library\clsUtil.wb': fnErase_buttons
		library 'Library\clsUtil.wb': fnAsk_file1
		library 'Library\clsUtil.wb': Fnopen_Csv
		library 'Library\clsUtil.wb': fnasci
		library 'Library\clsUtil.wb': fnBr_filename$
		library 'Library\clsUtil.wb': Fnmessagebox
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
		gosub Enum
		! fnErase_buttons


	end if
fnend
def fn_askScreen1(&sourceFile$,&sFileNo$,&forwNo; ___,returnN)
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
	sourceFile$=defaultFilter$

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

	fnLbl(lc+=1,col1pos,'Starting File Number:', col1len,1)
	fnTxt(lc,col2pos,8,8,0,'1000',0,'Select the starting fileno.  Should be a few letters followed by several digits of numbers with leading zeros.')
	resp$(resp_sFileNo:=rc+=1)=sFileNo$

	fnLbl (lc+=1,col1pos,'Forwarder Number:', col1len,1)
	fncombof('masforw',lc,col2pos,width,'MASFORW//8',1,3,4,10, '',1,1,'Select the Forwarder to assign to imported claims.',0,0,'BH')
	resp$(resp_forwNo:=rc+=1)=forwNo$
	!     fnChk(2,2,'enable Open Claims')
	!     if enableOpen then resp$(1)='True' else resp$(1)='False'
	!     fnChk(3,2,'enable Closed Claims')
	!     if enableClosed then resp$(2)='True' else resp$(2)='False'
	!     fnChk(4,2,'enable Paperless Scan')
	!     if enablePaperlessScan then resp$(3)='True' else resp$(3)='False'
	!     fnChk(6,2,'Include matching accounts only')
	!     if enableMatchingAccountsOnly then resp$(4)='True' else resp$(4)='False'
	!     ! fnLbl(2,2,'Forwarder Number(s):',20,1)
	!     ! fnTxt(2,24,40,256)
	!     ! resp$(1)=forwarderFilter$
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then
		returnN=99
		goto AskScreen1_xit
	else
		sourceFile$	=resp$(resp_sourceFile)
		sFileNo$   	=resp$(resp_sFileNo)
		forwNo$    	=resp$(resp_forwNo)(1:pos(resp$(resp_forwNo),' ')-1)
		forwNo     		=val(forwNo$)

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
		tempMessageboxResponse=Fnmessagebox("The file you specified does not exist.\n"&sourceFile$, 48+5,env$('program_caption'))
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
def fnPosOfAny(textToSearch$*1024,mat searchFor$; fromEnd,___,returnN,howMany,x)
	howMany=udim(mat searchFor$)
	mat posIs(howMany)
	for x=1 to howMany
		posIs(x)=pos(searchFor$,searchFor$(x), fromEnd)
	nex x
	if fromEnd then
		returnN=max(mat posIs)
	else
		returnN=min(mat posIs)
	end if
	fnPosOfAny=returnN
fnend
include: cm\enum\common
include: cm\err
