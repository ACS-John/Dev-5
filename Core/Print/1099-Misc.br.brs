! Very similar to S:\Core\Print\1099-NEC.br

! r: testing zone
	fn_setup
	if fn1099MiscAsk (seltpN,typeN,minAmt,beg_date,end_date) then
		debug=1
		fn_1099testPrint
	end if
	end
! /r

def fn_1099testPrint
	! r: test values
	dim testAddr$(3)*60
	testAddr$(1)='Recipient Addr part 1'
	testAddr$(2)='Recipient Addr part 2'
	testAddr$(3)='Recipient City State and Zip'
	dim testBox(20)
	!            zzzzzzzzzz
	testBox(1 )=1000000100
	testBox(2 )=2000000200
	testBox(3 )=3000000300
	testBox(4 )=4000000400
	testBox(5 )=5000000500
	testBox(6 )=6000000600
	testBox(7 )=7000000700
	testBox(8 )=8000000800
	testBox(9 )=9000000900
	testBox(10)=1001001000
	testBox(11)=1101101100
	testBox(12)=1201201200
	testBox(13)=1301301300
	testBox(14)=1401401400
	testBox(15)=1501501500
	testBox(16)=1601601600
	testBox(17)=1701701700
	testBox(18)=1801801800
	testBox(19)=1901901900
	testBox(20)=2002002000
	! /r
	disableCopyAWarning=1
	fn_1099print('account1','Recipient One'  ,mat testAddr$,'111-11-1111',mat testBox)
	fn_1099print('Account2','Recipient Two'  ,mat testAddr$,'222-22-2222',mat testBox)
	fn_1099print('Account3','Recipient Three',mat testAddr$,'333-33-3333',mat testBox)
	disableCopyAWarning=0
	fn_1099print_close
fnend

def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		on error goto Ertn
		! r: constants
		dim resp$(128)*256
		dim optCopy$(5)*72
		optCopy$(1)='A - For Internal Revenue Service Center'
		optCopy$(2)='1 - For State Tax Department'
		optCopy$(3)='B - For Recipient'
		optCopy$(4)='C - For Payer'
		optCopy$(5)='2 - To be filed with recipient''s state income tax return, when required'
		dim copyFile$(5)*128,ssnMask(5)
		taxYear$=date$(days(date$)-120,'CCYY')
		copyFile$(1)='S:\Core\pdf\'&taxYear$&'\1099-Misc\Copy A.pdf' : ssnMask(1)=0
		copyFile$(2)='S:\Core\pdf\'&taxYear$&'\1099-Misc\Copy 1.pdf' : ssnMask(2)=0
		copyFile$(3)='S:\Core\pdf\'&taxYear$&'\1099-Misc\Copy B.pdf' : ssnMask(3)=0
		copyFile$(4)='S:\Core\pdf\'&taxYear$&'\1099-Misc\Copy C.pdf' : ssnMask(4)=1
		copyFile$(5)='S:\Core\pdf\'&taxYear$&'\1099-Misc\Copy 2.pdf' : ssnMask(5)=1
		! /r
	end if
fnend

def library fn1099MiscAsk(&seltpN,&typeN,&minAmt,&beg_date,&end_date)
	if ~setup then fn_setup
	fn1099MiscAsk=fn_ask(seltpN,typeN,minAmt,beg_date,end_date)
fnend
def fn_ask(&seltpN,&typeN,&minAmt,&beg_date,&end_date; ___, _
	returnN,rc,lc,mylen,mypos,mylen,resc_taxYear,respc_deduction,respc_minAmt,respc_phone, _
	respc_Print1099,respc_perPage,respc_export_ams,respc_exportIris,resp_export_file, _
	ckey_defaultFilename,ckey_margins,ckey_test)
	! local retained values: awi_setup$, taxYear$, mat deductionFullName$, mat deductionOption$,deductionOptionCount, and many many more
	if awi_setup$<>env$('cursys')&env$('cno') then ! r: read or set values for ASK_INFO screen
		awi_setup$=env$('cursys')&env$('cno')
		taxYear$=date$(days(date$)-120,'CCYY')
		if env$('CurSys')='PR' then
			dim deductionFullName$(20)*20,deductionOption$(20)*20
			fnDedNames(mat deductionFullName$)
			deductionOptionCount=0
			for j=1 to udim(mat deductionFullName$)
				deductionFullName$(j)=rtrm$(deductionFullName$(j))
				if trim$(deductionFullName$(j))<>'' then
					deductionOption$(deductionOptionCount+=1)=deductionFullName$(j)
				end if
			next j
			mat deductionOption$(deductionOptionCount)
			for j=1 to 8
				typeOption$(j)=str$(j)
			next j
		end if

		minAmt=fnpcreg_read('Filter - Minimum Amount',tmp$, '600')

		dim destinationOpt$(3)
		fnpcreg_read('Print 1099'       	,destinationOpt$(1)	,'False' )
		fnpcreg_read('Export 1'         	,destinationOpt$(2)	,'False')
		fnpcreg_read('Export IRIS'        ,destinationOpt$(3)	,'True')
		fnpcreg_read('Enable Background'	,enableBackground$	,'True' )
		fnpcreg_read('2 Per Page'       	,perPage$       	,'True')
		copyCurrentN=fnPcRegReadN('Copy Current',2)
		fnureg_read('1099 - Export Filename',outputFilename$,os_filename$(env$('Documents')&'\ACS\[TaxYear] 1099-Misc Export\Company [CompanyNumber].csv'))
		fncreg_read('Phone Number',ph$)
		fncreg_read('Email',email$)
		dim seltp$*256
		seltpN=fnPcReg_Read('seltpN',seltp$)
		if env$('cursys')='PR' then
			typeN=fnpcreg_read('1099 Box to Print',type$)
		end if
	end if ! /r
	ASK_INFO: !
	fnTos
	rc=lc=0 : mylen=40 : mypos=mylen+3
	fnLbl(lc+=1,1,'Tax Year:',mylen,1)
	fnTxt(lc,mypos,4,0,1,'',1,'Year to pr 1099s for')
	resp$(resc_taxYear=rc+=1)=taxYear$
	if env$('cursys')='PR' then
		lc+=1
		fnLbl(lc+=1,1,'Miscellaneous Deduction to Print:',mylen,1)
		fnComboA('deductions',lc,mypos,mat deductionOption$,'Select the deduction you want printed.')
		resp$(respc_deduction=rc+=1)=seltp$
		fnLbl(lc+=1,1,'1099 Box to Print:',mylen,1)
		fnComboA('type',lc,mypos,mat typeOption$,'Select the code for the 1099 vendor type.')
		resp$(respc_type=rc+=1)=type$
	else if env$('cursys')='GL' or env$('cursys')='CL' then
		fnLbl(lc+=1,1,'Payee Type to Print:',mylen,1)
		resp$(respc_deduction=rc+=1)=seltp$
		fnComboF('Payeetype',lc,mypos,27,'[Q]\[CurSys]mstr\PayeeType.dat',1,2,3,25,'',0,0, 'The payee type is a code used to detemine which box should be used on a 1099 misc form.  Enter the code for the payee type to print.')
	end if
	lc+=1
	fnLbl(lc+=1,1,'Minimum Amount to Print:',mylen,1)
	fnTxt(lc,mypos,12,0,1,'10',0,'Enter the minimum amount that should be printed.')
	resp$(respc_minAmt=rc+=1)=str$(minAmt)
	fnLbl(lc+=1,1,'Your Telephone Number:',mylen,1)
	fnTxt(lc,mypos,12,0,1,'',0,'You can use dashes, etc.')
	resp$(respc_phone=rc+=1)=ph$
	fnLbl(lc+=1,1,'Email:',mylen,1)
	fnTxt(lc,mypos,30,64,0,'',0,'')
	resp$(respc_email=rc+=1)=email$
	lc+=1
	fnLbl(lc+=1,1,'Payee Name Format:',mylen,1)
	dim optNameFormat$(2)*20
	optNameFormat$(1)='First Name First'
	optNameFormat$(2)='Last Name First'
	fnComboA('nameFormat',lc,mypos,mat optNameFormat$, '',20)
	fncreg_read('Payee Name Format',resp$(respc_nameFormat=rc+=1),optNameFormat$(1))
	lc+=1
	fnOpt(lc+=1,3,'Print 1099-Misc')
	resp$(respc_Print1099=rc+=1)=destinationOpt$(1)
	fnLbl(lc+=1,5,'Copy:',12,1,0)
	fnComboA('Copy',lc,19,mat optCopy$, '',20)
	resp$(respc_copyCurrent=rc+=1)=optCopy$(copyCurrentN)
	! fnLbl(lc+=1,20,'(2 per page is not yet available with Backgrounds)',50,0)
	fnChk(lc+=1,20,'Enable Background',1)
	resp$(respc_enableBackground=rc+=1)=enableBackground$
	fnChk(lc+=1,20,'2 Per Page',1)
	resp$(respc_perPage=rc+=1)=perPage$
	lc+=1
	fnOpt(lc+=1,3,'Export CSV for IRIS')
	resp$(respc_exportIris=rc+=1)=destinationOpt$(3)
	fnOpt(lc+=1,3,'Export for Advanced Micro Solutions')
	resp$(respc_export_ams=rc+=1)=destinationOpt$(2)
	fnLbl(lc+=1,5,'Export File:',12,1,0,franum)
	fnTxt(lc,19,20,128,0,'72',0,'Choose a destination location for the ACS export.',franum)
	resp$(resp_export_file=rc+=1)=outputFilename$
	fnButton(lc,5+12+20+5,'Default',ckey_defaultFilename=14,'Choose to set the default for the selected destination software.',0,0,franum)
	fnLbl(lc+=1,19,'([CompanyNumber] and [TaxYear] will be substituted in filename)',0,0,0,franum)

	fnCmdKey('&Margins',ckey_margins=1021,0,0,'Manually adjust margins for hitting forms')
	fnCmdKey('&Next',1,1,0,'Proceed to next screen.')
	fnCmdKey('&Test',ckey_test=10,0,0,'Produce a Test 1099')
	fnCmdKey('&Cancel',5,0,1,'Returns to menu')
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		! r: gather local variables from mat resp$
		if env$('cursys')='PR' then
			seltp$=resp$(respc_deduction)
			seltpN=srch(mat deductionFullName$,seltp$)
			type$=resp$(respc_type)
			typeN=val(type$)
		else if env$('cursys')='GL' or env$('cursys')='CL' then
			seltpN=val(resp$(respc_deduction)(1:2))
			seltp$=resp$(respc_deduction)
		end if
		minAmt=val(resp$(respc_minAmt))
		ph$=resp$(respc_phone)
		email$=resp$(respc_email)
		beg_date=val(taxYear$&'0101')
		end_date=val(taxYear$&'1231')
		copyCurrentN=srch(mat optCopy$,resp$(respc_copyCurrent))
		enableBackground$=resp$(respc_enableBackground)
		perPage$=resp$(respc_perPage)
		destinationOpt$(1)=resp$(respc_Print1099)
		destinationOpt$(2)=resp$(respc_export_ams)
		destinationOpt$(3)=resp$(respc_exportIris)
		outputFilename$=resp$(resp_export_file)
		! /r
		! r: validate, respond and/or reject
		dim ml$(0)*128
		if env$('cursys')='PR' and seltpN=0 and ckey<>ckey_test then
			mat ml$(2)
			ml$(1)='You must indicate which deduction you want printed.'
			ml$(2)='        Click OK to correct.'
			fnMsgBox(mat ml$,resp$,cap$,0)
			goto ASK_INFO
		end if
		if env$('cursys')='PR' and (typeN=0 or typeN>8) then
			mat ml$(2)
			ml$(1)='You must enter a valid 1099 Box to Print.'
			ml$(2)='        Click OK to correct.'
			fnMsgBox(mat ml$,resp$,cap$,0)
			goto ASK_INFO
		end if
		if ckey=ckey_defaultFilename then
				outputFilename$=os_filename$(env$('Documents')&'\ACS\[TaxYear] 1099-Misc Export\Company [CompanyNumber].csv')
			goto ASK_INFO
		else if ckey=ckey_margins then
			fn_ask_margins
			goto ASK_INFO
		end if
		! /r
		! r: save stuff
		fnpcreg_write('Filter - Minimum Amount',str$(minAmt))
		fncreg_write('Payee Name Format',resp$(respc_nameFormat))
		if env$('cursys')='CL' or env$('cursys')='GL' then
			fnpcreg_write('seltp',seltp$(1:2))
		else ! env$('cursys')='PR'
			fnpcreg_write('seltp',seltp$)
		end if
		if env$('cursys')='PR' then
			fnpcreg_write('1099 Box to Print',type$)
		end if
		fnpcreg_write('Copy Current',str$(copyCurrentN))
		fnpcreg_write('Print 1099',destinationOpt$(1))
		fnpcreg_write('Enable Background',enableBackground$)
		fnpcreg_write('2 Per Page',perPage$)
		fnpcreg_write('Export 1',destinationOpt$(2))
		fnpcreg_write('Export IRIS',destinationOpt$(3))
		fnureg_write('1099 - Export Filename',outputFilename$)
		fncreg_write('Phone Number',ph$)
		fncreg_write('Email',email$)
		! /r
		if copyCurrentN=1 and enableBackground$='True' and ~disableCopyAWarning then fnFormCopyAwithBackgroundWarn
		if ckey=ckey_test then
			fn_1099testPrint
			goto ASK_INFO
		end if
		returnN=1
	end if
	fn_ask=returnN
fnend
	def fn_ask_margins(; ___,lc,mypos,mylen,ckey_defaultMargins,x)
		! sets local form1y,form2y,left
		dim amResp$(10)*64
		gosub SetDefaultMargins
		amResp$(1)=fnPcRegRead$('form 1 Y'	,defaultMargin$(1))
		amResp$(2)=fnPcRegRead$('form 2 Y'	,defaultMargin$(2))
		amResp$(3)=fnPcRegRead$('X'        	,defaultMargin$(4))
		AskMargins: !
		fnTos
		lc=0 : mylen=30 : mypos=mylen+2
		fnLbl(lc+=1,1,'form 1 Distance from Top (mm):',mylen,1) 	: fnTxt(lc,mypos,3,0,1,'30')
		fnLbl(lc+=1,1,'form 2 Distance from Top (mm):',mylen,1) 	: fnTxt(lc,mypos,3,0,1,'30')
		lc+=1
		fnLbl(lc+=1,1,'Left Margin Size (mm):',mylen,1)          	: fnTxt(lc,mypos,3,0,1,'30')
		fnCmdKey('&Save',1,1,0)
		fnCmdKey('&Defaults',ckey_defaultMargins=1022)
		fnCmdKey('&Cancel',5,0,1)
		fnAcs(mat amResp$,ckey)
		if ckey=ckey_defaultMargins then
			for x=1 to udim(mat defaultMargin$) : amResp$(x)=defaultMargin$(x) : nex x
			goto AskMargins
		else if ckey<>5 then
			fnPcReg_write('form 1 Y' 	,amResp$(1))
			fnPcReg_write('form 2 Y' 	,amResp$(2))
			fnPcReg_write('X'         	,amResp$(3))
			form1y 	=val(amResp$(1))
			form2y    	=val(amResp$(2))
			left=      val(amResp$(3))
		end if
	fnend
	SetDefaultMargins: ! r:
		defaultMargin$(1)=           '2'  	! form 1 top margin
		defaultMargin$(2)=         '134'  	! form 2 top margin
		defaultMargin$(3)=           '5'  	! left
	return ! /r

! dim box(20)
! dim recipientAddr$(3)*30
def library fn1099MiscPrint(vn$*8,nam$*30,mat recipientAddr$,ss$*11,mat box)
	if ~setup then fn_setup
	fn1099MiscPrint=fn_1099print(vn$,nam$,mat recipientAddr$,ss$,mat box)
fnend
def fn_1099print(vn$*8,nam$*30,mat recipientAddr$,ss$*11,mat box; ___, _
		lineN,yn$*1,city$*64,state$*2,zip$*10)
	! inherrits local: disableCopyAWarning
	if ~ten99initialized then ! r: initialize output destination (if needed)
		ten99initialized=1
		if env$('CurSys')='PR' or env$('CurSys')='GL' or env$('CurSys')='CL' then
			open #hCompany=fnH: 'Name=[Q]\[CurSys]mstr\company.h[cno],Shr', internal,input,relative
			dim companyNameAddr$(3)*40
			dim fed$*12
			read #hCompany,using 'form pos 1,3*C 40,C 12': mat companyNameAddr$,fed$
			close #hCompany:
		else
			companyNameAddr$(1)='Test Company - The first company to try'
			companyNameAddr$(2)='123 Test Drive'
			companyNameAddr$(3)='Testerville, TS  12345-6789'
			fed$='12-456789012'
		end if
		fnpcreg_read('Export 1' ,ten99Export$,'False')
		fnpcreg_read('Export IRIS' ,ten99ExportIris$,'False')
		gosub SetDefaultMargins
		form1y 	=fnPcRegReadN('form 1 Y'	,val(defaultMargin$(1)))
		form2y 	=fnPcRegReadN('form 2 Y'	,val(defaultMargin$(2)))
		left   	=fnPcRegReadN('X'        	,val(defaultMargin$(3)))

		! pr 'printing with form1y =';form1y
		! pr 'printing with  form2y=';form2y
		! pr 'printing with form3y =';form3y
		! pr 'printing with  left  =';left
		! pause

		fnPcReg_read('2 Per Page',perPage$,'True' )
		fnPcReg_read('Enable Background',enableBackground$,'True' )
		dim outputFilename$*256
		fnureg_read('1099 - Export Filename',outputFilename$,os_filename$(env$('Documents')&'\ACS\[TaxYear] 1099-Misc Export\Company [CompanyNumber].csv'))
		dim ph$*12
		dim email$*128
		fnCreg_read('Phone Number',ph$)
		fnCreg_read('Email',email$)
		copyCurrentN=fnPcRegReadN('Copy Current',2)
		if ten99ExportIris$='True' or ten99Export$='True' then
			outputFilename$=srep$(outputFilename$,'[CompanyNumber]',env$('cno'))
			outputFilename$=srep$(outputFilename$,'[companynumber]',env$('cno'))
			outputFilename$=srep$(outputFilename$,'[companyNumber]',env$('cno'))
			outputFilename$=srep$(outputFilename$,'[COMPANYNUMBER]',env$('cno'))
			outputFilename$=srep$(outputFilename$,'[TaxYear]',taxYear$)
			outputFilename$=srep$(outputFilename$,'[taxyear]',taxYear$)
			outputFilename$=srep$(outputFilename$,'[taxYear]',taxYear$)
			outputFilename$=srep$(outputFilename$,'[TAXYEAR]',taxYear$)
		end if
		if ten99Export$='True' then
			fnMakeSurePathExists(outputFilename$)
			open #hExport=fnH: 'Name='&br_filename$(outputFilename$)&',REPLACE',d,o ! ioerr ASK_INFO
		else if ten99ExportIris$='True' then
			fnMakeSurePathExists(outputFilename$)
			open #hExport=fnH: 'Name='&br_filename$(outputFilename$)&',recL=4096,REPLACE',d,o ! ioerr ASK_INFO
			! r: pr IRIS CSV header
				pr #hExport: 'Form Type,';
				pr #hExport: 'Tax Year,';
				pr #hExport: 'Payer TIN Type,';
				pr #hExport: 'Payer Taxpayer ID Number,';
				pr #hExport: 'Payer Name Type,';
				pr #hExport: 'Payer Business or Entity Name Line 1,';
				pr #hExport: 'Payer Business or Entity Name Line 2,';
				pr #hExport: 'Payer First Name,';
				pr #hExport: 'Payer Middle Name,';
				pr #hExport: 'Payer Last Name (Surname),';
				pr #hExport: 'Payer Suffix,Payer Country,';
				pr #hExport: 'Payer Address Line 1,Payer Address Line 2,';
				pr #hExport: 'Payer City/Town,';
				pr #hExport: 'Payer State/Province/Territory,';
				pr #hExport: 'Payer ZIP/Postal Code,';
				pr #hExport: 'Payer Phone Type,';
				pr #hExport: 'Payer Phone,';
				pr #hExport: 'Payer Email Address,';
				pr #hExport: 'Recipient TIN Type,';
				pr #hExport: 'Recipient Taxpayer ID Number,';
				pr #hExport: 'Recipient Name Type,';
				pr #hExport: 'Recipient Business or Entity Name Line 1,';
				pr #hExport: 'Recipient Business or Entity Name Line 2,';
				pr #hExport: 'Recipient First Name,';
				pr #hExport: 'Recipient Middle Name,';
				pr #hExport: 'Recipient Last Name (Surname),';
				pr #hExport: 'Recipient Suffix,';
				pr #hExport: 'Recipient Country,';
				pr #hExport: 'Recipient Address Line 1,';
				pr #hExport: 'Recipient Address Line 2,';
				pr #hExport: 'Recipient City/Town,Recipient State/Province/Territory,Recipient ZIP/Postal Code,';
				pr #hExport: 'Office Code,';
				pr #hExport: 'Form Account Number,';
				pr #hExport: 'FATCA Filing Requirements,';
				pr #hExport: '2nd TIN Notice,';
				pr #hExport: 'Box  1,'; ! - Rents,';
				pr #hExport: 'Box  2,'; ! - Royalties,';
				pr #hExport: 'Box  3,'; ! - Other Income,';
				pr #hExport: 'Box  4,'; ! - Federal income tax withheld,';
				pr #hExport: 'Box  5,'; ! - Fishing boat proceeds,';
				pr #hExport: 'Box  6,'; ! - Medical and health care payments,';
				pr #hExport: 'Box  7,'; ! - Direct sales of $5000 or more of consumer products to a recipient for resale,';
				pr #hExport: 'Box  8,'; ! - Subtitute payments in lieu of dividends or interest,';
				pr #hExport: 'Box  9,'; ! - Crop insurance proceeds,';
				pr #hExport: 'Box 10,'; ! - Gross proceeds paid to an attorney,';
				pr #hExport: 'Box 11,'; ! - Fish purchased for resale,';
				pr #hExport: 'Box 12,'; ! - Section 409A deferrals,';
				pr #hExport: 'Box 14,'; ! - Excess golden parachute payments,';
				pr #hExport: 'Box 15,'; ! - Nonqualified deferred compensation,';
				pr #hExport: 'Combined Federal/State Filing,';
				pr #hExport: 'State 1,State 1 - State Tax Withheld,State 1 - State/Payer state number,State 1 - State income,State 1 - Local income tax withheld,State 1 - Special Data Entries,';
				pr #hExport: 'State 2,State 2 - State Tax Withheld,State 2 - State/Payer state number,State 2 - State income,State 2 - Local income tax withheld,State 2 - Special Data Entries,';
				pr #hExport: '' ! ',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
			! /r
		else
			fnpa_open('',optCopy$(copyCurrentN),'PDF')
		end if
	end if ! /r
	if ten99ExportIris$='True' then ! r: pr #hExport: IRIS CSV entry
		pr #hExport: '1099-MISC,';
		pr #hExport: taxYear$&',';
		pr #hExport: 'EIN,'; ! EIN or SSN for company
		pr #hExport: fed$&','; ! dashes and numbers only
		pr #hExport: 'B,'; ! B for business or I for individutal Payer Name Type
		pr #hExport: fn_c$(companyNameAddr$(1));
		pr #hExport: ','; ! Payer Business or Entity Name Line 2
		pr #hExport: ','; ! Payer First Name
		pr #hExport: ','; ! Payer Middle Name
		pr #hExport: ','; ! Payer Last Name (Surname)
		pr #hExport: ','; ! Payer Suffix
		pr #hExport: 'US,'; ! Payer Country
		pr #hExport: fn_c$(companyNameAddr$(2));
		pr #hExport: ','; ! fn_c$(companyNameAddr$(3));
		fncsz(companyNameAddr$(3),city$,state$,zip$)
		pr #hExport: city$&',';  ! Payer City/Town
		pr #hExport: state$&','; ! Payer State/Province/Territory
		pr #hExport: zip$&',';   ! Payer ZIP/Postal Code
		pr #hExport: 'D,'; ! Payer Phone Type - D for Domestic or I for International
		pr #hExport: ph$&','; ! Payer Phone
		pr #hExport: email$&','; ! Payer Email Address
		dim recipientType$*1
		if fn_isBusiness(ss$) then recipientType$='B' else recipientType$='I'
		if recipientType$='B' then
			pr #hExport: 'EIN,';  ! Recipient TIN Type
			pr #hExport: ss$&','; ! Recipient Taxpayer ID Number
			pr #hExport: recipientType$&',';    ! Recipient Name Type B for business or I for individual
			pr #hExport: nam$&','; ! Recipient Business or Entity Name Line 1
			pr #hExport: ','; ! Recipient Business or Entity Name Line 2
			pr #hExport: ','; ! Recipient First Name
			pr #hExport: ','; ! Recipient Middle Name
			pr #hExport: ','; ! Recipient Last Name (Surname)
			pr #hExport: ','; ! Recipient Suffix
		else
			pr #hExport: 'SSN,';  ! Recipient TIN Type
			pr #hExport: ss$&','; ! Recipient Taxpayer ID Number
			pr #hExport: recipientType$&',';    ! Recipient Name Type B for business or I for individual

			pr #hExport: ','; ! Recipient Business or Entity Name Line 1
			pr #hExport: ','; ! Recipient Business or Entity Name Line 2
			dim nameFirst$*64,nameMiddle$*64,nameLast$*64,nameSuffix$*64
			fnNameParse(nam$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$)
			pr #hExport: nameFirst$&','; ! Recipient First Name
			pr #hExport: nameMiddle$&','; ! Recipient Middle Name
			pr #hExport: nameLast$&','; ! Recipient Last Name (Surname)
			pr #hExport: nameSuffix$&','; ! Recipient Suffix
		end if
		pr #hExport: 'US,'; ! Recipient Country
		pr #hExport: recipientAddr$(1)&','; ! Recipient Address Line 1
		if udim(mat recipientAddr$)=2 then
			pr #hExport: ','; ! Recipient Address Line 2
		else
			pr #hExport: recipientAddr$(2)&','; ! Recipient Address Line 2
		end if
		fncsz(recipientAddr$(udim(mat recipientAddr$)),city$,state$,zip$)
		pr #hExport: city$&',';  ! 'Recipient City/Town,';
		pr #hExport: state$&','; ! 'Recipient State/Province/Territory,';
		pr #hExport: zip$&',';   ! 'Recipient ZIP/Postal Code,';
		pr #hExport: '1234,'; ! Office Code
		pr #hExport: vn$&','; ! Form Account Number
		if box(13) then yn$='Y' else yn$='N' 
		pr #hExport: yn$&','; ! FATCA Filing Requirements  Y for checked N for unchecked
		pr #hExport: 'N,'; ! 2nd TIN Notice  Y for checked N for unchecked
		pr #hExport: str$(box(1 ))&','; ! Box 1 - Rents
		pr #hExport: str$(box(2 ))&','; ! Box 2 - Royalties
		pr #hExport: str$(box(3 ))&','; ! Box 3 - Other Income
		pr #hExport: str$(box(4 ))&','; ! Box 4 - Federal income tax withheld
		pr #hExport: str$(box(5 ))&','; ! Box 5 - Fishing boat proceeds
		pr #hExport: str$(box(6 ))&','; ! Box 6 - Medical and health care payments
		pr #hExport: str$(box(7 ))&','; ! Box 7 - Direct sales of $5000 or more of consumer products to a recipient for resale
		pr #hExport: str$(box(8 ))&','; ! Box 8 - Subtitute payments in lieu of dividends or interest
		pr #hExport: str$(box(9 ))&','; ! Box 9 - Crop insurance proceeds
		pr #hExport: str$(box(10))&','; ! Box 10 - Gross proceeds paid to an attorney
		pr #hExport: str$(box(11))&','; ! Box 11 - Fish purchased for resale
		pr #hExport: str$(box(12))&','; ! Box 12 - Section 409A deferrals
		pr #hExport: str$(box(14))&','; ! Box 14 - Excess golden parachute payments
		pr #hExport: str$(box(15))&','; ! Box 15 - Nonqualified deferred compensation
		pr #hExport: ','; ! fnpayroll_client_state$&','; ! Combined Federal/State Filing
		pr #hExport: ','; ! fnpayroll_client_state$&','; ! State 1
		pr #hExport: ','; ! str$(box(16))&','; ! State 1 - State Tax Withheld
		pr #hExport: ','; ! str$(box(17))&','; ! State 1 - State/Payer state number
		pr #hExport: ','; ! str$(box(18))&','; ! State 1 - State income
		pr #hExport: ','; ! str$(box(--))&','; ! State 1 - Local income tax withheld
		pr #hExport: ','; ! State 1 - Special Data Entries
		pr #hExport: ','; ! State 2
		pr #hExport: ','; ! State 2 - State Tax Withheld
		pr #hExport: ','; ! State 2 - State/Payer state number
		pr #hExport: ','; ! State 2 - State income
		pr #hExport: ','; ! State 2 - Local income tax withheld
		pr #hExport: ','; ! State 2 - Special Data Entries
		pr #hExport: ''
		! /r
	else if ten99Export$='True' then ! r: export AMS
		pr #hExport: '01 ';' '
		pr #hExport: '02 ';ph$
		pr #hExport: '03 ';companyNameAddr$(1)
		pr #hExport: '04 ';box(1)
		pr #hExport: '05 ';' '
		pr #hExport: '06 ';companyNameAddr$(2)
		pr #hExport: '07 ';box(2)
		pr #hExport: '08 ';companyNameAddr$(3)
		pr #hExport: '09 ';box(3)
		pr #hExport: '10 ';box(4)
		pr #hExport: '11 ';fed$
		pr #hExport: '12 ';ss$
		pr #hExport: '13 ';box(5)
		pr #hExport: '14 ';box(6)
		pr #hExport: '15 ';box(7)
		pr #hExport: '16 ';box(8)
		pr #hExport: '17 ';nam$
		pr #hExport: '18 ';' '
		pr #hExport: '19 ';' '
		pr #hExport: '20 ';box(10)
		pr #hExport: '21 ';recipientAddr$(1)
		pr #hExport: '22 ';recipientAddr$(2)
		pr #hExport: '23 ';' '
		pr #hExport: '24 ';0
		pr #hExport: '25 ';vn$
		pr #hExport: '26 ';' '
		pr #hExport: '27 ';0
		pr #hExport: '28 ';' '
		pr #hExport: '29 ';0
		pr #hExport: '30 ';' ' ! 0
		! pr #hExport: '31 ';' '
		! pr #hExport: '32 ';0
		pr #hExport: '*'
		! /r
	else ! r: pr PDF
		column1 	= left +  15
		column2 	= left + 102
		column3 	= left + 137
		column4 	= left + 168
		column5 	= left + 141 ! year in 2022 and following
		ten99Count+=1
		if ten99Count=1 then yOffset=form1y
		if ten99Count=2 then yOffset=form2y ! print bottom : pause 
		if enableBackground$='True' and ten99Count=1 then
			fnpa_background(CopyFile$(copyCurrentN))
		end if
		fnpa_FontSize
		! r: draw developer lines
		developerLinesEnabled=0
		if developerLinesEnabled and env$('acsDeveloper')<>'' then
			for lineN=1 to udim(mat lineXy)
				fnpa_txt(str$(lineN)  ,left+1,fn_line(lineN))
				fnpa_txt('('&str$(lineXy(lineN))&')' ,left+10,fn_line(lineN))
				fnpa_txt(rpt$('_ ',50),left+1,fn_line(lineN))
			nex lineN
		end if
		! /r
		! r: print the text in the boxes
			! r: left side
				! PAYER'S name, street address, city, etc
				fnpa_txt(companyNameAddr$(1)	,column1    	,fn_line(1))
				fnpa_txt(companyNameAddr$(2)	,column1    	,fn_line(2))
				fnpa_txt(companyNameAddr$(3)	,column1    	,fn_line(3))
				fnpa_txt(ph$                 	,column1    	,fn_line(5))
				fnpa_txt(fed$                	,column1    	,fn_line(7))  ! PAYER'S TIN
				fnpa_txt(ss$                 	,column1+45	 	,fn_line(7))  ! RECIPIENT'S TIN
				fnpa_txt(nam$                	,column1    	,fn_line(9)-2)  ! RECIPIENT'S name
				if udim(mat recipientAddr$)=3 and trim$(recipientAddr$(3))='' then mat recipientAddr$(2)
				if udim(mat recipientAddr$)=2 or (udim(mat recipientAddr$)=3 and trim$(recipientAddr$(2))='') then
					fnpa_txt(recipientAddr$(1)       	,column1    	,fn_line(10)+2) ! address line 1
				else if udim(mat recipientAddr$)=3 then
					fnpa_txt(rtrm$(recipientAddr$(1))	,column1    	,fn_line(10))
					fnpa_txt(trim$(recipientAddr$(2))	,column1    	,fn_line(11)) ! address line 2
				else
					pr 'udim(mat recipientAddr$)=';udim(mat recipientAddr$);' this is unexpected by '&program$ : pause
				end if
				fnpa_txt(recipientAddr$(udim(mat recipientAddr$))       	,column1    	,fn_line(12)) !  CSZ
			!/r
			! r: right side (column2 and column3)
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(1) ),column2   		,fn_line(1) )
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(2) ),column2   		,fn_line(4) )
				fnpa_txt(taxyear$                             ,column5-5   		,fn_line(4) )
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(3) ),column2   		,fn_line(6) )
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(6) ),column3   		,fn_line(6) ) ! fed withheld
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(5) ),column2   		,fn_line(8) )
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(6) ),column3   		,fn_line(8) )
				! if sum(mat box)>5000 then
				! 	fnpa_txt('X'                                ,column2+21 	,fn_line(9))
				! end if
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(8) ),column3    	,fn_line(9) )
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(9) ),column2    	,fn_line(11))
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(10)),column3    	,fn_line(11))
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(11)),column2    	,fn_line(12))
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(12)),column3    	,fn_line(12))
				fnpa_txt(vn$                                   ,column1    	,fn_line(14))
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(13)),column2    	,fn_line(13))
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(14)),column3    	,fn_line(13))
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(15)),column2    	,fn_line(14))
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(16)),column3    	,fn_line(14))
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(17)),column4    	,fn_line(14))
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(18)),column2    	,fn_line(15))
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(19)),column3    	,fn_line(15))
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(20)),column4    	,fn_line(15))
			! /r
		! /r

		if perPage$='False' or ten99Count=2 then
			fnpa_newpage
			ten99Count=0
		end if
		! /r
	end if
	Xit: !
fnend
	def fn_c$*256(c$*256)
		c$=trim$(c$)
		if pos(c$,'"')>0 then c$=srep$(c$,'"','')
		if pos(c$,',')>0 then c$='"'&c$&'"'
		c$&=',' ! c$=c$&','
		fn_c$=c$
	fnend
	def fn_line(lineNumber)
		! inherrits local yOffset
		! retains setup_line,mat lineXy
		if ~setup_line then
			setup_line=1
			dim lineXy(15)
			lineXy( 1)= 13 ! PAYER'S name and Box 1 Rents
			lineXy( 2)= 17 ! PAYER's addr
			lineXy( 3)= 21 ! PAYER's csz
			lineXy( 4)= 26 ! Box 2 Royalties
			lineXy( 5)= 30 !
			lineXy( 6)= 35 ! box 2
			lineXy( 7)= 48 ! PAYER's TIN
			lineXy( 8)= 52 ! box 5b,6b
			lineXy( 9)= 65 ! RECIPIENT'S name  street address
			lineXy(10)= 72 ! box 7,8
			lineXy(11)= 77 ! recipient's CSZ
			lineXy(12)= 88 ! City or town, box 11,12
			lineXy(13)=101 ! Account number, FATCA filing, box 13,14
			lineXy(14)=111 ! box 15a,16a,17a
			lineXy(15)=116 ! box 15b,16b,17b (box(18-20))
		end if
		fn_line=lineXy(lineNumber)+yOffset
	fnend
	def fn_isBusiness(xx$; ___,xx12n,returnN)
		xx$=trim$(xx$)
		xx12n=val(xx$(1:2)) conv IbFinis
		if xx12n and xx$(3:3)='-' then returnN=1
		IbFinis: !
		fn_isBusiness=returnN
	fnend
def library fn1099MiscPrintClose
	if ~setup then fn_setup
	fn1099MiscPrintClose=fn_1099print_close
fnend
def fn_1099print_close
		if ten99Export$='True' or ten99ExportIris$='True' then
			dim msgTxt$(0)*256
			mat msgTxt$(2)
			msgTxt$(2)=os_filename$(file$(hExport))
			close #hExport:
			msgTxt$(1)='Created file:'
			fnMsgBox(mat msgTxt$)
		else
			fnpa_finis
		end if
		ten99initialized=0
fnend

include: ertn