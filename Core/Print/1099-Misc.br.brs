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

! dim empAddr$(3)*30,box(17)
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
	respc_Print1099,respc_perPage,respc_export_ams,resp_export_file, _
	ckey_defaultFilename,ckey_margins,ckey_test)
	! local retained values: awi_setup$, taxYear$, mat deductionFullName$, mat deductionOption$,deductionOptionCount, and many many more
	if awi_setup$<>env$('cursys')&env$('cno') then ! r: read or set values for ASK_INFO screen
		awi_setup$=env$('cursys')&env$('cno')
		taxYear$=date$(days(date$)-180,'CCYY')
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

		fnpcreg_read('Print 1099'       	,destinationOpt$(1)	,'True' )
		fnpcreg_read('Export 1'         	,destinationOpt$(2)	,'False')
		fnpcreg_read('Enable Background'	,enableBackground$	,'True' )
		fnpcreg_read('2 Per Page'       	,perPage$       	,'True')
		copyCurrentN=fnPcRegReadN('Copy Current',2)
		fnureg_read('1099 - Export Filename',output_filename$,os_filename$(env$('Desktop')&'\ACS [TaxYear] 1099 Export (Company [CompanyNumber]).txt'))
		fncreg_read('Phone Number',ph$)
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
	resp$(resc_taxYear:=rc+=1)=taxYear$
	if env$('cursys')='PR' then
		lc+=1
		fnLbl(lc+=1,1,'Miscellaneous Deduction to Print:',mylen,1)
		fnComboA('deductions',lc,mypos,mat deductionOption$,'Select the deduction you want printed.')
		resp$(respc_deduction:=rc+=1)=seltp$
		fnLbl(lc+=1,1,'1099 Box to Print:',mylen,1)
		fnComboA('type',lc,mypos,mat typeOption$,'Select the code for the 1099 vendor type.')
		resp$(respc_type:=rc+=1)=type$
	else if env$('cursys')='GL' or env$('cursys')='CL' then
		fnLbl(lc+=1,1,'Payee Type to Print:',mylen,1)
		resp$(respc_deduction:=rc+=1)=seltp$
		fnComboF('Payeetype',lc,mypos,27,'[Q]\[CurSys]mstr\PayeeType.dat',1,2,3,25,'',0,0, 'The payee type is a code used to detemine which box should be used on a 1099 misc form.  Enter the code for the payee type to print.')
	end if
	lc+=1
	fnLbl(lc+=1,1,'Minimum Amount to Print:',mylen,1)
	fnTxt(lc,mypos,12,0,1,'10',0,'Enter the minimum amount that should be printed.')
	resp$(respc_minAmt:=rc+=1)=str$(minAmt)
	fnLbl(lc+=1,1,'Your Telephone Number:',mylen,1)
	fnTxt(lc,mypos,12,0,1,'',0,'You can use dashes, etc.')
	resp$(respc_phone:=rc+=1)=ph$
	lc+=1
	fnOpt(lc+=1,3,'Print 1099-Misc')
	resp$(respc_Print1099:=rc+=1)=destinationOpt$(1)
	fnLbl(lc+=1,5,'Copy:',12,1,0)
	fnComboA('Copy',lc,19,mat optCopy$, '',20)
	resp$(respc_copyCurrent:=rc+=1)=optCopy$(copyCurrentN)
	! fnLbl(lc+=1,20,'(2 per page is not yet available with Backgrounds)',50,0)
	fnChk(lc+=1,20,'Enable Background',1)
	resp$(respc_enableBackground:=rc+=1)=enableBackground$
	fnChk(lc+=1,20,'2 Per Page',1)
	resp$(respc_perPage:=rc+=1)=perPage$
	lc+=1
	fnOpt(lc+=1,3,'Export for Advanced Micro Solutions')
	resp$(respc_export_ams:=rc+=1)=destinationOpt$(2)
	fnLbl(lc+=1,5,'Export File:',12,1,0,franum)
	fnTxt(lc,19,20,128,0,'72',0,'Choose a destination location for the ACS export.',franum)
	resp$(resp_export_file:=rc+=1)=output_filename$
	fnButton(lc,5+12+20+5,'Default',ckey_defaultFilename=14,'Choose to set the default for the selected destination software.',0,0,franum)
	fnLbl(lc+=1,19,'([CompanyNumber] and [TaxYear] will be substituted in filename)',0,0,0,franum)

	fnCmdKey('&Margins',ckey_margins:=1021,0,0,'Manually adjust margins for hitting forms')
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
		beg_date=val(taxYear$&'0101')
		end_date=val(taxYear$&'1231')
		copyCurrentN=srch(mat optCopy$,resp$(respc_copyCurrent))
		enableBackground$=resp$(respc_enableBackground)
		perPage$=resp$(respc_perPage)
		destinationOpt$(1)=resp$(respc_Print1099)
		destinationOpt$(2)=resp$(respc_export_ams)
		output_filename$=resp$(resp_export_file)
		! /r
		! r: validate, respond and/or reject
		dim ml$(0)*128
		if env$('cursys')='PR' and seltpN=0 and ckey<>ckey_test then
			mat ml$(2)
			ml$(1)='You must indicate which deduction you want printed.'
			ml$(2)='        Click OK to correct.'
			fnmsgbox(mat ml$,resp$,cap$,0)
			goto ASK_INFO
		end if
		if env$('cursys')='PR' and (typeN=0 or typeN>8) then
			mat ml$(2)
			ml$(1)='You must enter a valid 1099 Box to Print.'
			ml$(2)='        Click OK to correct.'
			fnmsgbox(mat ml$,resp$,cap$,0)
			goto ASK_INFO
		end if
		if ckey=ckey_defaultFilename then
				output_filename$=os_filename$(env$('Desktop')&'\ACS [TaxYear] 1099 Export (Company [CompanyNumber]).txt')
			goto ASK_INFO
		else if ckey=ckey_margins then
			fn_ask_margins
			goto ASK_INFO
		end if
		! /r
		! r: save stuff
		fnpcreg_write('Filter - Minimum Amount',str$(minAmt))
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
		fnureg_write('1099 - Export Filename',output_filename$)
		fncreg_write('Phone Number',ph$)
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
	def fn_ask_margins(; ___,lc,mypos,mylen)
		! sets local form1y,form2y,left
		dim amResp$(10)*64
		gosub SetDefaultMargins
		amResp$(1)=fnPcRegRead$('form 1 Y'	,defaultMargin$(1))
		amResp$(2)=fnPcRegRead$('form 2 Y'	,defaultMargin$(2))
		amResp$(3)=fnPcRegRead$('X'        	,defaultMargin$(4))
		fnTos
		lc=0 : mylen=30 : mypos=mylen+2
		fnLbl(lc+=1,1,'form 1 Distance from Top (mm):',mylen,1) 	: fnTxt(lc,mypos,3,0,1,'30')
		fnLbl(lc+=1,1,'form 2 Distance from Top (mm):',mylen,1) 	: fnTxt(lc,mypos,3,0,1,'30')
		lc+=1
		fnLbl(lc+=1,1,'Left Margin Size (mm):',mylen,1)          	: fnTxt(lc,mypos,3,0,1,'30')
		fnCmdSet(4)
		fnAcs(mat amResp$,ckey)
		if ckey<>5 then
			fnPcReg_write('form 1 Y' 	,amResp$(1))
			fnPcReg_write('form 2 Y' 	,amResp$(2))
			fnPcReg_write('X'         	,amResp$(3))
			form1y 	=val(amResp$(1))
			form2y    	=val(amResp$(2))
			left=      val(amResp$(3))
		end if
	fnend
	SetDefaultMargins: ! r:
		defaultMargin$(1)=           '5'  	! form 1 top margin
		defaultMargin$(2)=         '144'  	! form 2 top margin
		defaultMargin$(3)=           '5'  	! left
	return ! /r
def library fn1099MiscPrint(vn$*8,nam$*30,mat empAddr$,ss$*11,mat box)
	if ~setup then fn_setup
	fn1099MiscPrint=fn_1099print(vn$,nam$,mat empAddr$,ss$,mat box)
fnend
def fn_1099print(vn$*8,nam$*30,mat empAddr$,ss$*11,mat box; ___, _
		lineN)
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
		fnureg_read('1099 - Export Filename',output_filename$,os_filename$(env$('Desktop')&'\ACS [TaxYear] 1099 Export (Company [CompanyNumber]).txt'))
		dim ph$*12
		fnCreg_read('Phone Number',ph$)
		copyCurrentN=fnPcRegReadN('Copy Current',2)
		if ten99Export$='True' then
			dim output_filename$*256
			output_filename$=srep$(output_filename$,'[CompanyNumber]',env$('cno'))
			output_filename$=srep$(output_filename$,'[companynumber]',env$('cno'))
			output_filename$=srep$(output_filename$,'[COMPANYNUMBER]',env$('cno'))
			output_filename$=srep$(output_filename$,'[TaxYear]',taxYear$)
			output_filename$=srep$(output_filename$,'[taxyear]',taxYear$)
			output_filename$=srep$(output_filename$,'[TAXYEAR]',taxYear$)
			open #hExport=fnH: 'Name='&br_filename$(output_filename$)&',REPLACE',d,o ioerr ASK_INFO
		else
			fnpa_open('',optCopy$(copyCurrentN),'PDF')
		end if
	end if ! /r

	if ten99Export$='True' then
		! r: export one
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
		pr #hExport: '21 ';empAddr$(1)
		pr #hExport: '22 ';empAddr$(2)
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
	else
		! r: pr one
		column1 	= left +  15
		column2 	= left + 102
		column3 	= left + 137
		column4 	= left + 168
		ten99Count+=1
		if ten99Count=1 then yOffset=topmargin
		if ten99Count=2 then yOffset=bottom
		if enableBackground$='True' and ten99Count=1 then
			fnpa_background(CopyFile$(copyCurrentN))
		end if
		fnpa_FontSize
		! ! r: draw developer lines
		! if env$('acsDeveloper')<>'' then
		! 	for lineN=1 to udim(mat lineXy)
		! 		fnpa_txt(str$(lineN)  ,left+1,fn_line(lineN))
		! 		fnpa_txt('('&str$(lineXy(lineN))&')' ,left+10,fn_line(lineN))
		! 		fnpa_txt(rpt$('_ ',50),left+1,fn_line(lineN))
		! 	nex lineN
		! end if
		! ! /r
		! r: print the text in the boxes
			! r: left side
				! PAYER'S name, street address, city, etc
				fnpa_txt(companyNameAddr$(1),column1,fn_line(1))
				fnpa_txt(companyNameAddr$(2),column1,fn_line(2))
				fnpa_txt(companyNameAddr$(3),column1,fn_line(3))
				fnpa_txt(ph$,column1,fn_line(5))
				fnpa_txt(fed$,column1,fn_line(7))   ! PAYER'S TIN
				fnpa_txt(ss$,column1+45,fn_line(7)) ! RECIPIENT'S TIN
				fnpa_txt(nam$,column1,fn_line(9))  ! RECIPIENT'S name
				if udim(mat empAddr$)=2 then
					fnpa_txt(empAddr$(1),column1,fn_line(10)) ! address line 1
					fnpa_txt(empAddr$(2),column1,fn_line(12)) !  CSZ
				else if udim(mat empAddr$)=3 then
					fnpa_txt(rtrm$(empAddr$(1))&'  '&trim$(empAddr$(2)),column1,fn_line(10)) ! address line 2
					fnpa_txt(empAddr$(3),column1,fn_line(12)) !  CSZ
				else
					pr 'udim(mat empAddr$)=';udim(mat empAddr$);' this is unexpected by '&program$ : pause
				end if
			!/r
			! r: right side (column2 and column3)
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(1) ),column2   		,fn_line(1) )
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(2) ),column2   		,fn_line(4) )
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(3) ),column2   		,fn_line(6) )
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(6) ),column3   		,fn_line(6) ) ! fed withheld
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(5) ),column2   		,fn_line(7) )
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(6) ),column3   		,fn_line(7) )
				if box(7)>5000 then
					fnpa_txt('X'                                ,column2+21 	,fn_line(9))
				end if
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(8) ),column3    	,fn_line(9) )
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(9) ),column2    	,fn_line(11))
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(10)),column3    	,fn_line(11))
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(11)),column2    	,fn_line(12))
				fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',box(12)),column3    	,fn_line(12))
				fnpa_txt(vn$                                   ,column1    	,fn_line(13))
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
	def fn_line(lineNumber)
		! inherrits local yOffset
		! retains setup_line,mat lineXy
		if ~setup_line then
			setup_line=1
			dim lineXy(15)
			lineXy( 1)=15 ! PAYER'S name and Box 1 Rents
			lineXy( 2)=19 ! PAYER's addr
			lineXy( 3)=23 ! PAYER's csz
			lineXy( 4)= 28 ! Box 2 Royalties
			lineXy( 5)= 32 !
			lineXy( 6)= 36 ! box 2
			lineXy( 7)= 50 ! PAYER's TIN
			lineXy( 8)= 54 ! box 5b,6b
			lineXy( 9)= 66 ! RECIPIENT'S name  street address
			lineXy(10)= 75 ! box 7,8
			lineXy(11)= 79 ! recipient's CSZ
			lineXy(12)= 90 ! City or town, box 11,12
			lineXy(13)=103 ! Account number, FATCA filing, box 13,14
			lineXy(14)=113 ! box 15a,16a,17a
			lineXy(15)=117 ! box 15b,16b,17b (box(18-20))
		end if
		fn_line=lineXy(lineNumber)+yOffset
	fnend
def library fn1099MiscPrintClose
	if ~setup then fn_setup
	fn1099MiscPrintClose=fn_1099print_close
fnend
def fn_1099print_close
		if ten99Export$='True' then
			close #hExport:
		else
			fnpa_finis
		end if
		ten99initialized=0
fnend

include: ertn