! Replace S:\Core\Print\1099-Misc.br
! library for all 1099 forms
dim a$(3)*40,box(11),ph$*12

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
	dim testBox(10)
	! zz,zzz,zzz
	testBox(1 )=90000100
	testBox(2 )=90000200
	testBox(3 )=90000300
	testBox(4 )=90000400
	testBox(5 )=90000500
	testBox(6 )=90000600
	testBox(7 )=90000700
	testBox(8 )=90000800
	testBox(9 )=90000900
	testBox(10)=90001000
	! /r
	disableCopyAWarning=1
	fn_1099print('account1','Recipient One'  ,mat testAddr$,'111-11-1111',mat testBox)
	fn_1099print('Account2','Recipient Two'  ,mat testAddr$,'222-22-2222',mat testBox)
	fn_1099print('Account3','Recipient Three',mat testAddr$,'333-33-3333',mat testBox)
	disableCopyAWarning=0
	fn_1099print_close
fnend
! dim companyNameAddr$(3)*40,ss$*11
dim empAddr$(3)*30,box(11)
def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		on error goto Ertn
		! r: constants
		dim ml$(0)*128
		dim resp$(128)*256
		dim optCopy$(5)*72
		optCopy$(1)='A - For Internal Revenue Service Center'
		optCopy$(2)='1 - For State Tax Department'
		optCopy$(3)='B - For Recipient'
		optCopy$(4)='C - For Payer'
		optCopy$(5)='2 - To be filed with recipient;;s state income tax return, when required'
	dim copyCurrent$*72
	!
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
		fncomboa('deductions',lc,mypos,mat deductionOption$,'Select the deduction you want printed.')
		resp$(respc_deduction:=rc+=1)=seltp$
		fnLbl(lc+=1,1,'1099 Box to Print:',mylen,1)
		fncomboa('type',lc,mypos,mat typeOption$,'Select the code for the 1099 vendor type.')
		resp$(respc_type:=rc+=1)=type$
	else if env$('cursys')='GL' or env$('cursys')='CL' then
		fnLbl(lc+=1,1,'Payee Type to Print:',mylen,1)
		resp$(respc_deduction:=rc+=1)=seltp$
		fncombof('Payeetype',lc,mypos,27,'[Q]\[CurSys]mstr\PayeeType.dat',1,2,3,25,'',0,0, 'The payee type is a code used to detemine which box should be used on a 1099 misc form.  Enter the code for the payee type to print.')
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
	fncomboa('Copy',lc,19,mat optCopy$, '',20)
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
		fnpcreg_write('2 Per Page',twoPerPage$)
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
		defaultMargin$(1)=          '5'    	! form 1 top margin
		defaultMargin$(2)=         '144'    ! form 2 top margin
		defaultMargin$(3)=          '5'    	! left
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
		fncreg_read('1099 - Your Phone Number',ph$)
		fncreg_read('1099 - Copy Current'    ,copyCurrent$,optCopy$(1)) : copyCurrent=max(1,srch(mat optCopy$,copyCurrent$))

		if ten99Export$='True' then
			dim output_filename$*256
			output_filename$=srep$(output_filename$,'[CompanyNumber]',env$('cno'))
			output_filename$=srep$(output_filename$,'[companynumber]',env$('cno'))
			output_filename$=srep$(output_filename$,'[COMPANYNUMBER]',env$('cno'))
			output_filename$=srep$(output_filename$,'[TaxYear]',taxYear$)
			output_filename$=srep$(output_filename$,'[taxyear]',taxYear$)
			output_filename$=srep$(output_filename$,'[TAXYEAR]',taxYear$)
			open #hExport=fnH: 'Name='&br_filename$(output_filename$)&',REPLACE',display,output ioerr ASK_INFO
		else
			fnpa_open('',copyCurrent$,'PDF')
		end if
	end if ! /r

	if ten99Export$='True' then
		! r: export one
		pr #hExport: '01 ';' '
		pr #hExport: '02 ';ph$
		pr #hExport: '03 ';a$(1)
		pr #hExport: '04 ';box(1)
		pr #hExport: '05 ';' '
		pr #hExport: '06 ';a$(2)
		pr #hExport: '07 ';box(2)
		pr #hExport: '08 ';a$(3)
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
		column1=15 +left
		column2=85+left+17
		column3=119 +left+18
		ten99Count+=1
		if ten99Count=1 then yOffset=topmargin
		if ten99Count=2 then yOffset=bottom
		if enableBackground$='True' and ten99Count=1 then
			fnpa_background(CopyFile$(copyCurrent))
		end if
		if debug then
			for tmp=1 to 10
				fnpa_txt('line '&str$(tmp),1,fn_line(tmp))
			nex tmp
		end if
		fnpa_FontSize
		fnpa_txt(a$(1)(1:30),column1,fn_line(1))
		fnpa_txt(a$(2)(1:30),column1,fn_line(1)+5)
		fnpa_txt(a$(3)(1:30),column1,fn_line(1)+10)
		fnpa_txt(ph$,column1,fn_line(1)+20)
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(1)),column2,fn_line(1))
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(2)),column2,fn_line(2))
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(3)),column2,fn_line(3))
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(4)),column2,fn_line(5)+5) ! fed withheld 
		fnpa_txt(fed$,column1,fn_line(4))
		fnpa_txt(ss$,column1+45,fn_line(4))
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(5)),column2,fn_line(4))
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(6)),column3,fn_line(4))
		fnpa_txt(nam$(1:30),column1,fn_line(5)-8)
		fnpa_txt(empAddr$(1),column1,fn_line(5)+5) ! address line 1
		if udim(mat empAddr$)=2 then
			fnpa_txt(empAddr$(2),column1,fn_line(5)+18) !  CSZ
		else if udim(mat empAddr$)=3 then
			fnpa_txt(empAddr$(2),column1,fn_line(5)+8) ! address line 2
			fnpa_txt(empAddr$(3),column1,fn_line(5)+18) !  CSZ
		else
			pr 'udim(mat empAddr$)=';udim(mat empAddr$);' this is unexpected by '&program$ : pause
		end if
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(7)),column2,fn_line(1)+21) ! net income
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(8)),column3,fn_line(5))
		fnpa_txt(vn$,column1,fn_line(9))
		if perPage$='False' or ten99Count=2 then
			fnpa_newpage
			ten99Count=0
		end if
		! /r
	end if
	Xit: !
fnend
	def fn_line(lineNumber)
		if lineNumber=1  then
			lineReturn=yOffset+10
		else if lineNumber=2  then
			lineReturn=yOffset+23
		else if lineNumber=3 then
			lineReturn=yOffset+32
		else if lineNumber=4 then
			lineReturn=yOffset+43+2+4
		else if lineNumber=5 then
			lineReturn=yOffset+56+2+8
		else if lineNumber>5 then
			lineReturn=yOffset+56+2+((lineNumber-5)*13)
		else
			pr 'i dunno' : pause
		end if
		fn_line=lineReturn
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