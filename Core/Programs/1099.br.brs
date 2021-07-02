! Replace S:\Core\programs\1099.br
! library for all 1099 forms
dim empAddr$(3)*30,ss$*11,a$(3)*40,box(11),ph$*12
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

	dim copyFile$(5)*128,ssnMask(5)
	taxYear$=date$(days(date$)-120,'CCYY')
	copyFile$(1)='S:\Core\pdf\'&taxYear$&'\1099-Nec\Copy A.pdf' : ssnMask(1)=0
	copyFile$(2)='S:\Core\pdf\'&taxYear$&'\1099-Nec\Copy 1.pdf' : ssnMask(2)=0
	copyFile$(3)='S:\Core\pdf\'&taxYear$&'\1099-Nec\Copy B.pdf' : ssnMask(3)=0
	copyFile$(4)='S:\Core\pdf\'&taxYear$&'\1099-Nec\Copy C.pdf' : ssnMask(4)=1
	copyFile$(5)='S:\Core\pdf\'&taxYear$&'\1099-Nec\Copy 2.pdf' : ssnMask(5)=1
		! /r
	end if
fnend
def library fn1099print_close
		if ten99Export$='True' then
			close #hExport:
		else
			fnpa_finis
		end if
		ten99initialized=0
fnend
def library fn1099print(vn$*8,nam$*30,mat empAddr$,ss$*11,mat box)
	if ~setup then fn_setup
	if ~ten99initialized then ! r: initialize output destination (if necessary)
		dim a$(3)*40
		if env$('CurSys')='PR' then
			open #hCompany=fnH: 'Name=[Q]\PRmstr\company.h[cno],Shr', internal,input,relative
			read #hCompany,using 'Form POS 1,3*C 40,C 12': mat a$,fed$
			close #hCompany:
		else if env$('CurSys')='GL' then
			open #hCompany=fnH: 'Name=[Q]\GLmstr\Company.h[cno],Shr',internal,input,relative
			read #hCompany,using ' Form POS 1,3*C 40,C 12': mat a$,fed$
			close #hCompany:
		else if env$('CurSys')='CL' then
			open #hCompany=fnH: 'Name=[Q]\CLmstr\Company.h[cno],Shr',internal,input,relative
			read #hCompany,using ' Form POS 1,3*C 40,C 12': mat a$,fed$
			close #hCompany:
		end if
		ten99initialized=1
		fnreg_read('1099 - Export 1' ,ten99Export$,'False')
		topmargin	=fnreg_read('1099 - Form 1 Y',tmp$,'5'  )
		bottom    	=fnreg_read('1099 - Form 2 Y',tmp$,'144')
		left      	=fnreg_read('1099 - X'       ,tmp$,'5'  )
		fnreg_read('1099 - 2 Per Page',twoPerPage$,'False' )
		fnreg_read('1099 - Enable Background',enableBackground$,'True' )
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
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(1)  ),column2,fn_line(1))
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(2)  ),column2,fn_line(2))
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(3)  ),column2,fn_line(3))
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(4)  ),column2,fn_line(5)+5) ! fed withheld 
		fnpa_txt(fed$,column1,fn_line(4))
		fnpa_txt(ss$,column1+45,fn_line(4))
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(5)  ),column2,fn_line(4))
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(6)  ),column3,fn_line(4))
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
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(7)  ),column2,fn_line(1)+21) ! nec income
		fnpa_txt(cnvrt$('pic(zz,zzz,zzz.zz',box(8)  ),column3,fn_line(5))
		fnpa_txt(vn$,column1,fn_line(9))
		if twoPerPage$='False' or ten99Count=2 then
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
def fn_ask_margins
	fnreg_read('1099 - Form 1 Y',amResp$(1),'5' )
	fnreg_read('1099 - Form 2 Y',amResp$(2),'144')
	fnreg_read('1099 - X'       ,amResp$(3),'5' )
	fnTos
	lc=0 : mylen=30 : mypos=mylen+2
	fnLbl(lc+=1,1,'Form 1 Distance from Top (mm):',mylen,1)
	fnTxt(lc,mypos,3,0,1,'30')
	fnLbl(lc+=1,1,'Form 2 Distance from Top (mm):',mylen,1)
	fnTxt(lc,mypos,3,0,1,'30')
	fnLbl(lc+=1,1,'Left Margin Size (mm):',mylen,1)
	fnTxt(lc,mypos,3,0,1,'30')
	fnCmdSet(4)
	fnAcs(mat amResp$,ckey)
	if ckey<>5 then
		fnreg_write('1099 - Form 1 Y' ,amResp$(1))
		fnreg_write('1099 - Form 2 Y' ,amResp$(2))
		fnreg_write('1099 - X'        ,amResp$(3))
		topmargin= val(amResp$(1))
		bottom=    val(amResp$(2))
		left=      val(amResp$(3))
	end if
fnend
def library fnask_1099_info(&seltp,&type,&min1,&beg_date,&end_date)
	if ~awi_setup then ! r:
		awi_setup=1
		if ~setup then fn_setup
		! r: read or set values for ASK_INFO screen
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

		fncreg_read('1099 - Filter - Minimum Amount',tmp$,'600') : min1=val(tmp$)

		fnreg_read('Print 1099'              ,destinationOpt$(1),'True' )
		fnreg_read('1099 - Export 1'         ,destinationOpt$(2),'False')
		fnreg_read('1099 - Enable Background',enableBackground$  ,'True' )
		fnreg_read('1099 - 2 Per Page'       ,twoPerPage$        ,'False' )
		fncreg_read('1099 - Copy Current'    ,copyCurrent$,optCopy$(1)) : copyCurrent=max(1,srch(mat optCopy$,copyCurrent$))
		fnureg_read('1099 - Export Filename',output_filename$,os_filename$(env$('Desktop')&'\ACS [TaxYear] 1099 Export (Company [CompanyNumber]).txt'))
		fncreg_read('1099 - Your Phone Number',ph$)
		dim seltp$*256
		fncreg_read('1099 - seltp',seltp$)
		if env$('curSys')='PR' then seltp=srch(mat deductionFullName$,seltp$)
		if env$('curSys')='GL' or env$('curSys')='CL' then seltp=val(seltp$(1:2))
		! /r
	end if ! /r
	awiReturn=0
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
		resp$(respc_type:=rc+=1)=''
	else if env$('cursys')='GL' then
		fnLbl(lc+=1,1,'Payee Type to Print:',mylen,1)
		fncombof('PayeeType',lc,mypos,27,'[Q]\GLmstr\PayeeType.dat',1,2,3,25,'',0,0, 'The payee type is a code used to detemine which box should be used on a 1099 misc form.  Enter the code for the payee type to print.')
		resp$(respc_deduction:=rc+=1)=seltp$
	else if env$('cursys')='CL' then
		fnLbl(lc+=1,1,'Payee Type to Print:',mylen,1)
		fncombof('Payeetype',lc,mypos,27,'[Q]\CLmstr\PayeeType.dat',1,2,3,25,'',0,0, 'The payee type is a code used to detemine which box should be used on a 1099 misc form.  Enter the code for the payee type to print.')
		resp$(respc_deduction:=rc+=1)=seltp$

	end if
	lc+=1
	fnLbl(lc+=1,1,'Minimum Amount to Print:',mylen,1)
	fnTxt(lc,mypos,12,0,1,'10',0,'Enter the minimum amount that should be printed.')
	resp$(respc_min1:=rc+=1)=str$(min1)
	fnLbl(lc+=1,1,'Your Telephone Number:',mylen,1)
	fnTxt(lc,mypos,12,0,1,'',0,'You can use dashes, etc.')
	resp$(respc_phone:=rc+=1)=ph$
	lc+=1
	fnOpt(lc+=1,3,'Print 1099-NEC')
	resp$(respc_Print1099:=rc+=1)=destinationOpt$(1)
	fnLbl(lc+=1,5,'Copy:',12,1,0)
	fncomboa('Copy',lc,19,mat optCopy$, '',20)
	resp$(respc_copyCurrent:=rc+=1)=copyCurrent$
	fnLbl(lc+=1,20,'(2 per page is not yet available with Backgrounds)',50,0)
	fnChk(lc+=1,20,'Enable Background',1)
	resp$(respc_enableBackground:=rc+=1)=enableBackground$
	fnChk(lc+=1,20,'2 Per Page',1)
	resp$(respc_twoPerPage:=rc+=1)=twoPerPage$
	lc+=1
	fnOpt(lc+=1,3,'Export for Advanced Micro Solutions')
	resp$(respc_export_ams:=rc+=1)=destinationOpt$(2)
	fnLbl(lc+=1,5,'Export File:',12,1,0,franum)
	fnTxt(lc,19,20,128,0,'72',0,'Choose a destination location for the ACS export.',franum)
	resp$(resp_export_file:=rc+=1)=output_filename$
	fnButton(lc,5+12+20+5,'Default',14,'Choose to set the default for the selected destination software.',0,0,franum)
	fnLbl(lc+=1,19,'([CompanyNumber] and [TaxYear] will be substituted in filename)',0,0,0,franum)

	fnCmdKey('&Margins',ckey_margins:=1021,0,0,'Manually adjust margins for hitting forms')
	fnCmdKey('&Next',1,1,0,'Proceed to next screen.')
	fnCmdKey('&Cancel',5,0,1,'Returns to menu')
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		! r: gather local variables from mat resp$
		if env$('cursys')='PR' then
			seltp$=resp$(respc_deduction)
			seltp=srch(mat deductionFullName$,seltp$)
			type=val(resp$(respc_type))
		else if env$('cursys')='GL' or env$('cursys')='CL' then
			seltp=val(resp$(respc_deduction)(1:2))
			seltp$=resp$(respc_deduction)
		end if
		min1=val(resp$(respc_min1))
		ph$=resp$(respc_phone)
		beg_date=val(taxYear$&'0101')
		end_date=val(taxYear$&'1231')
		copyCurrent$=resp$(respc_copyCurrent)
		enableBackground$=resp$(respc_enableBackground)
		twoPerPage$=resp$(respc_twoPerPage)
		destinationOpt$(1)=resp$(respc_Print1099)
		destinationOpt$(2)=resp$(respc_export_ams)
		output_filename$=resp$(resp_export_file)
		! /r
		! r: validate, respond and/or reject
		if env$('cursys')='PR' and seltp=0 then
			mat ml$(2)
			ml$(1)='You must indicate which deduction you want printed.'
			ml$(2)='        Click OK to correct.'
			fnmsgbox(mat ml$,resp$,cap$,0)
			goto ASK_INFO
		end if
		if env$('cursys')='PR' and (type=0 or type>8) then
			mat ml$(2)
			ml$(1)='You must enter a valid 1099 type.'
			ml$(2)='        Click OK to correct.'
			fnmsgbox(mat ml$,resp$,cap$,0)
			goto ASK_INFO
		end if
		if ckey=14 then
				output_filename$=os_filename$(env$('Desktop')&'\ACS [TaxYear] 1099 Export (Company [CompanyNumber]).txt')
			goto ASK_INFO
		else if ckey=ckey_margins then
			fn_ask_margins
			goto ASK_INFO
		end if
		! /r
		! r: save stuff
		fncreg_write('1099 - Filter - Minimum Amount',str$(min1))
		fncreg_write('1099 - seltp',seltp$)
		fncreg_write('1099 - Copy Current',copyCurrent$)
		fnreg_write('Print 1099',destinationOpt$(1))
		fnreg_write('1099 - Enable Background',enableBackground$)
		fnreg_write('1099 - 2 Per Page',twoPerPage$)
		fnreg_write('1099 - Export 1',destinationOpt$(2))
		fnureg_write('1099 - Export Filename',output_filename$)
		fncreg_write('1099 - Your Phone Number',ph$)
		! /r
		if copyCurrent$=optCopy$(1) and enableBackground$='True' then let fnFormCopyAwithBackgroundWarn
		awiReturn=1
	end if
	fnask_1099_info=awiReturn
fnend
include: ertn