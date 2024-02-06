fn_setup
! r: testing zone

	! r: test fn_line
		! for x=1 to 20
		! pr '('&str$(x)&')=';fn_line(x)
		! nex x
	! /r

	fn_test

! /r
end
def fn_test(; ___, _
			controlNumber$, _
			nameFirst$,nameMiddle$,nameLast$,nameSuffix$,retirementPlanX$, _
			box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$, _
			box12dCode$,box12dAmt$,state$,stcode$,printLocality$, box14Amt, _
			taxYear$,ssrate,ssmax,mcrate,mcmax, _
			pn1,dc1,state$,enableAskCLocality,cLocality$, _
			x)

	if fn_ask_w2_info(taxYear$,ssrate,ssmax,mcrate,mcmax, _
		pn1,dc1,state$,enableAskCLocality,cLocality$) then

		! r: set variables
			dim sss$(3)*11
			sss$(1)='111-11-1111' : sss$(2)='222-22-2222' : sss$(3)='333-33-3333'
			controlNumber$     	='control'
			dim testW (13)
			mat testW           	=(0)
			nameFirst$         	='Testy'
			nameMiddle$        	='T'
			nameLast$          	='McTesterson'
			nameSuffix$        	=''
			retirementPlanX$   	=''
			dim testK$(3)*30
			mat testK$          	=('')
			box12aCode$        	=''
			box12aAmt$         	='12.1'
			box12bCode$        	=''
			box12bAmt$         	='12.2'
			box12cCode$        	=''
			box12cAmt$         	='12.3'
			box12dCode$        	=''
			box12dAmt$         	='12.4'
			state$              	='TX'
			stcode$             	='TX'
			printLocality$     	=''
			box14Amt            	=14
		! /r

		for sampleW2=1 to 3
			for x=1 to 12 : testW(x)=sampleW2 : nex x
			fn_w2Text (sss$(sampleW2),controlNumber$,mat testW, _
				nameFirst$,nameMiddle$,nameLast$,nameSuffix$,retirementPlanX$,mat testK$, _
				box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$, _
				box12dCode$,box12dAmt$,state$,stcode$,printLocality$, box14Amt)
		next sampleW2
	end if
	fn_w2PrintClose

fnend

! global
dim outputFilename$*512
dim topmargin
dim left
dim bottom
dim taxYearLocal$
dim line$*4096
dim contactName$*64  ! 27 ! Employer Contact Name
dim contactPhone$*64 ! 15 ! Employer Contact Phone Number
dim contactPhExt$*64 !  5 ! Employer Contact Phone Extension
dim contactFax$*64   ! 10 ! Employer Contact Fax Number
dim contactEmail$*64 ! 40 ! Employer Contact E-Mail/Internet


def library fnask_w2_info(&taxYear$,&ssrate,&ssmax,&mcrate,&mcmax,&pn1,&dc1,&state$,enableAskCLocality,&cLocality$)
	if setup$<>env$('cursys')&env$('cno') then fn_setup
	fnask_w2_info=fn_ask_w2_info(taxYear$, ssrate,ssmax,mcrate,mcmax, pn1,dc1,state$,enableAskCLocality,cLocality$)
fnend
def fn_ask_w2_info(&taxYear$, _
	&ssrate,&ssmax,&mcrate,&mcmax, _
	&pn1,&dc1,&state$,enableAskCLocality,&cLocality$; ___, _
	awiReturn)
	! inherrits local: enableBackground$,bottom,w2Copy,enableW3$,outputFilename$

	if ~awi_setup then ! r:
		awi_setup=1
		! r: constants

		dim resp$(128)*256
		dim w2destinationOpt$(3)*18
		dim optW2Copy$(6)*68
		dim w2Copy$*68
		optW2Copy$(1)='A - For Social Security Administration' ! Send with W-3
		optW2Copy$(2)='1 - For State, City or Local Tax Department'
		optW2Copy$(3)='B - To Be Filed with Employee''s Federal Tax Return'
		optW2Copy$(4)='C - For Employee''s Records'
		optW2Copy$(5)='2 - To Be Filed with Employee''s State, City or Local Tax Department'
		optW2Copy$(6)='D - For Employer'
		dim cLlocalityToolTip$*256
		! /r
		! r: read or set values for ASK_INFO screen
		taxYear$=date$(days(date$)-120,'CCYY')
		if env$('client')='Payroll Done Right' then taxYear$=date$(days(date$)-100,'CCYY') ! client requested to print 2019 early
		empStart$='[All]'
		empEnd$='[All]'
		ssrate=.062
		if taxYear$='2016' then
			ssmax=118500 ! 2016
		else if taxYear$='2017' then
				ssmax=127200 ! 2017
		else if taxyear$='2018' then
				ssmax=128400
		else if taxyear$='2019' then
				ssmax=132900
		else if taxyear$='2020' then
				ssmax=137700
		else if taxyear$='2021' then
				ssmax=142800
		else if taxyear$='2023' then ! from page 28 of 15-T  ( https://www.irs.gov/pub/irs-prior/p15--2024.pdf )
				ssmax=168600
		end if
		mcrate=.0145
		mcmax=999999
		fncreg_read('W-2 - cLocality',cLocality$,'NO')
		fnreg_read('Print W-2'                          ,w2destinationOpt$(1),'True' )
		fnreg_read('Export for Advanced Micro Solutions',w2destinationOpt$(2),'False')
		fnreg_read('Export for Social Security Administration',w2destinationOpt$(3),'False')
		fnreg_read('Print W-3 also'                     ,enableW3$           ,'True' )
		fnreg_read('W-2 - Enable Background'            ,enableBackground$   ,'True' )
		fncreg_read('W-2 - Copy Current',w2Copy$,optW2Copy$(1)) : w2Copy=srch(mat optW2Copy$,w2Copy$) : if w2Copy<=0 then w2Copy=1
		w2Copy$=optW2Copy$(w2Copy)
		fncreg_read(env$('cursys')&' Name Format',nameFormat$,optNameFormat$(1))
		fnureg_read('W-2 - Export Filename',outputFilename$,os_filename$(env$('Documents')&'\ACS\[TaxYear] '&formName$&' Export\Company [CompanyNumber].csv'))
		fncreg_read('Qualified Pension Plan' ,tmp$) : pn1=val(tmp$)
		fncreg_read('Dependent Care Benefits',tmp$) : dc1=val(tmp$)
		fncreg_read('W-2 - State',state$)

		fnreg_read('W-2 - form 1 Y'              ,tmp$   ,'10' ) : topmargin=val(tmp$)
		fnreg_read('W-2 - form 2 Y'              ,tmp$   ,'151') : bottom=val(tmp$)
		! enableDateRange=0
		disableSSMCedit=0
		enableEmpRange=0
		! /r
	end if ! /r
	if env$('cursys')='PR' then
		enablePayrollDeductions=1
		enableAskState=0
		cLlocalityToolTip$='If you have answered that you have local withholdings in the company information file, you must enter the locality name'
	else if env$('cursys')='GL' then
		enablePayrollDeductions=0
		enableAskState=1
		cLlocalityToolTip$=''
	else
		pr 'not configured for anything but GL and PR yet.'
		pause
	end if
	ASK_INFO: !
	fnTos ! build and display the ASK_INFO screen
	rc=cf=0: mylen=21: mypos=mylen+2
	if enableAskCLocality then
		fraWidth=94
	else
		fraWidth=82 ! 68
	end if

	fraGeneralHeight=2
	! if enableDateRange then fraGeneralHeight+=2
	if enableEmpRange then fraGeneralHeight+=3
	if enableAskCLocality then fraGeneralHeight+=5
	fnFra(1,1,fraGeneralHeight,fraWidth,'General','Normally this would the first and last day of the calendar year',0)
	cf+=1 : franum=cf : lc=0
	fnLbl(lc+=1,1,'Tax Year:',mylen,1,0,franum)
	fnTxt(lc,mypos,4,0,1,'',1,'Year to pr W-2s for',franum)
	resp$(resc_taxYear=rc+=1)=taxYear$
	! if enableDateRange then
	! 	fnLbl(lc+=1,1,'Starting Date:',mylen,1,0,franum)
	! 	fnTxt(lc,mypos,10,0,1,'3',0,'First day of calendar year',franum)
	! 	resp$(respc_startdate=rc+=1)='0101'&date$(days(date$)-180,'YY')
	! 	fnLbl(lc+=1,1,'Ending Date:',mylen,1,0,franum)
	! 	fnTxt(lc,mypos,10,0,1,'3',0,'Last day of calendar year',franum)
	! 	resp$(respc_enddate=rc+=1)='1231'&date$(days(date$)-180,'YY')
	! 	lc+=1
	! end if
	if enableEmpRange then
		fnLbl(lc+=1,1,'Starting Employee:',mylen,1,0,franum)
		fnCmbEmp(lc,mypos,1,franum)
		resp$(respc_empStart=rc+=1)=''
		fnLbl(lc+=1,1,'Ending Employee:',mylen,1,0,franum)
		fnCmbEmp(lc,mypos,1,franum)
		resp$(respc_empEnd=rc+=1)=''
		lc+=1
	end if
	if enableAskCLocality then
		fnLbl(lc+=1,1,'Locality Name:',mylen,1,0,franum)
		fnTxt(lc,mypos,12,0,1,'',0,cLocalityToolTip$,franum)
		resp$(resp_cLocality=rc+=1)=cLocality$
		fnLbl(lc   ,mypos+12+2,'Enter the locality name if the same on all employees.',57,0,0,franum)
		fnLbl(lc+=1,mypos+12+2,'Enter NO (or blank) if it is not applicable.',57,0,0,franum)
		fnLbl(lc+=1,mypos+12+2,'Enter YES if applicable, but not he same on all employees',57,0,0,franum)
		lc+=1
	end if
	fnLbl(lc+=1,1,'Employee Name Format:',mylen,1,0,franum)
	fnComboA('nameFormat',lc,mypos,mat optNameFormat$, '',20,franum)
	resp$(resp_namcde=rc+=1)=nameFormat$
	fra2Height=5 : fra2Y=fraGeneralHeight+3
	fnFra(fra2Y,1,fra2Height,fraWidth,'Print W-2s','',0)
	cf+=1 : franum=cf : lc=0
	mylen=46: mypos=mylen+2
	fnLbl(1,1,'Social Security Withholding Rate:',mylen,1,0,franum)
	fnTxt(1,mypos,10,0,1,'34',disableSSMCedit,'Use format such as .062.',franum)
	resp$(respc_ssrate=rc+=1)=str$(ssrate)
	fnLbl(2,1,'Maximum Wage Subject to SS Withholdings:',mylen,1,0,franum)
	fnTxt(2,mypos,10,0,1,'10',disableSSMCedit,'Enter the maximum wage subject to social security withholdings for the current year just ended.',franum)
	resp$(respc_ssmax=rc+=1)=str$(ssmax)
	fnLbl(4,1,'Medicare Withholding Rate:',mylen,1,0,franum)
	fnTxt(4,mypos,10,0,1,'34',disableSSMCedit,'Use format such as .0145 .',franum)
	resp$(respc_mcrate=rc+=1)=str$(mcrate)
	fnLbl(5,1,'Maximum Wage Subject to Medicare Withholdings:',mylen,1,0,franum)
	fnTxt(5,mypos,10,0,1,'10',disableSSMCedit,'At the present time there is no maximum.  Enter a number larger than any one''s wages can be. For example, 999999.00',franum)
	resp$(respc_mcmax=rc+=1)=str$(mcmax)

	fra3Y=fra2Y+fra2Height+2 : fra3Height=9 : lc=0
	fnFra(fra3Y,1,fra3Height,fraWidth,'Printing or Exporting','You have the option to either pr the W-2s or export them to another system for printing.')
	cf+=1 : franum=cf : mylen=26 : mypos=mylen+2
	fnOpt(lc+=1,3,'Print W-2',0,franum)
	resp$(respc_PrintW2=rc+=1)=w2destinationOpt$(1)
	! fnLbl(1,fraWidth-50,'(2 per page is not yet available with Backgrounds)',50,1,0,franum)
	fnLbl(lc+=1,5,'Copy:',12,1,0,franum)
	fnComboA('w2Copy',2,19,mat optW2Copy$, '',20,franum)
	resp$(respc_w2copy=rc+=1)=w2Copy$
	fnChk(lc,68,'W-2 - Enable Background',1,franum)
	resp$(respc_enableBackground=rc+=1)=enableBackground$
	lc+=1
	fnChk(lc+=1,68,'Print W-3 also',1,franum)
	resp$(respc_w3=rc+=1)=enableW3$
	lc+=1
	fnOpt(lc+=1,3,'Export for Social Security Administration',0,franum)
	resp$(respc_export_ssa=rc+=1)=w2destinationOpt$(3)


	fnOpt(lc+=1,3,'Export for Advanced Micro Solutions',0,franum)
	resp$(respc_export_ams=rc+=1)=w2destinationOpt$(2)
	fnLbl(lc+=1,5,'Export File:',12,1,0,franum)
	fnTxt(lc,19,20,256,0,'72',0,'Choose a destination location for the ACS export.',franum)
	resp$(resp_w2_export_file=rc+=1)=outputFilename$
	fnButton(lc,5+12+20+5,'Default',14,'Choose to set the default for the selected destination software.',0,0,franum)
	fnLbl(lc+=1,19,'([CompanyNumber] and [TaxYear] will be substituted in filename)',0,0,0,franum)

	if enablePayrollDeductions then
		fra4Y=fra3y+fra3Height+2 ! 25
		fnFra(fra4Y,1,2,fraWidth,'Identify the Following Deductions','You have twenty miscellaneous deductions available to you. If you have Qualified Pension or Dependent Care, start with the first deduction and count down to identify the number of the deduction.')
		cf+=1 : franum=cf
		fnLbl(1,1,'Qualified Pension Plan:',mylen,1,0,franum)
		fnTxt(1,mypos,2,0,1,'30',0,'If you have a qualified pension plan that requires the pension plan box to be checked, count down from your 1st miscellaneous deduction to determine the number to enter here.',franum)
		resp$(respc_qpenplan=rc+=1)=str$(pn1)
		fnLbl(2,1,'Dependent Care Benefits:',mylen,1,0,franum)
		fnTxt(2,mypos,2,0,1,'30',0,'If you have dependent care benefits that should be identifies on the W-2, count down from your 1st miscellaneous deduction to determine the number to enter here.',franum)
		resp$(respc_depCareBen=rc+=1)=str$(dc1)
	else if enableAskState then
		fra4Y=fra3y+fra3Height+2 ! 25
		fnFra(fra4Y,1,2,fraWidth,'State','')
		cf+=1 : franum=cf
		fnLbl(1,1,'State Name:',mylen,1,0,franum)
		fnTxt(1,mypos,2,0,1,'',0,'If you have a qualified pension plan that requires the pension plan box to be checked, count down from your 1st miscellaneous deduction to determine the number to enter here.',franum)
		resp$(respc_state=rc+=1)=state$
	end if
	fnCmdKey('&Margins',ckey_margins=1021,0,0,'Manually adjust margins for hitting forms')
	fnCmdKey('&Next',1,1,0,'Proceed to next screen.')
	fnCmdKey('&Cancel',5,0,1,'Returns to menu')
	ckey=fnAcs(mat resp$)
	! r: ASK_INFO screen - respond to FKeys, and get local values from mat resp$
	if ckey=5 then
		awiReturn=0
	else
		awiReturn=1
		taxYear$=resp$(resc_taxYear)
		nameFormat$=resp$(resp_namcde)
		! if enableDateRange then
		! 	beg_date=val(resp$(respc_startdate))
		! 	end_date=val(resp$(respc_enddate))
		! else
			beg_date=val(taxYear$&'0101')
			end_date=val(taxYear$&'1231')
		! end if
		!   pr beg_date,end_date : pause ! 1154
		numb=empno=endnum=0
		if enableEmpRange then
			empStart$=resp$(respc_empStart)(1:8)
			empEnd$  =resp$(respc_empEnd)(1:8)
			if empStart$<>'[All]' then
				numb=val(empStart$(1:8))
				empno=numb
			end if
			if enableAskCLocality then
				cLocality$=uprc$(rtrm$(resp$(resp_cLocality)))
				if cLocality$='' then cLocality$='NO'
			end if
			if empEnd$<>'[All]' then
				endnum=val(empEnd$(1:8))
			end if
		end if
		ssrate=val(resp$(respc_ssrate))
		ssmax=val(resp$(respc_ssmax))
		mcrate=val(resp$(respc_mcrate))
		mcmax=val(resp$(respc_mcmax))
		w2Copy$=resp$(respc_w2copy) : w2Copy=srch(mat optW2Copy$,resp$(respc_w2copy))
		enableBackground$=resp$(respc_enableBackground)
		enableW3$=resp$(respc_w3)
		w2destinationOpt$(1)=resp$(respc_PrintW2)
		w2destinationOpt$(2)=resp$(respc_export_ams)
		w2destinationOpt$(3)=resp$(respc_export_ssa)
		outputFilename$=resp$(resp_w2_export_file)
		if enablePayrollDeductions then
			pn1=val(resp$(respc_qpenplan))
			dc1=val(resp$(respc_depCareBen))
		else if enableAskState then
			state$=resp$(respc_state)
		end if
		if ckey=14 then
				outputFilename$=os_filename$(env$('Documents')&'\ACS\[TaxYear] '&formName$&' Export\Company [CompanyNumber].csv')
			goto ASK_INFO
		else if ckey=ckey_margins then
			fn_ask_margins
			goto ASK_INFO
		end if
		if beg_date=0 or end_date=0 then goto ASK_INFO
		fncreg_write('W-2 - cLocality',cLocality$)
		fncreg_write('W-2 - Copy Current',w2Copy$)
		fnreg_write('Print W-2',w2destinationOpt$(1))
		fnreg_write('Print W-3 also',enableW3$)
		fnreg_write('W-2 - Enable Background',enableBackground$)
		fnreg_write('W-3 - Enable Background',enableBackground$)
		fnreg_write('Export for Advanced Micro Solutions',w2destinationOpt$(2))
		fnreg_write('Export for Social Security Administration',w2destinationOpt$(3))
		fnureg_write('W-2 - Export Filename',outputFilename$)
		fncreg_write('Qualified Pension Plan',str$(pn1))
		fncreg_write('Dependent Care Benefits',str$(dc1))
		fncreg_write(env$('cursys')&' Name Format',nameFormat$)
		fncreg_write('W-2 - State',state$)

		! /r
		if w2destinationOpt$(3)='True' then

			dim bsoUser$*8
			dim submitterName$*27
			dim submitterPhone$*15
			dim submitterPhExt$*5
			dim submitterFax$*10
			dim submitterEmail$*40
			fnreg_read('BSO User Identification',bsoUser$) ! BSO User ID: B7WYT74J
			fnreg_read('Submitter Name' ,submitterName$ ) 
			fnreg_read('Submitter Phone',submitterPhone$)
			fnreg_read('Submitter PhExt',submitterPhExt$)
			fnreg_read('Submitter Fax'  ,submitterFax$)
			fnreg_read('Submitter Email',submitterEmail$)

			dim contactName$*27  	! Employer Contact Name
			dim contactPhone$*15 	! Employer Contact Phone Number
			dim contactPhExt$*5  	! Employer Contact Phone Extension
			dim contactFax$*10   	! Employer Contact Fax Number
			dim contactEmail$*40 	! Employer Contact E-Mail/Internet
			fncreg_read('Contact Name',contactName$)
			fncreg_read('Phone Number',contactPhone$)
			fncreg_read('Phone Extension',contactPhExt$)
			fncreg_read('Fax Extension',contactFax$)
			fncreg_read('Email',contactEmail$)

			fnTos : lc=rc=0 : mylen=25 : mypos=mylen+2
				lc+=1 
				fnLbl(lc+=1,1,'BSO User ID:',mylen,1)
				fnTxt(lc,mypos,8,0,0,'',0,'BSO User Identification is required.')
				resp$(resp_bsoUserId=rc+=1)=bsoUser$
				lc+=1 
				fnLbl(lc+=1,1,'Submitter Contact Name:',mylen,1)
				fnTxt(lc,mypos,27)
				resp$(resp_submitterName=rc+=1)=submitterName$
				fnLbl(lc+=1,1,'Submitter Phone:',mylen,1)
				fnTxt(lc,mypos,15)
				resp$(resp_submitterPhone=rc+=1)=submitterPhone$
				fnTxt(lc,mypos+17,5)
				resp$(resp_submitterPhExt=rc+=1)=submitterPhExt$
				fnLbl(lc+=1,1,'Fax:',mylen,1)
				fnTxt(lc,mypos,10)
				resp$(resp_submitterFax=rc+=1)=submitterFax$
				fnLbl(lc+=1,1,'Email:',mylen,1)
				fnTxt(lc,mypos,40)
				resp$(resp_submitterEmail=rc+=1)=submitterEmail$
				lc+=1
				lc+=1
				fnLbl(lc+=1,1,'Company Contact Name:',mylen,1)
				fnTxt(lc,mypos,27)
				resp$(resp_contactName=rc+=1)=contactName$
				fnLbl(lc+=1,1,'Phone:',mylen,1)
				fnTxt(lc,mypos,15)
				resp$(resp_contactPhone=rc+=1)=contactPhone$
				fnTxt(lc,mypos+17,5)
				resp$(resp_contactPhExt=rc+=1)=contactPhExt$
				fnLbl(lc+=1,1,'Fax:',mylen,1)
				fnTxt(lc,mypos,10)
				resp$(resp_contactFax=rc+=1)=contactFax$
				fnLbl(lc+=1,1,'Email:',mylen,1)
				fnTxt(lc,mypos,40)
				resp$(resp_contactEmail=rc+=1)=contactEmail$
				fncmdset(2)
			ckey=fnAcs(mat resp$)
			if ckey=5 then
				goto ASK_INFO
			else
				bsoUser$=resp$(resp_bsoUserId)
				submitterName$ =resp$(resp_submitterName )
				submitterPhone$=resp$(resp_submitterPhone)
				submitterPhExt$=resp$(resp_submitterPhExt)
				submitterFax$=resp$(resp_submitterFax)
				submitterEmail$=resp$(resp_submitterEmail)
				fnreg_write('BSO User Identification',bsoUser$)
				fnreg_write('Submitter Name' ,submitterName$ )
				fnreg_write('Submitter Phone',submitterPhone$)
				fnreg_write('Submitter PhExt',submitterPhExt$)
				fnreg_write('Submitter Fax'  ,submitterFax$  )
				fnreg_write('Submitter Email',submitterEmail$)
				
				contactName$=resp$(resp_contactName)
				contactPhone$=resp$(resp_contactPhone)
				contactPhExt$=resp$(resp_contactPhExt)
				contactFax$=resp$(resp_contactFax)
				contactEmail$=resp$(resp_contactEmail)
				fncreg_write('Contact Name',contactName$)
				fncreg_write('Phone Number',contactPhone$)
				fncreg_write('Phone Extension',contactPhExt$)
				fncreg_write('Fax Extension',contactFax$)
				fncreg_write('Email',contactEmail$)
			end if
		end if
		if w2Copy$=optW2Copy$(1) and enableBackground$='True' and w2destinationOpt$(1)='True' then
			fn_FormCopyAwithBackgroundWarn
		end if
	end if
	Xit: !
	taxYearLocal$=taxYear$
	fn_ask_w2_info=awiReturn
fnend



def fn_setup ! libraries and Company
	if setup$='' then
		autoLibrary
		on error goto Ertn
	end if
	if setup$<>env$('cursys')&env$('cno') then
		setup$=env$('cursys')&env$('cno')
		dim optNameFormat$(2)*20,nameFormat$*20
		optNameFormat$(1)='First Name First'
		optNameFormat$(2)='Last Name First'
		! r: get mat a$ and ein$  (same as in w3.br.brs)
		dim a$(3)*40
		dim ein$*12
		if env$('cursys')='GL' then
			open #hCompany=fnH: 'Name=[Q]\GLmstr\Company.h[cno],Shr',i,i
			read #hCompany,using 'form pos 1,3*C 40,2*C 12,pos 618,40*N 1': mat a$,ein$
			close #hCompany:
			hCompany=0
		else if env$('cursys')='PR' then
			open #hCompany=fnH: 'Name=[Q]\PRmstr\Company.h[cno],Shr',i,i
			read #hCompany,using 'form pos 1,3*C 40,C 12': mat a$,ein$
			close #hCompany:
			hCompany=0
		else
			pr ' Core\Print\W2.br.brs is not ready for cursys='&env$('cursys')&', please enhance code.'
			pr ' need to read mat a$ (comp name and address) and ein$ (federal id)'
			pause
		end if
		! /r

		dim aCity$*64,aSt$*64,aZip$*64
		fnCsz(a$(3),aCity$,aSt$,aZip$)

		formName$='W-2'

	end if
fnend
def fn_ask_margins
! if env$('acsdeveloper')='' then pr bell; : goto am_xit
	fnreg_read('W-2 - form 1 Y'   	,amResp$(1), '10')
	fnreg_read('W-2 - form 2 Y'   	,amResp$(2),'151')
	fnreg_read('W-2 - X'           	,amResp$(3), '12')
	fnreg_read('W-3 - Margin Top' 	,amResp$(4),  '5')
	fnreg_read('W-3 - Margin Left'	,amResp$(5),  '7')
	fnTos
	mylen=30 : mypos=mylen+2 : lc=0
	fnLbl(lc+=1,1,'form 1 Distance from Top (mm):',mylen,1) 	: fnTxt(lc,mypos,3,0,1,'30')
	fnLbl(lc+=1,1,'form 2 Distance from Top (mm):',mylen,1) 	: fnTxt(lc,mypos,3,0,1,'30')
	fnLbl(lc+=1,1,'Left Margin (mm):',mylen,1)               	: fnTxt(lc,mypos,3,0,1,'30')
	lc+=1
	fnLbl(lc+=1,1,'W-3 Top (mm):',mylen,1)                   	: fnTxt(lc,mypos,3,0,1,'30')
	fnLbl(lc+=1,1,'W-3 Left Margin (mm):',mylen,1)          	: fnTxt(lc,mypos,3,0,1,'30')
	fnCmdSet(4)
	fnAcs(mat amResp$,ckey)
	if ckey<>5 then
		fnreg_write('W-2 - form 1 Y'    	,amResp$(1))
		fnreg_write('W-2 - form 2 Y'    	,amResp$(2))
		fnreg_write('W-2 - X'            	,amResp$(3))
		fnreg_write('W-3 - Margin Top'  	,amResp$(4))
		fnreg_write('W-3 - Margin Left' 	,amResp$(5))

		topmargin  	=val(amResp$(1))
		bottom      	=val(amResp$(2))
		left        	=val(amResp$(3))
	end if
fnend

def library fnW2Text(ss$,controlNumber$,mat w,nameFirst$*64,nameMiddle$*64,nameLast$*64,nameSuffix$*64,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$*6; box14Amt)
	if setup$<>env$('cursys')&env$('cno') then fn_setup
	fnW2Text=fn_w2Text(ss$,controlNumber$,mat w,nameFirst$,nameMiddle$,nameLast$,nameSuffix$,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$, box14Amt)
fnend

def fn_w2Text(ss$,controlNumber$,mat w, _
	nameFirst$*64,nameMiddle$*64,nameLast$*64,nameSuffix$*64, _
	retirementPlanX$,mat k$, _
	box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$, _
	state$,stcode$,printLocality$*6; box14Amt,___ )
	! r: variable definations
	! topmargin       how far down the page (mm) is the top of W-2
	! maskSsn         if 1 than turn all but the last 4 of the SSN into *s
	! ss$             social security number (with dashes)
	! mat a$          company name and address
	! controlNumber$  control number

	! box12aCode$     box 12a code
	! box12aAmt$      box 12a amount

	! box12bCode$     box 12b code
	! box12bAmt$      box 12b amount

	! box12cCode$     box 12c code
	! box12cAmt$      box 12c amount

	! box12dCode$     box 12d code
	! box12dAmt$      box 12d amount

	! w(4)            EIC
	! /r
	if ~w2setup then ! r:
		w2setup=1
		fnreg_read('W-2 - X',tmp$   ,'12' ) : left=val(tmp$)
		fncreg_read('W-2 - cLocality',cLocality$,'NO')
		w2Col1=left
		w2Col2=left+117
		w2Col3=left+160
		w2Box12CodePos=w2Col3-14
		w2Box12AmtPos=w2Box12CodePos+18
		box14AmtPos=w2Box12AmtPos
		! r: set ssn mask by copy
			dim w2ssnMask(6) 	! W2CopyFile2pp$(6)*128,W2CopyFile$(6)*128,
			w2ssnMask(1)=0   	! W2CopyFile$(1)='S:\Core\pdf\'&taxYear$&'\W-2\Copy A.pdf' :  W2CopyFile2pp$(1)='S:\Core\pdf\'&taxYear$&'\W-2\Copy A - 2pp.pdf'
			w2ssnMask(2)=0   	! W2CopyFile$(2)='S:\Core\pdf\'&taxYear$&'\W-2\Copy 1.pdf' :  W2CopyFile2pp$(2)='S:\Core\pdf\'&taxYear$&'\W-2\Copy 1 - 2pp.pdf'
			w2ssnMask(3)=0   	! W2CopyFile$(3)='S:\Core\pdf\'&taxYear$&'\W-2\Copy B.pdf' :  W2CopyFile2pp$(3)='S:\Core\pdf\'&taxYear$&'\W-2\Copy B - 2pp.pdf'
			w2ssnMask(4)=1   	! W2CopyFile$(4)='S:\Core\pdf\'&taxYear$&'\W-2\Copy C.pdf' :  W2CopyFile2pp$(4)='S:\Core\pdf\'&taxYear$&'\W-2\Copy C - 2pp.pdf'
			w2ssnMask(5)=1   	! W2CopyFile$(5)='S:\Core\pdf\'&taxYear$&'\W-2\Copy 2.pdf' :  W2CopyFile2pp$(5)='S:\Core\pdf\'&taxYear$&'\W-2\Copy 2 - 2pp.pdf'
			w2ssnMask(6)=1   	! W2CopyFile$(6)='S:\Core\pdf\'&taxYear$&'\W-2\Copy D.pdf' :  W2CopyFile2pp$(6)='S:\Core\pdf\'&taxYear$&'\W-2\Copy D - 2pp.pdf'
		! /r
	end if ! /r
	maskSsn=w2ssnMask(w2Copy)
	taxYear$=taxYearLocal$
	printCount+=1
	if ~w2Initialized then ! r:
		w2Initialized=1
		! pr 'initializing w2' : pause
		if w2destinationOpt$(1)='True' then
			fnpa_open('',w2Copy$,'PDF')
			! pr 'fnpa_open pdf' : pause
		else if w2destinationOpt$(2)='True' or w2destinationOpt$(3)='True' then
			outputFilename$=srep$(outputFilename$,'[CompanyNumber]',env$('cno'))
			outputFilename$=srep$(outputFilename$,'[companynumber]',env$('cno'))
			outputFilename$=srep$(outputFilename$,'[COMPANYNUMBER]',env$('cno'))
			outputFilename$=srep$(outputFilename$,'[TaxYear]',taxYear$)
			outputFilename$=srep$(outputFilename$,'[taxyear]',taxYear$)
			outputFilename$=srep$(outputFilename$,'[TAXYEAR]',taxYear$)
			fnMakeSurePathExists(outputFilename$)
		if w2destinationOpt$(2)='True' then
			open #hExport=fnH: 'Name='&br_filename$(outputFilename$)&',recl=512,EOL=CRLF,REPLACE',d,o ! ioerr AskInfo
		else if w2destinationOpt$(3)='True' then
			open #hO=fnH: 'Name='&br_filename$(outputFilename$)&',recl=514,EOL=CRLF,REPLACE',external,o ! ioerr AskInfo
			gosub Efw2_Ra
			gosub Efw2_Re
		end if
		end if
	end if ! /r
	if w2destinationOpt$(1)='True' then ! ! r: Print PDF

		if printCount/2<>int(printCount/2) then ! it's the first one on a page
			if enableBackground$='True' then
				fnpa_background('S:\Core\pdf\'&taxYear$&'\W-2\Copy '&w2Copy$(1:1)&'.pdf')
			end if
		end if
		if printCount/2=int(printCount/2) then ! it's the second one on a page
			w2Yoffset=bottom
		else
			w2Yoffset=topmargin
		end if


		fnpa_fontsize
		if maskSsn then
			fnpa_txt('***-**-'&ss$(8:11),left+44,fn_line(1))
		else
			fnpa_txt(ss$,left+44,fn_line(1))
		end if
		! if env$('acsdeveloper')<>'' then specialform2018=1
		! removed 1/21/2022        ! if env$('client')='Thomasboro' or env$('client')='Cerro Gordo V' or env$('client')='Cerro Gordo T' or env$('client')='Kincaid' or env$('client')='Hope Welty' or env$('client')='Bethany' then specialform2018=1
		fnpa_txt(ein$,w2Col1,fn_line(2))
		fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(2)),w2Col2,fn_line(2))
		fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(1)),w2Col3,fn_line(2))
		fnpa_txt(a$(1),w2Col1,fn_line(3))
		fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(5)),w2Col2,fn_line(3))
		fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(3)),w2Col3,fn_line(3))
		fnpa_txt(a$(2),w2Col1,fn_line(4))
		fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(11)),w2Col2,fn_line(4))
		fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(12)),w2Col3,fn_line(4))
		fnpa_txt(a$(3),w2Col1,fn_line(5))
		fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(6)),w2Col2,fn_line(5))
		fnpa_txt(controlNumber$,w2Col1,fn_line(6))
		! fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',w(4)),w2Col2,fn_line(6)) ! EIC - no longer reported - just greyed out
		fnpa_txt(cnvrt$('pic(zzzzzzzzzz.zz',dcb),w2Col3,fn_line(6))
		fnpa_txt((rtrm$(nameFirst$)&' '&rtrm$(nameMiddle$))(1:17),w2Col1,fn_line(7))
		fnpa_txt(rtrm$(nameLast$),left+44,fn_line(7))
		fnpa_txt(rtrm$(nameSuffix$),left+92,fn_line(7))
		fnpa_txt(box12aCode$,w2Box12CodePos,fn_line(7))
		fnpa_txt(box12aAmt$,w2Box12AmtPos,fn_line(7))
		fnpa_txt(k$(2),w2Col1,fn_line(8))
		fnpa_txt(retirementPlanX$,left+119,fn_line(8))
		fnpa_txt(box12bCode$,w2Box12CodePos,fn_line(8))
		fnpa_txt(box12bAmt$,w2Box12AmtPos,fn_line(8))
		fnpa_txt(k$(3),w2Col1,fn_line(9))
		fnpa_txt(box12cCode$,w2Box12CodePos,fn_line(9))
		fnpa_txt(box12cAmt$,w2Box12AmtPos,fn_line(9))
		fnpa_txt(box12dCode$,w2Box12CodePos,fn_line(10))
		fnpa_txt(box12dAmt$,w2Box12AmtPos,fn_line(10))
		if box14Amt<>0 then
			fnpa_txt(cnvrt$('pic(zzzzzzzzzzzzz.zz',box14Amt),left+109,fn_line(10))
		end if
		if env$('client')='Zaleski' then ! cLocality$<>'NO' then
			! do nothing
		else if env$('client')='Kathys Bookkeeping' or env$('client')='Thomasboro' then
			speciallastline=w2Yoffset+100
			fnpa_txt(state$,left-3,speciallastline)
			fnpa_txt(stcode$,left+10,speciallastline)
			fnpa_txt(cnvrt$('pic(zzzzzzzzzzzzz.zz',w( 9)),left+ 51,speciallastline)
			fnpa_txt(cnvrt$('pic(zzzzzzzzzzzzz.zz',w( 7)),left+ 79,speciallastline)
			fnpa_txt(cnvrt$('pic(zzzzzzzzzzzzz.zz',w(10)),left+109,speciallastline)
			fnpa_txt(cnvrt$('pic(zzzzzzzzzzzzz.zz',w( 8)),left+137,speciallastline)
			fnpa_txt(printLocality$(1:6),left+164,speciallastline)
		else
			fnpa_txt(state$                                	,left-3  ,fn_line(11))
			fnpa_txt(stcode$                               	,left+10 ,fn_line(11))
			fnpa_txt(cnvrt$('pic(zzzzzzzzzzzzz.zz',w( 9)) 	,left+51 ,fn_line(11))
			fnpa_txt(cnvrt$('pic(zzzzzzzzzzzzz.zz',w( 7)) 	,left+79 ,fn_line(11))
			fnpa_txt(cnvrt$('pic(zzzzzzzzzzzzz.zz',w(10))	,left+109,fn_line(11))
			fnpa_txt(cnvrt$('pic(zzzzzzzzzzzzz.zz',w( 8)) 	,left+137,fn_line(11))
			fnpa_txt(printLocality$(1:6)                   	,left+164,fn_line(11))
		end if


		if printCount/2=int(printCount/2) then ! it's the second one on a page
			fnpa_newpage
		end if

	! /r
	else if w2destinationOpt$(2)='True' then ! r: Export AMS
		pr #hExport: 'ROAN='&controlNumber$
		pr #hExport: 'FEIN='&ein$
		pr #hExport: 'WAGES='&str$(w(2))
		pr #hExport: 'FITW='&str$(w(1))
		pr #hExport: 'PNAME1='&a$(1)
		pr #hExport: 'PNAME2='
		pr #hExport: 'SSWAGES='&str$(w(5))
		pr #hExport: 'SSWH='&str$(w(3))
		pr #hExport: 'PADDR1='&a$(2)
		pr #hExport: 'PADDR2='&a$(3)
		pr #hExport: 'MCWAGES='&str$(w(11))
		pr #hExport: 'MCWH='&str$(w(12))
		pr #hExport: 'SSN='&srep$(ss$,' ','')
		pr #hExport: 'SSTIPS='&str$(w(6))
		pr #hExport: 'ALLOCATIP='  ! 'ALLOCATIP=';0
		pr #hExport: 'RNAME1='&(rtrm$(nameLast$)&','&nameFirst$)(1:24)
		pr #hExport: 'RNAME2='&(k$(2)(1:24))
! 	pr #hExport: 'AEIC=';w(4)    ! this field is no longer supported 1/4/2017
		pr #hExport: 'DEPDCARE='&str$(dcb)
		pr #hExport: 'RADDR1='
		pr #hExport: 'RADDR2='&(k$(3)(1:24))
		if box14Amt<>0 then
			pr #hExport: 'LAB14A=TRANS'
		else
			pr #hExport: 'LAB14A='
		end if
		pr #hExport: 'BOX14A='&str$(box14Amt) ! pr #hExport: 'BOX14A=0'
		pr #hExport: 'LAB12A='&box12aCode$
		pr #hExport: 'BOX12A='&box12aAmt$
		pr #hExport: 'CNTRYCODE='
		pr #hExport: 'RCOUNTRY='
		pr #hExport: 'EESTAT=' ! 0'
		pr #hExport: 'EERETR='&retirementPlanX$
		pr #hExport: 'EESICK=' ! 0'
		pr #hExport: 'LAB14B='
		pr #hExport: 'BOX14B=0'
		pr #hExport: 'LAB12B='&box12bCode$
		pr #hExport: 'BOX12B='&box12bAmt$
		pr #hExport: 'LAB14C='
		pr #hExport: 'BOX14C=0'
		pr #hExport: 'LAB12C='&box12cCode$
		pr #hExport: 'BOX12C='&box12cAmt$
		pr #hExport: 'LAB14D='
		pr #hExport: 'BOX14D=0'
		pr #hExport: 'LAB12D='&box12dCode$
		pr #hExport: 'BOX12D='&box12dAmt$
		pr #hExport: 'BOX11Q='&str$(nqp)
		pr #hExport: 'NQPLANS='
		pr #hExport: 'STATE1='&state$
		pr #hExport: 'SEIN1='&stcode$
		pr #hExport: 'SWAGES1='&str$(w(9))
		pr #hExport: 'SITW1='&str$(w(7))
		pr #hExport: 'LWAGES1='&str$(w(10))
		pr #hExport: 'LITW1='&str$(w(8))
		pr #hExport: 'LOCAL1='&printLocality$
		pr #hExport: 'STATE2='
		pr #hExport: 'SEIN2='
		pr #hExport: 'SWAGES2=0'
		pr #hExport: 'SITW2=0'
		pr #hExport: 'LWAGES2=0'
		pr #hExport: 'LITW2=0'
		pr #hExport: 'LOCAL2='
		pr #hExport: 'FName='&nameFirst$(1:24)
		pr #hExport: 'LName='&nameLast$(1:24)
		pr #hExport: 'TAG='
		pr #hExport: 'EBAT='
		pr #hExport: 'PHONE='
		pr #hExport: '*'
	! /r
	else if w2destinationOpt$(3)='True' then ! r: Export SSA
		! if env$('acsDeveloper')<>'' then debug=1
		dim kCity$*64,kSt$*64,kZip$*64
		fnCsz(k$(3),kCity$,kSt$,kZip$)
		! if debug then pr #hExport: '_______RA (Submitter) Record – Required__________dev break_________________________________________'
		! What records are optional in an EFW2 file and which ones are required?
		gosub Efw2_Rw
		! if debug then pr #hO: '__________________________________________dev break_________________________________________'

	! /r
	end if
	! r: accumulate totals for W3
	dim wTotal(13)
	for wItem=1 to udim(mat wTotal)
		wTotal(wItem)+=w(wItem)
	nex wItem
	 ! /r
fnend ! 1131
def fn_line(lineNumber; ___,returnN)
	! (1)=1
	! (2)=10
	! (3)=18.5
	! (4)=27
	! (5)=35.5
	! (6)=44
	! (7)=52.5
	! (8)=61
	! (9)=69.5
	! (10)=78
	! (11)=90
	! (12)=98.5
	! (13)=107
	! (14)=115.5
	! (15)=124
	! (16)=135.5
	! (17)=141
	! (18)=149.5
	! (19)=158
	! (20)=166.5
	if lineNumber=1 then
		returnN=w2Yoffset+1
	else  ! if lineNumber>=1 and lineNumber<=14 then
		returnN=w2Yoffset+10+(8.5*(lineNumber-2))
		! if specialform2018=1 and lineNumber=12 then
		! 	returnN+=.5
		! 	goto LineFinis
		! else if specialform2018=1 and lineNumber=8 then
		! 	returnN+=2
		! 	goto LineFinis
		! end if
		if lineNumber>=11 then
				returnN+=3.5
		end if
	end if
	LineFinis: ! skip here for special pre-printed forms
	fn_line=returnN
fnend
	def fn_oC(text$*256,fieldLength; format$)
		line$&=rpad$(trim$(text$)(1:fieldLength),fieldLength)
	fnend
	def fn_oN(num,fieldLength; format$)
		line$&=lpad$(str$(num),fieldLength,'0')
	fnend
	def fn_oOut
		if len(line$)<>512 then pr 'wrong length len(line$)=';len(line$) : pause
		write #hO,using Fo1: line$,chr$(13)&chr$(10)
		Fo1: form pos 1,c 512,c 2
		line$=''
	fnend
	Efw2_Rw: ! r: • RW (Employee) Record – Required
		fn_oC('RW',2)         : cntRw+=1
		fn_oC(ss$           	,  9) !
		fn_oC(nameFirst$   	, 15) !
		fn_oC(nameMiddle$  	, 15) !
		fn_oC(nameLast$    	, 20) !
		fn_oC(nameSuffix$  	,  4) !
		fn_oC(k$(2)         	, 22) !
		fn_oC(k$(2)         	, 22) !
		fn_oC(kCity$       	, 22) !
		fn_oC(kSt$         	,  2) !
		fn_oC(kZip$        	,  5) !
		fn_oC('',  4) ! 139-142 ZIP Code Extension
		fn_oC('',  5) ! 143-147 Blank
		fn_oC('', 23) ! 148-170 Foreign State/Province
		fn_oC('', 15) ! 171-185 Foreign Postal Code
		fn_oC('',  2) ! 186-187 Country Code
		fn_oN(w(2) , 11) : tot(1)+=w(2) ! 188-198 Wages, Tips and Other Compensation
		fn_oN(w(1) , 11) : tot(2)+=w(1) ! 199-209 Federal Income Tax Withheld
		fn_oN(w(5) , 11) : tot(3)+=w(6) ! 210-220 Social Security Wages
		fn_oN(w(3) , 11) : tot(4)+=w(12)! 221-231 Social Security Tax Withheld
		fn_oN(w(11), 11) : tot(5)+=w(11)! 2322-242 Medicare Wages and Tips
		fn_oN(w(12), 11) : tot(6)+=w(3) ! 243-253 Medicare Tax Withheld
		fn_oN(w(6) , 11) : tot(7)+=w(5) ! 254-264 Social Security Tips
		fn_oC('', 11) ! 265-275 Blank
		fn_oC('', 11) ! 276-297 Deferred Compensation Contributions to Section 401(k) (Code D)
		fn_oC('', 11) ! 287-297 Deferred Compensation Contributions to Section 401(k) (Code D)
		fn_oC('', 11) ! 298-308 Deferred Compensation Contributions to Section 403(b) (Code E)
		fn_oC('', 11) ! 309-319 Deferred Compensation Contributions to Section 408(k)(6) (Code F)
		fn_oC('', 11) ! 320-330 Deferred Compensation Contributions to Section 457(b) (Code G)
		fn_oC('', 11) ! 331-341 Deferred Compensation Contributions to Section 501(c)(18)(D) (Code H)
		fn_oC('', 11) ! 342-352 Blank
		fn_oC('', 11) ! 353-363 Nonqualified Plan Section 457 Distributions or Contributions
		fn_oC('', 11) ! 364-374 Employer Contributions to a Health Savings Account (Code W)
		fn_oC('', 11) ! 375-385 Nonqualified Plan Not Section 457 Distributions or Contributions
		fn_oC('', 11) ! 386-396 Nontaxable Combat Pay (Code Q)
		fn_oC('', 11) ! 397-407 Blank
		fn_oC('', 11) ! 408-418 Employer Cost of Premiums for Group Term Life Insurance Over $50,000 (Code C)
		fn_oC('', 11) ! 419-429 Income from the Exercise of Nonstatutory Stock Options (Code V)
		fn_oC('', 11) ! 430-440 Deferrals Under a Section 409A Nonqualified Deferred Compensation Plan (Code Y)
		fn_oC('', 11) ! 441-451 Designated Roth Contributions to a Section 401(k) Plan (Code AA)
		fn_oC('', 11) ! 452-462 Designated Roth Contributions Under a Section 403(b) Salary Reduction Agreement (Code BB)
		fn_oC('', 11) ! 463-473 Cost of Employer-Sponsored Health Coverage (Code DD)
		fn_oC('', 11) ! 474-484 Permitted Benefits Under a Qualified Small Employer Health Reimbursement Arrangement (Code FF)
		fn_oC('',  1) ! 485 Blank
		fn_oC('',  1) ! 486 Statutory Employee Indicator
		fn_oC('',  1) ! 487 Blank
		fn_oC('',  1) ! 488 Retirement Plan Indicator
		fn_oC('',  1) ! 489 Third-Party Sick Pay Indicator
		fn_oC('', 23) ! 490-512 Blank
		if len(line$)<>512 then pr 'wrong length len(line$)=';len(line$) : pause
		fn_oOut
	return ! /r
	! • RO (Employee Optional) Record – Optional
	! r: • RS (State) Record – Optional
		! fn_oC('RS',2) !   1-2
		! fn_oC('',  2) !   3-9   State Code
		! fn_oC('',  5) !  10-24  Taxing Entity Code
		! fn_oC('',  9) !  25-39  Social Security Number (SSN)
		! fn_oC('', 15) !  40-54  Employee First Name
		! fn_oC('', 15) !  55-69  Employee Middle Name or Initial
		! fn_oC('', 20) !  70-84  Employee Last Name
		! fn_oC('',  4) !  85-99  Suffix
		! fn_oC('', 22) ! 100-114 Location Address
		! fn_oC('', 22) ! 115-129 Delivery Address
		! fn_oC('', 22) ! 117-138 City
		! fn_oC('',  2) ! 139-140 State Abbreviation
		! etc...
	! /r
	Efw2_Rt: ! r: • RT (Total) Record – Required
		fn_oC('RT',2)
		fn_oN(cntRw,   7) !     3-9 Total Number of RW Records
		fn_oN(tot(1), 15) !   10-24 Total Wages, Tips and Other Compensation
		fn_oN(tot(2), 15) !   25-39 Total Federal Income Tax Withheld
		fn_oN(tot(3), 15) !   40-54 Total Social Security Wages
		fn_oN(tot(4), 15) !   55-69 Total Social Security Tax Withheld
		fn_oN(tot(5), 15) !   70-84 Total Medicare Wages and Tips
		fn_oN(tot(6), 15) !   85-99 Total Medicare Tax Withheld
		fn_oN(tot(7), 15) ! 100-114 Total Social Security Tips
		fn_oC('',     15) ! 115-129 Blank
		fn_oN(0,      15) ! 130-144 Total Dependent Care Benefits
		fn_oN(0,      15) ! 145-159 Total Deferred Compensation Contributions to Section 401(k) (Code D)
		fn_oN(0,      15) ! 160-174 Total Deferred Compensation Contributions to Section 403(b) (Code E)
		fn_oN(0,      15) ! 175-189 Total Deferred Compensation Contributions to Section 408(k)(6) (Code F)
		fn_oN(0,      15) ! 190-204 Total Deferred Compensation Contributions to Section 457(b) (Code G)
		fn_oN(0,      15) ! 205-219 Total Deferred Compensation Contributions to Section 501(c)(18)(D) (Code H)
		fn_oC('',     15) ! 220-234 Blank
		fn_oN(0,      15) ! 235-249 Total Nonqualified Plan Section 457 Distributions or Contributions
		fn_oN(0,      15) ! 250-264 Total Employer Contributions to a Health Savings Account (Code W)
		fn_oN(0,      15) ! 265-279 Total Nonqualified Plan Not Section 457 Distributions or Contributions
		fn_oN(0,      15) ! 280-294 Total Nontaxable Combat Pay (Code Q)
		fn_oN(0,      15) ! 295-309 Total Cost of Employer-Sponsored Health Coverage (Code DD)
		fn_oN(0,      15) ! 310-324 Total Employer Cost of Premiums for Group Term Life Insurance Over $50,000 (Code C)
		fn_oN(0,      15) ! 325-339 Total Income Tax Withheld by Payer of Third-Party Sick Pay
		fn_oN(0,      15) ! 340-354 Total Income from the Exercise of Nonstatutory Stock Options (Code V)
		fn_oN(0,      15) ! 355-369 Total Deferrals Under a Section 409A Nonqualified Deferred Compensation Plan (Code Y)
		fn_oN(0,      15) ! 370-384 Total Designated Roth Contributions to a Section 401(k) Plan (Code AA)
		fn_oN(0,      15) ! 385-399 Total Designated Roth Contributions Under a Section 403(b) Salary Reduction Agreement (Code BB)
		fn_oN(0,      15) ! 400-414 Total Permitted Benefits Under a Qualified Small Employer Health Reimbursement Arrangement (Code FF)
		fn_oC('',     98) ! 415-512 Blank
		fn_oOut
	return ! /r
	! • RU (Total Optional) Record – Optional
	! • RV (State Total) Record – Optional
	Efw2_Rf: ! r: • RF (Final) Record – Required
		fn_oC('RF'       	,  2)
		fn_oC(''          	,  5) !  3-7   Blank
		fn_oN(printCount 	,  9) !  8-16  Number of RW Records
		fn_oC(''          	,496) ! 17-512 Blank
		fn_oOut
	return ! /r
	Efw2_Ra:  ! r: • RA (Submitter) Record – Required
		fn_oC('RA',2)
		fn_oC(ein$   	, 9) !  3-11 Submitter's Employer Identification Number (EIN)
		fn_oC(bsoUser$ 	, 8) ! 12-19 User Identification (User ID    ! XXX TODO: Ask this on the screen
		fn_oC(''        	, 4) ! 20-23 Software Vendor Code
		fn_oC(''        	, 5) ! 24-28 Blanks
		fn_oC('0'       	, 1) ! 30-35 Resubmission Indicator 1 Enter "1" if this file is being resubmitted. Otherwise, enter “0” (zero).
		fn_oC(''        	, 6) ! 30-35 Resub Wage File Identifier (WFID)
		fn_oC('98'      	, 2) ! 36-37 Software Code 2 Enter one of the following codes to indicate the software used to create your file:   • 98 = In-House Program   • 99 = Off-the-Shelf Software
		! if len(line$)<>37 then pr 'wrong length len(line$)=';len(line$) : pause
		fn_oC(a$(1)     	,57) ! 38-94 Company Name 57 Enter the company name. Left justify and fill with blanks.
		fn_oC(''        	,22) ! location address
		fn_oC(a$(2)     	,22) ! delivery address
		fn_oC(aCity$   	,22) !
		fn_oC(aSt$      	, 2) !
		fn_oC(aZip$    	, 5) !
		fn_oC(''        	, 4) ! zip extension
		fn_oC(''        	, 5) ! 172-176 Blank 5 Fill with blanks. Reserved for SSA use.
		! if len(line$)<>176 then pr 'wrong length len(line$)=';len(line$) : pause
		fn_oC('', 23) ! 177-199 Foreign State/Province
		fn_oC('', 15) ! 200-214 Foreign Postal Code
		fn_oC('',  2) ! 215-216 Country Code
		fn_oC('', 57) ! 217-273 Submitter Name This is a required field. Enter the name of the organization to receive error notification if this file cannot be processed.
		! if len(line$)<>273 then pr 'wrong length len(line$)=';len(line$) : pause
		fn_oC('', 22) ! 274-295 Location Address
		fn_oC('', 22) ! 296-317 Delivery Address
		fn_oC('', 22) ! 318-339 City
		fn_oC('',  2) ! 340-341 State Abbreviation
		fn_oC('',  5) ! 342-346 ZIP Code 5 This is a required field. Enter the submitter's ZIP code. For a foreign address, fill with blanks.
		fn_oC('',  4) ! 347-350 ZIP Code Extension
		if len(line$)<>350 then pr 'wrong length len(line$)=';len(line$) : pause
		fn_oC('',  5) ! 351-355 Blank
		fn_oC('', 23) ! 356-378 Foreign State/Province
		fn_oC('', 15) ! 379-393 Foreign Postal Code
		fn_oC('',  2) ! 394-395 Country Code
		fn_oC(submitterName$, 27) ! 396-422 Contact Name
		fn_oC(submitterPhone$, 15) ! 423-437 Contact Phone Number
		fn_oC(submitterPhExt$,  5) ! 438-442 Contact Phone Extension
		fn_oC('',  3) ! 443-445 Blank
		if len(line$)<>445 then pr 'wrong length len(line$)=';len(line$) : pause
		fn_oC(submitterEmail$, 40) ! 446-485 Contact E-Mail/Internet
		fn_oC('',  3) ! 486-488 Blank
		fn_oC(submitterFax$, 10) ! 489-498 Contact Fax
		fn_oC('',  1) ! 499 Blank
		fn_oC('',  1) ! 500 Preparer Code
		fn_oC('', 12) ! 501-512 Blank
		fn_oOut
	return ! /r
	Efw2_Re: ! r: • RE (Employer) Record –Required
		fn_oC('RE',2)
		fn_oC(taxYear$,4) ! 3-6 Tax Year
		fn_oC('',  1) !       7 Agent Indicator Code
		fn_oC(ein$,  9) !    8-16 Employer /Agent Identification Number (EIN)
		fn_oC('',  9) !   17-25 Agent for EIN
		fn_oC('',  1) !      26 Terminating Business Indicator
		fn_oC('',  4) !   27-30 Establishment Number
		fn_oC('',  9) !   31-39 Other EIN
		fn_oC(a$(1), 57) !   40-96  Employer Name
		fn_oC('', 22) !  97-118 Location Address
		fn_oC(a$(2), 22) ! 119-140 Delivery Address
		fn_oC(aCity$, 22) ! 141-162
		fn_oC(aSt$,  2) ! 163-164
		fn_oC(aip$,  5) ! 165-169
		fn_oC('',  4) ! 170-173
		fn_oC('N', 1) !     174 Kind of Employer
														! This is a required field.
														! Enter the appropriate kind of employer:
														! F = Federal govt.
														! (Federal government entity or instrumentality)
														! S = State/local non-501c.
														! (State or local government or instrumentality (this includes cities, townships, counties, special-purpose districts or other publicly-owned entities with governmental authority))
														! T = 501c non-govt.
														! (Non-governmental tax-exempt Section 501(c) organization (types of 501(c) non-governmental organizations include private foundations, public charities, social and recreation clubs and veterans’ organizations))
														! Y = State/local 501c.
														! (State or local government or instrumentality where the employer received a determination letter from the IRS indication that they are also a tax-exempt organization under Section 501(c)(3))
														! N = None Apply
		fn_oC('',  4) ! 175-178 Blank
		fn_oC('', 23) ! 179-201 Foreigh State/Province
		fn_oC('', 15) ! 202-216 Foreign Postal Code
		fn_oC('',  2) ! 217-218 Country Code
		fn_oC('',  1) !     219 Employment Code
													! This is a required field.
													! Enter the appropriate employment code:
													! A = Agriculture  Form 943
													! H = Household Schedule H
													! M = Military Form 941
													! Q = Medicare Qualified Government Employment Form 941
													! X = Railroad CT-1
													! F = Regular Form 944
													! R = Regular (all others) Form 941
													! If the Tax Jurisdiction Code in position 220 of the RE (Employer) Record is blank (domestic), reporting Employment Code ‘Q’ (MQGE) is valid for tax year 1983 through the current tax year.
													! If the Tax Jurisdiction Code in position 220 of the RE (Employer) Record is P, V, G, S, or N (not domestic), reporting Employment Code ‘Q’ (MQGE) is valid for tax years 1986 through the current tax year.
													! Note: Railroad reporting is not applicable for Puerto Rico and territorial employers.
		fn_oC('',  1) ! 220 Tax Jurisdiction Code
													! This is a required field.
													! Enter the code that identifies the type of income tax withheld from the employee’s earnings.
													! Blank =              W-2
													! V      = Virgin Islands               W-2VI
													! G      = Guam                          W-2GU
													! S      = American Samoa               W-2AS
													! N      = Northern Mariana Islands    W-2CM
													! P      = Puerto Rico           W-2PR/499R-2
		fn_oC('',  1) ! Third-Party Sick Pay Indicator
		fn_oC(contactName$, 27) ! Employer Contact Name
		fn_oC(contactPhone$, 15) ! Employer Contact Phone Number
		fn_oC(contactPhExt$,  5) ! Employer Contact Phone Extension
		fn_oC(contactFax$, 10) ! Employer Contact Fax Number
		fn_oC(contactEmail$, 40) ! Employer Contact E-Mail/Internet
		fn_oC('',194) ! Blank
		fn_oOut
	return ! /r

def library fnFormCopyAwithBackgroundWarn
	if setup$<>env$('cursys')&env$('cno') then fn_setup
	fnFormCopyAwithBackgroundWarn=fn_FormCopyAwithBackgroundWarn
fnend
def fn_FormCopyAwithBackgroundWarn
	! if ~fcawbwSetup then
	! 	fcawbwSetup=1
	! 	dim fcawbwText$(0)*128
	! 	fnAddOneC(mat fcawbwText$,'The IRS Warns you should not pr Copy A with the background.')
	! 	fnAddOneC(mat fcawbwText$,'This form is provided for informational purposes only. Copy A appears')
	! 	fnAddOneC(mat fcawbwText$,'in red, similar to the official IRS form. The official printed version')
	! 	fnAddOneC(mat fcawbwText$,'of this IRS form is scannable, but the online version of it, printed')
	! 	fnAddOneC(mat fcawbwText$,'from this website, is not. Do not pr and file this Copy A.')
	! 	fnAddOneC(mat fcawbwText$,'A penalty may be imposed for filing forms that can''t be scanned.')
	! 	fnAddOneC(mat fcawbwText$,'')
	! 	fnAddOneC(mat fcawbwText$,'To order official IRS information returns such as Forms W-2 and W-3,')
	! 	fnAddOneC(mat fcawbwText$,'which include a scannable Copy A for filing, visit www.irs.gov/orderforms')
	! 	fnAddOneC(mat fcawbwText$,'and click on Employer and Information returns.')
	! 	fnAddOneC(mat fcawbwText$,'The IRS will mail you the scannable forms and any other products you order.')
	! end if
	! if ~fcawbwToldAlready then
	! 	fcawbwToldAlready=1
	! 	fnMsgBox(mat fcawbwText$,response$,'Notification')
	! end if
fnend


def library fnW2PrintClose
	fnW2PrintClose=fn_w2PrintClose
fnend
def fn_w2PrintClose
	! inherrits local: printCount
	if printCount then
		if w2destinationOpt$(3)='True' then
			gosub Efw2_Rt
			gosub Efw2_Rf
			fn_reportFileCreated(hO)
			cntRw=0
			mat tot=(0)
		else if w2destinationOpt$(2)='True' then
			fn_reportFileCreated(hExport)
		else
			fnpa_finis
			! pr 'fnpa_finis for w2' : pause
		end if
		w2Initialized=0
		if enableW3$='True' and w2destinationOpt$(1)='True' then fnW3(taxyear$,printCount,mat wTotal,dcb,state$,stcode$)
		printCount=0
		mat wTotal=(0)
	else
		mat msgTxt$(1)
		msgTxt$(1)='Nothing created.'
		fnMsgBox(mat msgTxt$)
	end if
fnend
	def fn_reportFileCreated(&h)
		dim msgTxt$(0)*256
		mat msgTxt$(2)
		msgTxt$(2)=os_filename$(file$(h))
		close #h:
		h=0
		msgTxt$(1)='Created file:'
		fnMsgBox(mat msgTxt$)
	fnend


include: ertn