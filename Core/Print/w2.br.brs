! r: testing zone
fn_setup
library program$: fnNameParse
nameFormat$=optNameFormat$(2)
fncreg_write('Employee Name Format',nameFormat$)
dim n1$*64,n2$*64,n3$*64
pr 'name format: "'&nameFormat$&'"'
dim nFull$(4)*256
nFull$(1)='JOINER,JR JOHNNY C.'
nFull$(2)='CRUZ,  BRYAN EDUARDO DEL'
nFull$(3)='SAN NICOLAS, CHRISTOPHER'
nFull$(4)='GARCIA,JR JESUS'
for nFullItem=1 to udim(mat nFull$)
	fnNameParse(nFull$(nFullItem),n1$,n2$,n3$,n4$)
	pr 'source name: "'&nFull$(nFullItem)&'"'
	pr '     first: "'&n1$&'"'
	pr '    middle: "'&n2$&'"'
	pr '      last: "'&n3$&'"'
	pr '    suffix: "'&n4$&'"'
nex nFullItem
end
! /r
def library fnask_w2_info(&taxYear$,&beg_date,&end_date,&empStart$,&empEnd$,&ssrate,&ssmax,&mcrate,&mcmax,mat w2destinationOpt$,&enableW3$,&enableBackground$,&w2Copy,&w2Copy$,&exportFormatID,&w2laser_output_filename$,&pn1,&dc1,&topmargin,&bottom,&state$,enableAskCLocality,&cLocality$)
	if ~awi_setup then ! r:
		awi_setup=1
		if ~setup then let fn_setup
		! r: constants
		dim resp$(128)*256
		dim optW2Copy$(6)*68
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
		empStart$='[All]'
		empEnd$='[All]'
		ssrate=.062
		if taxYear$='2016' then
			ssmax=118500 ! 2016
		else ! if taxYear$='2017' then
				ssmax=127200 ! 2017
		end if
		mcrate=.0145
		mcmax=999999
		fncreg_read('W-2 - cLocality',cLocality$,'NO')
		fnreg_read('Print W-2'                          ,w2destinationOpt$(1),'True' )
		fnreg_read('Export for Advanced Micro Solutions',w2destinationOpt$(2),'False')
		! fnreg_read('Export for Center Piece Software'   ,w2destinationOpt$(3),'False')  ! removed access 01/03/2017
		fnreg_read('Print W-3 also'                     ,enableW3$           ,'True' )
		fnreg_read('W-2 - Enable Background'            ,enableBackground$   ,'True' )
		fncreg_read('W-2 - Copy Current',w2Copy$,optW2Copy$(1)) : w2Copy=srch(mat optW2Copy$,w2Copy$) : if w2Copy<=0 then w2Copy=1
		fncreg_read('Employee Name Format',nameFormat$,optNameFormat$(1))
		w2Copy$=optW2Copy$(w2Copy)
		fnureg_read('W-2 - Export Filename',w2laser_output_filename$,os_filename$(env$('Desktop')&'\ACS [TaxYear] W-2 Export (Company [CompanyNumber]).txt'))
		fncreg_read('Qualified Pension Plan' ,tmp$) : pn1=val(tmp$)
		fncreg_read('Dependent Care Benefits',tmp$) : dc1=val(tmp$)
		fncreg_read('W-2 - State',state$)
		!
		fnreg_read('W-2 - Form 1 Y'              ,tmp$   ,'10' ) : topmargin=val(tmp$)
		fnreg_read('W-2 - Form 2 Y'              ,tmp$   ,'151') : bottom=val(tmp$)
		enableDateRange=0
		disableSSMCedit=0
		enableEmpRange=0
		! /r
	end if ! /r
	awiReturn=0 
	if env$('cursys')='PR' then
		enablePayrollDeductions=1
		enableAskState=0
		cLlocalityToolTip$="If you have answered that you have local withholdings in the company information file, you must enter the locality name"
	else if env$('cursys')='GL' then
		enablePayrollDeductions=0
		enableAskState=1
		cLlocalityToolTip$=""
	else
		pr 'not configured for anything but GL and PR yet.'
		pause
	end if
	ASK_INFO: !
	! r: build and display the ASK_INFO screen
	fnTos(sn$="Prw2-2")
	rc=cf=0: mylen=21: mypos=mylen+2 
	if enableAskCLocality then 
		fraWidth=94
	else
		fraWidth=82 ! 68
	end if
	!
	fraGeneralHeight=2
	if enableDateRange then fraGeneralHeight+=2
	if enableEmpRange then fraGeneralHeight+=3
	if enableAskCLocality then fraGeneralHeight+=5
	fnFra(1,1,fraGeneralHeight,fraWidth,"General","Normally this would the first and last day of the calendar year",0)
	cf+=1 : franum=cf : lc=0
	fnLbl(lc+=1,1,"Tax Year:",mylen,1,0,franum)
	fnTxt(lc,mypos,4,0,1,"",1,"Year to pr W-2s for",franum)
	resp$(resc_taxYear:=rc+=1)=taxYear$
	if enableDateRange then
		fnLbl(lc+=1,1,"Starting Date:",mylen,1,0,franum)
		fnTxt(lc,mypos,10,0,1,"3",0,"First day of calendar year",franum)
		resp$(respc_startdate:=rc+=1)='0101'&date$(days(date$)-180,'YY')
		fnLbl(lc+=1,1,"Ending Date:",mylen,1,0,franum)
		fnTxt(lc,mypos,10,0,1,"3",0,"Last day of calendar year",franum)
		resp$(respc_enddate:=rc+=1)='1231'&date$(days(date$)-180,'YY')
		lc+=1
	end if
	if enableEmpRange then
		fnLbl(lc+=1,1,"Starting Employee:",mylen,1,0,franum)
		fncmbemp(lc,mypos,1,franum)
		resp$(respc_empStart:=rc+=1)=""
		fnLbl(lc+=1,1,"Ending Employee:",mylen,1,0,franum)
		fncmbemp(lc,mypos,1,franum)
		resp$(respc_empEnd:=rc+=1)=""
		lc+=1
	end if
	if enableAskCLocality then
		fnLbl(lc+=1,1,"Locality Name:",mylen,1,0,franum)
		fnTxt(lc,mypos,12,0,1,"",0,cLocalityToolTip$,franum)
		resp$(resp_cLocality:=rc+=1)=cLocality$
		fnLbl(lc   ,mypos+12+2,"Enter the locality name if the same on all employees.",57,0,0,franum)
		fnLbl(lc+=1,mypos+12+2,"Enter NO (or blank) if it is not applicable.",57,0,0,franum)
		fnLbl(lc+=1,mypos+12+2,"Enter YES if applicable, but not he same on all employees",57,0,0,franum)
		lc+=1
	end if
	fnLbl(lc+=1,1,"Employee Name Format:",mylen,1,0,franum)
	fncomboa('nameFormat',lc,mypos,mat optNameFormat$, '',20,franum)
	resp$(resp_namcde:=rc+=1)=nameFormat$
	!
	fra2Height=5 : fra2Y=fraGeneralHeight+3
	fnFra(fra2Y,1,fra2Height,fraWidth,"Print W-2s","",0)
	cf+=1 : franum=cf : lc=0
	mylen=46: mypos=mylen+2
	fnLbl(1,1,"Social Security Withholding Rate:",mylen,1,0,franum)
	fnTxt(1,mypos,10,0,1,"34",disableSSMCedit,"Use format such as .062.",franum)
	resp$(respc_ssrate:=rc+=1)=str$(ssrate)
	fnLbl(2,1,"Maximum Wage Subject to SS Withholdings:",mylen,1,0,franum)
	fnTxt(2,mypos,10,0,1,"10",disableSSMCedit,"Enter the maximum wage subject to social security withholdings for the current year just ended.",franum)
	resp$(respc_ssmax:=rc+=1)=str$(ssmax)
	fnLbl(4,1,"Medicare Withholding Rate:",mylen,1,0,franum)
	fnTxt(4,mypos,10,0,1,"34",disableSSMCedit,"Use format such as .0145 .",franum)
	resp$(respc_mcrate:=rc+=1)=str$(mcrate)
	fnLbl(5,1,"Maximum Wage Subject to Medicare Withholdings:",mylen,1,0,franum)
	fnTxt(5,mypos,10,0,1,"10",disableSSMCedit,"At the present time there is no maximum.  Enter a number larger than any one's wages can be. For example, 999999.00",franum)
	resp$(respc_mcmax:=rc+=1)=str$(mcmax)
	!
	fra3Y=fra2Y+fra2Height+2 : fra3Height=6
	fnFra(fra3Y,1,fra3Height,fraWidth,"Printing or Exporting","You have the option to either pr the W-2s or export them to another system for printing.")
	cf+=1 : franum=cf : mylen=26 : mypos=mylen+2
	fnOpt(1,3,"Print W-2",0,franum)
	resp$(respc_PrintW2:=rc+=1)=w2destinationOpt$(1)
	fnLbl(1,fraWidth-50,"(2 per page is not yet available with Backgrounds)",50,1,0,franum)
	fnLbl(2,5,"Copy:",12,1,0,franum)
	fncomboa('w2Copy',2,19,mat optW2Copy$, '',20,franum)
	resp$(respc_w2copy:=rc+=1)=w2Copy$
	fnChk(2,68,'W-2 - Enable Background',1,franum)
	resp$(respc_enableBackground:=rc+=1)=enableBackground$
	! fnChk(3,68,'2 Per Page',1,franum)
	fnChk(4,68,'Print W-3 also',1,franum)
	resp$(respc_w3:=rc+=1)=enableW3$
	fnOpt(4,3,"Export for Advanced Micro Solutions",0,franum)
	resp$(respc_export_ams:=rc+=1)=w2destinationOpt$(2)
	! fnOpt(5,3,"Export for Center Piece Software",0,franum)  ! removed access 01/03/2017
	! resp$(respc_export_cps:=rc+=1)=w2destinationOpt$(3)  ! removed access 01/03/2017
	fnLbl(5,5,"Export File:",12,1,0,franum)
	fnTxt(5,19,20,80,0,'72',0,'Choose a destination location for the ACS export.',franum)
	resp$(resp_w2_export_file:=rc+=1)=w2laser_output_filename$
	fnButton(5,5+12+20+5,'Default',14,'Choose to set the default for the selected destination software.',0,0,franum)
	fnLbl(6,19,"([CompanyNumber] and [TaxYear] will be substituted in filename)",0,0,0,franum)
	!
	if enablePayrollDeductions then
		fra4Y=fra3y+fra3Height+2 ! 25
		fnFra(fra4Y,1,2,fraWidth,"Identify the Following Deductions","You have twenty miscellaneous deductions available to you. If you have Qualified Pension or Dependent Care, start with the first deduction and count down to identify the number of the deduction.")
		cf+=1 : franum=cf
		fnLbl(1,1,"Qualified Pension Plan:",mylen,1,0,franum)
		fnTxt(1,mypos,2,0,1,"30",0,"If you have a qualified pension plan that requires the pension plan box to be checked, count down from your 1st miscellaneous deduction to determine the number to enter here.",franum)
		resp$(respc_qpenplan:=rc+=1)=str$(pn1)
		fnLbl(2,1,"Dependent Care Benefits:",mylen,1,0,franum)
		fnTxt(2,mypos,2,0,1,"30",0,"If you have dependent care benefits that should be identifies on the W-2, count down from your 1st miscellaneous deduction to determine the number to enter here.",franum)
		resp$(respc_depCareBen:=rc+=1)=str$(dc1)
	else if enableAskState then
		fra4Y=fra3y+fra3Height+2 ! 25
		fnFra(fra4Y,1,2,fraWidth,"State","")
		cf+=1 : franum=cf
		fnLbl(1,1,"State Name:",mylen,1,0,franum)
		fnTxt(1,mypos,2,0,1,"",0,"If you have a qualified pension plan that requires the pension plan box to be checked, count down from your 1st miscellaneous deduction to determine the number to enter here.",franum)
		resp$(respc_state:=rc+=1)=state$
	end if
	fnCmdKey("&Margins",ckey_margins:=1021,0,0,"Manually adjust margins for hitting forms")
	fnCmdKey("&Next",1,1,0,"Proceed to next screen.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu")
	fnAcs(sn$,0,mat resp$,ckey)
	! /r
	! r: ASK_INFO screen - respond to FKeys, and get local values from mat resp$
	if ckey=5 then 
		awiReturn=0 
	else 
		awiReturn=1
		taxYear$=resp$(resc_taxYear)
		nameFormat$=resp$(resp_namcde)
		if enableDateRange then
			beg_date=val(resp$(respc_startdate))
			end_date=val(resp$(respc_enddate))
		else
			beg_date=val(taxYear$&'0101')
			end_date=val(taxYear$&'1231')
		end if
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
		if resp$(respc_PrintW2)="True" then 
			exportFormatID=0
		else if resp$(respc_export_ams)="True" then 
			exportFormatID=1
		! else if resp$(respc_export_cps)="True" then 
		!   exportFormatID=2
		end if
		w2destinationOpt$(1)=resp$(respc_PrintW2)
		w2destinationOpt$(2)=resp$(respc_export_ams)
		!   w2destinationOpt$(3)=resp$(respc_export_cps)  ! removed access 01/03/2017
		w2laser_output_filename$=resp$(resp_w2_export_file)
		if enablePayrollDeductions then
			pn1=val(resp$(respc_qpenplan))
			dc1=val(resp$(respc_depCareBen))
		else if enableAskState then
			state$=resp$(respc_state)
		end if
		if ckey=14 then 
			!     if exportFormatID=1
			!       w2laser_output_filename$=os_filename$("\1099ETC.W"&date$(days(date$)-180,'YY')&"\W2DATA\W2DAT.PRN")
			!     else if exportFormatID=2 then  ! removed access 01/03/2017
			!       w2laser_output_filename$=os_filename$("\CPS04\ASCIIW2.TXT")  ! removed access 01/03/2017
			!     else 
				w2laser_output_filename$=os_filename$(env$('Desktop')&'\ACS [TaxYear] W-2 Export (Company [CompanyNumber]).txt')
			!     end if 
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
		!   fnreg_write('Export for Center Piece Software'   ,w2destinationOpt$(3))  ! removed access 01/03/2017
		fnureg_write('W-2 - Export Filename',w2laser_output_filename$)
		fncreg_write('Qualified Pension Plan',str$(pn1))
		fncreg_write('Dependent Care Benefits',str$(dc1))
		fncreg_write('Employee Name Format',nameFormat$)
		fncreg_write('W-2 - State',state$)
		!
		! /r
		if w2Copy$=optW2Copy$(1) and enableBackground$='True' then let fn_FormCopyAwithBackgroundWarn
	end if 
	XIT: !
	fnask_w2_info=awiReturn
fnend
def fn_setup
	if ~setup then
		setup=1
		dim w2laser_output_filename$*256
		library 'S:\Core\Library': fnTos,fnFra,fnLbl,fnTxt,fnCmdKey,fnAcs,fnOpt
		library 'S:\Core\Library': fnmsgbox
		library 'S:\Core\Library': fnChk,fncmbemp,fnpa_finis
		library 'S:\Core\Library': fnButton,fnCmdSet
		library 'S:\Core\Library': fnpa_fontsize,fnpa_txt
		library 'S:\Core\Library': fnreg_read,fnreg_write
		library 'S:\Core\Library': fnureg_read,fnureg_write
		library 'S:\Core\Library': fncreg_read,fncreg_write
		library 'S:\Core\Library': fngethandle
		library 'S:\Core\Library': fncomboa,fnAddOneC
		on error goto ERTN
		dim optNameFormat$(2)*20,nameFormat$*20
		optNameFormat$(1)='First Name First'
		optNameFormat$(2)='Last Name First'
	end if
fnend
def fn_ask_margins
! if env$('acsdeveloper')='' then pr bell; : goto am_xit
	fnreg_read('W-2 - Form 1 Y',amResp$(1),'10' )
	fnreg_read('W-2 - Form 2 Y',amResp$(2),'151')
	fnreg_read('W-2 - X'       ,amResp$(3),'12' )
	fnTos(sn$='w2_ask_margins')
	mylen=30 : mypos=mylen+2
	fnLbl(lc+=1,1,"Form 1 Distance from Top (mm):",mylen,1)
	fnTxt(lc,mypos,3,0,1,'30')
	fnLbl(lc+=1,1,"Form 2 Distance from Top (mm):",mylen,1)
	fnTxt(lc,mypos,3,0,1,'30')
	fnLbl(lc+=1,1,"Left Margin Size (mm):",mylen,1)
	fnTxt(lc,mypos,3,0,1,'30')
	fnCmdSet(4)
	fnAcs(sn$,0,mat amResp$,ckey)
	if ckey<>5 then
		fnreg_write('W-2 - Form 1 Y' ,amResp$(1))
		fnreg_write('W-2 - Form 2 Y' ,amResp$(2))
		fnreg_write('W-2 - X'        ,amResp$(3))
		topmargin= val(amResp$(1))
		bottom=    val(amResp$(2))
		left=      val(amResp$(3))
	end if
fnend
def library fnNameParse(fullname$*128,&nameFirst$,&nameMiddle$,&nameLast$,&nameSuffix$)
	if ~setup then let fn_setup
	! passed in: fullname$
	! gathered: nameFormat$, mat optNameFormat$
	! returns: nameFirst$,nameMiddle$,nameLast$
	! calling program should include:  dim nameFirst$*15,nameMiddle$*15,nameLast$*20
	fncreg_read('Employee Name Format',nameFormat$,optNameFormat$(1))
	fullname$=uprc$(rtrm$(fullname$)): ! nameFormat$="s"
	npPosSpace1=pos(fullname$," ",1)
	npPosSpace2=pos(fullname$," ",npPosSpace1+1)
	if nameFormat$=optNameFormat$(1) then 
		! r: first name first
		nameFirst$=fullname$(1:max(min(15,npPosSpace1-1),1))
		if npPosSpace2>0 then 
			nameMiddle$=fullname$(npPosSpace1+1:npPosSpace2-1)
			nameLast$=fullname$(npPosSpace2+1:len(fullname$))
		end if
		if npPosSpace2=0 then
			nameLast$=fullname$(npPosSpace1+1:len(fullname$))
			nameMiddle$=""
		end if
		! /r
	else  ! last name first
		! r: last name first
		! npPosComma=pos(fullname$,',')
		! if npPosComma then
		!   ! r: last name, first name
		!   dim fullNameCopy$*256
		!   fullNameCopy$=fullname$
		!   nameLast$=fullNameCopy$(1:npPosComma-1)
		!   fullNameCopy$(1:npPosComma)=''
		!   fullNameCopy$=trim$(fullNameCopy$)
		!   npFncPosSpace1=pos(fullNameCopy$,' ')
		!   if npFncPosSpace1<=0 then npFncPosSpace1=len(fullNameCopy$)
		!   nameFirst$=trim$(fullNameCopy$(1:npFncPosSpace1))
		!   fullNameCopy$(1:npFncPosSpace1)=''
		!   nameMiddle$=trim$(fullNameCopy$)
		!   fullNameCopy$=''
		!   ! /r
		! else
		!   ! r: last name [space] first name
		!   ! /r
		! ! end if
		if npPosSpace1>0 and fullname$(npPosSpace1-1:npPosSpace1-1)="," then 
			nameLast$=fullname$(1:npPosSpace1-2) 
		else 
			nameLast$=fullname$(1:max(npPosSpace1-1,1))
		end if
		if npPosSpace2>0 then 
			nameFirst$=fullname$(npPosSpace1+1:npPosSpace2-1): nameMiddle$=fullname$(npPosSpace2+1:len(fullname$))
		else ! if npPosSpace2=0 then 
			nameFirst$=fullname$(npPosSpace1+1:len(fullname$)): nameMiddle$=""
		end if
		! /r
	end if
	nameFirst$=rtrm$(nameFirst$,',')
	! r: nameSuffix$ process
	nameSuffix$='' err npNsFinis
	if uprc$(nameFirst$&'*')<>uprc$(srep$(nameFirst$&'*',',JR*','')) then
		nameSuffix$='JR'
		nameFirst$=rtrm$(nameFirst$) 
		nameFirst$=nameFirst$(1:len(nameFirst$)-3)
	end if
	if uprc$(nameLast$&'*')<>uprc$(srep$(nameLast$&'*',',JR*','')) then
		nameSuffix$='JR'
		nameLast$=rtrm$(nameLast$) 
		nameLast$=nameLast$(1:len(nameLast$)-3)
	end if
	if uprc$(nameLast$&'*')<>uprc$(srep$(nameLast$&'*',',JR.*','')) then
		nameSuffix$='JR'
		nameLast$=rtrm$(nameLast$) 
		nameLast$=nameLast$(1:len(nameLast$)-4)
	end if
	npNsFinis: !
	! /r
	! pr nameFirst$,nameMiddle$,nameLast$
fnend
def library fnw2_text(w2Yoffset,maskSsn,mat a$,empId$*12,ss$,controlNumber$,mat w,dcb$,nameFirst$*64,nameMiddle$*64,nameLast$*64,nameSuffix$*64,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$*6; w2Box14Amt,specialform2018)
	if ~setup then let fn_setup
	! r: variable definations
	! topmargin       how far down the page (mm) is the top of W-2
	! maskSsn         if 1 than turn all but the last 4 of the SSN into *s
	! ss$             social security number (with dashes)
	! mat a$          company name and address
	! controlNumber$  control number
	! 
	! box12aCode$     box 12a code
	! box12aAmt$      box 12a amount
	!               
	! box12bCode$     box 12b code
	! box12bAmt$      box 12b amount
	!               
	! box12cCode$     box 12c code
	! box12cAmt$      box 12c amount
	!               
	! box12dCode$     box 12d code
	! box12dAmt$      box 12d amount
	! 
	! w(4)            EIC
	! /r
	if ~w2setup then
		w2setup=1
		fnreg_read('W-2 - X',tmp$   ,'12' ) : left=val(tmp$)
		fncreg_read('W-2 - cLocality',cLocality$,'NO')
		w2Col1=left     
		w2Col2=left+117 
		w2Col3=left+160 
		w2Box12CodePos=w2Col3-14
		w2Box12AmtPos=w2Box12CodePos+18
		w2Box14AmtPos=w2Box12AmtPos
	end if
	fnpa_fontsize
	if maskSsn then
		fnpa_txt('***-**-'&ss$(8:11),left+44,fn_line(1))
	else
		fnpa_txt(ss$,left+44,fn_line(1))
	end if
	! if env$('acsdeveloper')<>'' then let specialform2018=1
	if env$('client')='Thomasboro' or env$('client')="Cerro Gordo" or env$('client')="Cerro Gordo T" or env$('client')="Kincaid" or env$('client')="Hope Welty" or env$('client')="Bethany" then let specialform2018=1
	fnpa_txt(empId$,w2Col1,fn_line(2))
	fnpa_txt(cnvrt$("pic(--,---,---.##",w(2)),w2Col2,fn_line(2))
	fnpa_txt(cnvrt$("pic(--,---,---.##",w(1)),w2Col3,fn_line(2))
	fnpa_txt(a$(1),w2Col1,fn_line(3))
	fnpa_txt(cnvrt$("pic(--,---,---.##",w(5)),w2Col2,fn_line(3))
	fnpa_txt(cnvrt$("pic(--,---,---.##",w(3)),w2Col3,fn_line(3))
	fnpa_txt(a$(2),w2Col1,fn_line(4))
	fnpa_txt(cnvrt$("pic(--,---,---.##",w(11)),w2Col2,fn_line(4))
	fnpa_txt(cnvrt$("pic(--,---,---.##",w(12)),w2Col3,fn_line(4))
	fnpa_txt(a$(3),w2Col1,fn_line(5))
	fnpa_txt(cnvrt$("pic(--,---,---.##",w(6)),w2Col2,fn_line(5))
	fnpa_txt(controlNumber$,w2Col1,fn_line(6))
	! fnpa_txt(cnvrt$("pic(--,---,---.##",w(4)),w2Col2,fn_line(6)) ! EIC - no longer reported - just greyed out
	fnpa_txt(cnvrt$("pic(--,---,---.##",dcb),w2Col3,fn_line(6))
	fnpa_txt((rtrm$(nameFirst$)&" "&rtrm$(nameMiddle$))(1:17),w2Col1,fn_line(7))
	fnpa_txt(rtrm$(nameLast$),left+40,fn_line(7))
	fnpa_txt(rtrm$(nameSuffix$),left+92,fn_line(7))
	fnpa_txt(box12aCode$,w2Box12CodePos,fn_line(7))
	fnpa_txt(box12aAmt$,w2Box12AmtPos,fn_line(7))
	if specialform2018<>1 then 
		fnpa_txt(k$(2),w2Col1,fn_line(8))
		fnpa_txt(retirementPlanX$,left+118,fn_line(8))
		fnpa_txt(box12bCode$,w2Box12CodePos,fn_line(8))
		fnpa_txt(box12bAmt$,w2Box12AmtPos,fn_line(8))
		fnpa_txt(k$(3),w2Col1,fn_line(9))
		fnpa_txt(box12cCode$,w2Box12CodePos,fn_line(9))
		fnpa_txt(box12cAmt$,w2Box12AmtPos,fn_line(9))
	else if specialform2018=1 then
		fnpa_txt(k$(2),w2Col1,fn_line(8))
		fnpa_txt(retirementPlanX$,left+119,fn_line(8)) 
		fnpa_txt(box12bCode$,w2Box12CodePos,fn_line(8))
		fnpa_txt(box12bAmt$,w2Box12AmtPos,fn_line(8))
		fnpa_txt(k$(3),w2Col1,fn_line(9))
		fnpa_txt(box12cCode$,w2Box12CodePos,fn_line(9))
		fnpa_txt(box12cAmt$,w2Box12AmtPos,fn_line(9))
	end if 
	fnpa_txt(box12dCode$,w2Box12CodePos,fn_line(10))
	fnpa_txt(box12dAmt$,w2Box12AmtPos,fn_line(10))
	if w2Box14Amt<>0 then
		fnpa_txt(cnvrt$("pic(-,---,---,---.##",w2Box14Amt),left+109,fn_line(10))
	end if
	if env$('client')<>'Zaleski' then ! cLocality$<>'NO' then
		if specialform2018=1 then 
			! thomasboro had special pre-printed forms for 2018 tax year
			fnpa_txt(state$,left-4,fn_line(12))
			fnpa_txt(stcode$,left+10,fn_line(12))
			fnpa_txt(cnvrt$("pic(-,---,---,---.##",w(9)),left+51,fn_line(12))
			fnpa_txt(cnvrt$("pic(-,---,---,---.##",w(7)),left+79,fn_line(12))
			fnpa_txt(cnvrt$("pic(-,---,---,---.##",w(10)),left+109,fn_line(12))
			fnpa_txt(cnvrt$("pic(-,---,---,---.##",w(8)),left+137,fn_line(12))
			fnpa_txt(printLocality$(1:6),left+164,fn_line(12))
		else 
			fnpa_txt(state$,left-3,fn_line(11))
			fnpa_txt(stcode$,left+10,fn_line(11))
			fnpa_txt(cnvrt$("pic(-,---,---,---.##",w(9)),left+51,fn_line(11))
			fnpa_txt(cnvrt$("pic(-,---,---,---.##",w(7)),left+79,fn_line(11))
			fnpa_txt(cnvrt$("pic(-,---,---,---.##",w(10)),left+109,fn_line(11))
			fnpa_txt(cnvrt$("pic(-,---,---,---.##",w(8)),left+137,fn_line(11))
			fnpa_txt(printLocality$(1:6),left+164,fn_line(11))
		end if 
	end if
fnend
def fn_line(lineNumber)
	lReturn=0
	if lineNumber=1 then 
		lReturn=w2Yoffset+1
	else  ! if lineNumber>=1 and lineNumber<=14 then
		lReturn=w2Yoffset+10+(8.5*(lineNumber-2))
		if specialform2018=1 and lineNumber=12 then
			lReturn+=.5
			goto Thomasboroskip
		else if specialform2018=1 and lineNumber=8 then 
			lReturn+=2
			goto Thomasboroskip
		end if 
		if lineNumber>=11 then 
			! if env$('client')='Edinburg' then 
			! 	lReturn+=7
			! else
				lReturn+=3.5
			! end if
		end if
	end if 
	Thomasboroskip: ! skip here for special pre-printed forms
	fn_line=lReturn
fnend
def library fnFormCopyAwithBackgroundWarn
	if ~setup then let fn_setup
	fnFormCopyAwithBackgroundWarn=fn_FormCopyAwithBackgroundWarn
fnend
def fn_FormCopyAwithBackgroundWarn
	if ~fcawbwSetup then
		fcawbwSetup=1
		dim fcawbwText$(0)*128
		fnAddOneC(mat fcawbwText$,'The IRS Warns you should not pr Copy A with the background.')
		fnAddOneC(mat fcawbwText$,'This form is provided for informational purposes only. Copy A appears')
		fnAddOneC(mat fcawbwText$,'in red, similar to the official IRS form. The official printed version')
		fnAddOneC(mat fcawbwText$,'of this IRS form is scannable, but the online version of it, printed')
		fnAddOneC(mat fcawbwText$,'from this website, is not. Do not pr and file this Copy A.')
		fnAddOneC(mat fcawbwText$,'A penalty may be imposed for filing forms that can’t be scanned.')
		fnAddOneC(mat fcawbwText$,'')
		fnAddOneC(mat fcawbwText$,'To order official IRS information returns such as Forms W-2 and W-3,')
		fnAddOneC(mat fcawbwText$,'which include a scannable Copy A for filing, visit www.irs.gov/orderforms')
		fnAddOneC(mat fcawbwText$,'and click on Employer and Information returns.')
		fnAddOneC(mat fcawbwText$,'The IRS will mail you the scannable forms and any other products you order.')
	end if
	if ~fcawbwToldAlready then
		fcawbwToldAlready=1
		fnmsgbox(mat fcawbwText$,response$,'Notification')
	end if
fnend
include: ertn