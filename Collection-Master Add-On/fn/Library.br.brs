def library fnPremierCardiologyImport(; sourceId$)
	library 'S:\Collection-Master Add-On\Import Premier Cardiology Spreadsheet claims from AllData.br': fnPremierCardiologyImport
	fnPremierCardiologyImport=fnPremierCardiologyImport( sourceId$)
fnend
def library fnClaimFolder$*128(fileno$*128)
	library 'S:\Collection-Master Add-On\fn\claimFolder.br': fnClaimFolder$
	fnClaimFolder$=fnClaimFolder$(fileno$)
fnend
def library fnsession_size_setup(; &session_rows,&session_cols)
	library 'S:\Collection-Master Add-On\fn\session_size_setup.br': fnsession_size_setup
	! library program$(1:pos(program$,'\',-1))&'session_size_setup.br': fnsession_size_setup
	fnsession_size_setup=fnsession_size_setup( session_rows,session_cols)
fnend
def library fnmulti_select(mat ms_selected$,mat ms_unselected$; cap$*80,mat ms_grid_heading$,mat ms_grid_width,mat ms_grid_form$,ms_rotation_default)
	library 'S:\Collection-Master Add-On\fn\multi_select.br': fnmulti_select
	! library program$(1:pos(program$,'\',-1))&'multi_select.br': fnmulti_select
	fnmulti_select=fnmulti_select(mat ms_selected$,mat ms_unselected$, cap$,mat ms_grid_heading$,mat ms_grid_width,mat ms_grid_form$,ms_rotation_default)
fnend
! r: mtomSoap
def library fnLoadExistingCase(caseNumber$,fileBarNumber$,hearingDate$*64,notificationEmail$*256; custMemo$*256,dssNumber$)
	library 'S:\Collection-Master Add-On\fn\mtomSoap.br': fnLoadExistingCase
	fnLoadExistingCase=fnLoadExistingCase(caseNumber$,fileBarNumber$,hearingDate$,notificationEmail$, custMemo$,dssNumber$)
fnend
def library fnListMunicipalities
	library 'S:\Collection-Master Add-On\fn\mtomSoap.br': fnListMunicipalities
	fnListMunicipalities=fnListMunicipalities
fnend
def library fnCreateNewFiling(court$,countyNumber$,caseType$,fileBarNumber$,notificationEmail$*256; custMemo$*256,dssNumber$)
	library 'S:\Collection-Master Add-On\fn\mtomSoap.br': fnCreateNewFiling
	fnCreateNewFiling=fnCreateNewFiling(court$,countyNumber$,caseType$,fileBarNumber$,notificationEmail$, custMemo$,dssNumber$)
fnend
def library fnAddParty(; filingId$,partyRole$,partyType$,firstName$,lastName$,middleInitial$,suffix$,gender$,businessName$,SSNFTIN$,aka1type$,aka1$,aka2type$,aka2$,address1$,address2$,city$,state$,zipCode$,homePhone$,dateOfBirth$,dateOfDeath$)
	library 'S:\Collection-Master Add-On\fn\mtomSoap.br': fnAddParty
	fnAddParty=fnAddParty( filingId$,partyRole$,partyType$,firstName$,lastName$,middleInitial$,suffix$,gender$,businessName$,SSNFTIN$,aka1type$,aka1$,aka2type$,aka2$,address1$,address2$,city$,state$,zipCode$,homePhone$,dateOfBirth$,dateOfDeath$)
fnend
! /r
def library fnCptCode$*800(code$*5)
	library 'S:\Collection-Master Add-On\cpt.br': fnCptCode$
	fnCptCode$=fnCptCode$(code$)
fnend
def library fnVal(stringToConvert$*128)
  fnVal=fn_val(stringToConvert$)
fnend 
def fn_val(stringToConvert$*128; ___,returnN)
	returnN=val(stringToConvert$) conv ValConv
	goto ValXit
	ValConv: ! 
		stringToConvert$=srep$(stringToConvert$,'$','')
		stringToConvert$=srep$(stringToConvert$,',','')
		stringToConvert$=srep$(stringToConvert$,'"','')
		stringToConvert$=trim$(stringToConvert$)
		returnN=val(stringToConvert$) conv ignore
	goto ValXit
	ValXit: !
	fn_val=returnN
fnend 

