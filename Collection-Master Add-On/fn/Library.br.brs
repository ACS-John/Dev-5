def library fnsession_size_setup(; &session_rows,&session_cols)
	library 'Collection-Master Add-On\fn\session_size_setup.br': fnsession_size_setup
	! library program$(1:pos(program$,'\',-1))&'session_size_setup.br': fnsession_size_setup
	fnsession_size_setup=fnsession_size_setup( session_rows,session_cols)
fnend
def library fnmulti_select(mat ms_selected$,mat ms_unselected$; cap$*80,mat ms_grid_heading$,mat ms_grid_width,mat ms_grid_form$,ms_rotation_default)
	library 'Collection-Master Add-On\fn\multi_select.br': fnmulti_select
	! library program$(1:pos(program$,'\',-1))&'multi_select.br': fnmulti_select
	fnmulti_select=fnmulti_select(mat ms_selected$,mat ms_unselected$, cap$,mat ms_grid_heading$,mat ms_grid_width,mat ms_grid_form$,ms_rotation_default)
fnend
! r: mtomSoap
def library fnLoadExistingCase(caseNumber$,fileBarNumber$,hearingDate$*64,notificationEmail$*256; custMemo$*256,dssNumber$)
	library 'Collection-Master Add-On\fn\mtomSoap.br': fnLoadExistingCase
	fnLoadExistingCase=fnLoadExistingCase(caseNumber$,fileBarNumber$,hearingDate$,notificationEmail$, custMemo$,dssNumber$)
fnend
def library fnListMunicipalities
	library 'Collection-Master Add-On\fn\mtomSoap.br': fnListMunicipalities
	fnListMunicipalities=fnListMunicipalities
fnend
def library fnCreateNewFiling(court$,countyNumber$,caseType$,fileBarNumber$,notificationEmail$*256; custMemo$*256,dssNumber$)
	library 'Collection-Master Add-On\fn\mtomSoap.br': fnCreateNewFiling
	fnCreateNewFiling=fnCreateNewFiling(court$,countyNumber$,caseType$,fileBarNumber$,notificationEmail$, custMemo$,dssNumber$)
fnend
def library fnAddParty(; filingId$,partyRole$,partyType$,firstName$,lastName$,middleInitial$,suffix$,gender$,businessName$,SSNFTIN$,aka1type$,aka1$,aka2type$,aka2$,address1$,address2$,city$,state$,zipCode$,homePhone$,dateOfBirth$,dateOfDeath$)
	library 'Collection-Master Add-On\fn\mtomSoap.br': fnAddParty
	fnAddParty=fnAddParty( filingId$,partyRole$,partyType$,firstName$,lastName$,middleInitial$,suffix$,gender$,businessName$,SSNFTIN$,aka1type$,aka1$,aka2type$,aka2$,address1$,address2$,city$,state$,zipCode$,homePhone$,dateOfBirth$,dateOfDeath$)
fnend

! /r
