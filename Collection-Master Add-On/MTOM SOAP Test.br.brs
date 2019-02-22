fn_setup
fnTop(program$,'',1)
! r: highly subjective setup
	
	! bar number that can be used for test filing from the court: 25623
	filerBarNumber$='25623'  ! Kirk is 16805
	! filerBarNumber=16805 <-- Kirk 
	
	dim notificationEmail$*128
	notificationEmail$='jbowman@bqlaw.com'
	dim optNotificationEmail1$*128
	optNotificationEmail1$=''
	dim optNotificationEmail2$*128
	optNotificationEmail2$=''

	court$='County' ! or 'District'
	enableReplevin=0
! /r
fntop(program$)

! r: Menu
	do
		pk$='A-M'

		dim menuOption$(0)*128
		mat menuOption$(0)
		fnAddOneC(mat menuOption$,' List<Municipalities> listFilers();')
		fnAddOneC(mat menuOption$,' New      Non-Criminal Flow (4-8)') ! Always Civil cases
		fnAddOneC(mat menuOption$,' Existing Non-Criminal Flow (3,5-8)') ! Always Civil cases
		fnAddOneC(mat menuOption$,' >  loadExistingCase') ! Always Civil cases
		fnAddOneC(mat menuOption$,' >  CreateNewFiling')
		fnAddOneC(mat menuOption$,' >  addParty Plantiff')
		fnAddOneC(mat menuOption$,' >  addParty Defendant')
		fnAddOneC(mat menuOption$,' >  streamDocument')
		fnAddOneC(mat menuOption$,' >  submitFiling')

		choice=fnMenu(env$('program_caption'),Mat menuOption$, pk$,'John Bowman Services LLC') ! ,0,1) ! ; &Pk$, Title$*80, Footer$*80, Nnp, Autonumber,Mstart,Mat Custom_Menubar$, Mat Custom_Menuprg$, Mat Custom_Menustatus$,&Menu_Startup$,Menu_Offset,M_Timeout,M_Trust$*30,M_F93_Enable)

		choiceWalker=0
		if choice=(choiceWalker+=1)  then ! list<municipalities>
			fnListMunicipalities
		else if choice=(choiceWalker+=1) then ! New      Non-Criminal Flow (4-8)
			fnCreateNewFiling(court$,'1','CI',filerBarNumber$,notificationEmail$)
			fnAddParty ! Plantiff
			fnAddParty ! Defendant
			fnStreamDocument('','',program$(1:pos(program$,'\',-1))&'mtom soap sample 1.pdf')
			fnSubmitFiling('','')
		else if choice=(choiceWalker+=1) then ! Existing Non-Criminal Flow (3,5-8)
			fnLoadExistingCase('C99CR160000004',filerBarNumber$,'2018-11-05T13:00:00Z',notificationEmail$, 'This is a sample customer memo')
			fnCreateNewFiling(court$,'1','CI',filerBarNumber$,notificationEmail$)
			fnAddParty ! Plantiff
			fnAddParty ! Defendant
			fnStreamDocument('','',program$(1:pos(program$,'\',-1))&'mtom soap sample 1.pdf')
			fnSubmitFiling('','')
		else if choice=(choiceWalker+=1)  then ! loadExistingCase
			fnLoadExistingCase('C99CR160000004',filerBarNumber$,'2018-11-05T13:00:00Z',notificationEmail$, 'This is a sample customer memo')
		else if choice=(choiceWalker+=1)  then ! CreateNewFiling
			fnCreateNewFiling(court$,'1','CI',filerBarNumber$,notificationEmail$)
		else if choice=(choiceWalker+=1)  then ! addParty Plantiff
			fnAddParty ! Plantiff
		else if choice=(choiceWalker+=1)  then ! addParty Defendant
			fnAddParty ! Defendant
		else if choice=(choiceWalker+=1)  then ! streamDocument
			fnStreamDocument('','',program$(1:pos(program$,'\',-1))&'mtom soap sample 1.pdf')
		else if choice=(choiceWalker+=1)  then ! submitFiling
			fnSubmitFiling('','')
		else
			fnXit
		end if
	loop
! /r
def fn_setup
	if ~setup then
		setup=1
		library 'Collection-Master Add-On\fn\Library.br': fnLoadExistingCase
		library 'Collection-Master Add-On\fn\Library.br': fnListMunicipalities
		library 'Collection-Master Add-On\fn\Library.br': fnCreateNewFiling
		library 'Collection-Master Add-On\fn\Library.br': fnAddParty
		library 'Collection-Master Add-On\fn\Library.br': fnStreamDocument
		library 'Collection-Master Add-On\fn\Library.br': fnSubmitFiling
		library 'S:\Core\Library.br': fnAddOneC
		library 'S:\Core\Library.br': fnTop
		library 'S:\Core\Library.br': fnXit

		library "library\CLSUtil.wb": fnmenu
	end if
fnend

