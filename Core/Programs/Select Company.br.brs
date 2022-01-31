fn_setup
fnTop(program$)
gosub IfCoTryAgain
fncno(cno)
Menu1: ! r:
	fnTos
	! r: add the system buttons to the screen
	fnButtonOrDisabled(env$('enableClientSelection')=='Yes',2,2,env$('client')(1:37),fkey_client:=5201, 'Client Name is "'&env$('client')&'"',37)
	fnFra(4,1,udim(mat client_has$)+1,38, 'System','Click a system button to change and view the companies in that system')
	ch_line=1
	for ch_item=2 to udim(mat client_has$) ! starting at 2 to always skip CO = which is always #1
		if ~fnSystemIsAddOn(client_has$(ch_item)) then
			ch_line+=1
			fnButtonOrDisabled((~env$('cursys')==client_has$(ch_item)),ch_line,1,fnSystemName$(client_has$(ch_item))(1:37),1000+ch_item, '',37,1)
		end if
	next ch_item
	! /r
	!  r: add that company grid to the screen
	dim item$(0)*40
	if env$('cursys')='PR' then
		mat item$(3)
		mat colhdr$(3)
		colhdr$(1)='Number'
		colhdr$(2)='Name'
		colhdr$(3)='Pay Period End'
		mat colmask$(3)
		colmask$(1)='30'
		colmask$(2)=''
		colmask$(3)=''
	else
		mat item$(2)
		mat colhdr$(2)
		colhdr$(1)='Number'
		colhdr$(2)='Name'
		mat colmask$(2)
		colmask$(1)='30'
		colmask$(2)=''
	end if

	fnflexinit1(sn$&'_flex',3,42,10,60,mat colhdr$,mat colmask$,1)


	fngetdir2(fn_dataFolder$,mat filename$,'/od /ta','Company.*') ! fngetdir(temp$,mat filename$,empty$,'Company.*') ! /oe
	company_count=filename_item=0
	for filename_item=1 to udim(mat filename$)
		tmp_cno=val(filename$(filename_item)(10:14)) conv ACNO_CONV
		if tmp_cno<>99999 and filename$(filename_item)<>'' then ! don't display company 99999
			company_count+=1
			item$(1)=str$(tmp_cno)
			item$(2)=fn_companyName$(tmp_cno)
			if env$('cursys')='PR' then
				item$(3)=date$(days(fnCompanyPayPeriodEndingDate(tmp_cno),'ccyymmdd'),'ccyy/mm/dd')
			end if
			fnflexadd1(mat item$)
			if tmp_cno=cno then
				setenv('current_grid_row',str$(company_count))
			end if
		end if
	ACNO_CONV: !
	next filename_item
	! /r
	fnLbl(1,42,'Company:',9,0)
	fnButton(2,52,'Con&figure',14,'Select highlighted company and go to configure it.  Returns here upon exit.',1,9) ! ,0,0,1)  ! fnCmdKey('&Select',1,1)
	fnButton(2,62,'&Select',10,'',1,9) ! ,0,0,1)  ! fnCmdKey('&Select',1,1)
	fnButton(2,72,'&Add',2,'',1,9) ! fnCmdKey('&Add',2)
	fnButton(2,82,'Co&py',3,'',1,9)
	if company_count=>2 then ! Delete only allowed if there are 2 or more companies on the list
		fnButton(2,92,'&Delete',4,'',1,9)
	end if
	fnButton(1,102,'Open...',17,'',1,9)
	if (fnclient_is_converting or company_count=0) then
		fnButton(2,102,'I&mport',13,'',1,9)
	end if
	fnCmdKey('&Save',15,1,0)
	if exists(fn_dataFolder$&'\Company.h'&str$(cno)) then ! cancel only allowed if they have not deleted their current company
		fnCmdKey('&Cancel',5,0,1)
	end if
	ckey=fnAcs(mat resp$)

	if ckey=5 and exists(fn_dataFolder$&'\Company.h'&str$(cno)) then ! cancel
		goto Xit
	else if ckey=2 then
		goto CompanyAdd
	else if ckey=17 then ! Open...
		fnOpenPartial
		chain program$
	else if ckey=13 and (fnclient_is_converting or company_count=0) then ! import
		fnchain('S:\Core\Company Import.br')
	end if

	cno_selected=val(resp$(1))

	if ckey=3 then ! Copy
		fn_companyCopy(cno_selected)
	else if ckey=14 then ! Delete Company
		gosub CompanySelect
		fn_companyConfigure(scno)
	else if ckey=4 and company_count=>2 then ! Delete Company
		fn_companyDelete(cno_selected)
	else if ckey=10 then ! Select that Company
		gosub CompanySelect
		chain program$
	else if ckey>1000 and ckey<1200 then ! Select System 1001-1199 reserved for system selection
		curSys$=client_has$(ckey-1000)
		fnreg_write(session$&'.CurSys',curSys$)
		fncurSys$(curSys$)
		fn_setupOnCursysChange(cno,cnam$)
		chain program$
	else if fkey_client>0 and ckey=fkey_client then
		fnClientSelect
		fn_systemSetup
		if udim(mat client_has$)=>2 and srch(mat client_has$,env$('cursys'))<=0 then ! change it to the second available system (1st is CO) if they don't have the currently selected system
			fncurSys$(client_has$(2))
			fn_setupOnCursysChange(cno,cnam$)
		end if
	else if ckey=15 then ! SAVE
		gosub CompanySelect
		goto Xit
	end if
goto Menu1 ! /r
CompanySelect: ! r:
	fnputcno(cno_selected)
return  ! /r

CompanyAdd: ! r:
	fnTos
	mylen=25 : mypos=mylen+2
	respc=0
	fnLbl(1,1,'Company Number to add:',mylen,1)
	fnTxt(1,mypos,5,0,0,'30')
	resp$(respc+=1)='0'
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Menu1
	cno_selected=val(resp$(1))
	if fn_company_already_exists(cno_selected)=1 then goto Menu1
	fnputcno(cno_selected)
	fnCheckFileVersion
	if env$('cursys')='PR' or env$('cursys')='SU' or env$('cursystem')='Client Billing' or env$('cursys')='CL' then ! no AddCNo necessary - just copy in from *.h99999 and go straight to Company Information
		if exists('S:\[cursystem]\mstr\*.h99999') then
			fnCopy('S:\[cursystem]\mstr\*.h99999',fn_dataFolder$&'\*.h[cno]', 0,'errornotify')
		else if exists('S:\acs[cursys]\mstr\*.h99999') then
			fnCopy('S:\acs[cursys]\mstr\*.h99999',fn_dataFolder$&'\*.h[cno]', 0,'errornotify')
		end if
		if env$('cursys')='CL' then ! r: special processing for CL
			mat ml$(5)
			ml$(1)='Would you like to import data from an old'
			ml$(2)='ACS Accounts Payable system?'
			ml$(3)='This is only chance.'
			fnmsgbox(mat ml$,resp$,'',36)
			if resp$='Yes' then
				fnImportCLfromAP
				goto Xit
			end if
		end if  ! /r
		if exists ('S:\[cursystem]\Company.br') then
			fnchain('S:\[cursystem]\Company.br')
		else
			fnchain('S:\acs[cursys]\Company')
		end if
	else if env$('cursys')='UB' then
		fnchain('S:\Core\AddCNo')
	else if exists('S:\[cursystem]\Company.br') then
		fnchain('S:\[cursystem]\Company.br')
	else
		fnchain('S:\acs[cursys]\AddCNo')
	end if
! /r
Xit: fnXit
	def fn_company_already_exists(cae_cno; ___,returnN)
		if exists(fn_dataFolder$&'\Company.h'&str$(cae_cno)) then
			returnN=1
			! mat mg$(3)
			mat mg$(2)
			mg$(1)='Company number '&str$(cae_cno)&'. '&rtrm$(fn_companyName$(cae_cno))&' already exist!'
			mg$(2)='Would you like to erase it first?'
			fnmsgbox(mat mg$,response$,'',32+4+256) ! (i)+(yn)+(secondButtonDefault)
			if response$='Yes' then
				if fn_companyDelete(cae_cno) then returnN=0
			end if
		end if
		fn_company_already_exists=returnN
	fnend
	def fn_companyName$*40(cno; ___,return$*40,hTmp)
		open #hTmp=fnH: 'Name='&fn_dataFolder$&'\Company.h'&str$(cno),i,i ioerr COC_FINIS
		read #hTmp,using 'form pos 1,c 40': return$
		close #hTmp:
		COC_FINIS: !
		fn_companyName$=return$
	fnend
	def fn_companyDelete(cnoToDelete; ___,fileToDelete$*256,returnN,freeResponse)
		if fnConfirmDeleteHard('company',str$(cnoToDelete)&'. '&fn_companyName$(cnoToDelete)) then
			fileToDelete$=fnSrepEnv$(fn_dataFolder$)
			
			! pr 'before exists('&fileToDelete$&'\company.h'&str$(cnoToDelete);exists(fileToDelete$&'\company.h'&str$(cnoToDelete))
			freeResponse=fnFree(fileToDelete$&'\*.h'&str$(cnoToDelete))
			! pr 'freeResponse=';freeResponse
			! pr 'after  exists('&fileToDelete$&'\company.h'&str$(cnoToDelete);exists(fileToDelete$&'\company.h'&str$(cnoToDelete))
			! fnPause
			
			if cnoToDelete<>1 then fnFree(fileToDelete$&'\UBData\*.h'&str$(cnoToDelete))

			if exists(fileToDelete$&'\company.h'&str$(cnoToDelete)) then
				mat mg$(1)
				mg$(1)='Company Number '&str$(cnoToDelete)&' failed to delete.'
				fnmsgbox(mat mg$,resp$,'',0)
				returnN=0
				fnpause
			else
				mat mg$(1)
				mg$(1)='Company Number '&str$(cnoToDelete)&' has been Deleted!'
				fnmsgbox(mat mg$,resp$,'',0)
				returnN=1
			end if
		end if
		fn_companyDelete=returnN
	fnend
	def fn_companyConfigure(scno)
		setenv('xit_override','')
		setenv('xit_override','S:\Core\Programs\Select Company')
		if exists('S:\[cursystem]\Company.br') then
			fnchain('S:\[cursystem]\Company.br')
		else
			fnchain('S:\acs[cursys]\Company.br')
		end if
	fnend
	def fn_companyCopy(scno)
		dim dcnam$*40
		dcno=0
		dcnam$=fn_companyName$(scno)
		CC_SCREEN1: !
		fnTos
		lc=0
		mylen=29 : mypos=mylen+2
		fnLbl(lc+=1,1,'Source Company:',mylen,1)
		fnLbl(lc,mypos,str$(scno)&'. '&fn_companyName$(scno),50)
		lc+=1
		fnLbl(lc+=1,1,'&Destination Company Number:',mylen,1)
		fnTxt(lc,mypos,5,0,0,'30')
		resp$(1)=str$(dcno)
		fnLbl(lc+=1,1,'Destination Company Name:',mylen,1)
		fnTxt(lc,mypos,40,40)
		resp$(2)=dcnam$
		lc+=1
		fnLbl(lc+=1,1,'Warning',80,2,1)
		fnLbl(lc+=1,1,'Please make sure no one else is',80,2)
		fnLbl(lc+=1,1,'using either company number.',80,2)
		fnLbl(lc+=1,1,'If the destination company exists',80,2)
		fnLbl(lc+=1,1,'it will be overwritten!',80,2)
		fnCmdSet(2)
		ckey=fnAcs(mat resp$)
		if ckey<>5 then
			dcno=val(resp$(1))
			dcnam$=resp$(2)
			if fn_company_already_exists(dcno)=1 then goto CC_SCREEN1
			fnCopy(fn_dataFolder$&'\*.h'&str$(scno),fn_dataFolder$&'\*.h'&str$(dcno), 0,'errornotify')
			if uprc$(env$('cursys'))=uprc$('UB') then
				fnCopy('[Q]\UBmstr\ubData\*.h'&str$(scno),'[Q]\UBmstr\ubData\*.h'&str$(dcno), 0,'errornotify')
			end if
			fn_companyNameSet(dcno,dcnam$)
		end if
	fnend
	def fn_companyNameSet(cno,cnam$*40)
		cnam$=rtrm$(cnam$)
		if cnam$<>'' then
			open #h_company=fnH: 'Name='&fn_dataFolder$&'\Company.h'&str$(cno),i,outi,r
			rewrite #h_company,using 'form pos 1,c 40',rec=1: cnam$
			close #h_company:
		end if
	fnend

def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		on error goto Ertn

		dim filename$(999)*40
		dim resp$(10)*50 ! ,temp$*256
		dim mg$(3)*128
		! NOTE: Regardless of which ACS System:
		!  * Add Company program must be called acs[cursys]\ADDCNO.br
		!  * Company files must be named Company.hxx
	end if

	fn_systemSetup
fnend

def fn_dataFolder$*256(; ___,return$*256)
	if env$('cursys')='CLIENT BILLING' then
		return$='S:\Core\Data\acsllc'
	else
		return$='[Q]\[cursys]mstr'
	end if
	fn_dataFolder$=return$
fnend
def fn_systemSetup
	if ~fnclient_has_mat(mat client_has$) and env$('ACSDeveloper')='' then
		dim ml$(1)*128
		mat ml$(4)
		ml$(1)='Client '&env$('client')&' has nothing licensed.  Please perform an update.'
		ml$(2)='If you have already performed an update and are'
		ml$(3)='still receiving this message contact ACS at 1-800-643-6318'
		ml$(4)='Perform an Update now?'
		fnmsgbox(mat ml$,resp$,'',16+4)
		if uprc$(resp$)=uprc$('Yes') then
			chain 'S:\Core\Programs\Update'
		else
			goto Xit
		end if
	end if

	gosub IfCoTryAgain

	dim curSys$*40
	!  curSys$=fncurSys$
	fnreg_read(session$&'.CurSys',curSys$)
	curSys$=fncurSys$(curSys$)
	fn_setupOnCursysChange(cno,cnam$)
fnend
IfCoTryAgain: ! r: if cursys=CO than just pick the first thing they are licensed for
	if ( env$('cursys')='CO' or srch(mat client_has$,env$('cursys'))<=0 ) and udim(mat client_has$)=>2 then
		curSys$=client_has$(2)
		fnreg_write(session$&'.CurSys',curSys$)
		fncurSys$(curSys$)
		fn_setupOnCursysChange(cno,cnam$)
		! fnchain(program$)
	end if
return ! /r
dim cnam$*80
def fn_setupOnCursysChange(&cno,&cnam$)
	fnCno(cno,cnam$)
	if ~cno then
		cno=1
		fnputcno(cno)
		fncno(cno,cnam$)
	end if

	if ~exists('[Q]\[cursys]mstr') and curSys$<>'CO' and lwrc$(curSys$)<>lwrc$('Client Billing') then execute 'mkdir "[Q]\[cursys]mstr"'

fnend

include: ertn
