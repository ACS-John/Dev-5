fn_setup
fntop(program$)
gosub IfCoTryAgain
fncno(cno)
MENU1: ! r:
	fnTos
	! r: add the system buttons to the screen
	fnbutton_or_disabled(env$('enableClientSelection')=='Yes',2,2,env$('client')(1:37),fkey_client:=5201, 'Client Name is "'&env$('client')&'"',37)
	fnFra(4,1,udim(mat client_has$)+1,38, 'System','Click a system button to change and view the companies in that system')
	ch_line=1
	for ch_item=2 to udim(mat client_has$) ! starting at 2 to always skip CO = which is always #1
		if ~fnSystemIsAddOn(client_has$(ch_item)) then
			ch_line+=1
			fnbutton_or_disabled((~env$('cursys')==client_has$(ch_item)),ch_line,1,fnSystemNameFromAbbr$(client_has$(ch_item))(1:37),1000+ch_item, '',37,1)
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
	!

	fngetdir2(fn_dataFolder$,mat filename$,'/od /ta',"Company.*") ! fngetdir(temp$,mat filename$,empty$,"Company.*") ! /oe
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
	fnLbl(1,42,"Company:",9,0)
	fnButton(2,52,"Con&figure",14,'Select highlighted company and go to configure it.  Returns here upon exit.',1,9) ! ,0,0,1)  ! fnCmdKey("&Select",1,1)
	fnButton(2,62,"&Select",10,'',1,9) ! ,0,0,1)  ! fnCmdKey("&Select",1,1)
	fnButton(2,72,"&Add",2,'',1,9) ! fnCmdKey("&Add",2)
	fnButton(2,82,"Co&py",3,'',1,9)
	if company_count=>2 then ! Delete only allowed if there are 2 or more companies on the list
		fnButton(2,92,"&Delete",4,'',1,9)
	end if
	fnButton(1,102,"Open...",17,'',1,9)
	if (fnclient_is_converting or company_count=0) then
		fnButton(2,102,"I&mport",13,'',1,9)
	end if
	fnCmdKey("&Save",15,1,0)
	if exists(fn_dataFolder$&'\Company.h'&str$(cno)) then ! cancel only allowed if they have not deleted their current company
		fnCmdKey("&Cancel",5,0,1)
	end if
	fnAcs2(mat resp$,ck)
	!
	if ck=5 and exists(fn_dataFolder$&'\Company.h'&str$(cno)) then ! cancel
		goto Xit
	else if ck=2 then
		goto COMPANY_ADD
	else if ck=17 then ! Open...
		fnOpenPartial
		chain program$
	else if ck=13 and (fnclient_is_converting or company_count=0) then ! import
		fnchain('S:\Core\Company Import.br')
	end if
	!
	cno_selected=val(resp$(1))
	!
	if ck=3 then ! Copy
		fn_companyCopy(cno_selected)
	!   goto MENU1
	else if ck=14 then ! Delete Company
		gosub SELECT_COMPANY
		fn_companyConfigure(scno)
	else if ck=4 and company_count=>2 then ! Delete Company
		fn_companyDelete(cno_selected)
	!   goto MENU1
	else if ck=10 then ! Select that Company
		gosub SELECT_COMPANY
		chain program$
	else if ck>1000 and ck<1200 then ! Select System 1001-1199 reserved for system selection
		cursys$=client_has$(ck-1000)
		fnreg_write(session$&'.CurSys',cursys$)
		fncursys$(cursys$)
		fn_setup_on_cursys_change
		chain program$
	else if fkey_client>0 and ck=fkey_client then
		fnClientSelect
		fn_system_setup
		if udim(mat client_has$)=>2 and srch(mat client_has$,env$('cursys'))<=0 then ! change it to the second available system (1st is CO) if they don't have the currently selected system
			fncursys$(client_has$(2))
			fn_setup_on_cursys_change
		end if
	else if ck=15 then ! SAVE
		gosub SELECT_COMPANY
		goto Xit
	end if
goto MENU1 ! /r
SELECT_COMPANY: ! r:
	fnputcno(cno_selected)
return  ! /r
! ______________________________________________________________________
COMPANY_ADD: ! r:
	fnTos
	mylen=25 : mypos=mylen+2
	respc=0
	fnLbl(1,1,"Company Number to add:",mylen,1)
	fnTxt(1,mypos,5,0,0,"30")
	resp$(respc+=1)="0"
	fnCmdSet(2)
	fnAcs2(mat resp$,ck)
	if ck=5 then goto MENU1
	cno_selected=val(resp$(1))
	if fn_company_already_exists(cno_selected)=1 then goto MENU1
	fnputcno(cno_selected)
	fncheckfileversion
	if env$('cursys')='PR' or env$('cursys')='SU' or env$('cursys')='TM' or env$('cursys')='CL' then ! no AddCNo necessary - just copy in from *.h99999 and go straight to Company Information
		if exists('S:\'&fnSystemNameFromAbbr$&'\mstr\*.h99999') then
			fnCopy('S:\'&fnSystemNameFromAbbr$&'\mstr\*.h99999',fn_dataFolder$&'\*.h[cno]', 0,'errornotify')
		else if exists('S:\acs'&env$('cursys')&'\mstr\*.h99999') then
			fnCopy('S:\acs'&env$('cursys')&'\mstr\*.h99999',fn_dataFolder$&'\*.h[cno]', 0,'errornotify')
		end if
		if env$('cursys')='CL' then ! r: special processing for CL
			mat ml$(5)
			ml$(1)='Would you like to import data from an old'
			ml$(2)='ACS Accounts Payable system?'
			ml$(3)='This is only chance.'
			fnmsgbox(mat ml$,resp$,'',36)
			if resp$='Yes' then
				fnApMstrConversion
				fnchain("S:\acsCL\Conversion\GLBLD-CNV")
			end if
		end if  ! /r
		fnchain("S:\acs"&env$('cursys')&"\Company")
	else if env$('cursys')='UB' then
		fnchain(txt$="S:\Core\AddCNo")
	else if exists('S:\'&fnSystemNameFromAbbr$(env$('cursys'))&'\Company.br') then
		fnchain('S:\'&fnSystemNameFromAbbr$(env$('cursys'))&'\Company.br')
	else
		fnchain(txt$="S:\acs"&env$('cursys')&"\AddCNo")
	end if
! /r
Xit: fnXit
def fn_company_already_exists(cae_cno)
	cae_return=0
	if exists(fn_dataFolder$&'\Company.h'&str$(cae_cno)) then
		cae_return=1
		! mat mg$(3)
		mat mg$(2)
		mg$(1)="Company number "&str$(cae_cno)&". "&rtrm$(fn_companyName$(cae_cno))&" already exist!"
		mg$(2)="Would you like to erase it first?"
		fnmsgbox(mat mg$,response$,'',32+4+256) ! (i)+(yn)+(secondButtonDefault)
		if response$='Yes' then
			if fn_companyDelete(cae_cno) then cae_return=0
		end if
	end if
	fn_company_already_exists=cae_return
fnend
def fn_companyName$*40(cno)
	dim coc_return$*40
	coc_return$=''
	open #h_tmp:=fngethandle: 'Name='&fn_dataFolder$&'\Company.h'&str$(cno),internal,input ioerr COC_FINIS
	read #h_tmp,using "Form pos 1,c 40": coc_return$
	close #h_tmp:
	COC_FINIS: !
	fn_companyName$=coc_return$
fnend
def fn_companyDelete(cno; fileToDelete$*256)
	if fnConfirmDeleteHard('company',str$(cno)&". "&fn_companyName$(cno)) then
		! if env$('acsDeveloper')<>"" then pause
		fileToDelete$=fnSrepEnv$(fn_dataFolder$)
		if exists(fileToDelete$&'\company.h'&str$(cno)) then
			fnFree(fileToDelete$&'\*.h'&str$(cno))
			if exists(fileToDelete$&'\UBData\*.h[cno]') and cno<>1 then
				fnFree(fileToDelete$&'\UBData\*.h[cno]')
			end if
			execute 'Free "'&fileToDelete$&'\*.h'&str$(cno)&'" -n'
			mat mg$(1)
			mg$(1)='Company Number '&str$(cno)&' has been Deleted!'
			fnmsgbox(mat mg$,resp$,'',0)
			companyDeleteReturn=1
		else
			mat mg$(1)
			mg$(1)='Company Number '&str$(cno)&' failed to delete.'
			fnmsgbox(mat mg$,resp$,'',0)
			companyDeleteReturn=0
		end if
	end if
	fn_companyDelete=companyDeleteReturn
fnend
def fn_companyConfigure(scno)
	setenv("xit_override","")
	setenv("xit_override","S:\Core\Programs\Select Company")
	fnchain('S:\acs'&env$('cursys')&'\Company.br')
fnend
def fn_companyCopy(scno)
	dim dcnam$*40
	dcno=0
	dcnam$=fn_companyName$(scno)
	CC_SCREEN1: !
	fnTos
	lc=0
	mylen=29 : mypos=mylen+2
	fnLbl(lc+=1,1,"Source Company:",mylen,1)
	fnLbl(lc,mypos,str$(scno)&'. '&fn_companyName$(scno),50)
	lc+=1
	fnLbl(lc+=1,1,"&Destination Company Number:",mylen,1)
	fnTxt(lc,mypos,5,0,0,'30')
	resp$(1)=str$(dcno)
	fnLbl(lc+=1,1,"Destination Company Name:",mylen,1)
	fnTxt(lc,mypos,40,40)
	resp$(2)=dcnam$
	lc+=1
	fnLbl(lc+=1,1,"Warning",80,2,1)
	fnLbl(lc+=1,1,"Please make sure no one else is",80,2)
	fnLbl(lc+=1,1,"using either company number.",80,2)
	fnLbl(lc+=1,1,"If the destination company exists",80,2)
	fnLbl(lc+=1,1,"it will be overwritten!",80,2)
	fnCmdSet(2)
	fnAcs2(mat resp$,ck)
	if ck<>5 then
		dcno=val(resp$(1))
		dcnam$=resp$(2)
		if fn_company_already_exists(dcno)=1 then goto CC_SCREEN1
		fnCopy(fn_dataFolder$&'\*.h'&str$(scno),fn_dataFolder$&'\*.h'&str$(dcno), 0,'errornotify')
		if uprc$(env$('cursys'))=uprc$("UB") then
			fnCopy("[Q]\UBmstr\ubData\*.h"&str$(scno),"[Q]\UBmstr\ubData\*.h"&str$(dcno), 0,'errornotify')
		end if
		fn_companyNameSet(dcno,dcnam$)
	end if
fnend
def fn_companyNameSet(cno,cnam$*40)
	cnam$=rtrm$(cnam$)
	if cnam$<>'' then
		open #h_company:=fngethandle: 'Name='&fn_dataFolder$&'\Company.h'&str$(cno),internal,outIn,relative
		rewrite #h_company,using 'form pos 1,c 40',rec=1: cnam$
		close #h_company:
	end if
fnend
def fn_setup
	if setup<>1 then
		setup=1
		library 'S:\Core\Library': fnConfirmDeleteHard
		library 'S:\Core\Library': fnTos
		library 'S:\Core\Library': fnCno
		library 'S:\Core\Library': fnAcs2
		library 'S:\Core\Library': fnchain,fnXit
		library 'S:\Core\Library': fnputcno
		library 'S:\Core\Library': fngetdir2
		library 'S:\Core\Library': fncursys$
		library 'S:\Core\Library': fnLbl,fnCmdSet
		library 'S:\Core\Library': fntop
		library 'S:\Core\Library': fnTxt,fnCmdKey,fnSrepEnv$
		library 'S:\Core\Library': fncheckfileversion
		library 'S:\Core\Library': fnmsgbox,fnflexadd1,fnflexinit1
		library 'S:\Core\Library': fnButton,fnFra
		library 'S:\Core\Library': fngethandle
		library 'S:\Core\Library': fnclient_is_converting
		library 'S:\Core\Library': fnclient_has_mat
		library 'S:\Core\Library': fnreg_read,fnreg_write
		library 'S:\Core\Library': fnSystemNameFromAbbr$
		library 'S:\Core\Library': fnApMstrConversion
		library 'S:\Core\Library': fnbutton_or_disabled
		library 'S:\Core\Library': fnClientSelect
		library 'S:\Core\Library': fnOpenPartial
		library 'S:\Core\Library': fnCopy
		library 'S:\Core\Library': fnFree
		library 'S:\Core\Library': fnCompanyPayPeriodEndingDate
		library 'S:\Core\Library': fnSystemIsAddOn
		on error goto Ertn
		! ______________________________________________________________________
		dim filename$(999)*40
		dim resp$(10)*50,txt$*40 ! ,temp$*256
		dim mg$(3)*128
		! NOTE: Regardless of which ACS System:
		!  * Add Company program must be called acs[cursys]\ADDCNO.br
		!  * Company files must be named Company.hxx
	end if
	!
	fn_system_setup
fnend
def fn_dataFolder$*256(; ___,return$*256)
	if env$('acsDeveloper')<>'' and env$('cursys')='TM' then
		return$='S:\Core\Data\acsllc'
	else
		return$='[Q]\'&env$('cursys')&"mstr"
	end if
	fn_dataFolder$=return$
fnend
def fn_system_setup
	if ~fnclient_has_mat(mat client_has$) and env$("ACSDeveloper")="" then
		dim ml$(1)*128
		mat ml$(4)
		ml$(1)='Client '&env$('client')&' has nothing licensed.  Please perform an update.'
		ml$(2)='If you have already performed an update and are'
		ml$(3)='still receiving this message contact ACS at 1-800-643-6318'
		ml$(4)='Perform an Update now?'
		fnmsgbox(mat ml$,resp$,'',16+4)
		if uprc$(resp$)=uprc$("Yes") then
			chain 'S:\Core\Programs\Update'
		else
			goto Xit
		end if
	end if

	gosub IfCoTryAgain

	dim cursys$*40
	!  cursys$=fncursys$
	fnreg_read(session$&'.CurSys',cursys$)
	cursys$=fncursys$(cursys$)
	fn_setup_on_cursys_change
fnend
IfCoTryAgain: ! r: if cursys=CO than just pick the first thing they are licensed for
	if ( env$('cursys')='CO' or srch(mat client_has$,env$('cursys'))<=0 ) and udim(mat client_has$)=>2 then
		cursys$=client_has$(2)
		fnreg_write(session$&'.CurSys',cursys$)
		fncursys$(cursys$)
		fn_setup_on_cursys_change
		! fnchain(program$)
	end if
return ! /r
def fn_setup_on_cursys_change
	dim cnam$*80
	fnCno(cno,cnam$)
	if cno=0 then
		cno=1
		fnputcno(cno)
		fncno(cno,cnam$)
	end if
	!
	if ~exists('[Q]\'&cursys$&'mstr') and cursys$<>'CO' and cursys$<>'TM' then execute 'mkdir "[Q]\'&cursys$&'mstr"'
	! if ~exists('[Q]\INI\acs'&cursys$) then execute 'mkdir [Q]\INI\acs'&cursys$
fnend

include: ertn
