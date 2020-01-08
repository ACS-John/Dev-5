fn_setup
library program$: fncheckfileversion
fntop(program$)
fncheckfileversion
XIT: fnxit
IGNORE: continue 
def fn_setup
	if ~setup then 
		setup=1
		library 'S:\Core\Library': fntop
		library 'S:\Core\Library': fnerror
		library 'S:\Core\Library': fnxit
		library 'S:\Core\Library': fngethandle
		library 'S:\Core\Library': fnglcontrol
		library 'S:\Core\Library': fnindex_it
		library 'S:\Core\Library': fnCopy,fnFree,fnRename
		library 'S:\Core\Library': fnclient_has
		library 'S:\Core\Library': fngetdir2
		library 'S:\Core\Library': fnGetPp
		library 'S:\Core\Library': fncreg_write
		library 'S:\Core\Library': fnAddOneC
		library 'S:\Core\Library': fnKeyChange
		library 'S:\Core\Library': fnSystemNameFromAbbr$
		library 'S:\Core\Library': fnIniToReg
		library 'S:\Core\Library': fnOpenFile,fnCloseFile
		library 'S:\Core\Library': fnStatus,fnStatusPause
		library 'S:\Core\Library': fnInitialializeMeterLocation
		library 'S:\Core\Library': fnAutomatedSavePoint
		on error goto Ertn
		dim form$(0)*512
	end if
fnend

def library fncheckfileversion
	! Checks the File versions and calls conversion programs if necessary
	! This Fn is called from S:\Core\Program\Select Company.br and S:\Core\Company Import
	! this library function checks to make sure all file versions for the
	! current system are up to date - and runs the appropriate conversion  function if not
	if ~setup then let fn_setup
	! there are other Library statements in this program - but they are placed in the section for which system they really belong to.
	! _______________________________________________________________________
	dim tmpfile$*512,tmpkps(10),tmpkln(10),name$*512,kfname$*512
	dim kfnames$(1)*512
	! ____________
	fnStatus('Running fnCheckFileVersion for '&env$('cursys')&' and Company Number '&env$('cno')) ! XXX
	fn_cfv_add_missing_files
	! 
	if env$('cursys')='GL' then 
		fn_cfv_general_ledger
	else if env$('cursys')='PR' then 
		fn_cfv_payroll
		if fnclient_has('P2') then 
			fn_cfv_job_cost_payroll
		end if 
	else if env$('cursys')='UB' then 
		fn_cfv_utility_billing
	else if env$('cursys')='CL' then 
		fn_cfv_checkbook
	else if env$('cursys')='TM' then 
		fn_cfv_time_management
	end if 
	fnStatus('CheckFileVersion Completed')
fnend 
def fn_cfv_add_missing_files
	dim camf_filename$(0)*256
	dim camf_path$*256,camf_prog$*256,camf_ext$*128
	fngetdir2('S:\'&fnSystemNameFromAbbr$&'\mstr\',mat camf_filename$, '','*.h99999')
	for camf_item=1 to udim(mat camf_filename$)
		fnGetPp(camf_filename$(camf_item),camf_path$,camf_prog$,camf_ext$)
		! if lwrc$(camf_filename$(camf_item))='department' then pause
		if ~exists('[Q]\'&env$('cursys')&'mstr\'&camf_prog$&'.h[cno]') then 
			fnCopy('S:\'&fnSystemNameFromAbbr$&'\mstr\'&camf_filename$(camf_item),'[Q]\'&env$('cursys')&'mstr\'&camf_prog$&'.h[cno]')
		end if 
	next camf_item
fnend 
def fn_file_setup_data(fsad_name$*512,fsad_recl,fsad_version_proper)
	dim g_fs_name$*512
	g_fs_name$=fsad_name$
	fn_make_data_file_exist(g_fs_name$,fsad_recl,fsad_version_proper)
	fn_min_rln(fsad_name$,fsad_recl)
fnend 
def fn_file_setup_index(fsi_kfname$*512,fsi_kps$,fsi_kln$)
	dim g_fs_kfname$*512
	g_fs_kfname$=fsi_kfname$
	if ~exists(fsi_kfname$) or lwrc$(env$('force_reindex'))='yes' or ~fn_check_indexes(g_fs_name$,fsi_kfname$,fsi_kps$,fsi_kln$) then 
		fnindex_it(g_fs_name$,fsi_kfname$,fsi_kps$&' '&fsi_kln$)
	end if 
fnend 
def fn_make_data_file_exist(name$*512,myrln,version_proper)
	if exists(name$)=0 then 
		fnStatus('Creating new file: Name='&name$&',Shr,Use,RecL='&str$(myrln)&',Version='&str$(version_proper))
		open #tmp:=fngethandle: 'Name='&name$&',Shr,Use,RecL='&str$(myrln)&',Version='&str$(version_proper),internal,outIn 
		close #tmp: 
	end if 
	! 
fnend 
def fn_check_indexes(name$*512,mat kfnames$,mat kps$,mat kln$)
	ci_return=1 ! function should return 1 if all indexes tested are fine or 0 if any tested fail
	for ci_item=1 to udim(mat kfnames$)
		str2mat(kps$(ci_item),mat ci_kps$,'/')
		str2mat(kln$(ci_item),mat ci_kln$,'/')
		CI_OPEN_IT: ! 
		open #h_ci_tmp:=fngethandle: 'name='&name$&',KFName='&kfnames$(ci_item),internal,input,keyed ioerr CI_OPEN_ERR
		! 
		for x=1 to udim(mat ci_kps$)
			if kps(h_ci_tmp,x)<>val(ci_kps$(x)) then 
				fnStatus('Key Position mismatch!') ! should these use fnStatus ??
				fnStatus(' Data File: '&name$)
				fnStatus('Index File: '&kfnames$(ci_item))
				fnStatus('Key Part: '&str$(x))
				fnStatus('Key Position should be '&ci_kps$(x)&' but it is '&str$(kps(h_ci_tmp,x)))
				ci_return=0
			end if 
			! 
			if kln(h_ci_tmp,x)<>val(ci_kln$(x)) then 
				fnStatus('Key Length mismatch!')
				fnStatus(' Data File: '&name$)
				fnStatus('Index File: '&kfnames$(ci_item))
				fnStatus('Key Part: '&str$(x))
				fnStatus('Key Length should be '&ci_kln$(x)&' but it is '&str$(kln(h_ci_tmp,x)))
				ci_return=0
			end if 
			! 
		next x
		close #h_ci_tmp: 
	next ci_item
	goto CI_XIT
	CI_OPEN_ERR: ! 
	fnStatus('error '&str$(err)&' opening Name='&name$&',KFName='&kfnames$(ci_item))
	if err=607 or err=632 then 
		fnStatus('indexing to fix it')
		fnindex_it(g_fs_name$,fsi_kfname$,fsi_kps$&' '&fsi_kln$)
		goto CI_OPEN_IT
	else 
		fnStatus('error unhandled')
	end if 
	CI_XIT: ! 
	fn_check_indexes=ci_return
fnend 
def fn_min_rln(mr_filename$*512,mr_rln_minimum)
	open #h_mr_file:=fngethandle: "Name="&mr_filename$&",Shr",internal,input 
	mr_rln_current=rln(h_mr_file)
	close #h_mr_file: 
	if mr_rln_current<mr_rln_minimum then 
		fnCopy(mr_filename$,mr_filename$,mr_rln_minimum)
		fnFree("[Q]\x")
	end if  ! 
fnend  ! fn_min_rln
def fn_check_version(cv_version_current,cv_version_proper,cv_file$*256)
	cv_return=1 ! function should return 1 if version tested matches or 0 if versions are different
	if cv_version_current<>cv_version_proper then 
		fnStatus('Version Error of file:'&cv_file$ )
		fnStatus('     Version Current: '&str$(cv_version_current))
		fnStatus('     Version  Proper: '&str$(cv_version_proper))
		cv_return=0
	end if 
	fn_check_version=cv_return
fnend 
def fn_get_tmp(h_tmp,mat tmpkps,mat tmpkln,&tmpversion,&tmprln,&tmpfile$)
	mat tmpkps=(0)
	mat tmpkln=(0)
	tmpversion=version(tmp)
	tmprln=rln(tmp)
	tmpfile$=file$(tmp)
	for j=1 to udim(tmpkps)
		tmpkps(j)=kps(tmp,j)
		tmpkln(j)=kln(tmp,j)
	next j
	close #h_tmp: 
fnend 
def fn_cfv_time_management
	! open #h_tmwk1:=fngethandle:
	! fn_check_version(tmpversion,version_proper,'')
fnend
def fn_cfv_utility_billing
	if exists("[Q]\UBmstr")=0 then execute 'MkDir "[Q]\UBmstr"'
	if exists("[Q]\UBmstr\UBdata")=0 then execute 'MkDir "[Q]\UBmstr\UBdata"'
	if exists("[Q]\WorkOrder")=0 then execute 'MkDir "[Q]\WorkOrder"'
	! if ~exists('[Q]\INI\Utility Billing') then execute 'mkdir "[Q]\INI\Utility Billing"'
	! if ~exists('[Q]\INI\acs'&env$('cursys')&'\UBdata') then execute 'mkdir [Q]\INI\acs'&env$('cursys')&'\UBdata'
	fn_ini_move(env$('cursys'))
	fnIniToReg
	fn_reg_rename(env$('cursys'))
	! r: move ubBkNo.h into CReg and delete ubBkNo.h
	if exists('[Q]\UBmstr\ubBkNo.h[cno]') then 
		open #h_ubbkno:=fngethandle: "Name=[Q]\UBmstr\ubBkNo.h[cno]",internal,outIn,relative 
		read #h_ubbkno,using "Form POS 1,2*N 3",rec=1: bkno1,bkno2  noRec CFVUB_RPDATE_NOREC
		fncreg_write('Route Low',str$(bkno1)) ! Route Number Range Low
		fncreg_write('Route High',str$(bkno2)) ! Route Number Range High
		CFVUB_RPDATE_NOREC: ! 
		close #h_ubbkno,free: 
	end if 
	! /r
	! UB_COMPANY: !
	! Primary No Index
	! do company different
	! - don't make it exist,
	! - and skip out if it don't exist
	name$="[Q]\UBmstr\Company.h[cno]"
	kfname$=''
	myrln=133
	version_proper=0
	open #tmp:=fngethandle: 'Name='&name$&',Shr',internal,outIn,relative ioerr SKIP_UB_COMPANY
	fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmprln<>myrln then 
		fnStatus('Record Length Error in File: '&tmpfile$)
		fnStatus('         RLn: '&str$(tmprln))
		fnStatus('Fixing the Record Length of Company')
		fnCopy("[Q]\UBmstr\Company.h[cno]","[Q]\UBmstr\Company.h[cno]",133)
	end if 
	SKIP_UB_COMPANY: ! 
	! 
	fn_file_setup_data("[Q]\UBmstr\Customer.h[cno]",2067,1)
	fn_file_setup_index("[Q]\UBmstr\ubIndex.h[cno]","1","10")
	fn_file_setup_index("[Q]\UBmstr\ubIndx2.h[cno]","354","7")
	fn_file_setup_index("[Q]\UBmstr\ubIndx3.h[cno]","11","30")
	fn_file_setup_index("[Q]\UBmstr\ubIndx4.h[cno]","41","30")
	fn_file_setup_index("[Q]\UBmstr\ubIndx5.h[cno]","1741/1743","2/7")
	! 
	fn_file_setup_data("[Q]\UBmstr\ubAdrBil.h[cno]",130,0)
	fn_file_setup_index("[Q]\UBmstr\AdrIndex.h[cno]",'1','10')
	! 
	!     fn_file_setup_data("[Q]\UBmstr\Deposit1.h[cno]",16,0)
	!     fn_file_setup_index("[Q]\UBmstr\DepIdx1.h[cno]",'1','10')
	fn_file_setup_data("[Q]\UBmstr\Deposit2.h[cno]",73,0)
	fn_file_setup_index("[Q]\UBmstr\Deposit2Index.h[cno]",'1','10')
	! 
	fn_file_setup_data("[Q]\UBmstr\workOrder.h[cno]",600,0)
	fn_file_setup_index("[Q]\UBmstr\wkIndex.h[cno]",'1/11','10/8')
	! 
	fn_file_setup_data("[Q]\UBmstr\MeterType.h[cno]",128,1)
	fn_file_setup_index("[Q]\UBmstr\MeterTypeIdx.h[cno]",'1','5')
	! 
	! no need now that we have U4 Meter Location    !   fn_file_setup_data("[Q]\UBmstr\Meter.h[cno]",384,1)
	! no need now that we have U4 Meter Location    !   fn_file_setup_index("[Q]\UBmstr\Meter_Idx.h[cno]",'1/11','10/2')
	!
	fnInitialializeMeterLocation
	!
	if exists('[Q]\UBmstr\CityStZip.dat') then
		fnStatus('Migrating UB City State Zip records into Core City State Zip table...')
		open #hUbCsz:=fngethandle: "Name=[Q]\UBmstr\CityStZip.dat,KFName=[Q]\UBmstr\CityStZip.idx,Use,RecL=30,KPs=1,KLn=30,Shr",internal,outIn,keyed 
		dim cszData$(0)*128,cszDataN(0),csz$*30
		hCoCsz:=fn_open('CO City State Zip',mat cszData$,mat cszDataN,mat form$)
		do
			read #hUbCsz,using 'form pos 1,C 30': csz$ eof CszFinis
			restore #hCoCsz,key=csz$: nokey CszAdd
			goto CszNext
			CszAdd: !
			write #hCoCsz,using 'form pos 1,C 30': csz$
			delete #hUbCsz: 
			CszNext: !
		loop
		CszFinis: !
		close #hUbCsz:
		fnFree('[Q]\UBmstr\CityStZip.dat')
		fnFree('[Q]\UBmstr\CityStZip.idx')
		fnCloseFile(hCoCsz,'CO City State Zip')
	end if
	!
	if exists('[Q]\UBmstr\Collections-'&wsid$&'.h[cno]') then
		if fnCopy('[Q]\UBmstr\Collections-'&wsid$&'.h[cno]','[Q]\UBmstr\Collections-'&env$('acsUserId')&'.h[cno]') then
			fnFree('[Q]\UBmstr\Collections-'&wsid$&'.h[cno]')
		end if
	end if
	!
	if exists('[Q]\UBmstr\IpChg01.h[cno]') then
		open #hupipchg:=fngethandle: "Name=[Q]\UBmstr\IpChg01.h[cno],RecL=80,Use",internal,outIn ioerr ubipchgOpenErr
		read #hupipchg,using "Form pos 1,N 6": d2 ioerr ignore
		close #hupipchg,free: 
		for wsidItem=1 to 99
			fnFree('[Q]\UBmstr\IpChg'&cnvrt$('pic(##)',wsidItem)&'.h[cno]')
		nex wsidItem
		ubipchgOpenErr: !
		fncreg_write('Meter Reading Date Current',str$(d2))
	end if
	if exists("[Q]\UBmstr\per1000.h[cno]") then
		dim range(16)
		open #hPer1000:=fngethandle: "Name=[Q]\UBmstr\per1000.h[cno],Shr",internal,outIn,relative 
		read #hPer1000,using "Form pos 1,16*n 10,n 2,c 1": mat range,wrate,weg$
		fncreg_write('Per 1000 Usage - Rate Code ',weg$)
		fncreg_write('Per 1000 Usage - Service for Analysis ',str$(wrate))
		for rangeItem=1 to 16
			fncreg_write('Per 1000 Usage - Range '&str$(rangeItem),str$(range(rangeItem)))
		nex rangeItem
		close #hPer1000: 
		fnFree('[Q]\UBmstr\per1000.h[cno]')
	end if
	! 
	fn_file_setup_data("[Q]\UBmstr\ubData\RateMst.h[cno]",374,0)
	fn_file_setup_index("[Q]\UBmstr\ubData\RateIdx1.h[cno]",'1','4')
	fn_file_setup_index("[Q]\UBmstr\ubData\RateIdx2.h[cno]",'5','25')
	! 
fnend 
def fn_cfv_checkbook
	! Checkbook Only
	library 'S:\Core\Library': fntrmstr_v1_to_v2, fntralloc_v1_to_v2, fnunpdaloc_v1_to_v2, fnpaytrans_v1_to_v2, fnpaymstr_v0_to_v1, fnglmstrtorecl62, fntrmstr_v0_to_v1
	if exists("[Q]\CLmstr")=0 then execute "MkDir [Q]\CLmstr"
	! if ~exists('[Q]\INI\Checkbook') then execute 'mkdir "[Q]\INI\Checkbook"'
	fn_ini_move(env$('cursys'))
	fnIniToReg
	fn_reg_rename(env$('cursys'))
	! 
	! if ~exists('[Q]\CLmstr\PayeeType.dat') and exists('S:\acsCL\PayeeType.dat') then
	!   fnCopy('S:\acsCL\PayeeType.dat','[Q]\CLmstr\PayeeType.dat')
	!   fnCopy('S:\acsCL\PayeeType.Idx','[Q]\CLmstr\PayeeType.Idx')
	! end if
	!
	CL_TRMSTR1: ! Primary Non-Split Index
	fn_file_setup_data("[Q]\CLmstr\TrMstr.h[cno]",78,2)
	fn_file_setup_index("[Q]\CLmstr\TrIdx1.h[cno]",'1','11')
	open #tmp:=fngethandle: 'Name='&g_fs_name$&',KFName='&g_fs_kfname$&',Shr',internal,outIn,keyed 
	fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	if ~fn_check_version(tmpversion,version_proper:=2,g_fs_name$) then 
		if tmpversion=0 or tmpversion=-1 then 
			fntrmstr_v0_to_v1
			goto CL_TRMSTR1
		end if 
		if tmpversion=1 then 
			fntrmstr_v1_to_v2
			goto CL_TRMSTR1
		end if 
	end if 
	! CL_TRMSTR2: ! Secondary Split Index
	fn_file_setup_index("[Q]\CLmstr\TrIdx2.h[cno]",'28/1','8/11')
	! CL_TRMSTR3: ! Secondary 3-Split Index
	fn_file_setup_index("[Q]\CLmstr\TrIdx3.h[cno]",'16/12/4','2/4/8')
	! 
	CL_TRALLOC: ! 1 file and 1 key
	! (index file did not exists in previous versions,
	! so it'll build it if need be here, before it opens it)
	name$="[Q]\CLmstr\TrAlloc.h[cno]"
	kfname$="[Q]\CLmstr\TrAlloc-Idx.h[cno]"
	myrln=80
	version_proper=2
	fn_make_data_file_exist(name$,myrln,version_proper)
	if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
		fnindex_it(name$,kfname$,'1 11')
	end if 
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
	fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmpversion=1 or tmpversion=0 or tmpversion=-1 then 
		fntralloc_v1_to_v2
		goto CL_TRALLOC
	end if 
	if tmprln<>myrln then 
		pr 'Record Length Error in File: '&tmpfile$
		pr '         RLn: '&str$(tmprln)
	end if 
	if tmpkps(1)<>1 then 
		pr 'Key Position Error in '&kfname$
		pr '         KPs: '&str$(tmpkps(1))
	end if 
	if tmpkln(1)<>11 then 
		pr 'Key Length Error in '&kfname$
		pr '         KLn: '&str$(tmpkln(1))
	end if 
	! 
	CL_UNPDALOC1: ! Primary, Non-Split Index
	! it is important that if conversion from version 1 to 2 occur on this
	! that this file process before PayTrans - the file it is linked to
	! So that record sequence is maintained.
	name$="[Q]\CLmstr\UnPdAloc.h[cno]"
	kfname$="[Q]\CLmstr\UAIdx1.h[cno]"
	myrln=67
	version_proper=2
	fn_make_data_file_exist(name$,myrln,version_proper)
	L1840: ! 
	if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
		fnindex_it(name$,kfname$,'9 12')
	end if 
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
	fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmpversion=1 or tmpversion=0 or tmpversion=-1 then 
		fnunpdaloc_v1_to_v2
		goto CL_UNPDALOC1
	end if 
	if tmprln<>myrln then 
		pr 'Record Length Error in File: '&tmpfile$
		pr '         RLn: '&str$(tmprln)
	end if 
	x=1 : if tmpkps(x)<>9 then 
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
		pr 'fixing it'
		fnFree(kfname$)
		goto L1840
	end if 
	x=1 : if tmpkln(x)<>12 then 
		pr 'Key Length ('&str$(x)&') Error in '&kfname$
		pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	end if 
	! 
	! CL_UNPDALOC2: ! Secondary, Non-Split Index
	name$="[Q]\CLmstr\UnPdAloc.h[cno]"
	kfname$="[Q]\CLmstr\UAIdx2.h[cno]"
	if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
		fnindex_it(name$,kfname$,'1 20')
	end if 
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
	fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	x=1 : if tmpkps(x)<>1 then 
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	end if 
	x=1 : if tmpkln(x)<>20 then 
		pr 'Key Length ('&str$(x)&') Error in '&kfname$
		pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	end if 
	! 
	CL_PAYTRANS1: ! Primary Non-Split Index
	name$="[Q]\CLmstr\PayTrans.h[cno]"
	kfname$="[Q]\CLmstr\UnPdIdx1.h[cno]"
	myrln=114
	version_proper=2
	fn_make_data_file_exist(name$,myrln,version_proper)
	if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
		fnindex_it(name$,kfname$,'1 20')
	end if 
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed : fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmpversion=1 or tmpversion=0 or tmpversion=-1 then 
		fnpaytrans_v1_to_v2 : goto CL_PAYTRANS1
	end if 
	if tmprln<>myrln then 
		pr 'Record Length Error in File: '&tmpfile$
		pr '         RLn: '&str$(tmprln)
		pr 'Fixing it'
		fnCopy("[Q]\CLmstr\PayTrans.h[cno]","[Q]\X."&session$&' -'&str$(myrln))
		fnFree("[Q]\CLmstr\PayTrans.h[cno]")
		fnRename("[Q]\X."&session$,"[Q]\CLmstr\PayTrans.h[cno]")
	end if 
	x=1 : if tmpkps(x)<>1 then 
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	end if 
	x=1 : if tmpkln(x)<>20 then 
		pr 'Key Length ('&str$(x)&') Error in '&kfname$
		pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	end if 
	! 
	! CL_PAYTRANS2: ! seconday 3-Split Index
	name$="[Q]\CLmstr\PayTrans.h[cno]"
	kfname$="[Q]\CLmstr\UnPdIdx2.h[cno]"
	if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
		fnindex_it(name$,kfname$,'31/27/1 2/4/26')
	end if 
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
	fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	x=1 : if tmpkps(x)<>31 then 
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	end if 
	x=2 : if tmpkps(x)<>27 then 
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	end if 
	x=3 : if tmpkps(x)<>1 then 
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	end if 
	x=1 : if tmpkln(x)<>2 then 
		pr 'Key Length ('&str$(x)&') Error in '&kfname$
		pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	end if 
	x=2 : if tmpkln(x)<>4 then 
		pr 'Key Length ('&str$(x)&') Error in '&kfname$
		pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	end if 
	x=3 : if tmpkln(x)<>26 then 
		pr 'Key Length ('&str$(x)&') Error in '&kfname$
		pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	end if 
	! 
	CL_PAYMSTR1: ! Primary Non-Split Index
	name$="[Q]\CLmstr\PayMstr.h[cno]"
	kfname$="[Q]\CLmstr\PayIdx1.h[cno]"
	myrln=736
	version_proper=1
	fn_make_data_file_exist(name$,myrln,version_proper)
	if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
		fnindex_it(name$,kfname$,'1 8')
	end if 
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
	fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmpversion=0 or tmpversion=-1 then 
		fnpaymstr_v0_to_v1 : goto CL_PAYMSTR1
	end if 
	if tmprln<>myrln then 
		pr 'Record Length Error in File: '&tmpfile$
		pr '         RLn: '&str$(tmprln)
	end if 
	x=1 : if tmpkps(x)<>1 then 
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	end if 
	x=1 : if tmpkln(x)<>8 then 
		pr 'Key Length ('&str$(x)&') Error in '&kfname$
		pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	end if 
	! CL_RECMSTR1: ! Primary Non-Split Index
	name$="[Q]\CLmstr\RecMstr.h[cno]"
	kfname$="[Q]\CLmstr\RecIdx1.h[cno]"
	myrln=38
	version_proper=1
	fn_make_data_file_exist(name$,myrln,version_proper)
	if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
		fnindex_it(name$,kfname$,'1 8')
	end if 
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
	fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmpversion=0 or tmpversion=-1 then 
		fnpaymstr_v0_to_v1
		goto CL_PAYMSTR1
	end if 
	if tmprln<>myrln then 
		pr 'Record Length Error in File: '&tmpfile$
		pr '         RLn: '&str$(tmprln)
	end if 
	x=1 : if tmpkps(x)<>1 then 
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	end if 
	x=1 : if tmpkln(x)<>8 then 
		pr 'Key Length ('&str$(x)&') Error in '&kfname$
		pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	end if 
	! 
	! CL_PAYMSTR2: ! Secondary, Non-Split Index
	name$="[Q]\CLmstr\PayMstr.h[cno]"
	kfname$="[Q]\CLmstr\PayIdx2.h[cno]"
	if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
		fnindex_it(name$,kfname$,'9 30')
	end if 
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
	fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	x=1 : if tmpkps(x)<>9 then 
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	end if 
	x=1 : if tmpkln(x)<>28 then 
		pr 'Key Length ('&str$(x)&') Error in '&kfname$
		pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	end if 
	! 
	! CL_GLMSTR1: ! Primary Non-Split Index
	name$="[Q]\CLmstr\GLmstr.h[cno]"
	kfname$="[Q]\CLmstr\GLIndex.h[cno]"
	myrln=62
	!   version_proper=0
	!   fn_make_data_file_exist(name$,myrln,version_proper)
	!   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then
	!     fnindex_it(name$,kfname$,'1 12')
	!   end if
	fn_file_setup_data("[Q]\CLmstr\GLmstr.h[cno]",62,1)
	fn_file_setup_index("[Q]\CLmstr\GLIndex.h[cno]",'1','12')
	open #tmp:=fngethandle: 'Name='&g_fs_name$&',KFName='&g_fs_kfname$&',Shr',internal,outIn,keyed 
	fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	!   if tmprln<>myrln then
	!     pr 'Record Length Error in File: '&tmpfile$
	!     pr '         RLn: '&str$(tmprln)
	!   end if
	if tmprln=72 or tmprln=80 then let fnglmstrtorecl62
	!   x=1 : if tmpkps(x)<>1 then
	!     pr 'Key Position ('&str$(x)&') Error in '&kfname$
	!     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	!   end if
	!   x=1 : if tmpkln(x)<>12 then
	!     pr 'Key Length ('&str$(x)&') Error in '&kfname$
	!     pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	!   end if
	! 
	! CL_PAYEEGLBREAKDOWN: ! Primary Non-Split Index
	fn_file_setup_data("[Q]\CLmstr\payeeglbreakdown.h[cno]",56,1)
	fn_file_setup_index("[Q]\CLmstr\payeeglbkdidx.h[cno]",'1','8')
	! CL_GLCONTROL: ! Primary Non-Split Index
	fn_file_setup_data("[Q]\CLmstr\fundmstr.h[cno]",75,0)
	fn_file_setup_index("[Q]\CLmstr\fundidx1.h[cno]",'1','3')
	open #tmp:=fngethandle: 'Name='&g_fs_name$&',KFName='&g_fs_kfname$&',Shr',internal,outIn,keyed 
	fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmprln=63 then let fnglcontrol
	! 
	fn_file_setup_data("[Q]\CLmstr\BankMstr.h[cno]",64,1)
	fn_file_setup_index("[Q]\CLmstr\BankIdx1.h[cno]",'1','2')
	!
	if ~exists('[Q]\CLmstr\TransactionType.dat') then
		open #hTransactionType:=fngethandle: "Name=[Q]\CLmstr\TransactionType.dat,Version=1,KFName=[Q]\CLmstr\TransactionType.Idx,Use,RecL=26,KPs=1,KLn=1,Shr",internal,outIn,keyed
		write #hTransactionType,using 'form pos 1,n 1,C 25': 1,'Check (Disbursment)'
		write #hTransactionType,using 'form pos 1,n 1,C 25': 2,'Deposit   (Receipt)'
		close #hTransactionType:
	end if
	if ~exists('[Q]\CLmstr\PayeeType.dat') then
		open #hPayeeType:=fngethandle: "Name=[Q]\CLmstr\PayeeType.dat,Version=1,KFName=[Q]\CLmstr\PayeeType.Idx,Use,RecL=27,KPs=1,KLn=2,Shr",internal,outIn,keyed
		write #hPayeeType,using 'form pos 1,n 2,C 25': 0,'Not Applicable'
		write #hPayeeType,using 'form pos 1,n 2,C 25': 7,'Non-Employee Compensation'
		close #hPayeeType:
	end if
fnend 
def fn_cfv_payroll
	if exists("[Q]\PRmstr")=0 then execute "MkDir [Q]\PRmstr"
	! if ~exists('[Q]\INI\Payroll') then execute 'mkdir "[Q]\INI\Payroll"'
	fn_ini_move(env$('cursys'))
	fnIniToReg
	fn_reg_rename(env$('cursys'))
	! r: move DDInfo.h into CReg and delete DDInfo.h
	if exists('[Q]\PRmstr\DDInfo.h[cno]') then 
		open #h_prDdinfo:=fngethandle: "Name=[Q]\PRmstr\DDInfo.h[cno],USE,RecL=256",internal,outIn,relative 
		if lrec(h_prDdinfo)>=1 then 
			dim ddinfo_path$*30
			dim ddinfo_bankaccount$*20
			dim ddinfo_bankrouting$*20
			dim ddinfo_federalrouting$*20
			dim ddinfo_fedid$*12
			dim ddinfo_bankname$*23
			dim ddinfo_banksaccount$*12
			read #h_prDdinfo,using "form pos 1,c 30,c 20,c 20,c 20,c 12,c 23,c 12",rec=1: ddinfo_path$,ddinfo_bankaccount$,ddinfo_bankrouting$,ddinfo_federalrouting$,ddinfo_fedid$,ddinfo_bankname$,ddinfo_banksaccount$ ioerr CfvPrDdInfoNoRec
			fncreg_write('Direct Deposit Save File Path',ddinfo_path$) ! The path should contain the drive designation, any folders and a file name. Eg  'A:\DirDep.txt'
			fncreg_write('Direct Deposit Source Bank Account',ddinfo_bankaccount$) ! The right hand set of numbers at the bottom of your checks.
			fncreg_write('Direct Deposit Source Bank Routing',ddinfo_bankrouting$) ! The middle set of numbers at the bottom of your checks.
			fncreg_write('Direct Deposit Federal Reserve Routing',ddinfo_federalrouting$) ! Routing Number of Federal Reserve Used by Your Bank
			fncreg_write('Direct Deposit Source Bank Name',ddinfo_bankname$)
			fncreg_write('Direct Deposit Federal ID Number',ddinfo_fedid$) ! The Federal ID number can be found on any payroll report.
		end if
		CfvPrDdInfoNoRec: ! 
		close #h_prDdinfo,free: 
	end if 
	! /r
	! r: move CheckInfo.h into CReg and delete checkinfo.h
	if exists('[Q]\PRmstr\Checkinfo.h[cno]') then 
		open #h_pr_checkinfo:=fngethandle: "Name=[Q]\PRmstr\Checkinfo.h[cno],USE,RecL=128",internal,outIn,relative 
		read #h_pr_checkinfo,using "form pos 1,3*c 1,c 3,c 1,n 3,c 5",rec=1: pre$,acsclcv$,ficam1$,sc1$,accr$,bankcode,compcode$ noRec CFVPR_CHECKINFO_NOREC
		fncreg_write('Prenumbered Checks',pre$)
		fncreg_write('Post to CL',acsclcv$)
		fncreg_write('Post Employer Portion of FiCA',ficam1$)
		fncreg_write('Check Format',sc1$)
		fncreg_write('Print Vacation and Sick Leave on Check',accr$)
		fncreg_write('CL Bank Code',str$(bankcode))
		fncreg_write('Comp Time Code',compcode$)
	CFVPR_CHECKINFO_NOREC: ! 
		close #h_pr_checkinfo,free: 
	end if 
	! /r
	! r: move rpDate.h into CReg and delete rpDate.h
	if exists('[Q]\PRmstr\rpDate.h[cno]') then 
		open #h_pr_rpdate:=fngethandle: "Name=[Q]\PRmstr\rpDate.h[cno]",internal,outIn,relative 
		dim cfvpr_rpdate_d$*20
		read #h_pr_rpdate,using 'Form POS 1,N 6,C 20': cfvpr_rpdate_ppd,cfvpr_rpdate_d$  noRec CFVPR_RPDATE_NOREC
		fncreg_write('calculation date',str$(cfvpr_rpdate_ppd)) ! quarter ending date, i think - definately NOT the payroll calculation date!
		fncreg_write('calculation date text',cfvpr_rpdate_d$) ! quarter ending date
	CFVPR_RPDATE_NOREC: ! 
		close #h_pr_rpdate,free: 
	end if 
	! /r
	fn_file_setup_data("[Q]\PRmstr\DeptName.h[cno]",32,0)
	fn_file_setup_index("[Q]\PRmstr\DeptNameIdx-idx.h[cno]",'1','3')
	! 
	fn_file_setup_data("[Q]\PRmstr\Department.h[cno]",149,0)
	fn_file_setup_index("[Q]\PRmstr\DeptIdx.h[cno]",'1/9','8/3')
	! 
	fn_file_setup_data("[Q]\PRmstr\dd.h[cno]",72,0)
	fn_file_setup_index("[Q]\PRmstr\DDidx1.h[cno]",'1','10')
	! 
	fn_file_setup_data("[Q]\PRmstr\mglmstr.h[cno]",135,0)
	fn_file_setup_index("[Q]\PRmstr\mglidx1-idx.h[cno]",'1','3')
	! 
	fn_file_setup_data("[Q]\PRmstr\HourBreakdown.h[cno]",39,0)
	fn_file_setup_index("[Q]\PRmstr\HourBreakdown-idx.h[cno]",'1/9/14','8/5/8')
	! r: Dates.h
	fn_file_setup_data("[Q]\PRmstr\Dates.h[cno]",76,0)
	open #tmp:=fngethandle: "Name=[Q]\PRmstr\Dates.h[cno],Use,RecL=76,Shr",internal,outIn,relative 
	read #tmp,using "form pos 1,6*n 8",rec=1: beg_date,end_date,qtr1,qtr2,qtr3,qtr4 noRec PR_WRITE_BLANK_DATE_REC
	goto PR_CLOSE_DATE
	PR_WRITE_BLANK_DATE_REC: ! 
	write #tmp,using "form pos 1,6*n 8",rec=1: beg_date,end_date,qtr1,qtr2,qtr3,qtr4
	PR_CLOSE_DATE: ! 
	close #tmp: 
	! /r
	fn_file_setup_data("[Q]\PRmstr\PayrollChecks.h[cno]",224,0)
	fn_file_setup_index("[Q]\PRmstr\CheckIdx.h[cno]",'1','17')
	fn_file_setup_index("[Q]\PRmstr\CheckIdx2.h[cno]",'9/12/1','3/6/8')
	fn_file_setup_index("[Q]\PRmstr\checkidx3.h[cno]",'1/12/9','8/6/3')
	! 
	! r: DedNames.h setup
	! if exists("[Q]\PRmstr\dednames.h[cno]")=0 then
	dim pr_dednames_fullname$(20)*20
	dim pr_dednames_abrevname$(20)*8
	dim pr_dednames_newcalcode(20)
	dim pr_dednames_newdedfed(20)
	dim pr_dednames_newdedcode(20)
	dim pr_dednames_dedfica(20)
	dim pr_dednames_dedst(20)
	dim pr_dednames_deduc(20)
	dim pr_dednames_gl$(20)*12
	open #h_dednames:=fngethandle: "Name=[Q]\PRmstr\dednames.h[cno],RecL=920,use",internal,outIn,relative 
	if lrec(h_dednames)=0 then 
		write #h_dednames,using 'form pos 1,20*c 20,20*c 8,120*n 1,20*c 12': mat pr_dednames_fullname$,mat pr_dednames_abrevname$,mat pr_dednames_newdedcode,mat pr_dednames_newcalcode,mat pr_dednames_newdedfed,mat pr_dednames_dedfica,mat pr_dednames_dedst,mat pr_dednames_deduc,mat pr_dednames_gl$
	end if 
	close #h_dednames: ioerr ignore
	! end if
	! /r
	PrGlindex: !
	open #h_tmp:=fngethandle: "Name=[Q]\PRmstr\GLMstr.h[cno],Version=0,KFName=[Q]\PRmstr\GLIndex.h[cno],Use,RecL=62,KPs=1,KLn=12,Shr",internal,outIn,keyed ioerr Check4124OnPrGlindex
	close #h_tmp: 
	! 
	if ~exists('[Q]\PRmstr\EmpStatus.dat') then 
		open #h_pr_emp_status:=fngethandle: "Name=[Q]\PRmstr\EmpStatus.dat,KFName=[Q]\PRmstr\Empstatus.idx,Use,RecL=32,KPs=1,KLn=2,Shr",internal,outIn,keyed 
		write #h_pr_emp_status,using 'form pos 1,N 2,C 25': 9,'Terminated'
	end if 
	if exists('[Q]\PRmstr\RPMstr.h[cno]') and ~exists('[Q]\PRmstr\Employee.h[cno]') then
		fnAutomatedSavePoint('before RPMstr to Employee')  ! let's remove this after the first few rounds of testing...
		! r: Convert from RPMstr.h[cno] to Employee.h[cno]
		! only significant difference is that mat ta(2) has been removed to make room for w4step2 (initialized to 0 here)
		open #hIn  :=fngethandle: 'Name=[Q]\PRmstr\RPMstr.h[cno],NoShr',internal,outIn,relative
		open #hOut :=fngethandle: 'Name=[Q]\PRmstr\Employee.h[cno],version=0,KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],New,RecL=196,KPs=1,KLn=8,Shr',internal,outIn,keyed
		open #hOut2:=fngethandle: 'Name=[Q]\PRmstr\Employee.h[cno],version=0,KFName=[Q]\PRmstr\EmployeeIdx-name.h[cno],Use,RecL=196,KPs=9,KLn=30,Shr',internal,outIn,keyed
		
		dim em$(3)*30
		dim rs(2)
		dim em(16)
		dim ta(2)
		do
			read #hIn,using F_rpmstr: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd eof EoRpmstr
			F_rpmstr: form pos 1,n 8,3*c 30,c 11,2*n 1,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6,pd 5.2,2*pd 3,c 12,n 6
			write #hOut,using F_employee: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,0,'',ph$,bd
			F_employee: form pos 1,n 8,3*c 30,c 11,2*n 1,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6,pd 5.2,N 1,C 5,c 12,n 6
		loop
		EoRpmstr: !
		close #hIn:
		close #hOut:
		close #hOut2:
		if exists('[Q]\PRmstr\RPMstr-Old.h[cno]') then 
			pr 'something is wrong - please keep this window open and call support.'
			pause
		end if
		fnRename('[Q]\PRmstr\RPMstr.h[cno]','[Q]\PRmstr\RPMstr-Old-'&date$('ccyymmdd')&'-'&srep$(time$,':','')&'.h[cno]')
		fnFree('[Q]\PRmstr\RPIndex.h[cno]')
		fnFree('[Q]\PRmstr\RPIndx2.h[cno]')

		! /r
	end if
fnend 
Check4124OnPrGlindex: ! r:
 if err=4124 and (Check4124OnPrGlindexCount+=1)<=2 then
	 fnindex_it('[Q]\PRmstr\GLMstr.h[cno]','[Q]\PRmstr\GLIndex.h[cno]','1 12')
	 goto PrGlindex
 else
		fnStatus('Failure.')
		fnStatus('* Data File: PRmstr\GLMstr.h[cno]')
		fnStatus('* Index: PRmstr\GLIndex.h[cno]')
		fnStatus('* reindex completed however error 4124 persist.')
		fnStatusPause
 end if
 goto ERTN ! /r
def fn_cfv_job_cost_payroll
	! if ~exists('[Q]\INI\Payroll\Job Cost') then execute 'mkdir "[Q]\INI\Payroll\Job Cost"'
	fn_ini_move('JC')
	! r: JCMSTR.h
	fn_file_setup_data("[Q]\PRmstr\JCMSTR.h[cno]",300,0)
	fn_file_setup_index("[Q]\PRmstr\JCINDX.h[cno]",'1','6')
	fn_file_setup_index("[Q]\PRmstr\JCINDX2.h[cno]",'7','25')
	! /r
fnend 
def fn_cfv_general_ledger
	! General Ledger Only
	fn_file_setup_data("[Q]\GLmstr\ACTrans.h[cno]",72,0)
	fn_file_setup_index("[Q]\GLmstr\AcTrIdx.h[cno]",'1/71/17/13','12/2/2/4')
	!
	fn_file_setup_data("[Q]\GLmstr\GLTrans.h[cno]",73,0)
	!
	library 'S:\Core\Library': fnfinstmt_v0_to_v1,fnglmstr_338_416
	library 'S:\Core\Library': fnglpayee_v0_to_v1 
	if exists("[Q]\GLmstr")=0 then execute "MkDir [Q]\GLmstr"
	! if ~exists('[Q]\INI\General Ledger') then execute 'mkdir "[Q]\INI\General Ledger"'
	! if ~exists('[Q]\INI\General Ledger\Accountants') then execute 'mkdir "[Q]\INI\General Ledger\Accountants"'
	fn_ini_move(env$('cursys'))
	fnIniToReg
	fn_reg_rename(env$('cursys'))
	!
	if ~exists('[Q]\GLmstr\PayeeType.dat') then
		open #hPayeeType:=fngethandle: "Name=[Q]\GLmstr\PayeeType.dat,Version=1,KFName=[Q]\GLmstr\PayeeType.Idx,Use,RecL=27,KPs=1,KLn=2,Shr",internal,outIn,keyed
		write #hPayeeType,using 'form pos 1,n 2,C 25': 0,'Not Applicable'
		write #hPayeeType,using 'form pos 1,n 2,C 25': 7,'Non-Employee Compensation'
		close #hPayeeType:
	end if
	!
	! BudgetInfo: ! Primary Non-Split Index
	name$="[Q]\GLmstr\BudgetInfo.h[cno]"
	kfname$="[Q]\GLmstr\BudIndx.h[cno]"
	myrln=28
	version_proper=0
	fn_make_data_file_exist(name$,myrln,version_proper)
	if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
		fnindex_it(name$,kfname$,'1 14')
	end if 
	! GL_GLMSTR1: ! Primary Non-Split Index
	name$="[Q]\GLmstr\GLmstr.h[cno]"
	kfname$="[Q]\GLmstr\GLIndex.h[cno]"
	myrln=416
	version_proper=0
	fn_make_data_file_exist(name$,myrln,version_proper)
	if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
		fnindex_it(name$,kfname$,'1 12')
	end if 
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed : fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmprln<>myrln then 
		fnStatus('Record Length Error in File: '&tmpfile$)
		fnStatus('         RLn: '&str$(tmprln))
	end if 
	if tmprln=338 then let fnglmstr_338_416
	x=1 : if tmpkps(x)<>1 then 
		fnStatus('Key Position ('&str$(x)&') Error in '&kfname$)
		fnStatus('      KPs('&str$(x)&'): '&str$(tmpkps(x)))
	end if 
	x=1 : if tmpkln(x)<>12 then 
		fnStatus('Key Length ('&str$(x)&') Error in '&kfname$)
		fnStatus('      KLn('&str$(x)&'): '&str$(tmpkln(x)))
	end if 
	! 
	! GL_GLMSTR2: ! Secondary, Non-Split Index
	name$="[Q]\GLmstr\GLmstr.h[cno]"
	kfname$="[Q]\GLmstr\GLIndx2.h[cno]"
	if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
		fnindex_it(name$,kfname$,'13 30')
	end if 
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed : fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	x=1 : if tmpkps(x)<>13 then 
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	end if 
	x=1 : if tmpkln(x)<>30 then 
		pr 'Key Length ('&str$(x)&') Error in '&kfname$
		pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	end if 
	if tmpkln(x)=50 then 
		fnFree(kfname$)
		fnindex_it(name$,kfname$,'13 30')
	end if 
	! 
	fn_file_setup_data("[Q]\GLmstr\AcGLFnSc.h[cno]",83,1)
	fn_file_setup_index("[Q]\GLmstr\FnScIndx.h[cno]",'1','5')
	! r:  Six Files, with 1 primary index each
	!         acglfnsj, acglfnsi, acglfnsb, acglfnsc, acglfnsf, acglfnsg
	fn_file_setup_data("[Q]\GLmstr\acglfnsj.h[cno]",83,1)
	fn_file_setup_index("[Q]\GLmstr\Fnsjindx.h[cno]",'1','5')
	! r: GLmstr\acglfnsi
	name$="[Q]\GLmstr\acglfnsi.h[cno]"
	kfname$="[Q]\GLmstr\fnsiindx.h[cno]"
	fn_file_setup_data(name$,83,1)
	fn_file_setup_index(kfname$,'1','5')
	! myrln=83
	! version_proper=1
	! fn_make_data_file_exist(name$,myrln,version_proper)
	! if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
	!   fnindex_it(name$,kfname$,'1 5')
	! end if 
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed : fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmpversion=0 then let fnfinstmt_v0_to_v1
	if tmprln<>myrln then 
		pr 'Record Length Error in File: '&tmpfile$
		pr '         RLn: '&str$(tmprln)
	end if 
! If TMPRLN=81 OR TMPRLN=78 Then Let FNFINSTMT_v0_to_v1
	x=1 : if tmpkps(x)<>1 then 
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	end if 
	x=1 : if tmpkln(x)<>5 then 
		pr 'Key Length ('&str$(x)&') Error in '&kfname$
		pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	end if 
	!
	fn_file_setup_data("[Q]\GLmstr\acglfnsb.h[cno]",83,1)
	fn_file_setup_index("[Q]\GLmstr\Fnsbindx.h[cno]",'1','5')
	!
	fn_file_setup_data("[Q]\GLmstr\acglfnsb.h[cno]",83,1)
	fn_file_setup_index("[Q]\GLmstr\Fnsbindx.h[cno]",'1','5')
	!
	fn_file_setup_data("[Q]\GLmstr\acglfnsf.h[cno]",83,1)
	fn_file_setup_index("[Q]\GLmstr\Fnsfindx.h[cno]",'1','5')
	!
	fn_file_setup_data("[Q]\GLmstr\acglfnsg.h[cno]",83,1)
	fn_file_setup_index("[Q]\GLmstr\Fnsgindx.h[cno]",'1','5')
	!
	! /r
	! PAYEEGLBREAKDOWN: !
	name$="[Q]\GLmstr\payeeglbreakdown.h[cno]"
	kfname$="[Q]\GLmstr\Payeeglbkdidx.h[cno]"
	myrln=56
	version_proper=1
	fn_make_data_file_exist(name$,myrln,version_proper)
	L3510: ! 
	if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
		fnindex_it(name$,kfname$,'1 8')
	end if 
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmprln<>myrln then 
		pr 'Record Length Error in File: '&tmpfile$
		pr '         RLn: '&str$(tmprln)
	end if 
	x=1 : if tmpkps(x)<>1 then 
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
		pr 'fixing it'
		fnFree(kfname$)
		goto L3510
	end if 
	! GLPAYMSTR: ! Primary, Non-Split Index  (Vendor or payee files in g/l)
	if exists("[Q]\GLmstr\gl1099.h[cno]")<>0 then let fnglpayee_v0_to_v1
	if exists("[Q]\GLmstr\gl1099.h[cno]")<>0 then let fnFree("[Q]\GLmstr\gl1099.h[cno]")
	name$="[Q]\GLmstr\PayMstr.h[cno]"
	kfname$="[Q]\GLmstr\Payidx1.h[cno]"
	myrln=276
	version_proper=1
	fn_make_data_file_exist(name$,myrln,version_proper)
	L3600: ! 
	if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
		fnindex_it(name$,kfname$,'1 8') : fnglpayee_v0_to_v1
	end if 
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed : fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmprln<>myrln then 
		pr 'Record Length Error in File: '&tmpfile$
		pr '         RLn: '&str$(tmprln)
		fnglpayee_v0_to_v1
	end if 
	x=1 : if tmpkps(x)<>1 then 
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
		pr 'fixing it'
		fnFree(kfname$)
		goto L3600
	end if 
		! 
		! GLTR1099: ! Primary, Non-Split Index  (Vendor transactions)
	name$="[Q]\GLmstr\GlTr1099.h[cno]"
	kfname$="[Q]\GLmstr\gltridx1.h[cno]"
	myrln=64
	version_proper=1
	fn_make_data_file_exist(name$,myrln,version_proper)
	L3690: ! 
	if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
		fnindex_it(name$,kfname$,'1 8')
	end if 
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
	fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmprln<>myrln then 
		pr 'Record Length Error in File: '&tmpfile$
		pr '         RLn: '&str$(tmprln)
	end if 
	x=1 : if tmpkps(x)<>1 then 
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
		pr 'fixing it'
		fnFree(kfname$)
		goto L3690
	end if 
	! GLBREC: ! Primary, Non-Split Index
	name$="[Q]\GLmstr\Glbrec.h[cno]"
	kfname$="[Q]\GLmstr\glrecidx.h[cno]"
	myrln=68
	version_proper=1
	fn_make_data_file_exist(name$,myrln,version_proper)
	if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
	GlBrecIndex: ! 
		fnindex_it(name$,kfname$,'1 24')
	end if 
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed error GlBrecOpenErr
	fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmprln<>myrln then 
		pr 'Record Length Error in File: '&tmpfile$
		pr '         RLn: '&str$(tmprln)
	end if 
	x=1 : if tmpkps(x)<>1 then 
		fnStatus('Key Position ('&str$(x)&') Error in '&kfname$)
		fnStatus('      KPs('&str$(x)&'): '&str$(tmpkps(x)))
		fnStatus('fixing it')
		goto GlBrecIndex
	end if 
	goto GlBrecFinis
	GlBrecOpenErr: !
	if err=0632 then
		 goto GlBrecIndex
	else
		 goto ERTN
	end if
	GlBrecFinis: !
	! 
	! SCHEDULE: ! Primary, Non-Split Index  (General ledger schedules)
	dim sn$*78,ft$*78,gl$(80)*12
	name$="[Q]\GLmstr\acglschs.h[cno]"
	kfname$="[Q]\GLmstr\schindex.h[cno]"
	myrln=162
	version_proper=1
	fn_make_data_file_exist(name$,myrln,version_proper)
	L3870: fnindex_it(name$,kfname$,'1 3')
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed : fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmprln<>myrln then 
		pr 'Record Length Error in File: '&tmpfile$
		pr '         RLn: '&str$(tmprln)
	else 
		goto L4050
	end if 
	open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
	L3920: read #tmp, using "Form POS 1,N 2,2*C 78,3*N 1,80*C 12": sn,sn$,ft$,dp,rs,cm,mat gl$ eof EO_TMP conv L9000
	if sn=0 then goto L3920
	rewrite #tmp, using "Form POS 1,N 3,2*C 78,3*N 1": sn,sn$,ft$,dp,rs,cm
	if exists("[Q]\GLmstr\schedule"&str$(sn)&".h[cno]")=0 then open #schedule:=fngethandle: "Name=[Q]\GLmstr\schedule"&str$(sn)&".h[cno],KFName=[Q]\GLmstr\schedule_idx"&str$(sn)&".h[cno]"&',replace,RecL=12,kps=1,kln=12,Shr',internal,outIn,keyed: version(schedule,1): close #schedule: 
	if exists("[Q]\GLmstr\schedule_idx"&str$(sn)&".h[cno]")=0 then 
		fnindex_it("[Q]\GLmstr\schedule"&str$(sn)&".h[cno]","[Q]\GLmstr\schedule_idx"&str$(sn)&".h[cno]","1 12")
	end if 
	open #schedule:=fngethandle: "Name=[Q]\GLmstr\schedule"&str$(sn)&".h[cno],KFName=[Q]\GLmstr\schedule_idx"&str$(sn)&".h[cno]"&',use,RecL=12,kps=1,kln=12,Shr',internal,outIn,keyed: version(schedule,1) ! open to update gl breakdowns
	for j=1 to 80
		if val(gl$(j))=0 then goto L4010
		write #schedule,using "form pos 1,c 12": gl$(j)
	L4010: next j
	close #schedule: 
	goto L3920
	EO_TMP: ! 
	close #tmp:
	x=1 : if tmpkps(x)<>1 then 
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
		pr 'fixing it'
		fnFree(kfname$)
		goto L3870
	end if 
	L4050: ! 
	! 
	! r: these functions hav their own conversions and only need to be called to launch
	library 'S:\Core\Library': fnfscode,fnpedat$,fnpriorcd,fnpgnum,fnrx,fnstyp,fnps
	fnfscode
	fnpedat$
	fnpriorcd
	fnpgnum
	fnrx
	fnstyp
	fnps
	! /r
	! 
	if ~exists('[Q]\GLmstr\Period.h[cno]') then
		open #hGlPeriod:=fngethandle: "Name=[Q]\GLmstr\Period.h[cno],Version=1,KFName=[Q]\GLmstr\Period-Idx.h[cno],Use,RecL=35,KPs=1,KLn=2,Shr",internal,outIn,keyed 
		for periodRecord=1 to 12
			write #hGlPeriod,using 'form pos 1,N 2,C 30': periodRecord,date$(days(cnvrt$('pic(##)',periodRecord)&'0117','mmddyy'),'month')
		nex periodRecord
		write #hGlPeriod,using 'form pos 1,N 2,C 30': 13,'End of Year Adjustments'
		close #hGlPeriod:
	end if
	! 
	if exists("[Q]\GLmstr\GLWK1"&wsid$&".h[cno]") and ~exists("[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno]") then
		if fncopy("[Q]\GLmstr\GLWK1"&wsid$&".h[cno]","[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno]") then
			fnFree("[Q]\GLmstr\GLWK1"&wsid$&".h[cno]")
		end if
	end if
	!
	if exists("[Q]\GLmstr\GLWK1"&wsid$&".dat") and ~exists("[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".dat") then
		if fncopy("[Q]\GLmstr\GLWK1"&wsid$&".dat","[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".dat") then
			fnFree('[Q]\GLmstr\GLWK1'&wsid$&'.dat')
		end if
	end if
	! 
	! 
fnend 
L9000: ! r: skip bad schedule records
	reread #tmp, using "Form POS 1,c 2": a$ eof EO_TMP ioerr ignore
goto L3920 ! /r
def fn_ini_move(cursys$*2)
	dim imProgramOld$(0)*256
	dim imProgramNew$(0)*256
	imProgramCount=0
	mat imProgramOld$(imProgramCount)
	mat imProgramNew$(imProgramCount)
	if cursys$='PR' then
		fn_programMoveAdd('acsPR\newprfm','Payroll\Employee')
		fn_programMoveAdd('acsPR\newprcalk','Payroll\Calculation')
		fn_programMoveAdd('acsPR\newprRevCal','Payroll\Reverse Calculation')
		fn_programMoveAdd('acsPR\newprchangedate','Payroll\Change Payroll Dates')
		fn_programMoveAdd('acsPR\newprinput','Payroll\Enter Time Sheets')
		fn_programMoveAdd('acsPR\newprCkPrt','Payroll\Print Payroll Checks')
		if fnclient_has('P2') then 
			fn_programMoveAdd('acsPR\Category','Payroll\Job Cost\Category')
		end if
	else if cursys$='UB' then
		fn_programMoveAdd('acsUB\ubfm','Utility Billing\Customer')
		fn_programMoveAdd('acsUB\ubipcoll','Utility Billing\Collections')
		fn_programMoveAdd('acsUB\ubPDTnOf','Utility Billing\Past Due Turn Off List')
		fn_programMoveAdd('acsUB\ubUsage','Utility Billing\Usage Report')
		fn_programMoveAdd('acsUB\ubIpChg','Utility Billing\Enter Readings and Charges')
		fn_programMoveAdd('acsUB\workOrderAdd','Utility Billing\Work Order Add')
		fn_programMoveAdd('acsUB\workOrderList','Utility Billing\Work Order List')
		fn_programMoveAdd('acsUB\ubBilJrn','Utility Billing\Billing Journal')
		fn_programMoveAdd('acsUB\ubRate','Utility Billing\Rates')
		fn_programMoveAdd('acsUB\BkDraft','Utility Billing\Create Bank Draft File')
		fn_programMoveAdd('acsUB\ubpencal','Utility Billing\Penalty Calculation')
		fn_programMoveAdd('acsUB\ubUnbill','Utility Billing\Unbilled Customer Listing')
		fn_programMoveAdd('acsUB\FlexTran','Utility Billing\Transactions')
		fn_programMoveAdd('acsUB\ubNoUsage','Utility Billing\Zero Usage Report')
		fn_programMoveAdd('acsUB\Per1000','Utility Billing\Per 1000 Usage')
		fn_programMoveAdd('acsUB\PrintBill','Utility Billing\Print Bills')
		fn_programMoveAdd('acsUB\analyze','Utility Billing\Rate Analysis')
		fn_programMoveAdd('acsUB\label','Utility Billing\Labels')
	else if cursys$='GL' then
		fn_programMoveAdd('acsGL\glPrt109','General Ledger\Print 1099 Forms')
		fn_programMoveAdd('acsGL\acGLClos','General Ledger\Close Books at Year End')
		fn_programMoveAdd('acsGL\CloseMonth','General Ledger\Close Month')
		fn_programMoveAdd('acsGL\AcGLAcTB','General Ledger\Print Accumulated Trial Balance')
		fn_programMoveAdd('acsGL\GLInput','General Ledger\Enter Transactions')
		fn_programMoveAdd('acsGL\AcGlInc4','General Ledger\Four Column Budget Income Statement')
		fn_programMoveAdd('acsGL\ACGLTB','General Ledger\Trial Balance')
		fn_programMoveAdd('acsGL\AcGLBalC','General Ledger\Comparative Balance Sheet')
		fn_programMoveAdd('acsGL\AcGLBalY','General Ledger\Period Comparison Balance Sheet')
		if fnclient_has('G2') then ! Accountant's General Ledger Add-On
			fn_programMoveAdd('acsGL\AcPrReg','General Ledger\Accountants\Print Payroll Registers')
			fn_programMoveAdd('acsGL\Employee','General Ledger\Accountants\Employee')
		end if
	else if cursys$='CL' then
		fn_programMoveAdd('acsCL\Transaction','Checkbook\Transaction')
		fn_programMoveAdd('acsCL\cl1099','Checkbook\Print 1099 Forms')
		fn_programMoveAdd('acsCL\payee','Checkbook\Payee')
		fn_programMoveAdd('acsCL\UnpaidInvoice','Checkbook\Unpaid Invoice')
	end if
	dim favData$(0)*128,favDataN(0)
	hFavProgram=fn_open('CO Favorites', mat favData$, mat favDataN, mat form$, 0, 2)
	for imItem=1 to imProgramCount
		dim imIniFrom$*256
		dim imIniTo$*256
		dim imbrFrom$*256
		dim imbrTo$*256
		imBrFrom$='S:\'&imProgramOld$(imItem)&'.br'
		imBrTo$  ='S:\'&imProgramNew$(imItem)&'.br'
		imIniFrom$=fn_programIniFileName$(imProgramOld$(imItem), 1)
		imIniTo$  =fn_programIniFileName$(imProgramNew$(imItem), 1)
			 ! if pos(lwrc$(imBrFrom$),'acglbalc')>0 then pause
! if pos(lwrc$(imIniFrom$),'ubipcoll')>0 and env$('acsdeveloper')<>'' then pause
		if ~(imIniFrom$='S:\Core\Default\Program.ini' or imIniTo$='S:\Core\Default\Program.ini') then
			if exists(imIniFrom$) and ~exists(imIniTo$) then 
				if fnCopy(imIniFrom$,imIniTo$)>0 then
					fnFree(imIniFrom$)
				end if
			end if
		end if
		fnKeyChange(hFavProgram,'form pos '&str$(kps(hFavProgram))&',C '&str$(kln(hFavProgram)),imBrFrom$,imBrTo$)
	nex imItem
	hFavProgram=fnCloseFile(hFavProgram,'CO Favorites')
fnend
def fn_programMoveAdd(programNameOld$*256,programNameNew$*256)
	fnAddOneC(mat imProgramOld$,programNameOld$)
	imProgramCount=fnAddOneC(mat imProgramNew$,programNameNew$)
fnend
def fn_reg_rename(cursys$*2)
	if cursys$='PR' then
		! nuffin yet
		if fnclient_has('P2') then 
			! nuffin yet
		end if
	else if cursys$='UB' then
		! fn_rrOne('acsUB\ubDepChg','Utility Billing\Deposit Change List')  <-- just wrong - only with program caption name changes, not file name changes.
	else if cursys$='GL' then
		! nuffin yet
		if fnclient_has('G2') then ! Accountant's General Ledger Add-On
			! nuffin yet
		end if
	else if cursys$='CL' then
	end if
fnend
def fn_rrOne(from$*256,to$*256)
	if ~rr1Setup then 
		rr1Setup=1
		if env$('ACSDeveloper')<>'' then
			library 'S:\Core\Library': fnsreg_rename
		else
			library 'S:\Core\Library': fnreg_rename
		end if
		dim property$(0)*128
		mat property$(0)
		fnAddOneC(mat property$,'Orientation' )
		fnAddOneC(mat property$,'Height'      )
		fnAddOneC(mat property$,'Width'       )
		fnAddOneC(mat property$,'Lines'       )
		fnAddOneC(mat property$,'FontSize'    )
		fnAddOneC(mat property$,'TopMargin'   )
		fnAddOneC(mat property$,'BottomMargin')
		fnAddOneC(mat property$,'LeftMargin'  )
		fnAddOneC(mat property$,'RightMargin' )
	end if
	for propertyItem=1 to udim(mat property$)
		if env$('ACSDeveloper')<>'' then
			fnsreg_rename(env$('cursys')&'.'&trim$(from$)&'.Print.'&property$(propertyItem),env$('cursys')&'.'&trim$(to$)&'.Print.'&property$(propertyItem))
		else
			fnreg_rename(env$('cursys')&'.'&trim$(from$)&'.Print.'&property$(propertyItem),env$('cursys')&'.'&trim$(to$)&'.Print.'&property$(propertyItem))
		
		end if
	nex propertyItem
fnend
def fn_programIniFileName$*256(pif_program$*256; doNotCreate)
	dim pif_return$*256
	pif_return$=''
	pif_program$=trim$(pif_program$)
		posDotBr=pos(pif_program$,'.br')
		if posDotBr>0 then pif_program$(posDotBr:posDotBr+2)=''
	pif_return$='[Q]\INI\'&pif_program$&'.ini'
	fn_programIniFileName$=pif_return$
fnend 
include: fn_open
include: ertn
