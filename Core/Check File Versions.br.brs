fn_setup
library program$: fnCheckFileVersion
fnTop(program$)
fnCheckFileVersion
Xit: fnXit

def library fnCheckFileVersion
	! Checks the File versions and calls conversion programs if necessary
	! This Fn is called from S:\Core\Program\Select Company.br and S:\Core\Company Import
	! this library function checks to make sure all file versions for the
	! current system are up to date - and runs the appropriate conversion  function if not
	if ~setup then fn_setup
	! there are other Library statements in this program - but they are placed in the section for which system they really belong to.

	dim tmpfile$*512,tmpkps(10),tmpkln(10),name$*512,kfname$*512
	dim kfnames$(1)*512

	fnStatus('Running fnCheckFileVersion for '&env$('cursys')&' and Company Number '&env$('cno')) ! XXX
	fn_cfv_addMissingFiles

	if env$('cursys')='GL' then
		fn_cfv_general_ledger
	else if env$('cursys')='PR' then
		fn_cfv_payroll
		if fnClientHas('P2') then
			fn_cfv_job_cost_payroll
		end if
	else if env$('cursys')='UB' then
		fn_cfv_utility_billing
	else if env$('cursys')='CL' then
		fn_cfv_checkbook
	else if env$('cursystem')='Client Billing' then
		fn_cfv_client_billing
	end if
	fnStatus('CheckFileVersion Completed')
fnend
def fn_cfv_addMissingFiles(; ___,path$*256,prog$*256,ext$*128,item)
	dim camf_filename$(0)*256
	mat camf_filename$(0)
	fnGetDir2('S:\'&fnSystemName$&'\mstr\',mat camf_filename$, '','*.h99999')
	for item=1 to udim(mat camf_filename$)
		fnGetPp(camf_filename$(item),path$,prog$,ext$)
		! if lwrc$(camf_filename$(item))='department' then pause
		if ~exists('[Q]\[CurSys]mstr\'&prog$&'.h[cno]') then
			fnCopy('S:\[cursystem]\mstr\'&camf_filename$(item),'[Q]\[cursys]mstr\'&prog$&'.h[cno]')
		end if
	next item
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
		fnIndex(g_fs_name$,fsi_kfname$,fsi_kps$&' '&fsi_kln$)
	end if
fnend
def fn_make_data_file_exist(name$*512,myrln,version_proper)
	if exists(name$)=0 then
		fnStatus('Creating new file: Name='&name$&',Shr,Use,RecL='&str$(myrln)&',Version='&str$(version_proper))
		open #tmp=fnH: 'Name='&name$&',Shr,Use,RecL='&str$(myrln)&',Version='&str$(version_proper),internal,outIn
		close #tmp:
	end if
fnend
def fn_check_indexes(name$*512,mat kfnames$,mat kps$,mat kln$)
	ci_return=1 ! function should return 1 if all indexes tested are fine or 0 if any tested fail
	for ci_item=1 to udim(mat kfnames$)
		str2mat(kps$(ci_item),mat ci_kps$,'/')
		str2mat(kln$(ci_item),mat ci_kln$,'/')
		CI_OPEN_IT: !
		open #h_ci_tmp=fnH: 'name='&name$&',KFName='&kfnames$(ci_item),i,i,k ioerr CI_OPEN_ERR

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

		next x
		close #h_ci_tmp:
	next ci_item
	goto CI_XIT
	CI_OPEN_ERR: !
	fnStatus('error '&str$(err)&' opening Name='&name$&',KFName='&kfnames$(ci_item))
	if err=607 or err=632 then
		fnStatus('indexing to fix it')
		fnIndex(g_fs_name$,fsi_kfname$,fsi_kps$&' '&fsi_kln$)
		goto CI_OPEN_IT
	else
		fnStatus('error unhandled')
	end if
	CI_XIT: !
	fn_check_indexes=ci_return
fnend
def fn_min_rln(mr_filename$*512,mr_rln_minimum)
	if fn_recl(mr_filename$)<mr_rln_minimum then
		fnCopy(mr_filename$,'',mr_rln_minimum)
	end if  !
fnend  ! fn_min_rln
def fn_recl(filename$*512; ___,hTmp,returnN)
	open #hTmp=fnH: 'Name='&filename$&',Shr',i,i
	returnN=rln(hTmp)
	close #hTmp:
	fn_recl=returnN
fnend
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

def fn_getFileInfo(name$*512,kfname$*512,mat tmpkps,mat tmpkln,&tmpversion,&tmprln,&tmpfile$; ___,returnN,hTmp)
	if kfname$='' then
		open #hTmp=fnH: 'Name='&name$&',Shr',i,outi,r error GetFileInfoFail
	else
		open #hTmp=fnH: 'Name='&name$&',KFName='&kfname$&',Shr',i,outIn,k error GetFileInfoFail
	end if

	mat tmpkps=(0)
	mat tmpkln=(0)
	tmpversion=version(hTmp)
	tmprln=rln(hTmp)
	tmpfile$=file$(hTmp)
	for j=1 to udim(tmpkps)
		tmpkps(j)=kps(hTmp,j)
		tmpkln(j)=kln(hTmp,j)
	next j

	close #hTmp:

	returnN=1
	goto GfiFinis
	GetFileInfoFail: !
		returnN=0
	goto GfiFinis
	GfiFinis: !
	fn_getFileInfo=returnN
fnend

def fn_cfv_client_billing
	! open #h_tmwk1=fnH:
	! fn_check_version(tmpversion,version_proper,'')
fnend
def fn_cfv_utility_billing
	if ~exists('[Q]\UBmstr') then execute 'MkDir "[Q]\UBmstr"'
	if ~exists('[Q]\UBmstr\UBdata') then execute 'MkDir "[Q]\UBmstr\UBdata"'
	if ~exists('[Q]\WorkOrder') then execute 'MkDir "[Q]\WorkOrder"'
	fn_reg_rename(env$('cursys'))
	! r: move ubBkNo.h into CReg and delete ubBkNo.h
	if exists('[Q]\UBmstr\ubBkNo.h[cno]') then
		open #h_ubbkno=fnH: 'Name=[Q]\UBmstr\ubBkNo.h[cno]',i,outi,r
		read #h_ubbkno,using 'form pos 1,2*N 3',rec=1: bkno1,bkno2  noRec CFVUB_RPDATE_NOREC
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
	name$='[Q]\UBmstr\Company.h[cno]'
	kfname$=''
	myrln=133
	version_proper=0
	if fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$) then
		fn_check_version(tmpversion,version_proper,tmpfile$)
		if tmprln<>myrln then
			fnStatus('Record Length Error in File: '&tmpfile$)
			fnStatus('         RLn: '&str$(tmprln))
			fnStatus('Fixing the Record Length of Company')
			fnCopy('[Q]\UBmstr\Company.h[cno]','',133)
		end if
	end if

	fn_file_setup_data('[Q]\UBmstr\Customer.h[cno]',2067,1)
	fn_file_setup_index('[Q]\UBmstr\ubIndex.h[cno]','1','10')
	fn_file_setup_index('[Q]\UBmstr\ubIndx2.h[cno]','354','7')
	fn_file_setup_index('[Q]\UBmstr\ubIndx3.h[cno]','11','30')
	fn_file_setup_index('[Q]\UBmstr\ubIndx4.h[cno]','41','30')
	fn_file_setup_index('[Q]\UBmstr\ubIndx5.h[cno]','1741/1743','2/7')

	fn_file_setup_data('[Q]\UBmstr\ubAdrBil.h[cno]',130,0)
	fn_file_setup_index('[Q]\UBmstr\AdrIndex.h[cno]','1','10')

	!     fn_file_setup_data('[Q]\UBmstr\Deposit1.h[cno]',16,0)
	!     fn_file_setup_index('[Q]\UBmstr\DepIdx1.h[cno]','1','10')
	fn_file_setup_data('[Q]\UBmstr\Deposit2.h[cno]',73,0)
	fn_file_setup_index('[Q]\UBmstr\Deposit2Index.h[cno]','1','10')

	fn_file_setup_data('[Q]\UBmstr\workOrder.h[cno]',600,0)
	fn_file_setup_index('[Q]\UBmstr\wkIndex.h[cno]','1/11','10/8')

	fn_file_setup_data('[Q]\UBmstr\MeterType.h[cno]',128,1)
	fn_file_setup_index('[Q]\UBmstr\MeterTypeIdx.h[cno]','1','5')
	version_meterType=fn_version('[Q]\UBmstr\MeterType.h[cno]')
	if version_meterType=1 then
		open #hMeterType=fnH: 'Name=[Q]\UBmstr\MeterType.h[cno],Shr',internal,outin
		F_meterType_v1: form pos 1,c 5,c 40,c 9,n 2,n 2
		F_meterType_v2: form pos 1,c 5,c 40,c 9,n 2,c 4
		dim name$*40
		do
			read #hMeterType,using F_meterType_v1: key$,name$,readingMultiplier$,dialCount,readType eof EoMeterTypeV1
			readType$=str$(readType)
			rewrite #hMeterType,using F_meterType_v2: key$,name$,readingMultiplier$,dialCount,readType$
		loop
		EoMeterTypeV1: !
		version(hMeterType,version_meterType:=2)
		close #hMeterType:
		fnIndex('[Q]\UBmstr\MeterType.h[cno]','[Q]\UBmstr\MeterTypeIdx.h[cno]','1 5u')
	end if
	if version_meterType=2 then
		open #hMeterType=fnH: 'Name=[Q]\UBmstr\MeterType.h[cno],Shr',internal,outin
		! F_meterType_v3: form pos 1,c 5,c 40,c 9,n 2,c 4,c 20
		dim u4_device$*40
		fnreg_read('Hand Held Device',u4_device$, fnhand_held_device$)
		if trim$(u4_device$)='[Ask]' then u4_device$='[All]'
		do
			read #hMeterType: eof EoMeterTypeV2
			rewrite #hMeterType,using 'form pos 61,c 20': u4_device$
		loop
		EoMeterTypeV2: !
		version(hMeterType,version_meterType:=3)
		close #hMeterType:
	end if
	if version_meterType=3 then
		open #hMeterType=fnH: 'Name=[Q]\UBmstr\MeterType.h[cno],Shr',internal,outin
		dim u4_device$*40
		do
			read #hMeterType: eof EoMeterTypeV3
			rewrite #hMeterType,using 'form pos 81,c 8,c 3': '',''
		loop
		EoMeterTypeV3: !
		version(hMeterType,version_meterType:=4)
		close #hMeterType:
	end if

	! no need, replaced by U4 Meter Location    !   fn_file_setup_data('[Q]\UBmstr\Meter.h[cno]',384,1)
	! no need, replaced by U4 Meter Location    !   fn_file_setup_index('[Q]\UBmstr\Meter_Idx.h[cno]','1/11','10/2')

	hMeterLocation=fnInitialializeMeterLocation
	fnCloseFile(hMeterLocation,'U4 Meter Location')

	if exists('[Q]\UBmstr\CityStZip.dat') then
		fnStatus('Migrating UB City State Zip records into Core City State Zip table...')
		open #hUbCsz=fnH: 'Name=[Q]\UBmstr\CityStZip.dat,KFName=[Q]\UBmstr\CityStZip.idx,Use,RecL=30,KPs=1,KLn=30,Shr',i,outIn,k
		dim cszData$(0)*128,cszDataN(0),csz$*30
		hCoCsz:=fn_openFio('CO City State Zip',mat cszData$,mat cszDataN)
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

	fn_rename('[Q]\UBmstr\Collections-[wsid].h[cno]','[Q]\UBmstr\Collections-[acsUserId].h[cno]')

	if exists('[Q]\UBmstr\IpChg01.h[cno]') then
		open #hupipchg=fnH: 'Name=[Q]\UBmstr\IpChg01.h[cno],RecL=80,Use',internal,outIn ioerr ubipchgOpenErr
		read #hupipchg,using 'form pos 1,N 6': d2 ioerr ignore
		close #hupipchg,free:
		for wsidItem=1 to 99
			fnFree('[Q]\UBmstr\IpChg'&cnvrt$('pic(##)',wsidItem)&'.h[cno]')
		nex wsidItem
		ubipchgOpenErr: !
		fncreg_write('Meter Reading Date Current',str$(d2))
	end if
	if exists('[Q]\UBmstr\per1000.h[cno]') then
		dim range(16)
		open #hPer1000=fnH: 'Name=[Q]\UBmstr\per1000.h[cno],Shr',i,outi,r
		read #hPer1000,using 'form pos 1,16*n 10,n 2,c 1': mat range,wrate,weg$
		fncreg_write('Per 1000 Usage - Rate Code ',weg$)
		fncreg_write('Per 1000 Usage - Service for Analysis ',str$(wrate))
		for rangeItem=1 to 16
			fncreg_write('Per 1000 Usage - Range '&str$(rangeItem),str$(range(rangeItem)))
		nex rangeItem
		close #hPer1000:
		fnFree('[Q]\UBmstr\per1000.h[cno]')
	end if

	fn_file_setup_data('[Q]\UBmstr\ubData\RateMst.h[cno]',374,0)
	fn_file_setup_index('[Q]\UBmstr\ubData\RateIdx1.h[cno]','1','4')
	fn_file_setup_index('[Q]\UBmstr\ubData\RateIdx2.h[cno]','5','25')

fnend
def fn_cfv_checkbook
	! Checkbook Only
	if ~exists('[Q]\CLmstr') then execute 'MkDir [Q]\CLmstr'
	fn_reg_rename(env$('cursys'))

	CL_TRMSTR1: ! Primary Non-Split Index
	fn_file_setup_data('[Q]\CLmstr\TrMstr.h[cno]',78,2)
	fn_file_setup_index('[Q]\CLmstr\TrIdx1.h[cno]','1','11')
	fn_getFileInfo(g_fs_name$,g_fs_kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	if ~fn_check_version(tmpversion,version_proper:=2,g_fs_name$) then
		if tmpversion=0 or tmpversion=-1 then
			fn_checkbookTrmstr_v0_to_v1
			goto CL_TRMSTR1
		end if
		if tmpversion=1 then
			fn_checkbookTrmstr_v1_to_v2
			goto CL_TRMSTR1
		end if
	end if
	! CL_TRMSTR2: ! Secondary Split Index
	fn_file_setup_index('[Q]\CLmstr\TrIdx2.h[cno]','28/1','8/11')
	! CL_TRMSTR3: ! Secondary 3-Split Index
	fn_file_setup_index('[Q]\CLmstr\TrIdx3.h[cno]','16/12/4','2/4/8')
	!
	CL_TRALLOC: ! 1 file and 1 key
	! (index file did not exists in previous versions,
	! so it'll build it if need be here, before it opens it)
	name$='[Q]\CLmstr\TrAlloc.h[cno]'
	kfname$='[Q]\CLmstr\TrAlloc-Idx.h[cno]'
	myrln=80
	version_proper=2
	fn_make_data_file_exist(name$,myrln,version_proper)
	if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
		fnIndex(name$,kfname$,'1 11')
	end if
	fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmpversion=1 or tmpversion=0 or tmpversion=-1 then
		fn_checkbookTrAlloc_v1_to_v2
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

	CL_UNPDALOC1: ! Primary, Non-Split Index
	! it is important that if conversion from version 1 to 2 occur on this
	! that this file process before PayTrans - the file it is linked to
	! So that record sequence is maintained.
	name$='[Q]\CLmstr\UnPdAloc.h[cno]'
	kfname$='[Q]\CLmstr\UAIdx1.h[cno]'
	myrln=67
	version_proper=2
	fn_make_data_file_exist(name$,myrln,version_proper)
	L1840: !
	if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
		fnIndex(name$,kfname$,'9 12')
	end if
	fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmpversion=1 or tmpversion=0 or tmpversion=-1 then
		fn_checkbookUnpdaloc_v1_to_v2
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

	! CL_UNPDALOC2: ! Secondary, Non-Split Index
	name$='[Q]\CLmstr\UnPdAloc.h[cno]'
	kfname$='[Q]\CLmstr\UAIdx2.h[cno]'
	if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
		fnIndex(name$,kfname$,'1 20')
	end if
	fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	x=1 : if tmpkps(x)<>1 then
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	end if
	x=1 : if tmpkln(x)<>20 then
		pr 'Key Length ('&str$(x)&') Error in '&kfname$
		pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	end if

	CL_PAYTRANS1: ! Primary Non-Split Index
	name$='[Q]\CLmstr\PayTrans.h[cno]'
	kfname$='[Q]\CLmstr\UnPdIdx1.h[cno]'
	myrln=114
	version_proper=2
	fn_make_data_file_exist(name$,myrln,version_proper)
	if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
		fnIndex(name$,kfname$,'1 20')
	end if
	fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmpversion=1 or tmpversion=0 or tmpversion=-1 then
		fn_checkbookPaytrans_v1_to_v2 : goto CL_PAYTRANS1
	end if
	if tmprln<>myrln then
		pr 'Record Length Error in File: '&tmpfile$
		pr '         RLn: '&str$(tmprln)
		pr 'Fixing it'
		fnCopy('[Q]\CLmstr\PayTrans.h[cno]','',myrln)
	end if
	x=1 : if tmpkps(x)<>1 then
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	end if
	x=1 : if tmpkln(x)<>20 then
		pr 'Key Length ('&str$(x)&') Error in '&kfname$
		pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	end if

	! CL_PAYTRANS2: ! seconday 3-Split Index
	name$='[Q]\CLmstr\PayTrans.h[cno]'
	kfname$='[Q]\CLmstr\UnPdIdx2.h[cno]'
	if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
		fnIndex(name$,kfname$,'31/27/1 2/4/26')
	end if
	fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
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

	CL_PAYMSTR1: ! Primary Non-Split Index
	name$='[Q]\CLmstr\PayMstr.h[cno]'
	kfname$='[Q]\CLmstr\PayIdx1.h[cno]'
	myrln=736
	version_proper=1
	fn_make_data_file_exist(name$,myrln,version_proper)
	if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
		fnIndex(name$,kfname$,'1 8')
	end if
	fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmpversion=0 or tmpversion=-1 then
		fn_checkbookPaymstr_v0_to_v1
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
	! CL_RECMSTR1: ! Primary Non-Split Index
	name$='[Q]\CLmstr\RecMstr.h[cno]'
	kfname$='[Q]\CLmstr\RecIdx1.h[cno]'
	myrln=38
	version_proper=1
	fn_make_data_file_exist(name$,myrln,version_proper)
	if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
		fnIndex(name$,kfname$,'1 8')
	end if
	fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmpversion=0 or tmpversion=-1 then
		fn_checkbookPaymstr_v0_to_v1
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

	! CL_PAYMSTR2: ! Secondary, Non-Split Index
	name$='[Q]\CLmstr\PayMstr.h[cno]'
	kfname$='[Q]\CLmstr\PayIdx2.h[cno]'
	if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
		fnIndex(name$,kfname$,'9 30')
	end if
	fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	x=1 : if tmpkps(x)<>9 then
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	end if
	x=1 : if tmpkln(x)<>28 then
		pr 'Key Length ('&str$(x)&') Error in '&kfname$
		pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	end if

	! CL_GLMSTR1: ! Primary Non-Split Index
	name$='[Q]\CLmstr\GLmstr.h[cno]'
	kfname$='[Q]\CLmstr\GLIndex.h[cno]'
	myrln=62
	!   version_proper=0
	!   fn_make_data_file_exist(name$,myrln,version_proper)
	!   if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
	!     fnIndex(name$,kfname$,'1 12')
	!   end if
	fn_file_setup_data('[Q]\CLmstr\GLmstr.h[cno]',62,1)
	fn_file_setup_index('[Q]\CLmstr\GLIndex.h[cno]','1','12')
	fn_getFileInfo(g_fs_name$,g_fs_kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	!   if tmprln<>myrln then
	!     pr 'Record Length Error in File: '&tmpfile$
	!     pr '         RLn: '&str$(tmprln)
	!   end if
	if tmprln=72 or tmprln=80 then
		fnCopy('[Q]\CLmstr\GLmstr.h[cno]','[Q]\CLmstr\GLmstr.h[cno]',62)
		fnRemoveDeletedRecords('[Q]\CLmstr\GLmstr.h[cno]')
		fnIndex('[Q]\CLmstr\GLmstr.h[cno]','[Q]\CLmstr\GLIndex.h[cno]','1,12')
	end if
	!   x=1 : if tmpkps(x)<>1 then
	!     pr 'Key Position ('&str$(x)&') Error in '&kfname$
	!     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	!   end if
	!   x=1 : if tmpkln(x)<>12 then
	!     pr 'Key Length ('&str$(x)&') Error in '&kfname$
	!     pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	!   end if

	! CL_PAYEEGLBREAKDOWN: ! Primary Non-Split Index
	fn_file_setup_data('[Q]\CLmstr\payeeglbreakdown.h[cno]',56,1)
	fn_file_setup_index('[Q]\CLmstr\payeeglbkdidx.h[cno]','1','8')
	! CL_GLCONTROL: ! Primary Non-Split Index
	fn_file_setup_data('[Q]\CLmstr\fundmstr.h[cno]',75,0)
	fn_file_setup_index('[Q]\CLmstr\fundidx1.h[cno]','1','3')
	fn_getFileInfo(g_fs_name$,g_fs_kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmprln=63 then
		fnCopy('[Q]\CLmstr\FundMstr.h[cno]','',75)
		fnIndex('[Q]\CLmstr\FundMstr.h[cno]','[Q]\CLmstr\fundidx1.h[cno]','1,3')
	end if

	fn_file_setup_data('[Q]\CLmstr\BankMstr.h[cno]',64,1)
	fn_file_setup_index('[Q]\CLmstr\BankIdx1.h[cno]','1','2')
	if ~exists('[Q]\CLmstr\PayeeType.dat') then
		open #hPayeeType=fnH: 'Name=[Q]\CLmstr\PayeeType.dat,Version=1,KFName=[Q]\CLmstr\PayeeType.Idx,Use,RecL=27,KPs=1,KLn=2,Shr',i,outIn,k
		write #hPayeeType,using 'form pos 1,n 2,C 25': 0,'Not Applicable'
		write #hPayeeType,using 'form pos 1,n 2,C 25': 7,'Non-Employee Compensation'
		close #hPayeeType:
	end if

	if exists('[Q]\CLmstr\PostDat.h[cno]') then
		fnFree('[Q]\CLmstr\PostDat.h[cno]')
	end if

fnend
	def fn_checkbookUnpdaloc_v1_to_v2(; ___,unpdaloc,vn$*8,iv$*12,j)
			fnStatus('Payment Allocation file until it is updating from v1 to v2')
			open #unpdaloc=1: 'Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\UAIdx1.h[cno]',i,outIn,k
			if version(unpdaloc)=2 then
				fnStatus('UnPdAloc is already version 2')
			else
				version(unpdaloc,2)
				close #unpdaloc:
				fnCopy('[Q]\CLmstr\UnPdAloc.h[cno]','[Q]\CLmstr\UnPdAloc.h[cno]',67) ! change the record length
				! r: make sure the Key is right justified
				open #unpdaloc=1: 'Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\UAIdx1.h[cno]',i,outIn,k
				for j=1 to lrec(unpdaloc)
					read #unpdaloc,using 'form pos 1,C 8,c 12',rec=j: vn$,iv$ noRec L330
					vn$=lpad$(rtrm$(vn$),8): iv$=lpad$(rtrm$(iv$),12)
					rewrite #unpdaloc,using 'form pos 1,Cr 8,c 12',rec=j: vn$,iv$
					L330: !
				next j
				close #unpdaloc:
				! /r
				fnIndex('[Q]\CLmstr\UnPdAloc.h[cno]','[Q]\CLmstr\UAIdx1.h[cno]','9 12')
				fnIndex('[Q]\CLmstr\UnPdAloc.h[cno]','[Q]\CLmstr\UAIdx2.h[cno]','1 20')
			end if
	fnend
	def fn_checkbookPaytrans_v1_to_v2(; ___,paytrans1,j,gde,pdte,disamt,ddate)
		open #paytrans1=1: 'Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno]',i,outIn,k
		if version(paytrans1)=2 then
			fnStatus('PayTrans is already version 2')
		else
			fnStatus('updating PayTrans from v1 to v2')
			close #paytrans1:

			! change the record length of the file
			fnCopy('[Q]\CLmstr\PayTrans.h[cno]','[Q]\CLmstr\PayTrans.h[cno]',114)
			open #paytrans1=1: 'Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno]',i,outIn,k
			for j=1 to lrec(paytrans1)
				read #paytrans1,using 'form pos 96,N 1,N 6': gde,pdte eof L320
				rewrite #paytrans1,using 'form pos 90,N 1,N 6,N 10.2,N 8': gde,pdte,disamt=0,ddate=0
			next j
			L320: !
			version(paytrans1,2)
			close #paytrans1:
			fnIndex('[Q]\CLmstr\PayTrans.h[cno]','[Q]\CLmstr\UnPdIdx1.h[cno]','1 20')
			fnIndex('[Q]\CLmstr\PayTrans.h[cno]','[Q]\CLmstr\UnPdIdx2.h[cno]','31/27/1 2/4/26')
		end if
	fnend
	def fn_checkbookPaymstr_v0_to_v1(;___,paymstr,payalloc,payeegl,vn$*8,de$*30,gl$*12,pct)
		open #paymstr=fnH: 'Name=[Q]\CLmstr\PayMstr.h[cno]',i,outi,r
		if version(paymstr)=1 then

			fnStatus('PayMstr is already version 1')
		else
			fnStatus('updating PayMstr:  v0 to v1')
			close #paymstr:
			fnCopy('[Q]\CLmstr\PayMstr.h[cno]','[Q]\CLmstr\PayMstr.h[cno]',736)
			fnIndex('[Q]\CLmstr\PayMstr.h[cno]','[Q]\CLmstr\PayIdx2.h[cno]','9 30')
			open #paymstr1=fnH: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno]',i,outIn,k
			open #paymstr2=fnH: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx2.h[cno]',i,outIn,k
			version(paymstr1,1)
			open #payalloc=fnH: 'Name=[Q]\CLmstr\PayAlloc.h[cno]',i,i,r ioerr EO_PAYALLOC
			open #payeegl=fnH: 'Name=[Q]\CLmstr\payeeglbreakdown.h[cno],KFName=[Q]\CLmstr\Payeeglbkdidx.h[cno],Use,RecL=56,KPs=1,KLn=8,Shr',i,outIn,k
			version(payeegl,1)
			do
				read #payalloc,using 'form pos 1,C 8,C 12,PD 3.2,C 30': vn$,gl$,pct,de$ eof EO_PAYALLOC
				write #payeegl,using 'form pos 1,C 8,C 12,n 6.2,C 30': vn$,gl$,pct,de$
			loop

			EO_PAYALLOC: !
			version(paymstr1,1)
			close #paymstr1: ioerr ignore
			close #paymstr2: ioerr ignore
			close #payeegl: ioerr ignore
			close #payalloc,free: ioerr ignore
			fnIndex('[Q]\CLmstr\PayMstr.h[cno]','[Q]\CLmstr\Paydx1.h[cno]','1 8')
			fnIndex('[Q]\CLmstr\PayMstr.h[cno]','[Q]\CLmstr\PayIdx2.h[cno]','9 30')
			fnIndex('[Q]\CLmstr\payeeglbreakdown.h[cno]','[Q]\CLmstr\payeeglbkdidx.h[cno]','1 8')
		end if
	fnend
	def fn_checkbookTrAlloc_v1_to_v2(; ___,tralloc)
		fnStatus('Updating Checkbook Transaction Allocation from v1 to v2')
		! fnwait(message$='Converting: please wait...',0)
		fnIndex('[Q]\CLmstr\TrAlloc.h[cno]','[Q]\CLmstr\TrAlloc-Idx.h[cno]','1 11')
		open #tralloc=fnH: 'Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno]',i,outIn,k
		close #tralloc:
		open #tralloc: 'Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno]',i,outIn,k
		if version(tralloc)=2 then fnStatus('TrAlloc is already version 2') : goto Tralloc1to2_Finis
		version(tralloc,2)
	goto Tralloc1to2_Finis

	Tralloc1to2_Finis: !
		close #tralloc:
		fnCopy('[Q]\CLmstr\TrAlloc.h[cno]','[Q]\CLmstr\TrAlloc.h[cno]',80)
		fnIndex('[Q]\CLmstr\TrAlloc.h[cno]','[Q]\CLmstr\TrAlloc-Idx.h[cno]','1 11')
fnend
	def fn_checkbookTrmstr_v0_to_v1(; ___,hTr,pause$,amt,j)
		! converts the CL TRmstr file to version 1
		! meainging the amount changes from G 10.2 to PD 10.2
		fnStatus('Checkbook update Trans to v1: Updating Transaction file.')
		open #hTr=fnH: 'Name=[Q]\CLmstr\TrMstr.h[cno]',i,outi,r
		if version(hTr)=1 then
		else
			version(hTr,1)
			for j=1 to lrec(hTr)
				read #hTr,using 'form pos 18,n 10.2': amt eof L240
				rewrite #hTr,using 'form pos 18,pd 10.2': amt
			next j
			L240: !
			close #hTr:
		end if
	fnend
	def fn_checkbookTrmstr_v1_to_v2(; ___,hTr)
		! converts the CL TRmstr file from version 1 to Version 2
		fnStatus('Checkbook update Trans from v1 to v2')

		open #hTr=fnH: 'Name=[Q]\CLmstr\TRmstr.h[cno]',i,outi,r
		if version(hTr)<>1 and version(hTr)<>2 then
			fnStatus('TRmstr is not version 1.  You must update it to version 1 before running this conversion program')
		else if version(hTr)=2 then
			fnStatus('TRmstr is already version 2')
		else
			version(trmstr,2)
		end if
		close #hTr: ioerr ignore
		fnCopy('[Q]\CLmstr\TRmstr.h[cno]','[Q]\CLmstr\TRmstr.h[cno]',78)
		fnIndex('[Q]\CLmstr\TrMstr.h[cno]','[Q]\CLmstr\TrIdx1.h[cno]','1 11')
		fnIndex('[Q]\CLmstr\TrMstr.h[cno]','[Q]\CLmstr\TrIdx2.h[cno]','28/1 8/11')
	fnend
def fn_cfv_payroll
	if ~exists('[Q]\PRmstr') then execute 'MkDir [Q]\PRmstr'
	fn_reg_rename(env$('cursys'))
	! r: move DDInfo.h into CReg and delete DDInfo.h
	if exists('[Q]\PRmstr\DDInfo.h[cno]') then
		open #h_prDdinfo=fnH: 'Name=[Q]\PRmstr\DDInfo.h[cno],USE,RecL=256',i,outi,r
		if lrec(h_prDdinfo)>=1 then
			dim ddinfo_path$*30
			dim ddinfo_bankaccount$*20
			dim ddinfo_bankrouting$*20
			dim ddinfo_federalrouting$*20
			dim ddinfo_fedid$*12
			dim ddinfo_bankname$*23
			dim ddinfo_banksaccount$*12
			read #h_prDdinfo,using 'form pos 1,c 30,c 20,c 20,c 20,c 12,c 23,c 12',rec=1: ddinfo_path$,ddinfo_bankaccount$,ddinfo_bankrouting$,ddinfo_federalrouting$,ddinfo_fedid$,ddinfo_bankname$,ddinfo_banksaccount$ ioerr CfvPrDdInfoNoRec
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
		open #h_pr_checkinfo=fnH: 'Name=[Q]\PRmstr\Checkinfo.h[cno],USE,RecL=128',i,outi,r
		read #h_pr_checkinfo,using 'form pos 1,3*c 1,c 3,c 1,n 3,c 5',rec=1: pre$,posttoCheckbook$,ficam1$,sc1$,accr$,bankcode,compcode$ noRec CFVPR_CHECKINFO_NOREC
		fncreg_write('Prenumbered Checks',pre$)
		fncreg_write('Post to CL',posttoCheckbook$)
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
		open #h_pr_rpdate=fnH: 'Name=[Q]\PRmstr\rpDate.h[cno]',i,outi,r
		dim cfvpr_rpdate_d$*20
		read #h_pr_rpdate,using 'form pos 1,N 6,C 20': cfvpr_rpdate_ppd,cfvpr_rpdate_d$  noRec CFVPR_RPDATE_NOREC
		fncreg_write('calculation date',str$(cfvpr_rpdate_ppd)) ! quarter ending date, i think - definately NOT the payroll calculation date!
		fncreg_write('calculation date text',cfvpr_rpdate_d$) ! quarter ending date
	CFVPR_RPDATE_NOREC: !
		close #h_pr_rpdate,free:
	end if
	! /r
	fn_file_setup_data('[Q]\PRmstr\DeptName.h[cno]',32,0)
	fn_file_setup_index('[Q]\PRmstr\DeptNameIdx-idx.h[cno]','1','3')

	fn_file_setup_data('[Q]\PRmstr\Department.h[cno]',149,0)
	fn_file_setup_index('[Q]\PRmstr\DeptIdx.h[cno]','1/9','8/3')

	fn_file_setup_data('[Q]\PRmstr\dd.h[cno]',72,0)
	fn_file_setup_index('[Q]\PRmstr\DDidx1.h[cno]','1','10')

	fn_file_setup_data('[Q]\PRmstr\mglmstr.h[cno]',135,0)
	fn_file_setup_index('[Q]\PRmstr\mglidx1-idx.h[cno]','1','3')

	fn_file_setup_data('[Q]\PRmstr\HourBreakdown.h[cno]',39,0)
	fn_file_setup_index('[Q]\PRmstr\HourBreakdown-idx.h[cno]','1/9/14','8/5/8')
	! r: Dates.h
	fn_file_setup_data('[Q]\PRmstr\Dates.h[cno]',76,0)
	open #tmp=fnH: 'Name=[Q]\PRmstr\Dates.h[cno],Use,RecL=76,Shr',i,outi,r
	read #tmp,using 'form pos 1,6*n 8',rec=1: beg_date,end_date,qtr1,qtr2,qtr3,qtr4 noRec PR_WRITE_BLANK_DATE_REC
	goto PR_CLOSE_DATE
	PR_WRITE_BLANK_DATE_REC: !
	write #tmp,using 'form pos 1,6*n 8',rec=1: beg_date,end_date,qtr1,qtr2,qtr3,qtr4
	PR_CLOSE_DATE: !
	close #tmp:
	! /r
	fn_file_setup_data('[Q]\PRmstr\PayrollChecks.h[cno]',224,0)
	fn_file_setup_index('[Q]\PRmstr\CheckIdx.h[cno]','1','17')
	fn_file_setup_index('[Q]\PRmstr\CheckIdx2.h[cno]','9/12/1','3/6/8')
	fn_file_setup_index('[Q]\PRmstr\checkidx3.h[cno]','1/12/9','8/6/3')

	! r: DedNames.h setup
	! if exists('[Q]\PRmstr\dednames.h[cno]')=0 then
	open #hDedNames=fnH: 'Name=[Q]\PRmstr\dednames.h[cno],RecL=920,use',i,outi,r
	if lrec(hDedNames)=0 then
		dim pr_dednames_fullname$(20)*20
		dim pr_dednames_abrevname$(20)*8
		dim pr_dednames_newcalcode(20)
		dim pr_dednames_newdedfed(20)
		dim pr_dednames_newdedcode(20)
		dim pr_dednames_dedfica(20)
		dim pr_dednames_dedst(20)
		dim pr_dednames_deduc(20)
		dim pr_dednames_gl$(20)*12
		write #hDedNames,using 'form pos 1,20*c 20,20*c 8,120*n 1,20*c 12': mat pr_dednames_fullname$,mat pr_dednames_abrevname$,mat pr_dednames_newdedcode,mat pr_dednames_newcalcode,mat pr_dednames_newdedfed,mat pr_dednames_dedfica,mat pr_dednames_dedst,mat pr_dednames_deduc,mat pr_dednames_gl$
	end if
	close #hDedNames: ioerr ignore
	! end if
	! /r
	PrGlindex: !
	open #hTmp=fnH: 'Name=[Q]\PRmstr\GLMstr.h[cno],Version=0,KFName=[Q]\PRmstr\GLIndex.h[cno],Use,RecL=62,KPs=1,KLn=12,Shr',i,outIn,k ioerr Check4124OnPrGlindex
	close #hTmp:

	if ~exists('[Q]\PRmstr\EmpStatus.dat') then
		open #h_pr_emp_status=fnH: 'Name=[Q]\PRmstr\EmpStatus.dat,KFName=[Q]\PRmstr\Empstatus.idx,Use,RecL=32,KPs=1,KLn=2,Shr',i,outIn,k
		write #h_pr_emp_status,using 'form pos 1,N 2,C 25': 9,'Terminated'
	end if
	if exists('[Q]\PRmstr\RPMstr.h[cno]') and ~exists('[Q]\PRmstr\Employee.h[cno]') then
		! r: Convert from RPMstr.h[cno] to Employee.h[cno]
		! only significant difference is that mat ta(2) has been removed to make room for w4step2 (initialized to 0 here)
		open #hIn  =fnH: 'Name=[Q]\PRmstr\RPMstr.h[cno],NoShr',i,outi,r
		open #hOut =fnH: 'Name=[Q]\PRmstr\Employee.h[cno],version=0,KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],New,RecL=196,KPs=1,KLn=8,Shr',i,outIn,k
		open #hOut2=fnH: 'Name=[Q]\PRmstr\Employee.h[cno],version=0,KFName=[Q]\PRmstr\EmployeeIdx-name.h[cno],Use,RecL=196,KPs=9,KLn=30,Shr',i,outIn,k

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
	employeeVersion=fn_version('[Q]\PRmstr\Employee.h[cno]')
	employeeRecL=fn_recl('[Q]\PRmstr\Employee.h[cno]')
	if employeeVersion<1 or employeeRecL<244 then
		if fnCopy('[Q]\PRmstr\Employee.h[cno]','', 244)>0 then
			! pause
			open #hEmployee=fnH: 'Name=[Q]\PRmstr\Employee.h[cno],Shr',internal,outin
			version(hEmployee,1)
			do
				read #hEmployee: eof EoEmployeeV0
				rewrite #hEmployee,using 'form pos 174,n 4,c 1,pos 197,4*n 12.2': 2019,' ',0,0,0,0
			loop
			EoEmployeeV0: !
			close #hEmployee:
			fnIndex('[Q]\PRmstr\Employee.h[cno]','[Q]\PRmstr\EmployeeIdx-no.h[cno]'  ,'1 8')
			fnIndex('[Q]\PRmstr\Employee.h[cno]','[Q]\PRmstr\EmployeeIdx-name.h[cno]','9 30')
		else
			pr bell;'Employee record length change failed.  Payroll system unusable until fixed.'
			pause
		end if
	end if
fnend
	def fn_version(filename$*256; ___,returnN,hTmp)
		open #hTmp=fnH: 'Name='&filename$&',Shr',i,i
		returnN=version(hTmp)
		close #hTmp:
		fn_version=returnN
	fnend
	Check4124OnPrGlindex: ! r:
	 if err=4124 and (Check4124OnPrGlindexCount+=1)<=2 then
		 fnIndex('[Q]\PRmstr\GLMstr.h[cno]','[Q]\PRmstr\GLIndex.h[cno]','1 12')
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
	! r: JCMSTR.h
	fn_file_setup_data('[Q]\PRmstr\JCMSTR.h[cno]',300,0)
	fn_file_setup_index('[Q]\PRmstr\JCINDX.h[cno]','1','6')
	fn_file_setup_index('[Q]\PRmstr\JCINDX2.h[cno]','7','25')
	! /r
fnend
def fn_cfv_general_ledger
	! General Ledger Only
	fn_file_setup_data('[Q]\GLmstr\ACTrans.h[cno]',72,0)
	fn_file_setup_index('[Q]\GLmstr\AcTrIdx.h[cno]','1/71/17/13','12/2/2/4')

	fn_file_setup_data('[Q]\GLmstr\GLTrans.h[cno]',73,0)
	fn_file_setup_index('[Q]\GLmstr\glTrans-IdxAcct.h[cno]','1','12')

	if ~exists('[Q]\GLmstr') then execute 'MkDir [Q]\GLmstr'
	fn_reg_rename(env$('cursys'))

	if ~exists('[Q]\GLmstr\PayeeType.dat') then
		open #hPayeeType=fnH: 'Name=[Q]\GLmstr\PayeeType.dat,Version=1,KFName=[Q]\GLmstr\PayeeType.Idx,Use,RecL=27,KPs=1,KLn=2,Shr',i,outIn,k
		write #hPayeeType,using 'form pos 1,n 2,C 25': 0,'Not Applicable'
		write #hPayeeType,using 'form pos 1,n 2,C 25': 7,'Non-Employee Compensation'
		close #hPayeeType:
	end if

	! BudgetInfo: ! Primary Non-Split Index
	name$='[Q]\GLmstr\BudgetInfo.h[cno]'
	kfname$='[Q]\GLmstr\BudIndx.h[cno]'
	myrln=28
	version_proper=0
	fn_make_data_file_exist(name$,myrln,version_proper)
	if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
		fnIndex(name$,kfname$,'1 14')
	end if
	! GL_GLMSTR1: ! Primary Non-Split Index
	name$='[Q]\GLmstr\GLmstr.h[cno]'
	kfname$='[Q]\GLmstr\GLIndex.h[cno]'
	myrln=416
	version_proper=0
	fn_make_data_file_exist(name$,myrln,version_proper)
	if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
		fnIndex(name$,kfname$,'1 12')
	end if
	fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmprln<>myrln then
		fnStatus('Record Length Error in File: '&tmpfile$)
		fnStatus('         RLn: '&str$(tmprln))
	end if
	if tmprln=338 then fnglmstr_338_416
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
	name$='[Q]\GLmstr\GLmstr.h[cno]'
	kfname$='[Q]\GLmstr\GLIndx2.h[cno]'
	if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
		fnIndex(name$,kfname$,'13 30')
	end if
	fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
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
		fnIndex(name$,kfname$,'13 30')
	end if

	fn_file_setup_data('[Q]\GLmstr\AcGLFnSc.h[cno]',83,1)
	fn_file_setup_index('[Q]\GLmstr\agfsidx1.h[cno]','1','5')	! add delete for '[Q]\GLmstr\F nScIndx.h[cno]' - it was the old name of agfsidx1 (without the space in it)
	! Six Files, with 1 primary index each
	!         acglfnsj, acglfnsi, acglfnsb, acglfnsc, acglfnsf, acglfnsg
	fn_file_setup_data('[Q]\GLmstr\acglfnsj.h[cno]',83,1)
	fn_file_setup_index('[Q]\GLmstr\agfsidx2.h[cno]','1','5')
	! add delete for '[Q]\GLmstr\F nsjindx.h[cno]' - it was the old name of agfsidx1 (without the space in it)
	! r: GLmstr\acglfnsi
	name$='[Q]\GLmstr\acglfnsi.h[cno]'
	kfname$='[Q]\GLmstr\agfsidx3.h[cno]'
	fn_file_setup_data(name$,83,1)
	fn_file_setup_index(kfname$,'1','5')
	! myrln=83
	! version_proper=1
	! fn_make_data_file_exist(name$,myrln,version_proper)
	! if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
	!   fnIndex(name$,kfname$,'1 5')
	! end if
	fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmpversion=0 then fnfinstmt_v0_to_v1
	if tmprln<>myrln then
		pr 'Record Length Error in File: '&tmpfile$
		pr '         RLn: '&str$(tmprln)
	end if
	! If TMPRLN=81 OR TMPRLN=78 Then FNFINSTMT_v0_to_v1
	x=1 : if tmpkps(x)<>1 then
		pr 'Key Position ('&str$(x)&') Error in '&kfname$
		pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
	end if
	x=1 : if tmpkln(x)<>5 then
		pr 'Key Length ('&str$(x)&') Error in '&kfname$
		pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
	end if

	fn_file_setup_data('[Q]\GLmstr\acglfnsb.h[cno]',83,1)
	fn_file_setup_index('[Q]\GLmstr\agfsidx4.h[cno]','1','5')

	fn_file_setup_data('[Q]\GLmstr\acglfnsf.h[cno]',83,1)
	fn_file_setup_index('[Q]\GLmstr\agfsidx5.h[cno]','1','5')

	fn_file_setup_data('[Q]\GLmstr\acglfnsg.h[cno]',83,1)
	fn_file_setup_index('[Q]\GLmstr\agfsidx6.h[cno]','1','5')
	
	fn_file_setup_data('[Q]\GLmstr\acglfnsg.h[cno]',83,1)
	fn_file_setup_index('[Q]\GLmstr\agfsidx6.h[cno]','1','5')

	! /r
	! PAYEEGLBREAKDOWN: !
	name$='[Q]\GLmstr\payeeglbreakdown.h[cno]'
	kfname$='[Q]\GLmstr\Payeeglbkdidx.h[cno]'
	myrln=56
	version_proper=1
	fn_make_data_file_exist(name$,myrln,version_proper)
	L3510: !
	if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
		fnIndex(name$,kfname$,'1 8')
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
	if exists('[Q]\GLmstr\gl1099.h[cno]') then fnglpayee_v0_to_v1
	if exists('[Q]\GLmstr\gl1099.h[cno]') then fnFree('[Q]\GLmstr\gl1099.h[cno]')
	name$='[Q]\GLmstr\PayMstr.h[cno]'
	kfname$='[Q]\GLmstr\Payidx1.h[cno]'
	myrln=276
	version_proper=1
	fn_make_data_file_exist(name$,myrln,version_proper)
	L3600: !
	if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
		fnIndex(name$,kfname$,'1 8') : fnglpayee_v0_to_v1
	end if
	fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
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

		! GLTR1099: ! Primary, Non-Split Index  (Vendor transactions)
	name$='[Q]\GLmstr\GlTr1099.h[cno]'
	kfname$='[Q]\GLmstr\gltridx1.h[cno]'
	myrln=64
	version_proper=1
	fn_make_data_file_exist(name$,myrln,version_proper)
	L3690: !
	if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
		fnIndex(name$,kfname$,'1 8')
	end if
	fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
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
	name$='[Q]\GLmstr\Glbrec.h[cno]'
	kfname$='[Q]\GLmstr\glrecidx.h[cno]'
	myrln=68
	version_proper=1
	fn_make_data_file_exist(name$,myrln,version_proper)
	if lwrc$(env$('force_reindex'))='yes' or ~exists(kfname$) then
	GlBrecIndex: !
		fnIndex(name$,kfname$,'1 24')
	end if
	if ~fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$) then goto GlBrecOpenErr
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

	! SCHEDULE: ! Primary, Non-Split Index  (General ledger schedules)
	dim sn$*78,ft$*78,gl$(80)*12
	name$='[Q]\GLmstr\acglschs.h[cno]'
	kfname$='[Q]\GLmstr\schindex.h[cno]'
	myrln=162
	version_proper=1
	fn_make_data_file_exist(name$,myrln,version_proper)
	L3870: fnIndex(name$,kfname$,'1 3')
	fn_getFileInfo(name$,kfname$,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
	fn_check_version(tmpversion,version_proper,tmpfile$)
	if tmprln<>myrln then
		pr 'Record Length Error in File: '&tmpfile$
		pr '         RLn: '&str$(tmprln)
	else
		goto L4050
	end if
	open #tmp=fnH: 'Name='&name$&',KFName='&kfname$&',Shr',i,outIn,k
	L3920: read #tmp, using 'form pos 1,N 2,2*C 78,3*N 1,80*C 12': sn,sn$,ft$,dp,rs,cm,mat gl$ eof EO_TMP conv L9000
	if sn=0 then goto L3920
	rewrite #tmp, using 'form pos 1,N 3,2*C 78,3*N 1': sn,sn$,ft$,dp,rs,cm
	if exists('[Q]\GLmstr\schedule'&str$(sn)&'.h[cno]')=0 then open #schedule=fnH: 'Name=[Q]\GLmstr\schedule'&str$(sn)&'.h[cno],KFName=[Q]\GLmstr\schedule_idx'&str$(sn)&'.h[cno],replace,RecL=12,kps=1,kln=12,Shr',i,outIn,k: version(schedule,1): close #schedule:
	if exists('[Q]\GLmstr\schedule_idx'&str$(sn)&'.h[cno]')=0 then
		fnIndex('[Q]\GLmstr\schedule'&str$(sn)&'.h[cno]','[Q]\GLmstr\schedule_idx'&str$(sn)&'.h[cno]','1 12')
	end if
	open #schedule=fnH: 'Name=[Q]\GLmstr\schedule'&str$(sn)&'.h[cno],KFName=[Q]\GLmstr\schedule_idx'&str$(sn)&'.h[cno],use,RecL=12,kps=1,kln=12,Shr',i,outIn,k: version(schedule,1) ! open to update gl breakdowns
	for j=1 to 80
		if val(gl$(j))=0 then goto L4010
		write #schedule,using 'form pos 1,c 12': gl$(j)
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
	fnfscode
	fnpedat$
	fnpriorcd
	fnPgNum
	fnrx
	fnstyp
	fnPs
	! /r

	if ~exists('[Q]\GLmstr\Period.h[cno]') then
		open #hGlPeriod=fnH: 'Name=[Q]\GLmstr\Period.h[cno],Version=1,KFName=[Q]\GLmstr\Period-Idx.h[cno],Use,RecL=35,KPs=1,KLn=2,Shr',i,outIn,k
		for periodRecord=1 to 12
			write #hGlPeriod,using 'form pos 1,N 2,C 30': periodRecord,date$(days(cnvrt$('pic(##)',periodRecord)&'0117','mmddyy'),'month')
		nex periodRecord
		write #hGlPeriod,using 'form pos 1,N 2,C 30': 13,'End of Year Adjustments'
		close #hGlPeriod:
	end if
	if ~exists('[Q]\GLmstr\PRmstr.h[cno]') then
		open #hGlPrmstr=fnH: 'Name=[Q]\GLmstr\PRmstr.h[cno],',i,outIn,k
		
		if rln(hGlPrmstr)<280 then
			close #hGlPrmstr:
			fnCopy('[Q]\GLmstr\PRmstr.h[cno]','', 280)
		else
			close #hGlPrmstr:
		end if
	end if

	fn_rename('[Q]\GLmstr\GLWK1[wsid].dat','[Q]\GLmstr\GL_Work_[acsUserId].dat')
	fn_rename('[Q]\GLmstr\GLALLOCATIONS[wsid][cno]','[Q]\GLmstr\Allocations[acsUserId].h[cno]')


fnend

def fn_rename(from$*256,to$*256)
	if exists(from$) and ~exists(to$) then
		if fnCopy(from$,to$) then
			fnFree(from$)
		end if
	end if
fnend
L9000: ! r: skip bad schedule records
	reread #tmp, using 'form pos 1,c 2': a$ eof EO_TMP ioerr ignore
goto L3920 ! /r
! def fn_ini_move(cursys$*2) r:
! 	dim imProgramOld$(0)*256
! 	dim imProgramNew$(0)*256
! 	imProgramCount=0
! 	mat imProgramOld$(imProgramCount)
! 	mat imProgramNew$(imProgramCount)
! 	if cursys$='PR' then
! 		fn_programMoveAdd('acsPR\newprfm','Payroll\Employee')
! 		fn_programMoveAdd('acsPR\newprcalk','Payroll\Calculation')
! 		fn_programMoveAdd('acsPR\newprRevCal','Payroll\Reverse Calculation')
! 		fn_programMoveAdd('acsPR\newprchangedate','Payroll\Change Payroll Dates')
! 		fn_programMoveAdd('acsPR\newprinput','Payroll\Enter Time Sheets')
! 		fn_programMoveAdd('acsPR\newprCkPrt','Payroll\Print Payroll Checks')
! 		fn_programMoveAdd('Payroll\Print 1099 Forms','Payroll\Print 1099-NEC Forms')
! 		if fnClientHas('P2') then
! 			fn_programMoveAdd('acsPR\Category','Payroll\Job Cost\Category')
! 		end if
! 	else if cursys$='UB' then
! 		fn_programMoveAdd('acsUB\ubBudLst','Utility Billing\Budget Customer List')
! 		fn_programMoveAdd('acsUB\BudRpt1','Utility Billing\Budget Worksheet')
! 		! fn_programMoveAdd('acsUB\Export_IMSB','Utility Billing\Export File for IMSB')
! 		fn_programMoveAdd('acsUB\ECP_Export','Utility Billing\Export for External Collections Process')
! 		fn_programMoveAdd('acsUB\totalBal','Utility Billing\View Total Accounts Receivable')
! 		fn_programMoveAdd('acsUB\ubbargraph','Utility Billing\Bar Graph')
! 		fn_programMoveAdd('acsUB\billingrpt','Utility Billing\Secondary Water Usage Report')
! 		fn_programMoveAdd('acsUB\averagesewer','Utility Billing\Average Sewer Usage Report')
! 		fn_programMoveAdd('acsUB\ubdelinq','Utility Billing\Past Due Balance Breakdown')
! 		fn_programMoveAdd('acsUB\bill-rpt','Utility Billing\Final Billing Report')
! 		fn_programMoveAdd('acsUB\ubNotes','Utility Billing\Print Customer Notes')
! 		fn_programMoveAdd('acsUB\ubfm','Utility Billing\Customer')
! 		fn_programMoveAdd('acsUB\ubipcoll','Utility Billing\Collections')
! 		fn_programMoveAdd('acsUB\ubPDTnOf','Utility Billing\Past Due Turn Off List')
! 		fn_programMoveAdd('acsUB\ubUsage','Utility Billing\Usage Report')
! 		fn_programMoveAdd('acsUB\ubIpChg','Utility Billing\Enter Readings and Charges')
! 		fn_programMoveAdd('acsUB\workOrderAdd','Utility Billing\Work Order Add')
! 		fn_programMoveAdd('acsUB\workOrderList','Utility Billing\Work Order List')
! 		fn_programMoveAdd('acsUB\ubBilJrn','Utility Billing\Billing Journal')
! 		fn_programMoveAdd('acsUB\ubRate','Utility Billing\Rates')
! 		fn_programMoveAdd('acsUB\BkDraft','Utility Billing\Create Bank Draft File')
! 		fn_programMoveAdd('acsUB\ubpencal','Utility Billing\Penalty Calculation')
! 		fn_programMoveAdd('acsUB\ubUnbill','Utility Billing\Unbilled Customer Listing')
! 		fn_programMoveAdd('acsUB\FlexTran','Utility Billing\Transactions')
! 		fn_programMoveAdd('acsUB\ubNoUsage','Utility Billing\Zero Usage Report')
! 		fn_programMoveAdd('acsUB\Per1000','Utility Billing\Per 1000 Usage')
! 		fn_programMoveAdd('acsUB\PrintBill','Utility Billing\Print Bills')
! 		fn_programMoveAdd('acsUB\analyze','Utility Billing\Rate Analysis')
! 		fn_programMoveAdd('acsUB\label','Utility Billing\Labels')
! 	else if cursys$='GL' then
! 		fn_programMoveAdd('acsGL\glPrt109','General Ledger\Print 1099 Forms')
! 		fn_programMoveAdd('General Ledger\Print 1099 Forms','General Ledger\Print 1099-NEC Forms')
! 		fn_programMoveAdd('acsGL\acGLClos','General Ledger\Close Books at Year End')
! 		fn_programMoveAdd('acsGL\CloseMonth','General Ledger\Close Month')
! 		fn_programMoveAdd('acsGL\AcGLAcTB','General Ledger\Print Accumulated Trial Balance')
! 		fn_programMoveAdd('acsGL\GLInput','General Ledger\Enter Transactions')
! 		fn_programMoveAdd('acsGL\AcGlInc4','General Ledger\Four Column Budget Income Statement')
! 		fn_programMoveAdd('acsGL\ACGLTB','General Ledger\Trial Balance')
! 		fn_programMoveAdd('acsGL\AcGLBalC','General Ledger\Comparative Balance Sheet')
! 		fn_programMoveAdd('acsGL\AcGLBalY','General Ledger\Period Comparison Balance Sheet')
! 		if fnClientHas('G2') then ! Accountant's General Ledger Add-On
! 			fn_programMoveAdd('acsGL\AcPrReg','General Ledger\Accountants\Print Payroll Registers')
! 			fn_programMoveAdd('acsGL\Employee','General Ledger\Accountants\Employee')
! 		end if
! 	else if cursys$='CL' then
! 		fn_programMoveAdd('acsCL\Transaction','Checkbook\Transaction')
! 		fn_programMoveAdd('acsCL\cl1099','Checkbook\Print 1099 Forms')
! 		fn_programMoveAdd('Checkbook\Print 1099 Forms','Checkbook\Print 1099-NEC Forms') ! XXX TODO: we did not move the page in the help files.
! 		fn_programMoveAdd('acsCL\payee','Checkbook\Payee')
! 		fn_programMoveAdd('acsCL\UnpaidInvoice','Checkbook\Unpaid Invoice')
! 	end if
! 	dim favData$(0)*128,favDataN(0)
! 	hFavProgram=fn_openFio('CO Favorites',mat favData$,mat favDataN, 0,2)
! 	for imItem=1 to imProgramCount
! 		dim imIniFrom$*256
! 		dim imIniTo$*256
! 		dim imbrFrom$*256
! 		dim imbrTo$*256
! 		imBrFrom$='S:\'&imProgramOld$(imItem)&'.br'
! 		imBrTo$  ='S:\'&imProgramNew$(imItem)&'.br'
! 		imIniFrom$=fn_programIniFileName$(imProgramOld$(imItem), 1)
! 		imIniTo$  =fn_programIniFileName$(imProgramNew$(imItem), 1)
! 			 ! if pos(lwrc$(imBrFrom$),'acglbalc')>0 then pause
! ! if pos(lwrc$(imIniFrom$),'ubipcoll')>0 and env$('acsdeveloper')<>'' then pause
! 		if ~(imIniFrom$='S:\Core\Default\Program.ini' or imIniTo$='S:\Core\Default\Program.ini') then
! 			fn_rename(imIniFrom$,imIniTo$)
! 		end if
! 		fnKeyChange(hFavProgram,'form pos '&str$(kps(hFavProgram))&',C '&str$(kln(hFavProgram)),imBrFrom$,imBrTo$)
! 	nex imItem
! 	hFavProgram=fnCloseFile(hFavProgram,'CO Favorites')
! fnend
! def fn_programMoveAdd(programNameOld$*256,programNameNew$*256)
! 	fnAddOneC(mat imProgramOld$,programNameOld$)
! 	imProgramCount=fnAddOneC(mat imProgramNew$,programNameNew$)
! fnend /r
def fn_reg_rename(cursys$*2)
	if cursys$='PR' then
		fn_rrOne('Payroll\User Designed Reports','Payroll\Custom Reports')
		fn_rrOne('Payroll\User Designed Reports Proof List','Payroll\fn\Custom Reports Proof List')
		if fnClientHas('P2') then
			! nuffin yet
		end if
	else if cursys$='UB' then
		! fn_rrOne('acsUB\ubDepChg','Utility Billing\Deposit Change List')  <-- just wrong - only with program caption name changes, not file name changes.
	else if cursys$='GL' then
		! nuffin yet
		if fnClientHas('G2') then ! Accountant's General Ledger Add-On
			! nuffin yet
		end if
	else if cursys$='CL' then
	end if
fnend
def fn_rrOne(from$*256,to$*256)
	if ~rr1Setup then
		rr1Setup=1
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
include: fn_setup
