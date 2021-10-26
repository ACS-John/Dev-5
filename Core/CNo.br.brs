def library fnCno(&cno; &cnam$)
	if ~setup then fn_setup

	! r: Read CNo (normal method - tied to session and env$('cursys')
	fnreg_read(session$&'.'&env$('cursys')&'.cno',cno$)
	cno=val(cno$)
	! /r

	if ~cno then 	cno=1

	! r: read cnam
	dim cnam_read$*40
	cnam_read$=''
	if env$('ACSDeveloper')<>'' and env$('cursystem')='Client Billing' then
		open #tf1=fnH: 'Name=S:\Core\Data\acsllc\Company.h[cno],Shr',internal,input ioerr CNAM_XIT
	else
		open #tf1=fnH: 'Name=[Q]\[cursys]mstr\Company.h[cno],Shr',internal,input ioerr CNAM_XIT
	end if
	read #tf1,using 'Form pos 1,C 40': cnam_read$ ioerr ignore
	close #tf1:
	CNAM_XIT: !
	! /r
	cnam$=cnam_read$ soflow ignore
	fnSetEnv('cnam',rtrm$(cnam_read$))
	if env$('cno')<>str$(cno) then
		fnSetEnv('cno',str$(cno))
		execute 'config substitute [cno] '&str$(cno)
	end if
	fncno=cno
fnend
def library fnPutCno(cno)
	if ~setup then fn_setup
	fnPutCno=fn_putCno(cno)
fnend
def fn_putCno(cno)
	fnreg_write(session$&'.'&env$('CurSys')&'.cno',str$(cno))
	fnSetEnv('cno',str$(cno))
	execute 'config substitute [cno] '&str$(cno)
fnend
def library fnget_company_number_list(mat cno_list; sysid$*256)
	if ~setup then fn_setup
	if sysid$='' then sysid$=env$('cursys')
	fngetdir2('[Q]\'&sysid$&'mstr',mat filename$,'/od /ta','Company.*')
	company_count=filename_item=0
	mat cno_list(99999)
	for filename_item=1 to udim(mat filename$)
		tmp_cno=val(filename$(filename_item)(10:14)) conv ACNO_CONV
		if tmp_cno<>99999 and filename$(filename_item)<>'' then
			company_count+=1
			cno_list(company_count)=tmp_cno
		end if
	ACNO_CONV: !
	next filename_item
	mat cno_list(company_count)
	fnget_company_number_list=company_count
fnend
def fn_CnoLegacyNtoCReg(legacyFilename$*256,legacyForm$*64,registryKey$*128; valuePassedIn,___,fscode$)
	! Get_or_Put=1 then GET
	! Get_or_Put=2 then PUT
	if valuePassedIn>0 then get_or_put=2 else get_or_put=1
	if get_or_put=1 then
		fncreg_read(registryKey$,fscode$) : valuePassedIn=val(fscode$)
		if valuePassedIn=0 then
			open #tmp=fnH: 'Name='&legacyFilename$,i,outi,r ioerr LegacyOpenFail
			read #tmp,using legacyForm$,rec=1: valuePassedIn noRec ignore
			close #tmp: ioerr ignore
			fncreg_write(registryKey$,str$(valuePassedIn))
			LegacyOpenFail: !
		end if
	else if get_or_put=2 then
		fncreg_write(registryKey$,str$(valuePassedIn))
	end if
	fn_CnoLegacyNtoCReg=valuePassedIn
fnend
def library fnPeDat$*20(;pedat$*20)
	if ~setup then fn_setup
	! Get_or_Put=1 then GET
	! Get_or_Put=2 then PUT
	if trim$(pedat$)='' then get_or_put=1 else get_or_put=2
	if get_or_put=1 then
		fncreg_read('Pay Period Ending Date',pedat$)
		if pedat$='' then
			dim pedatLegacyFile$*256
			if exists('[temp]\pedat-[session].h[cno]') then
				pedatLegacyFile$='[temp]\pedat-[session].h[cno]'
			else if exists('[temp]\pedat$-[session].h[cno]') then
				pedatLegacyFile$='[temp]\pedat$-[session].h[cno]'
			else
				goto xLegacyOpenFail
			end if
			open #tmp=fnH: 'Name='&pedatLegacyFile$,i,outi,r ioerr xLegacyOpenFail
			read #tmp,using 'Form POS 1,C 20',rec=1: pedat$ noRec ignore
			close #tmp: ioerr ignore
			fncreg_write('Pay Period Ending Date',pedat$)
			xLegacyOpenFail: !
		end if
	else if get_or_put=2 then
		fncreg_write('Pay Period Ending Date',pedat$)
	end if
	fnpedat$=pedat$
fnend
def library fnFsCode(;fscode)
	if ~setup then fn_setup
	fnfscode=fn_CnoLegacyNtoCReg('[temp]\fscode-[session].dat','Form POS 1,N 9','Financial Statement Code', fscode)
fnend
def library fnPriorcd(;PriorCD)
	if ~setup then fn_setup
	fnpriorcd=fn_CnoLegacyNtoCReg('[temp]\priorcd-[session].dat','Form POS 1,N 9','PriorCD', PriorCD)
fnend
def library fnPgNum(;pgnum)
	if ~setup then fn_setup
	fnpgnum=fn_CnoLegacyNtoCReg('[temp]\PgNum-[session].dat','Form POS 1,N 9','PgNum', pgnum)
fnend
def library fnRx(;rx)
	if ~setup then fn_setup
	fnrx=fn_CnoLegacyNtoCReg('[temp]\rx-[session].dat','Form POS 1,N 9','rx', rx)
fnend
def library fnStyp(;STyp)
	if ~setup then fn_setup
	fnstyp=fn_CnoLegacyNtoCReg('[temp]\STyp-[session].dat','Form POS 1,N 9','STyp', STyp)
fnend
def library fnPs(;ps)
	if ~setup then fn_setup
	fnps=fn_CnoLegacyNtoCReg('[temp]\ps-[session].dat','Form POS 1,N 9','ps', ps)
fnend
def library fnUseDeptNo
	if ~setup then fn_setup
	if env$('cursys')<>'GL' then
		pr 'needs to read use department number setting some other way because cursys is not GL' : pause
		! open #tmp=fnH: 'Name=[Temp]\gld1-[session].dat,Use,RecL=9',i,outi,r
		! read #tmp ,using 'Form POS 150, n 1',rec=1: gld1 noRec ignore
		! close #tmp:
	end if
	if useDeptNosetup<>val(env$('cno')) then
		useDeptNosetup=val(env$('cno'))
		open #company=fnH: 'Name=[Q]\GLmstr\Company.h[cno],Shr',i,i,r
		read #company ,using 'Form POS 150, n 1',rec=1: gld1 noRec ignore
		close #company:
	end if
	fnUseDeptNo=gld1
fnend
def library fnDat(&dat$;get_or_put)
	if ~setup then fn_setup
	! Get_or_Put=0 then READ Dat$ (default to Read)
	! Get_or_Put=0or1 then READ Dat$
	! Get_or_Put=2 then REWRITE Dat$
	if get_or_put=0 or get_or_put=1 then
		fnreg_read('Report Heading Date',dat$)
		dat$=trim$(dat$)
		if dat$='' then
			dat$=date$('Month DD, CCYY')
			fnreg_write('Report Heading Date',dat$)
		end if
	else if get_or_put=2 then
		fnreg_write('Report Heading Date',dat$)
	end if
fnend
def library fnPrg(&curprg$; g_p,___,curprg_tmp$*1024)
	if ~setup then fn_setup
	if g_p=2 then ! Put
		!     r: remove leading  S:\
		curprg_tmp$=curprg$
		if uprc$(curprg_tmp$(1:3))='S:\' then
			curprg_tmp$(1:3)=''
		else if uprc$(curprg_tmp$(1:2))='S:' then
			curprg_tmp$(1:2)=''
		end if
		!     /r
		fnSetEnv('Core_Program_Current',curprg_tmp$)
	else ! Get
		curprg$=env$('Core_Program_Current')
	end if
fnend

def fn_setup_systemCache
	if ~setup_systemCache then
		setup_systemCache=1
		dim s$(0)*128,sN(0)
		hS=fn_openFio('CO Systems 2',mat s$,mat sN, 1)
		dim sId$(0)*256
		mat sId$(0)
		dim sName$(0)*256
		mat sName$(0)
		dim sIsAddOnN(0)
		mat sIsAddOnN(0)
		do
			read #hS,using form$(hS): mat s$,mat sN eof EoS
			fnAddOneC(mat sId$       	,trim$(lwrc$(s$(sys_id     )))   )
			fnAddOneC(mat sName$     	,trim$(      s$(sys_name   ))    )
			fnAddOneN(mat sIsAddOnN 	,            sN(sys_isChild)     )
		loop
		EoS: !
		! ! if env$('client')='ACS' then
		! 	fnAddOneC(mat sId$       	,'client billing'  )
		! 	fnAddOneC(mat sName$     	,'Client Billing'  )
		! 	! fnAddOneC(mat sNumber$   	,''  )
		! 	fnAddOneN(mat sIsAddOnN 	,0     )
		! ! end if
		close #hS:
	end if
fnend

def library fnSystemIsAddOn( sia_systemAbbr$*256; ___,returnN)
	if ~setup then fn_setup
	fn_setup_systemCache
	sia_systemAbbr$=lwrc$(trim$(sia_systemAbbr$))
	sia_which=srch(mat sId$,sia_systemAbbr$)
	if sia_which>0 then
		returnN=sIsAddOnN(sia_which)
		if returnN<>1 and returnN<>0 then
			pr bell;'invalid boolean'
			pr '  sia_systemAbbr$='&sia_systemAbbr$
			pr '  IsAnAddOn=';returnN
			pause
		end if
	else if sia_systemAbbr$(3:3)='-' then
		returnN=1
	end if
	fnSystemIsAddOn=returnN
fnend

def library fnSystemName$*256(; sysNo$*256)
	if ~setup then fn_setup
	fnSystemName$=fn_systemName$( sysNo$)
fnend
	def fn_systemName$*256(; sysNo$*256,___,return$*256,which)
		! inherrits: mat sId$, mat sName$
		if ~setup_systemCache then fn_setup_systemCache
		if sysNo$='' then sysNo$=env$('CurSys')
		sysNo$=lwrc$(sysNo$)
	
		which=srch(mat sId$,sysNo$)
		if which>0 then
			return$=sName$(which)
		end if
		fn_systemName$=return$
	fnend

def library fnCurSys$(; cursys_set$*256,resetCache,___,curSystem$*256)
	if ~setup then fn_setup
	if cursys_set$<>'' then
		dim cursys_cache$*256
		cursys_cache$=uprc$(cursys_set$)
		fnreg_write(session$&'.CurSys',cursys_cache$)
	else
		cursys_cache$=uprc$(env$('CurSys'))
	end if

	if cursys_cache$='' or resetCache then
		fnreg_read(session$&'.CurSys',cursys_cache$)
		if cursys_cache$='' then
			fngetdir2('S:\',mat system_abbr_list$, '/ON','??.mnu')
			if udim(system_abbr_list$)=>1 then
				cursys_cache$=trim$(system_abbr_list$(1)(1:len(system_abbr_list$(1))-4))
			end if
			if cursys_cache$='' then
				cursys_cache$='CO'
			end if
		end if
	end if
	
	cursys_cache$=fn_standardizeSysId$(cursys_cache$)
	if env$('CurSys')<>cursys_cache$ then
		fnSetEnv('CurSys',cursys_cache$)
		curSystem$=fn_systemName$
		if curSystem$='' then pr 'BLANK curSystem$' : pause
		fnSetEnv('CurSystem',curSystem$)

		! pr 'set curSys to '&env$('cursys')
		! pr 'set curSystem to '&env$('cursystem') : pause

	! gosub SetEnvCurSysData
	! SetEnvCurSysData: ! r:
		if curSystem$='Client Billing' then
			fnSetEnv('CurSysData','S:\Core\Data\acsllc')
		else
			fnSetEnv('CurSysData','[Q]\[cursys]mstr')
		end if
	! return ! /r
	end if
	fncursys$=cursys_cache$
fnend
def library fnStandardizeSysId$(return$*256)
	fnStandardizeSysId$=fn_standardizeSysId$(return$)
fnend
def fn_standardizeSysId$(return$*256)
	if uprc$(return$)='P1' then return$='PR' ! Payroll
	if uprc$(return$)='P2' then return$='PR' ! Job Cost Payroll
	if uprc$(return$)='P4' then return$='PR' ! version 4 Payroll
	if uprc$(return$)='G1' then return$='GL' ! General Ledger
	if uprc$(return$)='G2' then return$='GL' ! Accountant's GL
	if uprc$(return$)='G3' then return$='GL' ! Budget Management
	fn_standardizeSysId$=return$
fnend
include: fn_open
include: fn_setup
