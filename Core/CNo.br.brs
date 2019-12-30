! Replace S:\Core\CNo.br
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fngetdir2,fnerror
		library 'S:\Core\Library': fngethandle
		library 'S:\Core\Library': fnreg_read,fnreg_write
		library 'S:\Core\Library': fncreg_read,fncreg_write
	end if
fnend

def library fncno(&cno; &cnam$)
	if ~setup then let fn_setup
	! cursys$=env$('cursys')
	! r: Read CNo (normal method - tied to session and env$('cursys')
	fnreg_read(session$&'.'&env$('cursys')&'.cno',cno$)
	cno=val(cno$)
	! /r
	! r: read cno tied to WSID (v 5 but before feb 2015)
	if ~cno then 
		fnreg_read(wsid$&'.'&env$('cursys')&'.cno',cno$)
		cno=val(cno$)
		fn_putcno(cno)
	end if 
	! /r
	! r: legacy cno fetch
	if ~cno then ! it's not yet converted to be used in the registry (5.0)
		cno=1
	end if 
	! /r
	! r: read cnam
	dim cnam_read$*40
	cnam_read$=''
	if env$('ACSDeveloper')<>'' and env$('cursys')='TM' then
		open #tf1:=fngethandle: "Name=S:\Core\Data\acsllc\Company.h[cno],Shr",internal,input ioerr CNAM_XIT
	else
		open #tf1:=fngethandle: "Name=[Q]\[cursys]mstr\Company.h[cno],Shr",internal,input ioerr CNAM_XIT
	end if
	read #tf1,using "Form pos 1,C 40": cnam_read$ ioerr ignore
	close #tf1: 
	CNAM_XIT: ! 
	! /r
	cnam$=cnam_read$ soflow ignore
	setenv('cnam',rtrm$(cnam_read$))
	if env$('cno')<>str$(cno) then
		setenv('cno',str$(cno))
		execute 'config substitute [cno] '&str$(cno)
	end if
	fncno=cno
fnend 
def library fnputcno(cno)
	if ~setup then let fn_setup
	fnputcno=fn_putcno(cno)
fnend
def fn_putcno(cno)
	fnreg_write(session$&'.'&env$('CurSys')&'.cno',str$(cno))
	setenv('cno',str$(cno))
	execute 'config substitute [cno] '&str$(cno)
fnend 
def library fnget_company_number_list(mat cno_list; sysid$*256)
	if ~setup then let fn_setup
	if sysid$='' then sysid$=env$('cursys')
	fngetdir2('[Q]\'&sysid$&"mstr",mat filename$,'/od /ta',"Company.*")
	company_count=filename_item=0
	mat cno_list(99999)
	for filename_item=1 to udim(mat filename$)
		tmp_cno=val(filename$(filename_item)(10:14)) conv ACNO_CONV
		if tmp_cno<>99999 and filename$(filename_item)<>'' then ! don't display company 99999
			company_count+=1
			cno_list(company_count)=tmp_cno
		end if 
	ACNO_CONV: ! 
	next filename_item
	mat cno_list(company_count)
	fnget_company_number_list=company_count
fnend
def fn_CnoLegacyNtoCReg(legacyFilename$*256,legacyForm$*64,registryKey$*128; valuePassedIn)
	! Get_or_Put=1 then GET 
	! Get_or_Put=2 then PUT
	if valuePassedIn>0 then get_or_put=2 else get_or_put=1
	if get_or_put=1 then 
		fncreg_read(registryKey$,fscode$) : valuePassedIn=val(fscode$)
		if valuePassedIn=0 then
			open #tmp:=fngethandle: "Name="&legacyFilename$,internal,outIn,relative ioerr LegacyOpenFail
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
def library fnpedat$*20(;pedat$*20)
	if ~setup then let fn_setup
	! Get_or_Put=1 then GET 
	! Get_or_Put=2 then PUT
	if trim$(pedat$)="" then get_or_put=1 else get_or_put=2
	if get_or_put=1 then 
		fncreg_read('Pay Period Ending Date',pedat$)
		if pedat$='' then
			dim pedatLegacyFile$*256
			if exists(env$('temp')&"\pedat-"&session$&".h[cno]") then
				pedatLegacyFile$=env$('temp')&"\pedat-"&session$&".h[cno]"
			else if exists(env$('temp')&"\pedat$-"&session$&".h[cno]") then
				pedatLegacyFile$=env$('temp')&"\pedat$-"&session$&".h[cno]"
			else
				goto xLegacyOpenFail
			end if
			open #tmp:=fngethandle: "Name="&pedatLegacyFile$,internal,outIn,relative ioerr xLegacyOpenFail
			read #tmp,using "Form POS 1,C 20",rec=1: pedat$ noRec ignore
			close #tmp: ioerr ignore
			fncreg_write('Pay Period Ending Date',pedat$)
			xLegacyOpenFail: !
		end if
	else if get_or_put=2 then 
		fncreg_write('Pay Period Ending Date',pedat$)
	end if
	fnpedat$=pedat$
fnend 
def library fnfscode(;fscode)
	if ~setup then let fn_setup
	fnfscode=fn_CnoLegacyNtoCReg(env$('temp')&"\fscode-"&session$&".dat","Form POS 1,N 9",'Financial Statement Code', fscode)
fnend 
def library fnpriorcd(;PriorCD)
	if ~setup then let fn_setup
	fnpriorcd=fn_CnoLegacyNtoCReg(env$('temp')&"\priorcd-"&session$&".dat","Form POS 1,N 9",'PriorCD', PriorCD)
fnend
def library fnpgnum(;pgnum)
	if ~setup then let fn_setup
	fnpgnum=fn_CnoLegacyNtoCReg(env$('temp')&"\PgNum-"&session$&".dat","Form POS 1,N 9",'PgNum', pgnum)
fnend
def library fnrx(;rx)
	if ~setup then let fn_setup
	fnrx=fn_CnoLegacyNtoCReg(env$('temp')&"\rx-"&session$&".dat","Form POS 1,N 9",'rx', rx)
fnend
def library fnstyp(;STyp)
	if ~setup then let fn_setup
	fnstyp=fn_CnoLegacyNtoCReg(env$('temp')&"\STyp-"&session$&".dat","Form POS 1,N 9",'STyp', STyp)
fnend
def library fnps(;ps)
	if ~setup then let fn_setup
	fnps=fn_CnoLegacyNtoCReg(env$('temp')&"\ps-"&session$&".dat","Form POS 1,N 9",'ps', ps)
fnend
def library fnUseDeptNo
	if ~setup then let fn_setup
	if ~useDeptNosetup then ! r:
		useDeptNosetup=1
		if env$('cursys')="GL" then ! read directly from gl if in gl system
			open #company:=fngethandle: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative 
			read #company ,using "Form POS 150, n 1",rec=1: gld1 noRec ignore
			close #company: 
		else 
			pr 'needs to read use department number setting some other way because cursys is not GL' : pause
			! open #tmp:=fngethandle: "Name="&env$('temp')&"\gld1-"&session$&".dat,Use,RecL=9",internal,outIn,relative 
			! read #tmp ,using "Form POS 150, n 1",rec=1: gld1 noRec ignore
			! close #tmp: 
		end if
	end if ! /r
	fnUseDeptNo=gld1
fnend 
def library fndat(&dat$;get_or_put)
	if ~setup then let fn_setup
	! Get_or_Put=0 then READ Dat$ (default to Read)
	! Get_or_Put=1 then READ Dat$
	! Get_or_Put=2 then REWRITE Dat$
	if get_or_put=0 or get_or_put=1 then 
		fnreg_read('Report Heading Date',dat$)
		dat$=trim$(dat$)
		if dat$="" then 
			dat$=date$("Month DD, CCYY")
			fnreg_write('Report Heading Date',dat$)
		end if 
	else if get_or_put=2 then 
		fnreg_write('Report Heading Date',dat$)
	end if 
fnend 
def library fnprg(&curprg$; g_p)
	if g_p=2 then ! Put
		!     r: remove leading  S:\ 
		dim curprg_tmp$*1024
		curprg_tmp$=curprg$
		if uprc$(curprg_tmp$(1:3))='S:\' then ! or uprc$(curprg_tmp$(1:3))='S:\' then 
			curprg_tmp$(1:3)=''
		else if uprc$(curprg_tmp$(1:2))='S:' then 
			curprg_tmp$(1:2)=''
		end if 
		!     /r
		setenv('Core_Program_Current',curprg_tmp$)
	else ! Get
		curprg$=env$('Core_Program_Current')
	end if 
fnend
def library fnSystemIsAddOn( sia_systemAbbr$*256; ___,returnN) 
	sia_systemAbbr$=lwrc$(trim$(sia_systemAbbr$))
	if sia_systemAbbr$='u4' then
		returnN=1
	else if sia_systemAbbr$='p4' then
		returnN=1
	else if sia_systemAbbr$='u5' then
		returnN=1
	else if sia_systemAbbr$='g2' then
		returnN=1
	else if sia_systemAbbr$='em' then
		returnN=1
	else if sia_systemAbbr$(3:3)='-' then
		returnN=1
	end if
	fnSystemIsAddOn=returnN
fnend
def library fnSystemName$*40(; as2n_abbr$*256)
	if ~setup then let fn_setup
	fnSystemName$=fn_systemName$( as2n_abbr$)
fnend
def fn_systemName$*40(; as2n_abbr$*256)
	dim as2n_return$*40
	if as2n_abbr$='' then as2n_abbr$=env$('CurSys')
	as2n_abbr$=lwrc$(as2n_abbr$)
	!   if as2n_abbr$='aa' then
	!     as2n_return$='ACS Programmer'
	if as2n_abbr$='ar' then 
		as2n_return$='Accounts Receivable'
	else if as2n_abbr$='bl' then 
		as2n_return$='Business License'
	else if as2n_abbr$='cl' then 
		as2n_return$='Checkbook'
	else if as2n_abbr$='co' then 
		as2n_return$='ACS Core'
	else if as2n_abbr$='cr' then 
		as2n_return$='Cash Register'
	else if as2n_abbr$='ea' then 
		as2n_return$='Home Energy Assistance'
	else if as2n_abbr$='fa' then 
		as2n_return$='Fixed Asset'
	else if as2n_abbr$='gl' or as2n_abbr$='g1' then 
		as2n_return$='General Ledger'
	else if as2n_abbr$='g2' then 
		as2n_return$='General Ledger - Accountants Add-On'
	else if as2n_abbr$='hh' then 
		as2n_return$='LapTop Meter Reading'
	else if as2n_abbr$='mc' then 
		as2n_return$='Municipal Court'
	else if as2n_abbr$='po' then 
		as2n_return$='Purchase Order'
	!   else if as2n_abbr$='p1' then
	!     as2n_return$='Payroll (Legacy)'
	else if as2n_abbr$='p2' then 
		as2n_return$='Payroll - Job Cost Add-On'
	else if as2n_abbr$='pr' or as2n_abbr$='p4' then 
		as2n_return$='Payroll'
	else if as2n_abbr$='su' then 
		as2n_return$='Support Tracking'
	else if as2n_abbr$='ub' then 
		as2n_return$='Utility Billing'
	else if as2n_abbr$='u4' then 
		as2n_return$='Utility Billing - Hand Held Add-On'
	else if as2n_abbr$='ub-eft' then 
		as2n_return$='Utility Billing - EFT Add-On'
	else if as2n_abbr$='tm' then 
		as2n_return$='Time Management'
	else if as2n_abbr$='oe' then 
		as2n_return$='BR Order Entry'
	else if as2n_abbr$='cm' then 
		as2n_return$='Collection-Master Add-On'
	end if 
	fn_systemName$=as2n_return$
fnend 
def library fncursys$(; cursys_set$*256,resetCache)
	if ~setup then let fn_setup
	if cursys_set$<>'' then 
		cursys_cache$=uprc$(cursys_set$)
		fnreg_write(session$&'.CurSys',cursys_cache$)
	else 
		cursys_cache$=uprc$(env$('CurSys'))
	end if 
	! 
	if cursys_cache$="" or resetCache then 
		fnreg_read(session$&'.CurSys',cursys_cache$)
		if cursys_cache$="" then 
			fngetdir2('S:\',mat system_abbr_list$, '/ON','??.mnu')
			if udim(system_abbr_list$)=>1 then 
				cursys_cache$=trim$(system_abbr_list$(1)(1:len(system_abbr_list$(1))-4))
			end if 
			if cursys_cache$="" then 
				cursys_cache$="CO"
			end if 
		end if 
	end if 
	! 
	if uprc$(cursys_cache$)="P1" then cursys_cache$="PR" ! Payroll
	if uprc$(cursys_cache$)="P2" then cursys_cache$="PR" ! Job Cost Payroll
	if uprc$(cursys_cache$)="P4" then cursys_cache$="PR" ! version 4 Payroll
	if uprc$(cursys_cache$)="G1" then cursys_cache$="GL" ! General Ledger
	if uprc$(cursys_cache$)="G2" then cursys_cache$="GL" ! Accountant's GL
	if uprc$(cursys_cache$)="G3" then cursys_cache$="GL" ! Budget Management
	if env$('CurSys')<>cursys_cache$ then
		setenv('CurSys',cursys_cache$)
		! just fnSystemName$ instead       --->      setenv('CurSystem',fn_systemName$(cursys_cache$))
		execute 'config substitute [CurSys] '&cursys_cache$
	end if
	fncursys$=cursys_cache$
fnend 
include: ertn