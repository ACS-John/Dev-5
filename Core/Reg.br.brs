! System Registry - ACS System defaults, Clients can only read this, only developers can write to it, delivered to Clients in updates
def fn_sreg_setup(; ___,returnN)
	autoLibrary
	returnN=0
	if env$('ACSDeveloper')='' then
		open #sreg_h=fnH: 'Name=S:\Core\Data\System Registry.dat,Version=1,KFName=S:\Core\Data\System Registry.idx,Shr',internal,input,keyed
		returnN=1
	else
		open #sreg_h=fnH: 'Name=S:\Core\Data\System Registry.dat,Version=1,KFName=S:\Core\Data\System Registry.idx,Use,RecL=384,KPs=1,KLn=128,Shr',internal,outIn,keyed
		returnN=2
	end if
	fn_sreg_setup=returnN
	on error goto Ertn
fnend
def fn_sreg_close
	close #sreg_h: ioerr ignore
	sreg_setup=0
fnend
def library fnsreg_read(fieldName$*128,&fieldValue$; defaultIfNotRead$*128)
	if ~sreg_setup then sreg_setup=fn_sreg_setup
	fnsreg_read=fn_sreg_read(fieldName$,fieldValue$, defaultIfNotRead$)
fnend
def library fnsreg_rename(fieldName_old$*128,fieldNameNew$*128)
	if ~sreg_setup then sreg_setup=fn_sreg_setup
	fnsreg_rename=fn_sreg_rename(fieldName_old$,fieldNameNew$)
fnend
def library fnsreg_write(fieldName$*128,fieldValue$*256)
	if ~sreg_setup then sreg_setup=fn_sreg_setup
	fnsreg_write=fn_sreg_write(fieldName$,fieldValue$)
fnend
def fn_sreg_read(fieldName$*128,&fieldValue$; defaultIfNotRead$*128)
	dim tmpfieldValue$*256,key_compare$*128
	fieldName$=rpad$(lwrc$(trim$(fieldName$)),128)
	tmpfieldValue$=fieldValue$=''
	! pr 'read #reg_h'
	read #sreg_h,using 'form pos 1,C 128,v 256',key=fieldName$,release: key_compare$,tmpfieldValue$ ioerr SREG_LOAD_IOERR ! XXX
	SREG_LOAD_IOERR: !
	if key_compare$=fieldName$ then
		fieldValue$=rtrm$(tmpfieldValue$)
	else
		fieldValue$=defaultIfNotRead$ ! ''
	end if
	! pr 'load ';trim$(fieldName$);'=';fieldValue$
	fn_sreg_read=val(fieldValue$) conv ignore
fnend
def fn_sreg_write(fieldName$*128,fieldValue$*256)
	 if env$('ACSDeveloper')<>'' then
		 fieldName$=rpad$(lwrc$(trim$(fieldName$)),128)
		 rewrite #sreg_h,using 'form pos 1,c 128,c 256',key=fieldName$: fieldName$,fieldValue$ nokey SREG_WRITE ! XXX
		 ! pr 'rewrite #reg_h'
		 goto SREG_SAVE_XIT
		 SREG_WRITE: !
		 write #sreg_h,using 'form pos 1,c 128,c 256': fieldName$,fieldValue$
		 ! pr 'write #reg_h'
		 SREG_SAVE_XIT: !
		 ! pr 'save ';trim$(fieldName$);'=';fieldValue$
	 end if
fnend
def fn_sreg_rename(fieldName_old$*128,fieldNameNew$*128)
	 if env$('ACSDeveloper')<>'' then
		 fieldName_old$=rpad$(lwrc$(trim$(fieldName_old$)),128)
		 rewrite #sreg_h,using 'form pos 1,c 128',key=fieldName_old$: fieldNameNew$ nokey ignore
	 end if
fnend

! Multi-Client Registry - tied to nothing
def library fnmcreg_read(mcr_fieldName$*128,&mcr_fieldValue$; mcr_defaultIfNotRead$*128)
	fn_mcregSetup
	dim mcr_tmpfieldValue$*256,mcr_key_compare$*128
	mcr_fieldName$=rpad$(lwrc$(trim$(mcr_fieldName$)),128)
	mcr_tmpfieldValue$=mcr_fieldValue$=''
	! pr 'read #mcreg_h'
	read #mcreg_h,using 'form pos 1,C 128,v 256',key=mcr_fieldName$,release: mcr_key_compare$,mcr_tmpfieldValue$ ioerr mcreg_LOAD_IOERR ! XXX
	mcreg_LOAD_IOERR: !
	if mcr_key_compare$=mcr_fieldName$ then
		mcr_fieldValue$=rtrm$(mcr_tmpfieldValue$)
	else
		mcr_fieldValue$=mcr_defaultIfNotRead$
	end if
	! pr 'load ';trim$(mcr_fieldName$);'=';mcr_fieldValue$
	fnmcreg_read=val(mcr_fieldValue$) conv ignore
fnend
def library fnmcreg_write(mcw_fieldName$*128,mcw_fieldValue$*256)
	fn_mcregSetup
	mcw_fieldName$=rpad$(lwrc$(trim$(mcw_fieldName$)),128)
	rewrite #mcreg_h,using 'form pos 1,c 128,c 256',key=mcw_fieldName$: mcw_fieldName$,mcw_fieldValue$ nokey mcreg_WRITE ! XXX
	! pr 'rewrite #mcreg_h'
	goto mcreg_SAVE_XIT
	mcreg_WRITE: !
	write #mcreg_h,using 'form pos 1,c 128,c 256': mcw_fieldName$,mcw_fieldValue$ err mcreg_PreEtrn
	! pr 'write #mcreg_h'
	mcreg_SAVE_XIT: !
	! pr 'save ';trim$(mcw_fieldName$);'=';mcw_fieldValue$
fnend
mcreg_PreEtrn: ! r:
	if err=4126 then
		fn_mcregClose
		fnIndex(mcregFileData$,mcregFileIndex$,'1 128')
		fn_mcregSetup
	end if
	fnStatusClose
retry ! /r
def fn_mcregSetup
	if ~mcreg_setup then
		 mcreg_setup=1
		autoLibrary
		dim mcregFileData$*256
		dim mcregFileIndex$*256
		mcregFileData$=env$('QBase')&'\Data\Multi-Client Registry.dat'
		mcregFileIndex$=env$('QBase')&'\Data\Multi-Client Registry.idx'
		if ~exists(mcregFileData$) then fnmakesurepathexists(mcregFileData$)
		open #mcreg_h=fnH: 'Name='&mcregFileData$&',Version=1,KFName='&mcregFileIndex$&',Use,RecL=384,KPs=1,KLn=128,Shr',internal,outIn,keyed
	end if
	on error goto Ertn
fnend
def fn_mcregClose
	close #mcreg_h: ioerr ignore
	mcreg_setup=0
fnend

! Regurlar Registry - tied to Client only - saves their settings
def library fnreg_read(rr_fieldName$*128,&rr_fieldValue$; rr_defaultIfNotRead$*128,alsoUseDefaultIfReadBlank)
	if ~reg_setup then reg_setup=fn_regSetup
	fnreg_read=fn_regRead(rr_fieldName$,rr_fieldValue$, rr_defaultIfNotRead$,alsoUseDefaultIfReadBlank)
fnend
def library fnreg_write(rw_fieldName$*128,rw_fieldValue$*256)
	if ~reg_setup then reg_setup=fn_regSetup
	fnreg_write=fn_regWrite(rw_fieldName$,rw_fieldValue$)
fnend
def library fnreg_rename(fieldName_old$*128,fieldNameNew$*128)
	if ~reg_setup then reg_setup=fn_regSetup
	fnreg_rename=fn_regRename(fieldName_old$,fieldNameNew$)
fnend
def fn_regRead(rr_fieldName$*128,&rr_fieldValue$; rr_defaultIfNotRead$*128,alsoUseDefaultIfReadBlank)
	dim rr_tmpfieldValue$*256,rr_key_compare$*128
	rr_fieldName$=rpad$(lwrc$(trim$(rr_fieldName$)),128)
	rr_tmpfieldValue$=rr_fieldValue$=''
	! pr 'read #reg_h'
	read #reg_h,using 'form pos 1,C 128,v 256',key=rr_fieldName$,release: rr_key_compare$,rr_tmpfieldValue$ ioerr REG_LOAD_IOERR ! XXX
	REG_LOAD_IOERR: !
	if rr_key_compare$=rr_fieldName$ then
		rr_fieldValue$=rtrm$(rr_tmpfieldValue$)
	else
		rr_fieldValue$=rr_defaultIfNotRead$
	end if

	if alsoUseDefaultIfReadBlank and trim$(rr_fieldValue$)='' then
		rr_fieldValue$=rr_defaultIfNotRead$
	end if
	fn_regRead=val(rr_fieldValue$) conv ignore
fnend
def fn_regWrite(rw_fieldName$*128,rw_fieldValue$*256)
	rw_fieldName$=rpad$(lwrc$(trim$(rw_fieldName$)),128)
	rewrite #reg_h,using 'form pos 1,c 128,c 256',key=rw_fieldName$: rw_fieldName$,rw_fieldValue$ nokey REG_WRITE ! XXX
	! pr 'rewrite #reg_h'
	goto REG_SAVE_XIT
	REG_WRITE: !
	write #reg_h,using 'form pos 1,c 128,c 256': rw_fieldName$,rw_fieldValue$
	! pr 'write #reg_h'
	REG_SAVE_XIT: !
	! pr 'save ';trim$(rw_fieldName$);'=';rw_fieldValue$
	fn_regWrite=val(rw_fieldValue$) conv ignore
fnend
def fn_regRename(fieldName_old$*128,fieldNameNew$*128)
	fieldName_old$=rpad$(lwrc$(trim$(fieldName_old$)),128)
	rewrite #reg_h,using 'form pos 1,c 128',key=fieldName_old$: fieldNameNew$ nokey ignore
fnend
def fn_regSetup
	autoLibrary
	fnmakesurepathexists('[Q]\Data\')
	open #reg_h=fnH: 'Name=[Q]\Data\reg.dat,Version=1,KFName=[Q]\Data\reg.idx,Use,RecL=384,KPs=1,KLn=128,Shr',internal,outIn,keyed
	fn_regSetup=1
	on error goto Ertn
fnend

! Company Registry - tied to Client, System and Company Number
def library fnCreg_read(cr_fieldName$*128,&cr_fieldValue$; cr_defaultIfNotRead$*128,cr_alsoApplyDefaultIfReadBlank)
	fn_creg_setup
	dim cr_tmpfieldValue$*256,cr_key_compare$*128
	cr_fieldName$=rpad$(lwrc$(trim$(cr_fieldName$)),128)
	cr_tmpfieldValue$=cr_fieldValue$=''
	! pr 'read #creg_h'
	read #creg_h,using 'form pos 1,C 128,v 256',key=cr_fieldName$,release: cr_key_compare$,cr_tmpfieldValue$ ioerr ignore
	if cr_key_compare$=cr_fieldName$ then
		cr_fieldValue$=rtrm$(cr_tmpfieldValue$)
		if cr_alsoApplyDefaultIfReadBlank and cr_fieldValue$='' then cr_fieldValue$=cr_defaultIfNotRead$
	else
		cr_fieldValue$=cr_defaultIfNotRead$
	end if
	! pr 'load ';trim$(cr_fieldName$);'=';cr_fieldValue$
	fnCreg_read=val(cr_fieldValue$) conv ignore
fnend
def library fnCreg_write(cw_fieldName$*128,cw_fieldValue$*256)
	fn_creg_setup
	cw_fieldName$=rpad$(lwrc$(trim$(cw_fieldName$)),128)
	rewrite #creg_h,using 'form pos 1,c 128,c 256',key=cw_fieldName$: cw_fieldName$,cw_fieldValue$ nokey CREG_WRITE ! XXX
	! pr 'rewrite #creg_h'
	goto CREG_SAVE_XIT
	CREG_WRITE: !
	write #creg_h,using 'form pos 1,c 128,c 256': cw_fieldName$,cw_fieldValue$ err CReg_PreEtrn
	! pr 'write #creg_h'
	CREG_SAVE_XIT: !
	! pr 'save ';trim$(cw_fieldName$);'=';cw_fieldValue$
fnend
CReg_PreEtrn: ! r:
	if err=4126 then
		fn_creg_close
		fnIndex(cregFileData$,cregFileIndex$,'1 128')
		fn_creg_setup
	end if
	fnStatusClose
retry ! /r
def fn_creg_setup
	if creg_setup<>val(env$('CNo')) then
		if creg_setup>0 then let fn_creg_close
		 !
		autoLibrary
		dim cregFileData$*256
		dim cregFileIndex$*256

		if env$('acsDeveloper')<>'' and env$('cursys')='TM' then
			dim dataFolder$*256
			dataFolder$='S:\Core\Data\acsllc'
		else
			dataFolder$='[Q]\'&env$('cursys')&"mstr"
		end if


		cregFileData$ =datafolder$&'\reg-'&env$('CurSys')&'.h[cno]'
		cregFileIndex$=datafolder$&'\reg-'&env$('CurSys')&'-idx.h[cno]'
		open #creg_h=fnH: 'Name='&cregFileData$&',Version=1,KFName='&cregFileIndex$&',Use,RecL=384,KPs=1,KLn=128,Shr',internal,outIn,keyed
		fn_creg_setup=val(env$('CNo'))
		creg_setup=val(env$('CNo'))
	end if
	on error goto Ertn
fnend

! User Registry - tied to Unique_Computer_Id (stored in regurlar registry with key prepended)
def library fnUreg_read(ur_fieldName$*128,&ur_fieldValue$; ur_defaultIfNotRead$*256,alsoUseDefaultIfReadBlank)
	if ~reg_setup then reg_setup=fn_regSetup
	fnureg_read=fn_regRead(env$('Unique_Computer_Id')&'.'&ur_fieldName$,ur_fieldValue$, ur_defaultIfNotRead$,alsoUseDefaultIfReadBlank)
fnend
def library fnUreg_write(uw_fieldName$*128,uw_fieldValue$*256)
	if ~reg_setup then reg_setup=fn_regSetup
	fnureg_write=fn_regWrite(env$('Unique_Computer_Id')&'.'&uw_fieldName$,uw_fieldValue$)
fnend

! GLOBAL - affects ALL registries
def library fnReg_close ! closes all registries (sreg, creg and reg)
	close #reg_h: ioerr ignore
	reg_setup=0
	fn_mcregClose
	fn_creg_close
	fn_sreg_close
	Xit: ! This Xit label is only for use by ERTN - fnerror - if they try to exit a failed read or write to the registry, them just skip on past
fnend
def fn_cReg_close
	close #creg_h: ioerr ignore
	creg_setup=0
fnend



def library fnIniToReg
	if ~setup then fn_setup
	if ~reg_setup then reg_setup=fn_regSetup
	if env$('ACSDeveloper')<>'' and ~sreg_setup then sreg_setup=fn_sreg_setup
	fnIniToReg=fn_IniToReg
fnend
def fn_iniToReg
	if ~setup_iniToReg then
		setup_iniToReg=1

		dim property$(0)*128,default$(0)*128
		mat property$(0) : mat default$(0)
		fnAddOneC(mat property$,'Orientation' ) : fnAddOneC(mat default$,'Portrait')
		fnAddOneC(mat property$,'Height'      ) : fnAddOneC(mat default$,'11.000'  )
		fnAddOneC(mat property$,'Width'       ) : fnAddOneC(mat default$,'8.500'   )
		fnAddOneC(mat property$,'Lines'       ) : fnAddOneC(mat default$,'54'      )
		fnAddOneC(mat property$,'FontSize'    ) : fnAddOneC(mat default$,'10'      )
		fnAddOneC(mat property$,'TopMargin'   ) : fnAddOneC(mat default$,'0.500'   )
		fnAddOneC(mat property$,'BottomMargin') : fnAddOneC(mat default$,'0.500'   )
		fnAddOneC(mat property$,'LeftMargin'  ) : fnAddOneC(mat default$,'0.500'   )
		fnAddOneC(mat property$,'RightMargin' ) : fnAddOneC(mat default$,'0.500'   )

	end if
	dim program_plus$(1)*128,program_name$(1)*80,program_file$(1)*256,program_name_trim$(1)*80,ss_text$(1)*256
	fnGetProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
	for programItem=1 to udim(mat program_name$)
		if program_file$(programItem)<>'' then
			fniniopen('[Q]\INI\'&trim$(program_file$(programItem))&'.ini')
			! if pos(program_name$(programItem),'Print Payroll Checks')>0 then pause
			for propertyItem=1 to udim(mat property$)
				dim iniData$*128
				iniData$=fniniread$('',property$(propertyItem))
				if iniData$<>'' and iniData$<>default$(propertyItem) then
					fn_regWrite(env$('cursys')&'.'&trim$(program_name$(programItem))&'.Print.'&property$(propertyItem),iniData$)
				end if
			nex propertyItem

			execute 'free "S:\Core\Data\ini_default\'&trim$(program_file$(programItem))&'.ini"' err ignore
		end if
	nex programItem
fnend

! ! r: Create SReg from INI (for ACS Developer only) - should never need to be run again (6/13/2017)
! 	if ~setup then fn_setup
! 	autoLibrary
! 	! exec "free 's:\core\data\sys*.*'" err ignore
! 	mat acsSys$(0)
! 	fnAddOneC(mat acsSys$,'UB')
! 	fnAddOneC(mat acsSys$,'GL')
! 	fnAddOneC(mat acsSys$,'PR')
! 	fnAddOneC(mat acsSys$,'CL')
! 	fnAddOneC(mat acsSys$,'OE')
! 	for sysItem=1 to udim(mat acsSys$)
! 		fncursys$(acsSys$(sysItem))
! 		fnGetProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
! 		for programItem=1 to udim(mat program_name$)
! 			if program_file$(programItem)<>'' then
! 				fniniopen('S:\Core\Data\ini_default\'&trim$(program_file$(programItem))&'.ini')
! 				! if pos(program_name$(programItem),'Print Payroll Checks')>0 then pause
! 				for propertyItem=1 to udim(mat property$)
! 					dim iniData$*128
! 					iniData$=fniniread$('',property$(propertyItem))
! 					if iniData$<>'' and iniData$<>default$(propertyItem) then
! 						fn_sreg_write(acsSys$(sysItem)&'.'&trim$(program_name$(programItem))&'.Print.'&property$(propertyItem),iniData$)
! 					end if
! 				nex propertyItem
!
! 				execute 'free "S:\Core\Data\ini_default\'&trim$(program_file$(programItem))&'.ini"' err ignore
! 			end if
! 		nex programItem
! 	nex sysItem
! 	fnhamsterfio('CO System Registry')
! ! /r

include: fn_setup
