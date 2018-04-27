! r: System Registry - ACS System defaults, Clients can only read this, only developers can write to it, delivered to Clients in updates
def fn_sreg_setup
  library 'S:\Core\Library': fngethandle,fnerror
  sreg_setup_return=0
  if env$('ACSDeveloper')='' then
    open #sreg_h:=fngethandle: 'Name=S:\Core\Data\System Registry.dat,Version=1,KFName=S:\Core\Data\System Registry.idx,Shr',internal,input,keyed
    sreg_setup_return=1
  else
    open #sreg_h:=fngethandle: 'Name=S:\Core\Data\System Registry.dat,Version=1,KFName=S:\Core\Data\System Registry.idx,Use,RecL=384,KPs=1,KLn=128,Shr',internal,outIn,keyed
    sreg_setup_return=2
  end if
  fn_sreg_setup=sreg_setup_return
  on error goto ERTN
fnend
def fn_sreg_close
  close #sreg_h: ioerr ignore
  sreg_setup=0
fnend
def library fnsreg_read(field_name$*128,&field_value$; default_if_not_read$*128)
  if ~sreg_setup then sreg_setup=fn_sreg_setup
  fnsreg_read=fn_sreg_read(field_name$,field_value$, default_if_not_read$)
fnend
def library fnsreg_rename(field_name_old$*128,fieldNameNew$*128)
  if ~sreg_setup then sreg_setup=fn_sreg_setup
  fnsreg_rename=fn_sreg_rename(field_name_old$,fieldNameNew$)
fnend
def library fnsreg_write(field_name$*128,field_value$*256)
  if ~sreg_setup then sreg_setup=fn_sreg_setup
  fnsreg_write=fn_sreg_write(field_name$,field_value$)
fnend
def fn_sreg_read(field_name$*128,&field_value$; default_if_not_read$*128)
  dim tmpfield_value$*256,key_compare$*128
  field_name$=rpad$(lwrc$(trim$(field_name$)),128)
  tmpfield_value$=field_value$=''
  ! pr 'read #reg_h'
  read #sreg_h,using 'form pos 1,C 128,v 256',key=field_name$,release: key_compare$,tmpfield_value$ ioerr SREG_LOAD_IOERR ! XXX
  SREG_LOAD_IOERR: !
  if key_compare$=field_name$ then
    field_value$=rtrm$(tmpfield_value$)
  else
    field_value$=default_if_not_read$ ! ''
  end if
  ! pr 'load ';trim$(field_name$);'=';field_value$
fnend
def fn_sreg_write(field_name$*128,field_value$*256)
   if env$('ACSDeveloper')<>'' then
     field_name$=rpad$(lwrc$(trim$(field_name$)),128)
     rewrite #sreg_h,using 'form pos 1,c 128,c 256',key=field_name$: field_name$,field_value$ nokey SREG_WRITE ! XXX
     ! pr 'rewrite #reg_h'
     goto SREG_SAVE_XIT
     SREG_WRITE: !
     write #sreg_h,using 'form pos 1,c 128,c 256': field_name$,field_value$
     ! pr 'write #reg_h'
     SREG_SAVE_XIT: !
     ! pr 'save ';trim$(field_name$);'=';field_value$
   end if
fnend
def fn_sreg_rename(field_name_old$*128,fieldNameNew$*128)
   if env$('ACSDeveloper')<>'' then
     field_name_old$=rpad$(lwrc$(trim$(field_name_old$)),128)
     rewrite #sreg_h,using 'form pos 1,c 128',key=field_name_old$: fieldNameNew$ nokey ignore
   end if
fnend
! /r
! r: Regurlar Registry - tied to Client only - saves their settings
def library fnreg_read(rr_field_name$*128,&rr_field_value$; rr_default_if_not_read$*128)
  if ~reg_setup then reg_setup=fn_reg_setup
  fnreg_read=fn_reg_read(rr_field_name$,rr_field_value$, rr_default_if_not_read$)
fnend
def library fnreg_write(rw_field_name$*128,rw_field_value$*256)
  if ~reg_setup then reg_setup=fn_reg_setup
  fnreg_write=fn_reg_write(rw_field_name$,rw_field_value$)
fnend
def library fnreg_rename(field_name_old$*128,fieldNameNew$*128)
  if ~reg_setup then reg_setup=fn_reg_setup
  fnreg_rename=fn_reg_rename(field_name_old$,fieldNameNew$)
fnend
def fn_reg_read(rr_field_name$*128,&rr_field_value$; rr_default_if_not_read$*128)
  dim rr_tmpfield_value$*256,rr_key_compare$*128
  rr_field_name$=rpad$(lwrc$(trim$(rr_field_name$)),128)
  rr_tmpfield_value$=rr_field_value$=''
  ! pr 'read #reg_h'
  read #reg_h,using 'form pos 1,C 128,v 256',key=rr_field_name$,release: rr_key_compare$,rr_tmpfield_value$ ioerr REG_LOAD_IOERR ! XXX
  REG_LOAD_IOERR: !
  if rr_key_compare$=rr_field_name$ then
    rr_field_value$=rtrm$(rr_tmpfield_value$)
  else
    rr_field_value$=rr_default_if_not_read$ ! ''
  end if
  ! pr 'load ';trim$(rr_field_name$);'=';rr_field_value$
fnend
def fn_reg_write(rw_field_name$*128,rw_field_value$*256)
  rw_field_name$=rpad$(lwrc$(trim$(rw_field_name$)),128)
  rewrite #reg_h,using 'form pos 1,c 128,c 256',key=rw_field_name$: rw_field_name$,rw_field_value$ nokey REG_WRITE ! XXX
  ! pr 'rewrite #reg_h'
  goto REG_SAVE_XIT
  REG_WRITE: !
  write #reg_h,using 'form pos 1,c 128,c 256': rw_field_name$,rw_field_value$
  ! pr 'write #reg_h'
  REG_SAVE_XIT: !
  ! pr 'save ';trim$(rw_field_name$);'=';rw_field_value$
fnend
def fn_reg_rename(field_name_old$*128,fieldNameNew$*128)
  field_name_old$=rpad$(lwrc$(trim$(field_name_old$)),128)
  rewrite #reg_h,using 'form pos 1,c 128',key=field_name_old$: fieldNameNew$ nokey ignore
fnend
def fn_reg_setup
  library 'S:\Core\Library': fngethandle,fnerror,fnmakesurepathexists
	fnmakesurepathexists('[Q]\Data\')
  open #reg_h:=fngethandle: 'Name=[Q]\Data\reg.dat,Version=1,KFName=[Q]\Data\reg.idx,Use,RecL=384,KPs=1,KLn=128,Shr',internal,outIn,keyed
  fn_reg_setup=1
  on error goto ERTN
fnend
! /r
! r: GLOBAL - affects ALL registries
def library fnreg_close ! closes all registries (sreg, creg and reg)
  close #reg_h: ioerr ignore
  reg_setup=0
  fn_mcreg_close
  fn_creg_close
  fn_sreg_close
  XIT: ! This XIT label is only for use by ERTN - fnerror - if they try to exit a failed read or write to the registry, them just skip on past
fnend
def fn_creg_close
  close #creg_h: ioerr ignore
  creg_setup=0
fnend
IGNORE: continue
! <updateable region: ertn>
ERTN: fnerror(program$,err,line,act$,"xit")
  if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
  if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
  pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
  ERTN_EXEC_ACT: execute act$ : goto ERTN
! </updateable region: ertn>
! /r
! r: Multi-Client Registry - tied to nothing
def library fnmcreg_read(mcr_field_name$*128,&mcr_field_value$; mcr_default_if_not_read$*128)
  fn_mcreg_setup
  dim mcr_tmpfield_value$*256,mcr_key_compare$*128
  mcr_field_name$=rpad$(lwrc$(trim$(mcr_field_name$)),128)
  mcr_tmpfield_value$=mcr_field_value$=''
  ! pr 'read #mcreg_h'
  read #mcreg_h,using 'form pos 1,C 128,v 256',key=mcr_field_name$,release: mcr_key_compare$,mcr_tmpfield_value$ ioerr mcreg_LOAD_IOERR ! XXX
  mcreg_LOAD_IOERR: !
  if mcr_key_compare$=mcr_field_name$ then
    mcr_field_value$=rtrm$(mcr_tmpfield_value$)
  else
    mcr_field_value$=mcr_default_if_not_read$
  end if
  ! pr 'load ';trim$(mcr_field_name$);'=';mcr_field_value$
fnend
def library fnmcreg_write(mcw_field_name$*128,mcw_field_value$*256)
  fn_mcreg_setup
  mcw_field_name$=rpad$(lwrc$(trim$(mcw_field_name$)),128)
  rewrite #mcreg_h,using 'form pos 1,c 128,c 256',key=mcw_field_name$: mcw_field_name$,mcw_field_value$ nokey mcreg_WRITE ! XXX
  ! pr 'rewrite #mcreg_h'
  goto mcreg_SAVE_XIT
  mcreg_WRITE: !
  write #mcreg_h,using 'form pos 1,c 128,c 256': mcw_field_name$,mcw_field_value$ err mcreg_PreEtrn
  ! pr 'write #mcreg_h'
  mcreg_SAVE_XIT: !
  ! pr 'save ';trim$(mcw_field_name$);'=';mcw_field_value$
fnend
mcreg_PreEtrn: ! r:
  if err=4126 then
    fn_mcreg_close
    fnIndex_it(mcregFileData$,mcregFileIndex$,'1 128')
    fn_mcreg_setup
  end if
  fnStatusClose
retry ! /r
def fn_mcreg_setup
  if ~mcreg_setup then
     mcreg_setup=1
    library 'S:\Core\Library': fngethandle,fnerror,fnIndex_it,fnStatusClose
    dim mcregFileData$*256
    dim mcregFileIndex$*256
    mcregFileData$=env$('QBase')&'\Data\Multi-Client Registry.dat'
    mcregFileIndex$=env$('QBase')&'\Data\Multi-Client Registry.idx'
    open #mcreg_h:=fngethandle: 'Name='&mcregFileData$&',Version=1,KFName='&mcregFileIndex$&',Use,RecL=384,KPs=1,KLn=128,Shr',internal,outIn,keyed
  end if
  on error goto ERTN
fnend
def fn_mcreg_close
  close #mcreg_h: ioerr ignore
  mcreg_setup=0
fnend
! /r
! r: Company Registry - tied to Client, System and Company Number
def library fncreg_read(cr_field_name$*128,&cr_field_value$; cr_default_if_not_read$*128)
  fn_creg_setup
  dim cr_tmpfield_value$*256,cr_key_compare$*128
  cr_field_name$=rpad$(lwrc$(trim$(cr_field_name$)),128)
  cr_tmpfield_value$=cr_field_value$=''
  ! pr 'read #creg_h'
  read #creg_h,using 'form pos 1,C 128,v 256',key=cr_field_name$,release: cr_key_compare$,cr_tmpfield_value$ ioerr ignore
  if cr_key_compare$=cr_field_name$ then
    cr_field_value$=rtrm$(cr_tmpfield_value$)
  else
    cr_field_value$=cr_default_if_not_read$
  end if
  ! pr 'load ';trim$(cr_field_name$);'=';cr_field_value$
fnend
def library fncreg_write(cw_field_name$*128,cw_field_value$*256)
  fn_creg_setup
  cw_field_name$=rpad$(lwrc$(trim$(cw_field_name$)),128)
  rewrite #creg_h,using 'form pos 1,c 128,c 256',key=cw_field_name$: cw_field_name$,cw_field_value$ nokey CREG_WRITE ! XXX
  ! pr 'rewrite #creg_h'
  goto CREG_SAVE_XIT
  CREG_WRITE: !
  write #creg_h,using 'form pos 1,c 128,c 256': cw_field_name$,cw_field_value$ err CReg_PreEtrn
  ! pr 'write #creg_h'
  CREG_SAVE_XIT: !
  ! pr 'save ';trim$(cw_field_name$);'=';cw_field_value$
fnend
CReg_PreEtrn: ! r:
  if err=4126 then
    fn_creg_close
    fnIndex_it(cregFileData$,cregFileIndex$,'1 128')
    fn_creg_setup
  end if
  fnStatusClose
retry ! /r
def fn_creg_setup
  if creg_setup<>val(env$('CNo')) then
    if creg_setup>0 then let fn_creg_close
     !
    library 'S:\Core\Library': fngethandle,fnerror,fnIndex_it,fnStatusClose
    dim cregFileData$*256
    dim cregFileIndex$*256
    cregFileData$='[Q]\'&env$('CurSys')&'mstr\reg-'&env$('CurSys')&'.h[cno]'
    cregFileIndex$='[Q]\'&env$('CurSys')&'mstr\reg-'&env$('CurSys')&'-idx.h[cno]'
    open #creg_h:=fngethandle: 'Name='&cregFileData$&',Version=1,KFName='&cregFileIndex$&',Use,RecL=384,KPs=1,KLn=128,Shr',internal,outIn,keyed
    fn_creg_setup=val(env$('CNo'))
    creg_setup=val(env$('CNo'))
  end if
  on error goto ERTN
fnend
! /r
! r: User Registry - tied to Unique_Computer_Id (stored in regurlar registry with key prepended)
def library fnureg_read(ur_field_name$*128,&ur_field_value$; ur_default_if_not_read$*128)
  if ~reg_setup then reg_setup=fn_reg_setup
  fnureg_read=fn_reg_read(env$('Unique_Computer_Id')&'.'&ur_field_name$,ur_field_value$, ur_default_if_not_read$)
fnend
def library fnureg_write(uw_field_name$*128,uw_field_value$*256)
  if ~reg_setup then reg_setup=fn_reg_setup
  fnureg_write=fn_reg_write(env$('Unique_Computer_Id')&'.'&uw_field_name$,uw_field_value$)
fnend
! /r
def fn_setup
  if ~setup then
    setup=1
    library 'S:\Core\Library': fnAddOneC,fncursys$,fniniopen,fniniread$,fnGetProgramList,fnSystemName$
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
    !
    dim program_plus$(1)*128,program_name$(1)*80,program_file$(1)*80,program_name_trim$(1)*80,ss_text$(1)*256
  end if
fnend
def library fnIniToReg
  if ~setup then let fn_setup
  if ~reg_setup then reg_setup=fn_reg_setup
  if env$('ACSDeveloper')<>'' and ~sreg_setup then sreg_setup=fn_sreg_setup
  fnIniToReg=fn_IniToReg
fnend
def fn_iniToReg
  fnGetProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
  for programItem=1 to udim(mat program_name$)
    if program_file$(programItem)<>'' then
      fniniopen('[Q]\INI\'&trim$(program_file$(programItem))&'.ini')
      ! if pos(program_name$(programItem),'Print Payroll Checks')>0 then pause
      for propertyItem=1 to udim(mat property$)
        dim iniData$*128
        iniData$=fniniread$('',property$(propertyItem))
        if iniData$<>'' and iniData$<>default$(propertyItem) then
          fn_reg_write(env$('cursys')&'.'&trim$(program_name$(programItem))&'.Print.'&property$(propertyItem),iniData$)
        end if
      nex propertyItem
      !
      execute 'free "S:\Core\Data\ini_default\'&trim$(program_file$(programItem))&'.ini"' err ignore
    end if
  nex programItem
fnend
! r: Create SReg from INI (for ACS Developer only) - should never need to be run again (6/13/2017)
if ~setup then let fn_setup
library 'S:\Core\Library': fnhamsterfio
! exec "free 's:\core\data\sys*.*'" err ignore
mat acsSys$(0)
fnAddOneC(mat acsSys$,'UB')
fnAddOneC(mat acsSys$,'GL')
fnAddOneC(mat acsSys$,'PR')
fnAddOneC(mat acsSys$,'CL')
fnAddOneC(mat acsSys$,'OE')
for sysItem=1 to udim(mat acsSys$)
  fncursys$(acsSys$(sysItem))
  fnGetProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
  for programItem=1 to udim(mat program_name$)
    if program_file$(programItem)<>'' then
      fniniopen('S:\Core\Data\ini_default\'&trim$(program_file$(programItem))&'.ini') ! fnSystemName$(acsSys$(sysItem))&'\'
      ! if pos(program_name$(programItem),'Print Payroll Checks')>0 then pause
      for propertyItem=1 to udim(mat property$)
        dim iniData$*128
        iniData$=fniniread$('',property$(propertyItem))
        if iniData$<>'' and iniData$<>default$(propertyItem) then
          fn_sreg_write(acsSys$(sysItem)&'.'&trim$(program_name$(programItem))&'.Print.'&property$(propertyItem),iniData$)
        end if
      nex propertyItem
      !
      execute 'free "S:\Core\Data\ini_default\'&trim$(program_file$(programItem))&'.ini"' err ignore
    end if
  nex programItem
nex sysItem
fnhamsterfio('CO System Registry')
! /r

