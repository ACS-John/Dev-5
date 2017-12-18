10000 ! r: System Registry - ACS System defaults, Clients can only read this, only developers can write to it, delivered to Clients in updates
10020 def fn_sreg_setup
10040   library 'S:\Core\Library': fngethandle,fnerror
10060   sreg_setup_return=0
10080   if env$('ACSDeveloper')='' then
10100     open #sreg_h:=fngethandle: 'Name=S:\Core\Data\System Registry.dat,Version=1,KFName=S:\Core\Data\System Registry.idx,Shr',internal,input,keyed
10120     sreg_setup_return=1
10140   else
10160     open #sreg_h:=fngethandle: 'Name=S:\Core\Data\System Registry.dat,Version=1,KFName=S:\Core\Data\System Registry.idx,Use,RecL=384,KPs=1,KLn=128,Shr',internal,outin,keyed
10180     sreg_setup_return=2
10200   end if
10220   fn_sreg_setup=sreg_setup_return
10240   on error goto ERTN
10260 fnend
11000 def fn_sreg_close
11020   close #sreg_h: ioerr ignore
11040   sreg_setup=0
11060 fnend
12000 def library fnsreg_read(field_name$*128,&field_value$; default_if_not_read$*128)
12020   if ~sreg_setup then sreg_setup=fn_sreg_setup
12040   fnsreg_read=fn_sreg_read(field_name$,field_value$, default_if_not_read$)
12060 fnend
12200 def library fnsreg_rename(field_name_old$*128,fieldNameNew$*128)
12220   if ~sreg_setup then sreg_setup=fn_sreg_setup
12240   fnsreg_rename=fn_sreg_rename(field_name_old$,fieldNameNew$)
12260 fnend
13000 def library fnsreg_write(field_name$*128,field_value$*256)
13020   if ~sreg_setup then sreg_setup=fn_sreg_setup
13040   fnsreg_write=fn_sreg_write(field_name$,field_value$)
13060 fnend
16000 def fn_sreg_read(field_name$*128,&field_value$; default_if_not_read$*128)
16020   dim tmpfield_value$*256,key_compare$*128
16040   field_name$=rpad$(lwrc$(trim$(field_name$)),128)
16060   tmpfield_value$=field_value$=''
16080   ! pr 'read #reg_h'
16100   read #sreg_h,using 'form pos 1,C 128,v 256',key=field_name$,release: key_compare$,tmpfield_value$ ioerr SREG_LOAD_IOERR ! XXX
16120   SREG_LOAD_IOERR: !
16140   if key_compare$=field_name$ then
16160     field_value$=rtrm$(tmpfield_value$)
16180   else
16200     field_value$=default_if_not_read$ ! ''
16220   end if
16240   ! pr 'load ';trim$(field_name$);'=';field_value$
16260 fnend
18000 def fn_sreg_write(field_name$*128,field_value$*256)
18020    if env$('ACSDeveloper')<>'' then
18040      field_name$=rpad$(lwrc$(trim$(field_name$)),128)
18060      rewrite #sreg_h,using 'form pos 1,c 128,c 256',key=field_name$: field_name$,field_value$ nokey SREG_WRITE ! XXX
18080      ! pr 'rewrite #reg_h'
18100      goto SREG_SAVE_XIT
18120      SREG_WRITE: !
18140      write #sreg_h,using 'form pos 1,c 128,c 256': field_name$,field_value$
18160      ! pr 'write #reg_h'
18180      SREG_SAVE_XIT: !
18200      ! pr 'save ';trim$(field_name$);'=';field_value$
18220    end if
18240 fnend
19000 def fn_sreg_rename(field_name_old$*128,fieldNameNew$*128)
19020    if env$('ACSDeveloper')<>'' then
19040      field_name_old$=rpad$(lwrc$(trim$(field_name_old$)),128)
19060      rewrite #sreg_h,using 'form pos 1,c 128',key=field_name_old$: fieldNameNew$ nokey ignore
19220    end if
19240 fnend
19260 ! /r
20000 ! r: Regurlar Registry - tied to Client only - saves their settings
20020 def library fnreg_read(rr_field_name$*128,&rr_field_value$; rr_default_if_not_read$*128)
20040   if ~reg_setup then reg_setup=fn_reg_setup
20060   fnreg_read=fn_reg_read(rr_field_name$,rr_field_value$, rr_default_if_not_read$)
20080 fnend
20100 def library fnreg_write(rw_field_name$*128,rw_field_value$*256)
20120   if ~reg_setup then reg_setup=fn_reg_setup
20140   fnreg_write=fn_reg_write(rw_field_name$,rw_field_value$)
20160 fnend
20180 def library fnreg_rename(field_name_old$*128,fieldNameNew$*128)
20200   if ~reg_setup then reg_setup=fn_reg_setup
20220   fnreg_rename=fn_reg_rename(field_name_old$,fieldNameNew$)
20240 fnend
20260 def fn_reg_read(rr_field_name$*128,&rr_field_value$; rr_default_if_not_read$*128)
20280   dim rr_tmpfield_value$*256,rr_key_compare$*128
20300   rr_field_name$=rpad$(lwrc$(trim$(rr_field_name$)),128)
20320   rr_tmpfield_value$=rr_field_value$=''
20340   ! pr 'read #reg_h'
20360   read #reg_h,using 'form pos 1,C 128,v 256',key=rr_field_name$,release: rr_key_compare$,rr_tmpfield_value$ ioerr REG_LOAD_IOERR ! XXX
20380   REG_LOAD_IOERR: !
20400   if rr_key_compare$=rr_field_name$ then
20420     rr_field_value$=rtrm$(rr_tmpfield_value$)
20440   else
20460     rr_field_value$=rr_default_if_not_read$ ! ''
20480   end if
20500   ! pr 'load ';trim$(rr_field_name$);'=';rr_field_value$
20520 fnend
20540 def fn_reg_write(rw_field_name$*128,rw_field_value$*256)
20560   rw_field_name$=rpad$(lwrc$(trim$(rw_field_name$)),128)
20580   rewrite #reg_h,using 'form pos 1,c 128,c 256',key=rw_field_name$: rw_field_name$,rw_field_value$ nokey REG_WRITE ! XXX
20600   ! pr 'rewrite #reg_h'
20620   goto REG_SAVE_XIT
20640   REG_WRITE: !
20660   write #reg_h,using 'form pos 1,c 128,c 256': rw_field_name$,rw_field_value$
20680   ! pr 'write #reg_h'
20700   REG_SAVE_XIT: !
20720   ! pr 'save ';trim$(rw_field_name$);'=';rw_field_value$
20740 fnend
20760 def fn_reg_rename(field_name_old$*128,fieldNameNew$*128)
20780   field_name_old$=rpad$(lwrc$(trim$(field_name_old$)),128)
20800   rewrite #reg_h,using 'form pos 1,c 128',key=field_name_old$: fieldNameNew$ nokey ignore
20820 fnend
20840 def fn_reg_setup
20860   library 'S:\Core\Library': fngethandle,fnerror
20880   open #reg_h:=fngethandle: 'Name='&env$('Q')&'\Data\reg.dat,Version=1,KFName='&env$('Q')&'\Data\reg.idx,Use,RecL=384,KPs=1,KLn=128,Shr',internal,outin,keyed
20900   fn_reg_setup=1
20920   on error goto ERTN
20940 fnend
20960 ! /r
30000 ! r: GLOBAL - affects ALL registries
30020 def library fnreg_close ! closes all registries (sreg, creg and reg)
30040   close #reg_h: ioerr ignore
30060   reg_setup=0
30070   fn_mcreg_close
30080   fn_creg_close
30100   fn_sreg_close
30120   XIT: ! This XIT label is only for use by ERTN - fnerror - if they try to exit a failed read or write to the registry, them just skip on past
30140 fnend
30160 def fn_creg_close
30180   close #creg_h: ioerr ignore
30200   creg_setup=0
30220 fnend
30240 IGNORE: continue
30260 ! <updateable region: ertn>
30280 ERTN: fnerror(program$,err,line,act$,"xit")
30300   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
30320   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
30340   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
30360   ERTN_EXEC_ACT: execute act$ : goto ERTN
30380 ! </updateable region: ertn>
30400 ! /r
34000 ! r: Multi-Client Registry - tied to nothing
34020 def library fnmcreg_read(mcr_field_name$*128,&mcr_field_value$; mcr_default_if_not_read$*128)
34040   fn_mcreg_setup
34060   dim mcr_tmpfield_value$*256,mcr_key_compare$*128
34080   mcr_field_name$=rpad$(lwrc$(trim$(mcr_field_name$)),128)
34100   mcr_tmpfield_value$=mcr_field_value$=''
34120   ! pr 'read #mcreg_h'
34140   read #mcreg_h,using 'form pos 1,C 128,v 256',key=mcr_field_name$,release: mcr_key_compare$,mcr_tmpfield_value$ ioerr mcreg_LOAD_IOERR ! XXX
34160   mcreg_LOAD_IOERR: !
34180   if mcr_key_compare$=mcr_field_name$ then
34200     mcr_field_value$=rtrm$(mcr_tmpfield_value$)
34220   else
34240     mcr_field_value$=mcr_default_if_not_read$
34260   end if
34280   ! pr 'load ';trim$(mcr_field_name$);'=';mcr_field_value$
34300 fnend
34320 def library fnmcreg_write(mcw_field_name$*128,mcw_field_value$*256)
34340   fn_mcreg_setup
34360   mcw_field_name$=rpad$(lwrc$(trim$(mcw_field_name$)),128)
34380   rewrite #mcreg_h,using 'form pos 1,c 128,c 256',key=mcw_field_name$: mcw_field_name$,mcw_field_value$ nokey mcreg_WRITE ! XXX
34400   ! pr 'rewrite #mcreg_h'
34420   goto mcreg_SAVE_XIT
34440   mcreg_WRITE: !
34460   write #mcreg_h,using 'form pos 1,c 128,c 256': mcw_field_name$,mcw_field_value$ err mcreg_PreEtrn
34480   ! pr 'write #mcreg_h'
34500   mcreg_SAVE_XIT: !
34520   ! pr 'save ';trim$(mcw_field_name$);'=';mcw_field_value$
34540 fnend
34560 mcreg_PreEtrn: ! r:
34580   if err=4126 then
34600     fn_mcreg_close
34620     fnIndex_it(mcregFileData$,mcregFileIndex$,'1 128')
34640     fn_mcreg_setup
34660   end if
34680   fnstatus_close
34700 retry ! /r
34720 def fn_mcreg_setup
34760   if ~mcreg_setup then
34800      mcreg_setup=1
34820     library 'S:\Core\Library': fngethandle,fnerror,fnIndex_it,fnstatus_close
34840     dim mcregFileData$*256
34860     dim mcregFileIndex$*256
34880     mcregFileData$=env$('QBase')&'\Data\Multi-Client Registry.dat'
34900     mcregFileIndex$=env$('QBase')&'\Data\Multi-Client Registry.idx'
34920     open #mcreg_h:=fngethandle: 'Name='&mcregFileData$&',Version=1,KFName='&mcregFileIndex$&',Use,RecL=384,KPs=1,KLn=128,Shr',internal,outin,keyed
34980   end if
35000   on error goto ERTN
35020 fnend
35040 def fn_mcreg_close
35060   close #mcreg_h: ioerr ignore
35080   mcreg_setup=0
35100 fnend
35120 ! /r
40000 ! r: Company Registry - tied to Client, System and Company Number
40020 def library fncreg_read(cr_field_name$*128,&cr_field_value$; cr_default_if_not_read$*128)
40040   fn_creg_setup
40060   dim cr_tmpfield_value$*256,cr_key_compare$*128
40080   cr_field_name$=rpad$(lwrc$(trim$(cr_field_name$)),128)
40100   cr_tmpfield_value$=cr_field_value$=''
40120   ! pr 'read #creg_h'
40140   read #creg_h,using 'form pos 1,C 128,v 256',key=cr_field_name$,release: cr_key_compare$,cr_tmpfield_value$ ioerr CREG_LOAD_IOERR ! XXX
40160   CREG_LOAD_IOERR: !
40180   if cr_key_compare$=cr_field_name$ then
40200     cr_field_value$=rtrm$(cr_tmpfield_value$)
40220   else
40240     cr_field_value$=cr_default_if_not_read$
40260   end if
40280   ! pr 'load ';trim$(cr_field_name$);'=';cr_field_value$
40300 fnend
40320 def library fncreg_write(cw_field_name$*128,cw_field_value$*256)
40340   fn_creg_setup
40360   cw_field_name$=rpad$(lwrc$(trim$(cw_field_name$)),128)
40380   rewrite #creg_h,using 'form pos 1,c 128,c 256',key=cw_field_name$: cw_field_name$,cw_field_value$ nokey CREG_WRITE ! XXX
40400   ! pr 'rewrite #creg_h'
40420   goto CREG_SAVE_XIT
40440   CREG_WRITE: !
40460   write #creg_h,using 'form pos 1,c 128,c 256': cw_field_name$,cw_field_value$ err CReg_PreEtrn
40480   ! pr 'write #creg_h'
40500   CREG_SAVE_XIT: !
40520   ! pr 'save ';trim$(cw_field_name$);'=';cw_field_value$
40540 fnend
40560 CReg_PreEtrn: ! r:
40580   if err=4126 then
40600     fn_creg_close
40620     fnIndex_it(cregFileData$,cregFileIndex$,'1 128')
40640     fn_creg_setup
40660   end if
40680   fnstatus_close
40700 retry ! /r
40720 def fn_creg_setup
40760   if creg_setup<>val(env$('CNo')) then
40780     if creg_setup>0 then let fn_creg_close
40800      !
40820     library 'S:\Core\Library': fngethandle,fnerror,fnIndex_it,fnstatus_close
40840     dim cregFileData$*256
40860     dim cregFileIndex$*256
40880     cregFileData$=env$('Q')&'\'&env$('CurSys')&'mstr\reg-'&env$('CurSys')&'.h'&env$('CNo')
40900     cregFileIndex$=env$('Q')&'\'&env$('CurSys')&'mstr\reg-'&env$('CurSys')&'-idx.h'&env$('CNo')
40920     open #creg_h:=fngethandle: 'Name='&cregFileData$&',Version=1,KFName='&cregFileIndex$&',Use,RecL=384,KPs=1,KLn=128,Shr',internal,outin,keyed
40940     fn_creg_setup=val(env$('CNo'))
40960     creg_setup=val(env$('CNo'))
40980   end if
41000   on error goto ERTN
41020 fnend
41040 ! /r
50000 ! r: User Registry - tied to Unique_Computer_Id (stored in regurlar registry with key prepended)
50020 def library fnureg_read(ur_field_name$*128,&ur_field_value$; ur_default_if_not_read$*128)
50040   if ~reg_setup then reg_setup=fn_reg_setup
50060   fnureg_read=fn_reg_read(env$('Unique_Computer_Id')&'.'&ur_field_name$,ur_field_value$, ur_default_if_not_read$)
50080 fnend
50100 def library fnureg_write(uw_field_name$*128,uw_field_value$*256)
50120   if ~reg_setup then reg_setup=fn_reg_setup
50140   fnureg_write=fn_reg_write(env$('Unique_Computer_Id')&'.'&uw_field_name$,uw_field_value$)
50160 fnend
50180 ! /r
80000 def fn_setup
80040   if ~setup then
80060     setup=1
80080     library 'S:\Core\Library': fnAddOneC,fncursys$,fniniopen,fniniread$,fnGetProgramList,fnSystemName$
80100     dim property$(0)*128,default$(0)*128
80120     mat property$(0) : mat default$(0)
80140     fnAddOneC(mat property$,'Orientation' ) : fnAddOneC(mat default$,'Portrait')
80160     fnAddOneC(mat property$,'Height'      ) : fnAddOneC(mat default$,'11.000'  )
80180     fnAddOneC(mat property$,'Width'       ) : fnAddOneC(mat default$,'8.500'   )
80200     fnAddOneC(mat property$,'Lines'       ) : fnAddOneC(mat default$,'54'      )
80220     fnAddOneC(mat property$,'FontSize'    ) : fnAddOneC(mat default$,'10'      )
80240     fnAddOneC(mat property$,'TopMargin'   ) : fnAddOneC(mat default$,'0.500'   )
80260     fnAddOneC(mat property$,'BottomMargin') : fnAddOneC(mat default$,'0.500'   )
80280     fnAddOneC(mat property$,'LeftMargin'  ) : fnAddOneC(mat default$,'0.500'   )
80300     fnAddOneC(mat property$,'RightMargin' ) : fnAddOneC(mat default$,'0.500'   )
80320     !
80340     dim program_plus$(1)*128,program_name$(1)*80,program_file$(1)*80,program_name_trim$(1)*80,ss_text$(1)*256
80360   end if
80380 fnend
82000 def library fnIniToReg
82010   if ~setup then let fn_setup
82020   if ~reg_setup then reg_setup=fn_reg_setup
82030   if env$('ACSDeveloper')<>'' and ~sreg_setup then sreg_setup=fn_sreg_setup
82040   fnIniToReg=fn_IniToReg
82060 fnend
84000 def fn_iniToReg
84020   fnGetProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
84040   for programItem=1 to udim(mat program_name$)
84060     if program_file$(programItem)<>'' then
84080       fniniopen(env$('Q')&'\INI\'&trim$(program_file$(programItem))&'.ini')
84100       ! if pos(program_name$(programItem),'Print Payroll Checks')>0 then pause
84120       for propertyItem=1 to udim(mat property$)
84140         dim iniData$*128
84160         iniData$=fniniread$('',property$(propertyItem))
84180         if iniData$<>'' and iniData$<>default$(propertyItem) then
84200           fn_reg_write(env$('cursys')&'.'&trim$(program_name$(programItem))&'.Print.'&property$(propertyItem),iniData$)
84220         end if
84240       nex propertyItem
84260       !
84280       execute 'free "S:\Core\Data\ini_default\'&trim$(program_file$(programItem))&'.ini"' err ignore
84300     end if
84320   nex programItem
84340 fnend
86000 ! r: Create SReg from INI (for ACS Developer only) - should never need to be run again (6/13/2017)
86010 if ~setup then let fn_setup
86020 library 'S:\Core\Library': fnhamsterfio
86030 ! exec "free 's:\core\data\sys*.*'" err ignore
86040 mat acsSys$(0)
86050 fnAddOneC(mat acsSys$,'UB')
86060 fnAddOneC(mat acsSys$,'GL')
86070 fnAddOneC(mat acsSys$,'PR')
86080 fnAddOneC(mat acsSys$,'CL')
86090 fnAddOneC(mat acsSys$,'OE')
86100 for sysItem=1 to udim(mat acsSys$)
86110   fncursys$(acsSys$(sysItem))
86120   fnGetProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
86130   for programItem=1 to udim(mat program_name$)
86140     if program_file$(programItem)<>'' then
86150       fniniopen('S:\Core\Data\ini_default\'&trim$(program_file$(programItem))&'.ini') ! fnSystemName$(acsSys$(sysItem))&'\'
86160       ! if pos(program_name$(programItem),'Print Payroll Checks')>0 then pause
86170       for propertyItem=1 to udim(mat property$)
86180         dim iniData$*128
86190         iniData$=fniniread$('',property$(propertyItem))
86200         if iniData$<>'' and iniData$<>default$(propertyItem) then
86210           fn_sreg_write(acsSys$(sysItem)&'.'&trim$(program_name$(programItem))&'.Print.'&property$(propertyItem),iniData$)
86220         end if
86230       nex propertyItem
86240       !
86250       execute 'free "S:\Core\Data\ini_default\'&trim$(program_file$(programItem))&'.ini"' err ignore
86260     end if
86270   nex programItem
86280 nex sysItem
86290 fnhamsterfio('CO System Registry')
86300 ! /r
88000 def library fnread_program_print_property(key$*80,&value$; prgCapForSettingsOverride$*256)
88020   if ~reg_setup then reg_setup=fn_reg_setup
88040   if env$('ACSDeveloper')<>'' and ~sreg_setup then sreg_setup=fn_sreg_setup
88060   on error goto ERTN
88080   dim prg$*256
88100   if prgCapForSettingsOverride$='' then
88120     prg$=env$('Program_Caption')
88140   else
88160     prg$=prgCapForSettingsOverride$
88180   end if
88200   len_prg=len(prg$)
88220   ! if lwrc$(prg$(len_prg-2:len_prg))='.br' then prg$(len_prg-2:len_prg)='' ! remove the .br ending, if it is there
88240   ! if lwrc$(trim$(prg$))=lwrc$("acspr\checkfile") then prg$="acspr\checkhistory"
88260   if env$('ACSDeveloper')<>'' then !  if it is a developer
88280     fn_sreg_read(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
88300   else
88320     fn_reg_read(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
88340     if value$='' then
88360       fn_sreg_read(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
88380     end if
88400   end if
88420   if value$='' then
88440     if lwrc$(key$)=lwrc$('Orientation' ) then value$= 'Portrait'
88460     if lwrc$(key$)=lwrc$('Height'      ) then value$= '11.000'
88480     if lwrc$(key$)=lwrc$('Width'       ) then value$= '8.500'
88500     if lwrc$(key$)=lwrc$('Lines'       ) then value$= '54'
88520     if lwrc$(key$)=lwrc$('FontSize'    ) then value$= '10'
88540     if lwrc$(key$)=lwrc$('TopMargin'   ) then value$= '0.500'
88560     if lwrc$(key$)=lwrc$('BottomMargin') then value$= '0.500'
88580     if lwrc$(key$)=lwrc$('LeftMargin'  ) then value$= '0.500'
88600     if lwrc$(key$)=lwrc$('RightMargin' ) then value$= '0.500'
88640     if env$('ACSDeveloper')='' then ! if not a developer
88660        fn_reg_write(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
88680     end if
88620   end if
88700   ! pr 'read   "'&value$&'" from Reg:'&env$('cursys')&'.'&prg$&'.Print.'&key$
88720   ! pause
88760 fnend
90000 def library fnwrite_program_print_property(key$*80,value$*256; prgCapForSettingsOverride$*256)
90020   if ~reg_setup then reg_setup=fn_reg_setup
90040   if env$('ACSDeveloper')<>'' and ~sreg_setup then sreg_setup=fn_sreg_setup
90060   on error goto ERTN
90080   dim prg$*256
90100   if prgCapForSettingsOverride$='' then
90120     prg$=env$('Program_Caption')
90140   else
90160     prg$=prgCapForSettingsOverride$
90180   end if
90200   if env$('ACSDeveloper')<>'' then
90220     fn_sreg_write(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
90240   else
90260     fn_reg_write(env$('cursys')&'.'&prg$&'.Print.'&key$,value$)
90270     ! pr 'writting "'&value$&'" to Reg:'&env$('cursys')&'.'&prg$&'.Print.'&key$
90272     ! pause
90280   end if
90300 fnend
