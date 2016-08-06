18000   def library fnreg_read(rr_field_name$*128,&rr_field_value$)
18020     if ~reg_setup then let reg_setup=fn_reg_setup
18040     let fnreg_read=fn_reg_read(rr_field_name$,rr_field_value$)
18060   fnend 
18080   def library fnreg_write(rw_field_name$*128,rw_field_value$*256)
18100     if ~reg_setup then let reg_setup=fn_reg_setup
18120     let fnreg_write=fn_reg_write(rw_field_name$,rw_field_value$)
18140   fnend 
20000   def fn_reg_read(rr_field_name$*128,&rr_field_value$)
20040     dim rr_tmpfield_value$*256,rr_key_compare$*128
20060     let rr_field_name$=rpad$(lwrc$(trim$(rr_field_name$)),128)
20080     let rr_tmpfield_value$=rr_field_value$=''
20100 ! print 'read #reg_h'
20120     read #reg_h,using 'form pos 1,C 128,v 256',key=rr_field_name$,release: rr_key_compare$,rr_tmpfield_value$ ioerr REG_LOAD_IOERR ! XXX
20140 REG_LOAD_IOERR: ! 
20150     if rr_key_compare$=rr_field_name$ then 
20156       let rr_field_value$=rtrm$(rr_tmpfield_value$)
20160     else 
20162       let rr_field_value$=''
20170     end if 
20180 ! print 'load ';trim$(rr_field_name$);'=';rr_field_value$
20200   fnend 
20220   def fn_reg_write(rw_field_name$*128,rw_field_value$*256)
20260     let rw_field_name$=rpad$(lwrc$(trim$(rw_field_name$)),128)
20280     rewrite #reg_h,using 'form pos 1,c 128,c 256',key=rw_field_name$: rw_field_name$,rw_field_value$ nokey REG_WRITE ! XXX
20300 ! print 'rewrite #reg_h'
20320     goto REG_SAVE_XIT
20340 REG_WRITE: ! 
20360     write #reg_h,using 'form pos 1,c 128,c 256': rw_field_name$,rw_field_value$
20380 ! print 'write #reg_h'
20400 REG_SAVE_XIT: ! 
20420 ! print 'save ';trim$(rw_field_name$);'=';rw_field_value$
20440   fnend  ! fnreg_write
20460   def fn_reg_setup
20480     library 'R:\Core\library': fngethandle,fnerror,fnunique_computer_id$
20500     open #reg_h:=fngethandle: 'Name=Q:\data\reg.dat,Version=1,KFName=Q:\data\reg.idx,Use,RecL=384,KPs=1,KLn=128,Shr',internal,outin,keyed 
20520     let fn_reg_setup=1
20530     on error goto ERTN
20540   fnend  ! fn_reg_setup
22000   def library fnreg_close ! closes both the creg and the reg
22020     close #reg_h: ioerr ignore
22040     let reg_setup=0
22060     let fn_creg_close
22100 XIT: ! This XIT label is only for use by ERTN - fnerror - if they try to exit a failed read or write to the registry, let them just skip on past
22120   fnend 
23000   def fn_creg_close
23020     close #creg_h: ioerr ignore
23040     let creg_setup=0
23060   fnend 
42000 IGNORE: continue 
44000 ! <updateable region: ertn>
44020 ERTN: let fnerror(cap$,err,line,act$,"xit")
44040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
44060   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
44080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
44100 ERTN_EXEC_ACT: execute act$ : goto ERTN
44120 ! </updateable region: ertn>
60000   def library fncreg_read(cr_field_name$*128,&cr_field_value$)
60020     let fn_creg_setup
60040     dim cr_tmpfield_value$*256,cr_key_compare$*128
60060     let cr_field_name$=rpad$(lwrc$(trim$(cr_field_name$)),128)
60080     let cr_tmpfield_value$=cr_field_value$=''
60100 ! print 'read #creg_h'
60120     read #creg_h,using 'form pos 1,C 128,v 256',key=cr_field_name$,release: cr_key_compare$,cr_tmpfield_value$ ioerr CREG_LOAD_IOERR ! XXX
60140 CREG_LOAD_IOERR: ! 
60150     if cr_key_compare$=cr_field_name$ then 
60156       let cr_field_value$=rtrm$(cr_tmpfield_value$)
60160     else 
60162       let cr_field_value$=''
60170     end if 
60180 ! print 'load ';trim$(cr_field_name$);'=';cr_field_value$
60200   fnend  ! fncreg_read
61000   def library fncreg_write(cw_field_name$*128,cw_field_value$*256)
61020     let fn_creg_setup
61040     let cw_field_name$=rpad$(lwrc$(trim$(cw_field_name$)),128)
61060     rewrite #creg_h,using 'form pos 1,c 128,c 256',key=cw_field_name$: cw_field_name$,cw_field_value$ nokey CREG_WRITE ! XXX
61080 ! print 'rewrite #creg_h'
61100     goto CREG_SAVE_XIT
61120 CREG_WRITE: ! 
61140     write #creg_h,using 'form pos 1,c 128,c 256': cw_field_name$,cw_field_value$
61160 ! print 'write #creg_h'
61180 CREG_SAVE_XIT: ! 
61200 ! print 'save ';trim$(cw_field_name$);'=';cw_field_value$
61220   fnend 
62000   def fn_creg_setup
62020     let cno=val(env$('CNo'))
62040     if creg_setup<>cno then 
62060       if creg_setup>0 then let fn_creg_close
62080 ! 
62100       library 'R:\Core\library': fngethandle,fnerror
62120       open #creg_h:=fngethandle: 'Name=Q:\'&env$('CurSys')&'mstr\reg-'&env$('CurSys')&'.h'&env$('CNo')&',Version=1,KFName=Q:\'&env$('CurSys')&'mstr\reg-'&env$('CurSys')&'-idx.h'&env$('CNo')&',Use,RecL=384,KPs=1,KLn=128,Shr',internal,outin,keyed 
62140       let fn_creg_setup=cno
62160       let creg_setup=cno
62180     end if 
62200     on error goto ERTN
62220   fnend 
68000   def library fnureg_read(ur_field_name$*128,&ur_field_value$)
68020     if ~reg_setup then let reg_setup=fn_reg_setup
68040     let fnureg_read=fn_reg_read(fnunique_computer_id$&'.'&ur_field_name$,ur_field_value$)
68060   fnend 
68080   def library fnureg_write(uw_field_name$*128,uw_field_value$*256)
68100     if ~reg_setup then let reg_setup=fn_reg_setup
68120     let fnureg_write=fn_reg_write(fnunique_computer_id$&'.'&uw_field_name$,uw_field_value$)
68140   fnend 
