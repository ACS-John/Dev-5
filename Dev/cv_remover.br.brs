10000   library 'S:\Core\Library': fngethandle
10020   open #h_filelist:=fngethandle: 'name=filelist.txt',display,input 
10040   dim line$*1048,program_file$*256,xcv$*256
20000   do 
20020     linput #h_filelist: program_file$ eof FILELIST_EOF
20022     if program_file$(1:1)<>'!' then 
20030 !     pr 'File: '&program_file$ : pause
20060       let xcv$=fn_get_variable_value$('cv$',program_file$)
20100       pr xcv$
20120       if xcv$<>'' then 
20140         let fn_replace_in_file(program_file$,'cv$',xcv$)
20160         let fn_replace_in_file(program_file$,'let '&xcv$&'='&xcv$,'!')
20180       end if 
20190     end if 
20200   loop 
20220 FILELIST_EOF: ! 
20240   end 
40000   def fn_replace_in_file(program_file$*256,from$*256,to$*256)
40020     dim tmp$*512
40060     close #h_file: ioerr ignore
40080     open #h_file: 'name='&program_file$,display,input 
40100     open #h_out:=fngethandle: 'name=new.brs,recl=1048,replace',display,output 
40120     restore #h_file: 
40140     do 
40160       linput #h_file: line$ eof RIF_EOF
40180       let line$=srep$(line$,from$,to$)
40200       pr #h_out: line$
40220     loop 
40240 RIF_EOF: ! 
40260     pr 'from file: '&file$(h_file)
40280     pr ' new file: '&file$(h_out)
40290     let tmp$='copy '&file$(h_out)&' '&file$(h_file)
40300     close #h_out: 
40320     close #h_file: 
40360     execute tmp$
40380 !    pause
40400   fnend 
50000   def fn_get_variable_value$*256(var$,program_file$*256)
50020 ! 
50040     let h_file=101
50060     close #h_file: ioerr ignore
50080     open #h_file: 'name='&program_file$,display,input 
50100     dim gvv_return$*256
50120     let gvv_return$=''
50140     restore #h_file: 
50160     do 
50180       linput #h_file: line$ eof GV_EOF
50200       let line$=trim$(line$)
50220       do while pos(line$,' =')>0
50240         let line$=srep$(line$,' =','=')
50260       loop 
50280       let var_set_pos=pos(lwrc$(line$),' '&lwrc$(var$)&'=')
50300       if var_set_pos>0 then 
50320         let gvv_return$=line$(var_set_pos+len(' '&lwrc$(var$)&'='):len(line$))
50340         goto GV_EOF
50360       end if 
50380     loop 
50400 GV_EOF: ! 
50420     close #h_file: 
50440     let fn_get_variable_value$=gvv_return$
50460   fnend 
60360 IGNORE: continue 
