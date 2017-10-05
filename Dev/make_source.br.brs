00010   execute 'con gui off'
00020   pr border: 'Make Source'
00030   library 'S:\Core\Library': fngetdir2
00040   dim filename$(1)*256,prg_list$(1)*256,dir_destination$*128,dir_source$*128,ext$*128
00050   fn_make_source_init
00060   fn_make_source(':C:\ACS\Dev-5')
00070 ! fn_make_source('c:\vol002')
00080   fn_make_source_run
00090   end 
00100   def fn_make_source(dir_source$*128)
00110     dim dir_destination$*128
00120     dir_source$=rtrm$(dir_source$,'\')
00130     dir_destination$=dir_source$
00140     fngetdir2(dir_source$,mat filename$,'/s') ! option$)
00150     for file_item=1 to udim(mat filename$)
00160       dot_pos=pos(filename$(file_item),'.',-1)
00170       backslash_pos=pos(filename$(file_item),'\',-1)
00180       ext$=''
00190       if backslash_pos<=0 or dot_pos>backslash_pos then 
00200         ext$=filename$(file_item)(dot_pos+1:len(filename$(file_item)))
00210       end if 
00220       ext$=lwrc$(ext$)
00230       if (ext$='br' or ext$='wb' or ext$='lib' or ext$='cnv') and pos(filename$(file_item),'264')<=0 then 
00240         fn_make_source_add(filename$(file_item),dir_source$,dir_destination$)
00250       end if  ! ext$=...
00260     next file_item
00270 ! 
00280   fnend 
00290   def fn_make_source_init
00300     prg_list_count=0
00310     mat prg_list$(prg_list_count)
00320     open #proc_file:=1: 'Name=tmp'&session$&'.prc,RecL=1024,Replace',display,output 
00330   fnend  ! fn_make_source_init
00340   def fn_make_source_add(msa_item$*256,dir_source$*128,dir_destination$*128)
00350     mat prg_list$(prg_list_count+=1)
00360     prg_list$(prg_list_count)=msa_item$
00370 ! pr #proc_file: 'SKIP 2 IF ERR '&msa_item$
00380     pr #proc_file: 'List <"'&msa_item$&'" >"'&srep$(lwrc$(msa_item$&'.brs'),lwrc$(dir_source$),dir_destination$)&'"'
00390 ! pr #proc_file: 'clear' ! or del 1,99999
00392 ! pr #proc_file: '' ! or del 1,99999
00400   fnend  ! fn_make_source_add
00410   def fn_make_source_run
00420     dim msr_file$*256
00430     msr_file$=file$(proc_file)
00440     close #proc_file: 
00450     pr os_filename$(msr_file$)
00460 ! execute 'subproc '&msr_file$
00470   fnend  ! fn_make_source_run
00480   def fn_copy_to_source(cts_item$*256)
00490 ! execute 'Copy "'&cts_item$&'" "'&srep$(cts_item$,dir_source$,dir_destination$)&'" -n'
00500   fnend  ! fn_copy_to_source
