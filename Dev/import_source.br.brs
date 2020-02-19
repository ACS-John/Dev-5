10000   pr border: 'Import Source'
10020   execute 'con gui off'
10040   dim filename$(1)*256,prg_list$(1)*256,dir_destination$*128,dir_source$*128,ext$*128
10060   fn_import_source_init
10080   fn_import_source('','', 1)
10100 ! fn_import_source('\\DISKSTATION\public\ACS\Client','\\DISKSTATION\public\ACS\Source\Client')
10120   fn_import_source_run
10140   end 
10160   def fn_import_source(dir_source$*128,dir_destination$*128; skip_asci_files)
10180     dir_source$=rtrm$(dir_source$,'\')
10200     dir_destination$=rtrm$(dir_destination$,'\')
10220     execute 'sy -M xcopy '&dir_source$&'\*.* '&dir_destination$&'\*.* /t /y'
10240 ! fn_import_source_init
10260     fn_getdir2(dir_source$,mat filename$,'/s') ! option$)
10280     for file_item=1 to udim(mat filename$)
10300       pr f '10,10,Cc 60': 'phase 1 of 2: processing '&str$(file_item)&' of '&str$(udim(mat filename$))
10320       dot_pos=pos(filename$(file_item),'.',-1)
10340       backslash_pos=pos(filename$(file_item),'\',-1)
10360       ext$=''
10380       if backslash_pos<=0 or dot_pos>backslash_pos then 
10400         ext$=filename$(file_item)(dot_pos+1:len(filename$(file_item)))
10420       end if 
10440       ext$=lwrc$(ext$)
10460       if ext$='brs' or ext$='wbs' or ext$='libs' or ext$='cnvs' then 
10480         fn_import_source_add(filename$(file_item))
10500       else if ~skip_asci_files and (ext$='mnu' or ext$='txt' or ext$='cmd' or ext$='bat' or ext$='lay' or ext$='ini') then 
10520 !   IF pos(filename$(file_item),'100082.10')>0 then pause
10540         fn_copy_to_destination(filename$(file_item))
10560       end if  ! ext$=...
10580     next file_item
10600 ! fn_import_source_run
10620   fnend 
10640   def fn_import_source_init
10660     prg_list_count=0
10680     mat prg_list$(prg_list_count)
10700     open #proc_file:=1: 'Name=tmp'&session$&'.prc,RecL=1024,Replace',display,output 
10720   fnend  ! fn_import_source_init
10740   def fn_import_source_add(msa_item$*256)
10760     mat prg_list$(prg_list_count+=1)
10780     prg_list$(prg_list_count)=msa_item$
10800     if exists(srep$(msa_item$(1:len(msa_item$)-4),dir_source$,dir_destination$)) then 
10820       brs_date_modified=fn_datetime_modified(msa_item$)
10840       br_date_modified=fn_datetime_modified(srep$(msa_item$(1:len(msa_item$)-4),dir_source$,dir_destination$))
10860       if brs_date_modified=>br_date_modified then 
10880         pr #proc_file: 'Load '&msa_item$&',Source'
10900         pr #proc_file: 'renum labels_only'
10910         pr #proc_file: 'Replace '&srep$(msa_item$(1:len(msa_item$)-4),dir_source$,dir_destination$)
10920       end if 
10940     else 
10960       pr #proc_file: 'Load '&msa_item$&',Source'
10980       pr #proc_file: 'Save '&srep$(msa_item$(1:len(msa_item$)-4),dir_source$,dir_destination$)
11000     end if 
11020     pr #proc_file: ''
11040   fnend  ! fn_import_source_add
11060   def fn_datetime_modified(dm_filename$*256)
11080     dim dm_line$*256
11100     execute 'dir '&br_filename$(dm_filename$)&' >getdt_'&session$&'.tmp'
11120     open #h_dm:=25: 'Name=getdt_'&session$&'.tmp',display,input 
11140     linput #h_dm: dm_line$ ! consume header
11160     linput #h_dm: dm_line$
11180     close #h_dm,free: 
11200     dm_mm=val(dm_line$(27:28))
11220     dm_dd=val(dm_line$(30:31))
11240     dm_yy=val(dm_line$(33:34))
11260     dm_hh=val(dm_line$(37:38))
11280     dm_nn=val(dm_line$(40:41))
11300     if lwrc$(dm_line$(42:42))='p' then dm_hh+=12
11320     if dm_yy>80 then dm_cc=19 else dm_cc=20
11340     dm_cc=dm_cc*10000000000
11360     dm_yy=dm_yy*100000000
11380     dm_mm=dm_mm*1000000
11400     dm_dd=dm_dd*10000
11420     dm_hh=dm_hh*100
11440     dm_nn=dm_nn*1
11460     fn_datetime_modified=dm_cc+dm_yy+dm_mm+dm_dd+dm_hh+dm_nn
11480   fnend 
11500   def fn_import_source_run
11520     msr_file$=file$(proc_file)
11540     close #proc_file: 
11560     pr msr_file$ : pause  ! execute 'subproc '&msr_file$
11580   fnend  ! fn_import_source_run
11600   def fn_copy_to_destination(cts_item$*256)
11620     execute 'Copy "'&cts_item$&'" "'&srep$(cts_item$,dir_source$,dir_destination$)&'" -n'
11640   fnend  ! fn_copy_to_destination
11660   def fn_getdir2(dir$*256,mat filename$; option$,filter$*40)
11680 !
11700 ! Dir$=Directory to Read
11720 ! .            does not require but will accept a \ on the end
11740 ! filename$(x)=file names (includes path if /s option is used)
11760 ! option$: /s or /o-g or what ever you want use "dir /?"
11780 ! .        at dos prompt for complete list of options.
11800 !
11820     dim tmp$*255
11840 !
11860     mat filename$(0)
11880     filter$=trim$(filter$) : if filter$="" then filter$="*.*"
11900     option$=trim$(option$)
11920     dir$=trim$(dir$)
11940     if dir$(len(dir$):len(dir$))<>"\" then dir$=dir$&"\"
11960 ! _____________
11980     execute 'free '&env$('temp')&'\GetDir'&session$&'.tmp -n' ioerr ignore
12000     tmp$='Sy -M Dir '&os_filename$(rtrm$(dir$,'\'))&'\'&filter$&' /b '&option$&' >"'&os_filename$(env$('temp')&'\GetDir'&session$&'.tmp"')
12020     execute tmp$ ioerr XIT
12040     open #tf1:=20: "Name="&env$('temp')&"\GetDir"&session$&".tmp",display,input 
12060     filename_count=0
12080     do  ! for filename_count=1 to udim(filename$)
12100       linput #tf1: tmp$ eof XIT
12120       mat filename$(filename_count+=1)
12140       filename$(filename_count)=rtrm$(tmp$)
12160       if filename$(filename_count)=uprc$(filename$(filename_count)) then 
12180         filename$(filename_count)=lwrc$(filename$(filename_count)) ! never all caps-anything but
12200       end if  ! filename$(filename_count)=uprc$(filename$(filename_count))
12220     loop  ! next filename_count
12240     goto XIT
12260 !
12280 IGNORE: continue 
12300 XIT: ! 
12320     close #tf1,free: ioerr ignore
12340   fnend 
