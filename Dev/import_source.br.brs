	pr border: 'Import Source'
	execute 'con gui off'
	dim filename$(1)*256,prg_list$(1)*256,dir_destination$*128,dir_source$*128,ext$*128
	fn_import_source_init
	fn_import_source('','', 1)
! fn_import_source('\\DISKSTATION\public\ACS\Client','\\DISKSTATION\public\ACS\Source\Client')
	fn_import_source_run
	end 
	def fn_import_source(dir_source$*128,dir_destination$*128; skip_asci_files)
		dir_source$=rtrm$(dir_source$,'\')
		dir_destination$=rtrm$(dir_destination$,'\')
		execute 'sy -M xcopy '&dir_source$&'\*.* '&dir_destination$&'\*.* /t /y'
! fn_import_source_init
		fn_getdir2(dir_source$,mat filename$,'/s') ! option$)
		for file_item=1 to udim(mat filename$)
			pr f '10,10,Cc 60': 'phase 1 of 2: processing '&str$(file_item)&' of '&str$(udim(mat filename$))
			dot_pos=pos(filename$(file_item),'.',-1)
			backslash_pos=pos(filename$(file_item),'\',-1)
			ext$=''
			if backslash_pos<=0 or dot_pos>backslash_pos then 
				ext$=filename$(file_item)(dot_pos+1:len(filename$(file_item)))
			end if 
			ext$=lwrc$(ext$)
			if ext$='brs' or ext$='wbs' or ext$='libs' or ext$='cnvs' then 
				fn_import_source_add(filename$(file_item))
			else if ~skip_asci_files and (ext$='mnu' or ext$='txt' or ext$='cmd' or ext$='bat' or ext$='lay' or ext$='ini') then 
!   IF pos(filename$(file_item),'100082.10')>0 then pause
				fn_copy_to_destination(filename$(file_item))
			end if  ! ext$=...
		next file_item
! fn_import_source_run
	fnend 
	def fn_import_source_init
		prg_list_count=0
		mat prg_list$(prg_list_count)
		open #proc_file:=1: 'Name=tmp'&session$&'.prc,RecL=1024,Replace',display,output 
	fnend  ! fn_import_source_init
	def fn_import_source_add(msa_item$*256)
		mat prg_list$(prg_list_count+=1)
		prg_list$(prg_list_count)=msa_item$
		if exists(srep$(msa_item$(1:len(msa_item$)-4),dir_source$,dir_destination$)) then 
			brs_date_modified=fn_datetime_modified(msa_item$)
			br_date_modified=fn_datetime_modified(srep$(msa_item$(1:len(msa_item$)-4),dir_source$,dir_destination$))
			if brs_date_modified=>br_date_modified then 
				pr #proc_file: 'Load '&msa_item$&',Source'
				pr #proc_file: 'renum labels_only'
				pr #proc_file: 'Replace '&srep$(msa_item$(1:len(msa_item$)-4),dir_source$,dir_destination$)
			end if 
		else 
			pr #proc_file: 'Load '&msa_item$&',Source'
			pr #proc_file: 'Save '&srep$(msa_item$(1:len(msa_item$)-4),dir_source$,dir_destination$)
		end if 
		pr #proc_file: ''
	fnend  ! fn_import_source_add
	def fn_datetime_modified(dm_filename$*256)
		dim dm_line$*256
		execute 'dir '&br_filename$(dm_filename$)&' >getdt_'&session$&'.tmp'
		open #h_dm:=25: 'Name=getdt_'&session$&'.tmp',display,input 
		linput #h_dm: dm_line$ ! consume header
		linput #h_dm: dm_line$
		close #h_dm,free: 
		dm_mm=val(dm_line$(27:28))
		dm_dd=val(dm_line$(30:31))
		dm_yy=val(dm_line$(33:34))
		dm_hh=val(dm_line$(37:38))
		dm_nn=val(dm_line$(40:41))
		if lwrc$(dm_line$(42:42))='p' then dm_hh+=12
		if dm_yy>80 then dm_cc=19 else dm_cc=20
		dm_cc=dm_cc*10000000000
		dm_yy=dm_yy*100000000
		dm_mm=dm_mm*1000000
		dm_dd=dm_dd*10000
		dm_hh=dm_hh*100
		dm_nn=dm_nn*1
		fn_datetime_modified=dm_cc+dm_yy+dm_mm+dm_dd+dm_hh+dm_nn
	fnend 
	def fn_import_source_run
		msr_file$=file$(proc_file)
		close #proc_file: 
		pr msr_file$ : pause  ! execute 'subproc '&msr_file$
	fnend  ! fn_import_source_run
	def fn_copy_to_destination(cts_item$*256)
		execute 'Copy "'&cts_item$&'" "'&srep$(cts_item$,dir_source$,dir_destination$)&'" -n'
	fnend  ! fn_copy_to_destination
def fn_getdir2(dir$*256,mat filename$; option$,filter$*40)

	! Dir$=Directory to Read
	!              does not require but will accept a \ on the end
	! filename$(x)=file names (includes path if /s option is used)
	! option$: /s or /o-g or what ever you want use "dir /?"
	!          at dos prompt for complete list of options.

	dim tmp$*255

	mat filename$(0)
	filter$=trim$(filter$) : if filter$="" then filter$="*.*"
	option$=trim$(option$)
	dir$=trim$(dir$)
	if dir$(len(dir$):len(dir$))<>"\" then dir$=dir$&"\"
	execute 'free '&env$('temp')&'\GetDir'&session$&'.tmp -n' ioerr ignore
	tmp$='Sy -M Dir '&os_filename$(rtrm$(dir$,'\'))&'\'&filter$&' /b '&option$&' >"'&os_filename$(env$('temp')&'\GetDir'&session$&'.tmp"')
	execute tmp$ ioerr XIT
	open #tf1:=20: "Name="&env$('temp')&"\GetDir"&session$&".tmp",display,input 
	filename_count=0
	do  ! for filename_count=1 to udim(filename$)
		linput #tf1: tmp$ eof XIT
		mat filename$(filename_count+=1)
		filename$(filename_count)=rtrm$(tmp$)
		if filename$(filename_count)=uprc$(filename$(filename_count)) then 
			filename$(filename_count)=lwrc$(filename$(filename_count)) ! never all caps-anything but
		end if  ! filename$(filename_count)=uprc$(filename$(filename_count))
	loop  ! next filename_count
	goto XIT

	XIT: ! 
	close #tf1,free: ioerr ignore
fnend 
