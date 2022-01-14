execute 'con gui off'
pr border: 'Make Source'
autoLibrary
dim filename$(1)*256,prg_list$(1)*256,dir_destination$*128,dir_source$*128,ext$*128
fn_make_source_init
! fn_make_source(':C:\ACS\Dev-5')
fn_make_source(':C:\WB')
! fn_make_source('c:\vol002')
fn_make_source_run
end
def fn_make_source(dir_source$*128)
	dim dir_destination$*128
	dir_source$=rtrm$(dir_source$,'\')
	dir_destination$=dir_source$
	fngetdir2(dir_source$,mat filename$,'/s') ! option$)
	for file_item=1 to udim(mat filename$)
		dot_pos=pos(filename$(file_item),'.',-1)
		backslash_pos=pos(filename$(file_item),'\',-1)
		ext$=''
		if backslash_pos<=0 or dot_pos>backslash_pos then
			ext$=filename$(file_item)(dot_pos+1:len(filename$(file_item)))
		end if
		ext$=lwrc$(ext$)
		if (ext$='br' or ext$='wb' or ext$='lib' or ext$='cnv') and pos(filename$(file_item),'264')<=0 then
			fn_make_source_add(filename$(file_item),dir_source$,dir_destination$)
		end if  ! ext$=...
	next file_item
 
fnend
def fn_make_source_init
	prg_list_count=0
	mat prg_list$(prg_list_count)
	open #proc_file:=1: 'Name=tmp'&session$&'.prc,RecL=1024,Replace',d,o
fnend  ! fn_make_source_init
def fn_make_source_add(msa_item$*256,dir_source$*128,dir_destination$*128)
	mat prg_list$(prg_list_count+=1)
	prg_list$(prg_list_count)=msa_item$
	! pr #proc_file: 'skip 2 IF ERR '&msa_item$
	! pr #proc_file: 'List <"'&msa_item$&'" >"'&srep$(lwrc$(msa_item$&'.brs'),lwrc$(dir_source$),dir_destination$)&'"'
	pr #proc_file: 'Load "'&msa_item$&'"'
	pr #proc_file: 'Renum Labels_Only'
	pr #proc_file: 'List  >"'&srep$(lwrc$(msa_item$&'.brs'),lwrc$(dir_source$),dir_destination$)&'"'
	! pr #proc_file: 'clear' ! or del 1,99999
	! pr #proc_file: '' ! or del 1,99999
fnend  ! fn_make_source_add
def fn_make_source_run
	dim msr_file$*256
	msr_file$=file$(proc_file)
	close #proc_file:
	pr os_filename$(msr_file$)
	! execute 'subproc '&msr_file$
fnend  ! fn_make_source_run
def fn_copy_to_source(cts_item$*256)
	! execute 'Copy "'&cts_item$&'" "'&srep$(cts_item$,dir_source$,dir_destination$)&'" -n'
fnend  ! fn_copy_to_source
