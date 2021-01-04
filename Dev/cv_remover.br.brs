autoLibrary
open #h_filelist=fnH: 'name=filelist.txt',display,input 
do 
	dim program_file$*256
	linput #h_filelist: program_file$ eof FILELIST_EOF
	if program_file$(1:1)<>'!' then 
		!     pr 'File: '&program_file$ : pause
		dim xcv$*256
		xcv$=fn_get_variable_value$('cv$',program_file$)
		pr xcv$
		if xcv$<>'' then 
			fn_replace_in_file(program_file$,'cv$',xcv$)
			fn_replace_in_file(program_file$,'let '&xcv$&'='&xcv$,'!')
		end if 
	end if 
loop 
FILELIST_EOF: ! 
end 
def fn_replace_in_file(program_file$*256,from$*256,to$*256)
	dim tmp$*512
	close #h_file: ioerr ignore
	open #h_file: 'name='&program_file$,display,input 
	open #h_out=fnH: 'name=new.brs,recl=1048,replace',display,output 
	restore #h_file: 
	do 
		dim line$*1048
		linput #h_file: line$ eof RIF_EOF
		line$=srep$(line$,from$,to$)
		pr #h_out: line$
	loop 
	RIF_EOF: ! 
	pr 'from file: '&file$(h_file)
	pr ' new file: '&file$(h_out)
	tmp$='copy '&file$(h_out)&' '&file$(h_file)
	close #h_out: 
	close #h_file: 
	execute tmp$
	!    pause
fnend 
def fn_get_variable_value$*256(var$,program_file$*256)

	h_file=101
	close #h_file: ioerr ignore
	open #h_file: 'name='&program_file$,display,input 
	dim gvv_return$*256
	gvv_return$=''
	restore #h_file: 
	do 
		linput #h_file: line$ eof GV_EOF
		line$=trim$(line$)
		do while pos(line$,' =')>0
			line$=srep$(line$,' =','=')
		loop 
		var_set_pos=pos(lwrc$(line$),' '&lwrc$(var$)&'=')
		if var_set_pos>0 then 
			gvv_return$=line$(var_set_pos+len(' '&lwrc$(var$)&'='):len(line$))
			goto GV_EOF
		end if 
	loop 
	GV_EOF: ! 
	close #h_file: 
	fn_get_variable_value$=gvv_return$
fnend 
