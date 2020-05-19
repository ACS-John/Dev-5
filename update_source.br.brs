autoLibrary
pr border: 'Import Source'
execute 'con gui off'
fn_updatesource

def fn_filedatetime(filename$*255)
	dim infoline$*255,hh$*2
	execute "sy -M dir /N "&filename$&" >(import)\fileinfo"
	open #fileinfo:=21: "Name=(import)\fileinfo",display,input 
	do 
		linput #fileinfo: infoline$ eof NODATE
		if infoline$(3:3)="/" then goto PARSEDATE
	loop 
	PARSEDATE: ! 
	hh=val(infoline$(13:14))
	if infoline$(19:20)="PM" then hh+=12
	if hh<10 then hh$="0"&str$(hh) else hh$=str$(hh)
	fn_filedatetime=val(infoline$(7:10)&infoline$(1:2)&infoline$(4:5)&hh$&infoline$(16:17))
	goto GOTDATE
	NODATE: ! 
	fn_filedatetime=190001010800
	GOTDATE: ! 
	close #fileinfo,free: ioerr ignore
fnend 
def fn_initupdate(&lastcompile)
	dim lasttime$*256
	fnreg_read("LastCompile",lasttime$)
	if lasttime$="" then lastcompile=190001010800 else lastcompile=val(lasttime$)
	curtime=fn_datetime
	fnreg_write("LastCompile",str$(curtime))
	fnreg_write("OldLastCompile", str$(lastcompile))
	if not exists("(import)") then execute "sy -M md (import)"
	open #proc_file:=1: 'name=(import)\compile.prc,RecL=1024,Replace',display,output 
fnend 
def fn_updatesource
	dim filename$*255,msr_file$*255
	fn_initupdate(lastcompile)
	execute "sy -M sortfiles -D . -C "".br.brs|.br""" ioerr ROLLBACK
	open #dirfile:=20: "Name=(import)\brsfiles",display,input 
	do 
		linput #dirfile: filename$ eof DONE
		pr #proc_file: 'Load '&filename$&',Source'
		if exists(filename$(1:len(filename$)-4)) then 
			pr #proc_file: 'Replace '&filename$(1:len(filename$)-4)
		else 
			pr #proc_file: 'Save '&filename$(1:len(filename$)-4)
		end if 
		pr #proc_file: ''
	loop 
	goto DONE
	ROLLBACK: ! 
	fnreg_write("LastCompile", str$(lastcompile))
	DONE: ! 
	close #dirfile: ioerr ignore
	msr_file$=file$(proc_file)
	close #proc_file: 
	execute "subproc "&msr_file$
fnend 
def fn_datetime
	dim tm$*8
	tm$=time$
	fn_datetime=val(date$("CCYYMMDD")&tm$(1:2)&tm$(4:5))
fnend 