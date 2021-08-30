! Replace S:\Core\Ace\fnShortPath
! returns a 'Progra~1' from a 'program files' or something like that
def library fnshortpath$*256(longpath$*256)
	if env$('acsDebugShortPath')<>'' then do_debug=1
	autoLibrary
	on error goto Ertn
	option retain 
	!
	dim sp_return$*256
	dim getfilepath_ln$*256
	dim csTempPath$*256
	!
	longpath$=trim$(longpath$,'"')
	if trim$(longpath$)='' then 
		sp_return$=os_filename$('')&'\'
	else 
		csOption$=''
		if env$('BR_Model')='CLIENT/SERVER' then 
			if longpath$(1:2)='@:' then
				longpath$(1:2)=''
				csOption$='-@ '
				csTempPath$=env$('client_temp')&'\acs'
			else
				csOption$='-s '
				csTempPath$=env$('temp')&'\acs'
			end if
		else
				csTempPath$=env$('temp')&'\acs'
		end if
		if do_debug then
			optHide$=''
		else
			optHide$='-M '
		end if
		fnmakesurepathexists(csTempPath$&'\')
		fn_make_shortpath_cmd(csTempPath$)
		execute 'sy '&csOption$&optHide$&csTempPath$&"\ShortPath.cmd"&' "'&longpath$&'" '&csTempPath$&"\sp_"&session$&'.txt'
		open #tmp=fnH: 'Name='&csTempPath$&'\sp_'&session$&'.txt',display,input 
		linput #tmp: getfilepath_ln$
							if do_debug then pr 'getfilepath_ln$='&getfilepath_ln$ : pause
		if getfilepath_ln$='\' then 
			longpath$=rtrm$(longpath$)
			pr 'I thought this never happened and was safe to delete.' : if env$('acsDeveloper')<>'' then pause
		else 
			sp_return$=rtrm$(getfilepath_ln$) !    longpath$    changed this from longpath$ on 10/6/17 - just seems wrong and it wasn't working, so if fixed it.
		end if 
		close #tmp,free: ioerr ignore
	end if 
	goto Xit
	Xit: ! 
	if sp_return$(len(sp_return$)-1:len(sp_return$))='\\' then sp_return$=sp_return$(1:len(sp_return$)-1)
	fnshortpath$=sp_return$
fnend 
def fn_make_shortpath_cmd(msc_path$*256)
	if ~exists(msc_path$&'\ShortPath.cmd') or do_debug then 
		open #msc_tmp=fnH: 'name='&msc_path$&'\ShortPath.cmd,RecL=256,Replace',d,o 
		pr #msc_tmp: "echo off"
		pr #msc_tmp: "if '%1'=='' goto HELP"
		pr #msc_tmp: "if '%2'=='' goto HELP"
		pr #msc_tmp: "set homepath=""%~1"""
		pr #msc_tmp: "set homedir=%~d1"
		pr #msc_tmp: "for %%A in (%HOMEPATH%) do set HOMEPATH=%%~spnxA"
		pr #msc_tmp: "echo %homedir%%homepath%"
		pr #msc_tmp: "echo %homedir%%homepath% >%2"
		pr #msc_tmp: "goto Xit"
		pr #msc_tmp: ":HELP"
		pr #msc_tmp: "echo Proper Usage is:"
		pr #msc_tmp: "echo   %0 [path_input] [outputfile]"
		pr #msc_tmp: "echo   [path_input] - a long path to parse.  quote encapsulation is required"
		pr #msc_tmp: "echo                  i.e. ""C:\Long Path To Parse\etc"""
		pr #msc_tmp: "echo   [outputfile] - file to output resulting short path to."
		pr #msc_tmp: ":Xit"
		if do_debug then pr #msc_tmp: "echo passed:  %*"
		if do_debug then pr #msc_tmp: "echo returning:  %homedir%%homepath%"
		if do_debug then pr #msc_tmp: "echo test new method:  %~s1"
		if do_debug then pr #msc_tmp: "pause"
		close #msc_tmp: 
	end if 
fnend 
include: ertn