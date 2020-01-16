library program$ : fncheckcompiled
let fncheckcompiled
end
def library fncheckcompiled
	if env$('acsEnableComplier')='Yes' then
		dim filename$*256
		! pr 'entered into fncheckcompiled' : pause
		execute 'CD S:'
		if ~exists('S:\(import)') then execute 'mkdir S:\(import)'
		execute 'sy -M '&os_filename$('S:\sortfiles.exe')&' -D . -C ".br.brs|.br"' ioerr DONE
		open #hBrsFileList:=20: "Name=S:\(import)\brsfiles",display,input ioerr CC_ERR
		linput #hBrsFileList: filename$ eof DONE
		! pr filename$ : pause
		if env$('compile_without_asking')='Yes' then
			let docompile=2
			let setenv('compile_without_asking','')
		else
			let docompile=msgbox("You have uncompiled source files!  Recompile?", "ACS 5 - "&os_filename$(program$), "Yn", "Qst")
		end if
		if docompile=2 then
			let setenv("AfterRecompile", "S:\Core\Start")
			chain 'S:\Core\ReCompile.br' ! execute "Proc S:\ReCompile.prc" ioerr ignore
		end if
		DONE: !
		close #hBrsFileList:
	end if
fnend
CC_ERR: ! r:
	mb_response=msgbox(program$&' encountered an error '&str$(err)&' on line '&str$(line)&'.'&chr$(13)&'Close ACS?'&chr$(13)&'(Choose Cancel for developer pause.)','ACS 5 - S:\Core\CheckCompiled - Error','OKc','Excl')
	if mb_response=1 then execute 'system'
	pause
retry  ! /r
