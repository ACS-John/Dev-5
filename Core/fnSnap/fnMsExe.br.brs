def library fnMsExe$*256(l$)
	! this version is modified by ACS to work in our environment.
	! Returns the installed path of Microsoft programs such as
	! WinWord i.e. fnMsExe$("winword.exe")
	library 'S:\Core\Library': fngethandle
	execute "sys -M "&fn_pathToBrRegister$&"\BRRegister2.exe -B"&session$&" -N"&l$
	exefil=1
	ope #exefil:=fngethandle: "name="&env$('at')&fn_pathToBrRegister$&"\dbde"&session$&".txt",display,input
	dim exefil$*2048
	lin #exefil: exefil$
	clo #exefil,free:
	fnMsExe$=exefil$(1:256)
	exefil=0
fnend
def fn_pathToBrRegister$*256
	if ~setup_pathToBrRegister then
		library 'S:\Core\Library': fncopy
		dim pathToBrRegister$*256
		if env$('BR_MODEL')='CLIENT/SERVER' then
			pathToBrRegister$='C:\ProgramData\ACS'
		else !  not client server
			pathToBrRegister$=env$("temp")&'\ACS'
		end if
		if ~exists(env$('at')&pathToBrRegister$&'\BRRegister2.exe') then
			fncopy('S:\Core\fnSnap\BRRegister2.exe',env$('at')&pathToBrRegister$&"\BRRegister2.exe")
		end if
		
		setup_pathToBrRegister=1
	end if

	fn_pathToBrRegister$=pathToBrRegister$
fnend
