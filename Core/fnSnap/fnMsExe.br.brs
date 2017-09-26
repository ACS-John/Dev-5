22000 def library fnMsExe$*100(l$) 
22020   ! this version is modified by ACS to work in our environment.
22040   ! Returns the installed path of Microsoft programs such as
22060   ! WinWord i.e. fnMsExe$("winword.exe")
22080   library 'S:\Core\Library': fngethandle,fncopy
22100   dim pathToBrRegister$*256
32000   if env$('BR_MODEL')='CLIENT/SERVER' then
32020     pathToBrRegister$='C:\ProgramData\ACS'
32040   else !  not client server
32060     pathToBrRegister$=env$("temp")&'\ACS'
32080   end if
32100   if ~exists(env$('at')&pathToBrRegister$&'\BRRegister2.exe') then
32120     fncopy('S:\Core\fnSnap\BRRegister2.exe',env$('at')&pathToBrRegister$&"\BRRegister2.exe")
32140   end if
38000   execute "sys -M "&pathToBrRegister$&"\BRRegister2.exe -B"&session$&" -N"&l$
38020   let exefil=1
38040   open #exefil:=fngethandle: "name="&env$('at')&pathToBrRegister$&"\dbde"&session$&".txt",display,input 
38060   dim exefil$*100
38080   linput #exefil: exefil$
38100   close #exefil,free: 
38120   let fnMsExe$=exefil$
38140   let exefil=0
38160 fnend 