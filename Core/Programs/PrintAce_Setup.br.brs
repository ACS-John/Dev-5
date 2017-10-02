12000   if ~setup then let fn_setup
12040   let fntop(program$, cap$="PrintAce Install Dependencies")
13000   execute 'Sy -w '&os_filename$('S:\Core\ACS_PrAce_Support_Install_ocx.exe')
13020   execute 'Sy '&os_filename$('S:\Core\ACS_PrAce_Reg.cmd')&' /s'
60000 XIT: let fnxit
70000   def fn_setup
70020     if ~setup then 
70040       let setup=1
70080       library 'S:\Core\Library': fntop,fnxit,fnerror
70100       on error goto ERTN
70120       dim cap$*128
70140     end if 
70990   fnend 
72000 IGNORE: continue 
74000 ! <Updateable Region: ERTN>
74040 ERTN: let fnerror(program$,err,line,act$,"xit")
74060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
74080   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
74100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
74120 ERTN_EXEC_ACT: execute act$ : goto ERTN
74140 ! /region
