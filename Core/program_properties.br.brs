08000 def fn_setup
08020   let setup_library=1
08030   ! library 'S:\Core\Library': fnacs,fnlbl,fntxt,fncomboa,fnfra,fntab,fncmdkey,fntos
08032   library 'S:\Core\Library': fnfm
08040   library 'S:\Core\Library': fnerror,fnBackgroundDisable
08100 fnend 
22000 def library fnprogram_properties(; forceProgramCaption$*256)
22020   on error goto ERTN
22040   if ~setup_library then let fn_setup
22060   if forceProgramCaption$<>'' then 
22080     dim oldCap$*256
22100     oldCap$=env$('Program_Caption')
22120     setenv('Program_Caption',forceProgramCaption$)
22140   end if
22160   fnBackgroundDisable(1)
22180   fnfm('Properties')
22200   if forceProgramCaption$<>'' then 
22220     setenv('Program_Caption',oldCap$)
22240   end if
22260   fnBackgroundDisable(0)
48380 fnend 
50000 ERTN: ! r:
50020   fnerror(program$,err,line,act$,"xit")
50040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
50060   if trim$(env$("ACSDeveloper"))<>"" then 
50080     execute "list -"&str$(line) : pause : goto ERTN_EXEC_ACT
50100   end if 
50120   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue."
50140   pr "" : pause : goto ERTN_EXEC_ACT
50160 ERTN_EXEC_ACT: execute act$ : goto ERTN ! /r
56000 def library fnprogram_ini_filename$*256(pif_program$*256; doNotCreate) ! 
56002   if ~setup_library then let fn_setup
56010   fnprogram_ini_filename$=fn_program_ini_filename$(pif_program$, doNotCreate)
56020 fnend 
58000 def fn_program_ini_filename$*256(pif_program$*256; doNotCreate)
58020   dim pif_return$*256
58060   let pif_return$=''
58080   let pif_program$=trim$(pif_program$)
59020     posDotBr=pos(pif_program$,'.br')
59040     if posDotBr>0 then pif_program$(posDotBr:posDotBr+2)=''
60020   let pif_return$=env$('Q')&'\INI\'&pif_program$&'.ini'
62000   ! if env$('ACSDeveloper')<>'' then pr 'fn_program_ini_filename$ > pif_return$="'&pif_return$&'"' : pause
62020   fn_program_ini_filename$=pif_return$ ! pr pif_return$ : pause
62040 fnend 
