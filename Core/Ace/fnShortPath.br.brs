10000 ! Replace S:\Core\Ace\fnShortPath
10020 ! returns a 'Progra~1' from a 'program files' or something like that
20000   def library fnshortpath$*256(longpath$*256)
20002 ! do_debug=1
20060     library 'S:\Core\Library': fngethandle,fnerror
20080     on error goto ERTN
20100     option retain 
20120 ! ______________________________________________________________________
20140     dim getfilepath_ln$*256,sp_return$*256
20160 ! ______________________________________________________________________
30000     longpath$=trim$(longpath$,'"')
35000     if trim$(longpath$)='' then 
35020       sp_return$=os_filename$('')&'\'
38000     else 
38020       csOption$=''
38040       if env$('BR_Model')='CLIENT/SERVER' then 
38060         if longpath$(1:2)='@:' then
38080           longpath$(1:2)=''
38100           csOption$='-@ '
38120         else
38140           csOption$='-s '
38160         end if
38180       end if
38200       if do_debug then
38220         optHide$=''
38240       else
38260         optHide$='-M '
38280       end if
40040       fn_make_shortpath_cmd(env$('temp'))
40120       execute 'sy '&csOption$&optHide$&env$('temp')&"\ShortPath.cmd"&' "'&longpath$&'" '&env$('temp')&"\sp_"&session$&'.txt'
40160       open #tmp:=fngethandle: 'Name='&env$('temp')&'\sp_'&session$&'.txt',display,input 
40180       linput #tmp: getfilepath_ln$
40200       if len(rtrm$(getfilepath_ln$))<256 then 
40220         if getfilepath_ln$='\' then 
40240           sp_return$=rtrm$(longpath$)
40260         else 
40280           sp_return$=longpath$
40300         end if  ! getfilepath_ln$='\'   /   else 
45000       else 
45020         pr 'path returned from shortpath.cmd is longer than 256, but only 256 will fit - origional path is being returned instead.'
45040         pr '  Please call ACS Support     or'
45060         pr '  Type GO and press ENTER to continue.'
45080         pause 
45100         sp_return$=rtrm$(getfilepath_ln$)
45120       end if 
45140       close #tmp,free: ioerr ignore
45160     end if 
45180     goto XIT
50280 XIT: ! 
50300     if sp_return$(len(sp_return$)-1:len(sp_return$))='\\' then sp_return$=sp_return$(1:len(sp_return$)-1)
50320     fnshortpath$=sp_return$
50380   fnend 
62000   def fn_make_shortpath_cmd(msc_path$*256)
62020     if ~exists(msc_path$&'\ShortPath.cmd') or do_debug then 
62040       open #msc_tmp:=fngethandle: 'name='&msc_path$&'\ShortPath.cmd,RecL=256,Replace',display,output 
62060       pr #msc_tmp: "echo off"
62080       pr #msc_tmp: "if '%1'=='' goto HELP"
62100       pr #msc_tmp: "if '%2'=='' goto HELP"
62120       pr #msc_tmp: "set homepath=""%~1"""
62140       pr #msc_tmp: "set homedir=%~d1"
62160       pr #msc_tmp: "for %%A in (%HOMEPATH%) do set HOMEPATH=%%~spnxA"
62180       pr #msc_tmp: "echo %homedir%%homepath%"
62200       pr #msc_tmp: "echo %homedir%%homepath% >%2"
62220       pr #msc_tmp: "goto XIT"
62240       pr #msc_tmp: ":HELP"
62260       pr #msc_tmp: "echo Proper Usage is:"
62280       pr #msc_tmp: "echo   %0 [path_input] [outputfile]"
62300       pr #msc_tmp: "echo   [path_input] - a long path to parse.  quote encapsulation is required"
62320       pr #msc_tmp: "echo                  i.e. ""C:\Long Path To Parse\etc"""
62340       pr #msc_tmp: "echo   [outputfile] - file to output resulting short path to."
62360       pr #msc_tmp: ":XIT"
63000       if do_debug then pr #msc_tmp: "echo passed:  %*"
63020       if do_debug then pr #msc_tmp: "echo returning:  %homedir%%homepath%"
63030       if do_debug then pr #msc_tmp: "echo test new method:  %~s1"
63040       if do_debug then pr #msc_tmp: "pause"
63060       close #msc_tmp: 
63080     end if 
63100   fnend 
80000 IGNORE: continue 
80100 ! <Updateable Region: ERTN>
80120 ERTN: let fnerror(program$,err,line,act$,"xit")
80140   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
80160   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
80230   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
80240 ERTN_EXEC_ACT: execute act$ : goto ERTN
80250 ! /region
