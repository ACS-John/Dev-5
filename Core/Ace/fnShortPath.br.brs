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
30000     let longpath$=trim$(longpath$,'"')
35000     if trim$(longpath$)='' then 
35020       let sp_return$=os_filename$('')&'\'
40000     else 
40020 !     execute 'sy -M '&os_filename$("S:\Core\shortpath.cmd")&' "'&longpath$&"' '&env$('temp')&"\sp_"&session$&'.txt'
40040       let fn_make_shortpath_cmd(env$('temp'))
40060       if do_debug then 
40080         execute 'sy '&env$('temp')&"\ShortPath.cmd"&' "'&longpath$&'" '&env$('temp')&"\sp_"&session$&'.txt'
40100       else 
40120         execute 'sy -M '&env$('temp')&"\ShortPath.cmd"&' "'&longpath$&'" '&env$('temp')&"\sp_"&session$&'.txt'
40140       end if 
40160       open #tmp:=fngethandle: 'Name='&env$('temp')&'\sp_'&session$&'.txt',display,input 
40180       linput #tmp: getfilepath_ln$
40200       if len(rtrm$(getfilepath_ln$))<256 then 
40220         if getfilepath_ln$='\' then 
40240           let sp_return$=rtrm$(longpath$)
40260         else 
40280           let sp_return$=longpath$
40300         end if  ! getfilepath_ln$='\'   /   else 
45000       else 
45020         print 'path returned from shortpath.cmd is longer than 256, but only 256 will fit - origional path is being returned instead.'
45040         print '  Please call ACS Support     or'
45060         print '  Type GO and press ENTER to continue.'
45080         pause 
45100         let sp_return$=rtrm$(getfilepath_ln$)
45120       end if 
45140       close #tmp,free: ioerr ignore
45160     end if 
45180     goto XIT
50280 XIT: ! 
50300     if sp_return$(len(sp_return$)-1:len(sp_return$))='\\' then let sp_return$=sp_return$(1:len(sp_return$)-1)
50320     let fnshortpath$=sp_return$
50380   fnend 
62000   def fn_make_shortpath_cmd(msc_path$*256)
62020     if ~exists(msc_path$&'\ShortPath.cmd') or do_debug then 
62040       open #msc_tmp:=fngethandle: 'name='&msc_path$&'\ShortPath.cmd,RecL=256,Replace',display,output 
62060       print #msc_tmp: "echo off"
62080       print #msc_tmp: "if '%1'=='' goto HELP"
62100       print #msc_tmp: "if '%2'=='' goto HELP"
62120       print #msc_tmp: "set homepath=""%~1"""
62140       print #msc_tmp: "set homedir=%~d1"
62160       print #msc_tmp: "for %%A in (%HOMEPATH%) do set HOMEPATH=%%~spnxA"
62180       print #msc_tmp: "echo %homedir%%homepath%"
62200       print #msc_tmp: "echo %homedir%%homepath% >%2"
62220       print #msc_tmp: "goto XIT"
62240       print #msc_tmp: ":HELP"
62260       print #msc_tmp: "echo Proper Usage is:"
62280       print #msc_tmp: "echo   %0 [path_input] [outputfile]"
62300       print #msc_tmp: "echo   [path_input] - a long path to parse.  quote encapsulation is required"
62320       print #msc_tmp: "echo                  i.e. ""C:\Long Path To Parse\etc"""
62340       print #msc_tmp: "echo   [outputfile] - file to output resulting short path to."
62360       print #msc_tmp: ":XIT"
63000       if do_debug then print #msc_tmp: "echo passed:  %*"
63020       if do_debug then print #msc_tmp: "echo returning:  %homedir%%homepath%"
63030       if do_debug then print #msc_tmp: "echo test new method:  %~s1"
63040       if do_debug then print #msc_tmp: "pause"
63060       close #msc_tmp: 
63080     end if 
63100   fnend 
80000 IGNORE: continue 
80100 ! <Updateable Region: ERTN>
80120 ERTN: let fnerror(program$,err,line,act$,"xit")
80140   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
80160   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
80230   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
80240 ERTN_EXEC_ACT: execute act$ : goto ERTN
80250 ! /region
