10000 ! Replace S:\Core\Ace\fnShortPath
10020 ! returns a 'Progra~1' from a 'program files' or something like that
20000 def library fnshortpath$*256(longpath$*256)
20020   if env$('acsDebugShortPath')<>'' then do_debug=1
20040   library 'S:\Core\Library': fngethandle,fnerror,fnmakesurepathexists
20060   on error goto ERTN
20080   option retain 
20100   !
20102   dim sp_return$*256
20104   dim getfilepath_ln$*256
20106   dim csTempPath$*256
20140   !
30000   longpath$=trim$(longpath$,'"')
36000   if trim$(longpath$)='' then 
36020     sp_return$=os_filename$('')&'\'
36040   else 
36060     csOption$=''
36080     if env$('BR_Model')='CLIENT/SERVER' then 
36100       if longpath$(1:2)='@:' then
36120         longpath$(1:2)=''
36140         csOption$='-@ '
36160         csTempPath$=env$('client_temp')&'\acs'
36180       else
36200         csOption$='-s '
36220         csTempPath$=env$('temp')&'\acs'
36240       end if
36260     else
36280         csTempPath$=env$('temp')&'\acs'
36300     end if
36320     if do_debug then
36340       optHide$=''
36360     else
36380       optHide$='-M '
36400     end if
36420     fnmakesurepathexists(csTempPath$&'\')
40040     fn_make_shortpath_cmd(csTempPath$)
40120     execute 'sy '&csOption$&optHide$&csTempPath$&"\ShortPath.cmd"&' "'&longpath$&'" '&csTempPath$&"\sp_"&session$&'.txt'
40160     open #tmp:=fngethandle: 'Name='&csTempPath$&'\sp_'&session$&'.txt',display,input 
40180     linput #tmp: getfilepath_ln$
40190               if do_debug then pr 'getfilepath_ln$='&getfilepath_ln$ : pause
40220     if getfilepath_ln$='\' then 
40240       longpath$=rtrm$(longpath$)
40250       pr 'I thought this never happened and was safe to delete.' : if env$('acsDeveloper')<>'' then pause
40260     else 
40280       sp_return$=rtrm$(getfilepath_ln$) !    longpath$    changed this from longpath$ on 10/6/17 - just seems wrong and it wasn't working, so if fixed it.
40300     end if 
45140     close #tmp,free: ioerr ignore
45160   end if 
45180   goto XIT
50280   XIT: ! 
50300   if sp_return$(len(sp_return$)-1:len(sp_return$))='\\' then sp_return$=sp_return$(1:len(sp_return$)-1)
50320   fnshortpath$=sp_return$
50380 fnend 
62000 def fn_make_shortpath_cmd(msc_path$*256)
62020   if ~exists(msc_path$&'\ShortPath.cmd') or do_debug then 
62040     open #msc_tmp:=fngethandle: 'name='&msc_path$&'\ShortPath.cmd,RecL=256,Replace',display,output 
62060     pr #msc_tmp: "echo off"
62080     pr #msc_tmp: "if '%1'=='' goto HELP"
62100     pr #msc_tmp: "if '%2'=='' goto HELP"
62120     pr #msc_tmp: "set homepath=""%~1"""
62140     pr #msc_tmp: "set homedir=%~d1"
62160     pr #msc_tmp: "for %%A in (%HOMEPATH%) do set HOMEPATH=%%~spnxA"
62180     pr #msc_tmp: "echo %homedir%%homepath%"
62200     pr #msc_tmp: "echo %homedir%%homepath% >%2"
62220     pr #msc_tmp: "goto XIT"
62240     pr #msc_tmp: ":HELP"
62260     pr #msc_tmp: "echo Proper Usage is:"
62280     pr #msc_tmp: "echo   %0 [path_input] [outputfile]"
62300     pr #msc_tmp: "echo   [path_input] - a long path to parse.  quote encapsulation is required"
62320     pr #msc_tmp: "echo                  i.e. ""C:\Long Path To Parse\etc"""
62340     pr #msc_tmp: "echo   [outputfile] - file to output resulting short path to."
62360     pr #msc_tmp: ":XIT"
63000     if do_debug then pr #msc_tmp: "echo passed:  %*"
63020     if do_debug then pr #msc_tmp: "echo returning:  %homedir%%homepath%"
63030     if do_debug then pr #msc_tmp: "echo test new method:  %~s1"
63040     if do_debug then pr #msc_tmp: "pause"
63060     close #msc_tmp: 
63080   end if 
63100 fnend 
80000 IGNORE: continue 
80100 ! <Updateable Region: ERTN>
80120 ERTN: fnerror(program$,err,line,act$,"xit")
80140   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
80160   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
80230   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
80240 ERTN_EXEC_ACT: execute act$ : goto ERTN
80250 ! /region
