00010 ! Replace S:\Core\Xit.br
00020 ! ______________________________________________________________________
00030   def library fnxit(;cursys$)
00040     library 'S:\Core\Library': fnchain,fnerror,fncursys$,fnprocess
00050     on error goto ERTN
00060 ! ______________________________________________________________________
20000     if fncursys$="GL" and fnprocess=1 then let fnchain("S:\acsGL\acglAuto")
20200     if fncursys$="PR" and fnprocess=1 then let fnchain("S:\acsPR\newprAuto")
25000     if env$("xit_override")<>'' then 
25020       dim tmp$*1024
25040       tmp$=env$("xit_override")
25060       setenv("xit_override","")
25080       fnchain(tmp$)
25100     end if 
30000     fnchain('S:\Core\Menu.br',0,1)
50120 ! ______________________________________________________________________
50130 ! <Updateable Region: ERTN>
50140 ERTN: fnerror(program$,err,line,act$,"xit")
50150     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
50160     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
50170     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
50180 ERTN_EXEC_ACT: execute act$ : goto ERTN
50190 ! /region
50200 ! ______________________________________________________________________
50210 XIT: fnend 
