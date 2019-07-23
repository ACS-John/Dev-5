20000 ! Replace S:\Core\GetDir.br
20020 ! reads a directory into an array
20040 ! ______________________________________________________________________
20060   def library fngetdir(&dir$,mat filename$; option$,filter$*40)
20080 ! ______________________________________________________________________
20100     library 'S:\Core\Library': fngethandle,fnerror,fnpause
20120     on error goto ERTN
20140 ! ______________________________________________________________________
20160 ! Dir$=Directory to Read
20180 ! .            does not require but will accept a \ on the end
20200 ! filename$(x)=file names (includes path if /s option is used)
20220 ! option$: /s or /o-g or what ever you want use "dir /?"
20240 ! .        at dos prompt for complete list of options.
20260 ! ______________________________________________________________________
20280     dim tmp$*255
20300 ! ______________________________________________________________________
20320     mat filename$=("")
20360     filter$=trim$(filter$) : if filter$="" then filter$="*.*"
20380     option$=trim$(option$)
20400     dir$=trim$(dir$)
20420     if dir$(len(dir$):len(dir$))<>"\" then dir$=dir$&"\"
20440 ! _____________
20460     execute 'free '&env$('temp')&'\GetDir"&session$&".tmp -n' ioerr ignore
20480     tmp$='Sy -s -M Dir "'&rtrm$(os_filename$(dir$))&'\'&filter$&'" /b '&option$&' >"'&env$('temp')&'\GetDir'&session$&'.tmp"'
20500     execute tmp$
20520 ! pr TMP$ : fnPAUSE
20540     open #tf1:=fngethandle: "Name="&env$('temp')&'\'&"GetDir"&session$&".tmp",display,input 
20560     for x=1 to udim(filename$)
20580       linput #tf1: tmp$ eof XIT
20600       filename$(x)=rtrm$(tmp$)
20620       if filename$(x)=uprc$(filename$(x)) then ! never all caps-anything but
20640         filename$(x)=lwrc$(filename$(x))
20660       end if 
20680     next x
20700     goto XIT
20720 ! ______________________________________________________________________
20740 ! <Updateable Region: ERTN>
20760 ERTN: fnerror(program$,err,line,act$,"xit")
20780     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
20800     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
20820     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
20840 ERTN_EXEC_ACT: execute act$ : goto ERTN
20860 ! /region
20880 IGNORE: continue 
20900 XIT: ! 
20910     close #tf1,free: ioerr ignore
20920   fnend 
20940 ! ______________________________________________________________________
