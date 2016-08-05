10100   print border: 'Import Source'
10200   execute 'con gui off'
10300   let fnupdatesource
10400   def fndatetime
10500     dim tm$*8
10600     let tm$=time$
10700     let fndatetime=val(date$("CCYYMMDD")&tm$(1:2)&tm$(4:5))
10800   fnend 
10900   def fnfiledatetime(filename$*255)
11000     dim infoline$*255,hh$*2
11100     execute "sy -M dir /N "&filename$&" >(import)\fileinfo"
11200     open #fileinfo:=21: "Name=(import)\fileinfo",display,input 
11300     do 
11400       linput #fileinfo: infoline$ eof NODATE
11500       if infoline$(3:3)="/" then goto PARSEDATE
11600     loop 
11700 PARSEDATE: ! 
11800     let hh=val(infoline$(13:14))
11900     if infoline$(19:20)="PM" then let hh+=12
12000     if hh<10 then let hh$="0"&str$(hh) else let hh$=str$(hh)
12100     let fnfiledatetime=val(infoline$(7:10)&infoline$(1:2)&infoline$(4:5)&hh$&infoline$(16:17))
12200     goto GOTDATE
12300 NODATE: ! 
12400     let fnfiledatetime=190001010800
12500 GOTDATE: ! 
12600     close #fileinfo,free: ioerr ignore
12700   fnend 
12800   def fninitupdate(&lastcompile)
12900     dim lasttime$*256
13100     if lasttime$="" then let lastcompile=190001010800 else let lastcompile=val(lasttime$)
13200     let curtime=fndatetime
13500     if not exists("(import)") then execute "sy -M md (import)"
13600     open #proc_file:=1: 'Name=(import)\compile.prc,RecL=1024,Replace',display,output 
13700   fnend 
13800   def fnupdatesource
13900     dim filename$*255,msr_file$*255
14000     let fninitupdate(lastcompile)
14100     execute "sy -M sortfiles -D . -C "".br.brs|.br""" ioerr ROLLBACK
14200     open #dirfile:=20: "Name=(import)\brsfiles",display,input 
14250     print #proc_file: 'Let Scr_Freeze'
14300     do 
14400       linput #dirfile: filename$ eof DONE
14500       print #proc_file: 'Load '&filename$&',Source'
14600       if exists(filename$(1:len(filename$)-4)) then 
14700         print #proc_file: 'Replace '&filename$(1:len(filename$)-4)
14800       else 
14900         print #proc_file: 'Save '&filename$(1:len(filename$)-4)
15000       end if 
15100       print #proc_file: ''
15200     loop 
15250     print #proc_file: 'let Scr_Thaw'
15300     goto DONE
15400 ROLLBACK: ! 
15600 DONE: ! 
15700     if env$("AfterRecompile")="" then 
15800       print #proc_file: "Sy"
15900     else 
16000       print #proc_file: 'chain "'&env$("AfterRecompile")&'"'
16100     end if 
16200     close #dirfile: ioerr ignore
16300     let msr_file$=file$(proc_file)
16400     close #proc_file: 
16500     execute "subproc "&msr_file$
16600   fnend 
16700   goto ENDPROGRAM
16800 IGNORE: continue 
16900 ENDPROGRAM: ! 
