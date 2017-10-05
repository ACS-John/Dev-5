10000   library "S:\Core\Library": fnreg_read,fnreg_write
10100   pr border: 'Import Source'
10200   execute 'con gui off'
10300   fnupdatesource
10400   def fndatetime
10500     dim tm$*8
10600     tm$=time$
10700     fndatetime=val(date$("CCYYMMDD")&tm$(1:2)&tm$(4:5))
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
11800     hh=val(infoline$(13:14))
11900     if infoline$(19:20)="PM" then hh+=12
12000     if hh<10 then hh$="0"&str$(hh) else hh$=str$(hh)
12100     fnfiledatetime=val(infoline$(7:10)&infoline$(1:2)&infoline$(4:5)&hh$&infoline$(16:17))
12200     goto GOTDATE
12300 NODATE: ! 
12400     fnfiledatetime=190001010800
12500 GOTDATE: ! 
12600     close #fileinfo,free: ioerr ignore
12700   fnend 
12800   def fninitupdate(&lastcompile)
12900     dim lasttime$*256
13000     fnreg_read("LastCompile",lasttime$)
13100     if lasttime$="" then lastcompile=190001010800 else lastcompile=val(lasttime$)
13200     curtime=fndatetime
13300     fnreg_write("LastCompile",str$(curtime))
13400     fnreg_write("OldLastCompile", str$(lastcompile))
13500     if not exists("(import)") then execute "sy -M md (import)"
13600     open #proc_file:=1: 'name=(import)\compile.prc,RecL=1024,Replace',display,output 
13700   fnend 
13800   def fnupdatesource
13900     dim filename$*255,msr_file$*255
14000     fninitupdate(lastcompile)
14100     execute "sy -M sortfiles -D . -C "".br.brs|.br""" ioerr ROLLBACK
14200     open #dirfile:=20: "Name=(import)\brsfiles",display,input 
14300     do 
14400       linput #dirfile: filename$ eof DONE
14900       pr #proc_file: 'Load '&filename$&',Source'
15000       if exists(filename$(1:len(filename$)-4)) then 
15100         pr #proc_file: 'Replace '&filename$(1:len(filename$)-4)
15200       else 
15300         pr #proc_file: 'Save '&filename$(1:len(filename$)-4)
15400       end if 
15500       pr #proc_file: ''
15700     loop 
15800     goto DONE
15900 ROLLBACK: ! 
16000     fnreg_write("LastCompile", str$(lastcompile))
16100 DONE: ! 
16200     close #dirfile: ioerr ignore
16300     msr_file$=file$(proc_file)
16400     close #proc_file: 
16500     execute "subproc "&msr_file$
16600   fnend 
16700   goto ENDPROGRAM
16800 IGNORE: continue 
16900 ENDPROGRAM: ! 
