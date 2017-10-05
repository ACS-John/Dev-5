00010 ! Replace S:\acsPR\CmbBurden.br
00020 ! creates a screen ace combobox for personnel burden
00030   def library fncmbburden(myline,mypos; addall,container,indexfile$*200)
00040 ! ______________________________________________________________________
00050     library 'S:\Core\Library': fncno,fncombof,fnerror
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     dim df$*200,if$*200
00090 ! ______________________________________________________________________
00100     if addall<>1 then addall=0
00110     fncno(cno)
00120     if addall=0 then let fen$="Cburden.h"&str$(cno) else !:
            let fen$="CburdenALL.h"&str$(cno)
00130     if indexfile$="" then if$=env$('Q')&"\PRmstr\burdenidx.h"&str$(cno) else !:
            if$=indexfile$
00140     fncombof(fen$,myline,mypos,43,env$('Q')&"\PRmstr\burden.h"&str$(cno),1,8,9,30,if$,1+addall,0,"Select from the list of personnel burden records. To add a personnel burden record, take the Add option.",container)
00150     indexfile$=""
00160     goto XIT
00170 ! ______________________________________________________________________
00180 ! <Updateable Region: ERTN>
00190 ERTN: fnerror(program$,err,line,act$,"xit")
00200     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00210     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00220     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00230 ERTN_EXEC_ACT: execute act$ : goto ERTN
00240 ! /region
00250 ! ______________________________________________________________________
00260 XIT: fnend 
00270 ! ______________________________________________________________________
