00010 ! Replace S:\acsPR\CmbJob.br
00020 ! creates a screen ace combobox for job records
00030   def library fncmbjob(myline,mypos; addall,container,indexfile$*200)
00040 ! ______________________________________________________________________
00050     library 'S:\Core\Library': fncno,fncombof,fnerror
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     dim df$*200,if$*200
00090 ! ______________________________________________________________________
00100     if addall<>1 then addall=0
00110     fncno(cno)
00120     if addall=0 then let fen$="CJob.h"&str$(cno) else !:
            let fen$="CJobALL.h"&str$(cno)
00130     if indexfile$="" then let if$=env$('Q')&"\PRmstr\jcindx.h"&str$(cno) else !:
            let if$=indexfile$
00140     fncombof(fen$,myline,mypos,43,env$('Q')&"\PRmstr\jcmstr.h"&str$(cno),1,6,7,25,if$,1+addall,1,"Select from the list of jobs. To add a job, go to the Job Cost File.",container)
00150     let indexfile$=""
00160     goto XIT
00170 ! ______________________________________________________________________
00180 ! <Updateable Region: ERTN>
00190 ERTN: let fnerror(program$,err,line,act$,"xit")
00200     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00210     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00220     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00230 ERTN_EXEC_ACT: execute act$ : goto ERTN
00240 ! /region
00250 ! ______________________________________________________________________
00260 XIT: fnend 
00270 ! ______________________________________________________________________
