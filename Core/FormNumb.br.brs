00010 ! Replace S:\Core\FormNumb.br
00020 ! ______________________________________________________________________
00030   def library fnformnumb$(numb,decimals,size)
00040 ! ______________________________________________________________________
00050     library 'S:\Core\Library': fnerror
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     let fnformnumb$=lpad$(cnvrt$("N 10."&str$(decimals),numb),size)
00090     goto XIT
00100 ! ______________________________________________________________________
00110 ! <Updateable Region: ERTN>
00120 ERTN: let fnerror(program$,err,line,act$,"xit")
00130     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00140     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00150     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00160 ERTN_EXEC_ACT: execute act$ : goto ERTN
00170 ! /region
00180 ! ______________________________________________________________________
00190 XIT: fnend 
00200 ! ______________________________________________________________________
