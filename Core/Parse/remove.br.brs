00010 ! Replace S:\Core\Parse\Remove.br
00020 ! removes any 1 character (and$) from any sting (word$)
00030   def library fnremove(and$,&word$)
00040 ! ______________________________________________________________________
00050     library 'S:\Core\Library': fnerror
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080 BAK: x=pos(word$,and$,1)
00090     if x>0 then word$(x:x)="": goto BAK else goto XIT
00100 ! ______________________________________________________________________
00110 ! <Updateable Region: ERTN>
00120 ERTN: fnerror(program$,err,line,act$,"xit")
00130     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00140     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00150     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00160 ERTN_EXEC_ACT: execute act$ : goto ERTN
00170 ! /region
00180 ! ______________________________________________________________________
00190 XIT: fnend 
00200 ! ______________________________________________________________________
