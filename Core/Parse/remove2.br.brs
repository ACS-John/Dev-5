00010 ! Replace S:\Core\Parse\Remove2.br
00020 ! removes a big hunk instead of 1 character. !:
        ! only does it once instead of all instances. !:
        ! removes any character (and$) from any sting (word$)
00030   def library fnremove2(&and$,&word$)
00040 ! ______________________________________________________________________
00050     library 'S:\Core\Library': fnerror
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     and$=trim$(and$)
00090     let x=pos(word$,and$,1)
00100     if x=1 then let word$=word$(x+len(and$):len(word$))
00110     if x>1 then let word$=word$(1:x-1)&word$(x+len(and$):len(word$))
00120     goto XIT
00130 ! ______________________________________________________________________
00140 ! <Updateable Region: ERTN>
00150 ERTN: fnerror(program$,err,line,act$,"xit")
00160     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00170     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00180     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00190 ERTN_EXEC_ACT: execute act$ : goto ERTN
00200 ! /region
00210 ! ______________________________________________________________________
00220 XIT: fnend 
00230 ! ______________________________________________________________________
