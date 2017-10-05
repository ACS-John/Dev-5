00010 ! Replace S:\Core\inch2twip.br
00020 ! converts inches to twips !:
        ! rounds to 0 decimal places
00030 ! ______________________________________________________________________
00040   def library fninch2twip(&x)
00050 ! ______________________________________________________________________
00060     library 'S:\Core\Library': fnerror
00070     on error goto ERTN
00080 ! ______________________________________________________________________
00090     let x=round(x*1440,0)
00100     goto XIT
00110 ! ______________________________________________________________________
00120 ! <Updateable Region: ERTN>
00130 ERTN: fnerror(program$,err,line,act$,"xit")
00140     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00150     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00160     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00170 ERTN_EXEC_ACT: execute act$ : goto ERTN
00180 ! /region
00190 ! ______________________________________________________________________
00200 XIT: fnend 
00210 ! ______________________________________________________________________
