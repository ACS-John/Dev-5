00010 ! Replace S:\Core\SetMonth.br
00020 ! returns month names in an array !:
        ! make sure your mat MO$ is dimensioned to (12), duh
00030 ! ______________________________________________________________________
00040   def library fnsetmonth(mat mo$)
00050 ! ______________________________________________________________________
00060     library 'S:\Core\Library': fnerror
00070     on error goto ERTN
00080 ! ______________________________________________________________________
00090     mo$(01)="January" : mo$(02)="February" : mo$(03)="March" !:
          mo$(04)="April" : mo$(05)="May" : mo$(06)="June" !:
          mo$(07)="July" : mo$(08)="August" : mo$(09)="September" !:
          mo$(10)="October" : mo$(11)="November" : mo$(12)="December"
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
