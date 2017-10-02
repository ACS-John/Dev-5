00010 ! Replace S:\Core\SetMonth.br
00020 ! returns month names in an array !:
        ! make sure your mat MO$ is dimensioned to (12), duh
00030 ! ______________________________________________________________________
00040   def library fnsetmonth(mat mo$)
00050 ! ______________________________________________________________________
00060     library 'S:\Core\Library': fnerror
00070     on error goto ERTN
00080 ! ______________________________________________________________________
00090     let mo$(01)="January" : let mo$(02)="February" : let mo$(03)="March" !:
          let mo$(04)="April" : let mo$(05)="May" : let mo$(06)="June" !:
          let mo$(07)="July" : let mo$(08)="August" : let mo$(09)="September" !:
          let mo$(10)="October" : let mo$(11)="November" : let mo$(12)="December"
00100     goto XIT
00110 ! ______________________________________________________________________
00120 ! <Updateable Region: ERTN>
00130 ERTN: let fnerror(program$,err,line,act$,"xit")
00140     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00150     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00160     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00170 ERTN_EXEC_ACT: execute act$ : goto ERTN
00180 ! /region
00190 ! ______________________________________________________________________
00200 XIT: fnend 
00210 ! ______________________________________________________________________
