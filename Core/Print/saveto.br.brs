20000 ! Replace S:\Core\Print\SaveTo.br
20020 ! sets a report to save to a file just before displaying it to the user
20040   def library fnsavetoasstart(filenametocopyto$*400)
20060 ! ______________________________________________________________________
20080     library 'S:\Core\Library': fngethandle,fnerror
20100     on error goto ERTN
20120 ! ______________________________________________________________________
20140     open #tf1:=fngethandle: "Name="&env$('temp')&"\SaveTo.tmp,Replace,RecL=400",internal,output 
20160     write #tf1,using "Form pos 1,C 400": trim$(filenametocopyto$)
20180     close #tf1: 
20200     goto XIT
20220 XIT: fnend 
76020 ! <updateable region: ertn>
76040 ERTN: fnerror(program$,err,line,act$,"xit")
76060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76080   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
76120 ERTN_EXEC_ACT: execute act$ : goto ERTN
76140 ! </updateable region: ertn>
