10000 ! Replace R:\Core\Wait.br
10020 ! ______________________________________________________________________
10040   def library fnwait(;win,&cap$,message$*40,stopable)
10060 ! if stopable=1 will display "Cancel (F5)" button
10080 ! win = window number
10100 ! ______________________________________________________________________
10120     library 'R:\Core\Library': fnstatus,fnerror ! ,fncno
10140     on error goto ERTN
10160     if trim$(message$)="" then let message$="Please wait..."
10180 ! 
10200 ! close #win: ioerr 200
10220 ! open #win: "Srow=10,SCol=20,ERow=14,ECol=59,Border=Sr,Caption=<"&cap$,display,outin
10240 ! print #win: newpage
10260 ! print #win,fields "1,1,Cc 40,R,N": cnam$
10280 ! print #win,fields "2,1,Cc 40,R,N": "Company Number "&str$(cno)
10300     let fnstatus(message$) ! print #win,fields "4,1,Cc 40,N": message$
10320 ! if stopable=1 then
10340 !  print fields "15,34,C 11,B,5": "Cancel (F5)"
10360 ! else 
10380 !  print fields "15,34,C 11,R,N": "Do Not Stop"
10400 ! end if
10420     goto XIT
10440 ! ______________________________________________________________________
10460 ERTN: let fnerror(waitcap$="R:\Core\Wait.br",err,line,act$,"xit")
10480     if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
10500     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
10520     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
10540 ERTN_EXEC_ACT: execute act$ : goto ERTN
10560 ! ______________________________________________________________________
10580 XIT: fnend 
10600 ! ______________________________________________________________________
