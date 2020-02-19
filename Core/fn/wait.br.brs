10000 ! Replace S:\Core\Wait.br
10020 !
10040   def library fnwait(message$*40,stopable)
10060 ! if stopable=1 will display "Cancel (F5)" button
10080 ! win = window number
10100 !
10120     library 'S:\Core\Library': fnStatus,fnerror ! ,fncno
10140     on error goto Ertn
10160     if trim$(message$)="" then message$="Please wait..."
10300     fnStatus(message$)
10320 ! if stopable=1 then
10340 !   "Cancel (F5)"
10360 ! else 
10380 !   "Do Not Stop"
10400 ! end if
10420     goto XIT
10440 !
10460 ERTN: fnerror(program$,err,line,act$,"xit")
10480     if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
10500     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
10520     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
10540 ERTN_EXEC_ACT: execute act$ : goto ERTN
10560 !
10580 XIT: fnend 
10600 !
