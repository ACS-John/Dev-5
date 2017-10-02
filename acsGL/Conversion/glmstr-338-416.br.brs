00010 ! Replace S:\acsGL\Conversion\GLmstr-338-416
00011 ! convert "&env$('Q')&"\GLmstr from RECL 338 to RECL 416 Format
10000   def library fnglmstr_338_416
10020     library 'S:\Core\Library': fntop, fnerror,fncno,fnacglblds,fnstatus,fnCopy,fnindex_it
10040     on error goto ERTN
10060 ! ______________________________________________________________________
10080     dim cnam$*40,cap$*128,ml$(6)*48,resp$(5)*1,revb(13)
10100 ! ______________________________________________________________________
10120     let fntop(program$,cap$="GLmstr 338-416 Conversion")
10140     let fncno(cno,cnam$)
10160     let stopable$="xit"
10200     let fnstatus('Converting GLmstr from 338 to 416...')
10420 ! 
10440     let fnCopy(env$('Q')&"\GLmstr\GLmstr.h"&str$(cno),env$('Q')&"\GLmstr\GLmstr.h"&str$(cno),416) ! &" -416 -n"
10460     let stopable$="NO"
10520     open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno),internal,outin,keyed 
10540     do 
10560       read #1,using 'Form POS 339,13*PD 6.2': mat revb eof DONE
10580       mat revb=(0)
10600       rewrite #1,using 'Form POS 339,13*PD 6.2': mat revb
10620     loop 
10660 DONE: ! 
10680     close #1: 
10700     let fnindex_it(env$('Q')&"\GLmstr\GLmstr.h"&str$(cno),env$('Q')&"\GLmstr\glIndex.h"&str$(cno),"1 12")
10720     let fnacglblds
10740     goto XIT
10760 ! ______________________________________________________________________
10780 ! r: ertn - just uses stopable$ variable - otherwise standard
10800 ERTN: let fnerror(program$,err,line,act$,stopable$)
10820     if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
10840     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
10860     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
10880 ERTN_EXEC_ACT: execute act$ : goto ERTN
10900 ! /r
10920 ! ______________________________________________________________________
10940 XIT: fnend 
