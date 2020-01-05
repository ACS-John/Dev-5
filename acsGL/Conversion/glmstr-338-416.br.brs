00010 ! Replace S:\acsGL\Conversion\GLmstr-338-416
00011 ! convert [Q]\GLmstr from RECL 338 to RECL 416 Format
10000   def library fnglmstr_338_416
10020     library 'S:\Core\Library': fntop, fnerror,fncno,fnacglblds,fnStatus,fnCopy,fnindex_it
10040     on error goto Ertn
10060 ! ______________________________________________________________________
10080     dim cnam$*40,cap$*128,ml$(6)*48,resp$(5)*1,revb(13)
10100 ! ______________________________________________________________________
10120     fntop(program$,cap$="GLmstr 338-416 Conversion")
10140     fncno(cno,cnam$)
10160     stopable$="xit"
10200     fnStatus('Converting GLmstr from 338 to 416...')
10420 ! 
10440     fnCopy("[Q]\GLmstr\GLmstr.h[cno]","[Q]\GLmstr\GLmstr.h[cno]",416) ! &" -416 -n"
10460     stopable$="NO"
10520     open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno]",internal,outIn,keyed 
10540     do 
10560       read #1,using 'Form POS 339,13*PD 6.2': mat revb eof DONE
10580       mat revb=(0)
10600       rewrite #1,using 'Form POS 339,13*PD 6.2': mat revb
10620     loop 
10660 DONE: ! 
10680     close #1: 
10700     fnindex_it("[Q]\GLmstr\GLmstr.h[cno]","[Q]\GLmstr\glIndex.h[cno]","1 12")
10720     fnacglblds
10740     goto XIT
10760 ! ______________________________________________________________________
10780 ! r: ertn - just uses stopable$ variable - otherwise standard
10800 ERTN: fnerror(program$,err,line,act$,stopable$)
10820     if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
10840     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
10860     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
10880 ERTN_EXEC_ACT: execute act$ : goto ERTN
10900 ! /r
10920 ! ______________________________________________________________________
10940 XIT: fnend 
