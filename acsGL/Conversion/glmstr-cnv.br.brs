00020   dim cnam$*40,revb(13)
00030   library 'R:\Core\library': fnxit,fntop
00040   close #101: ioerr L60
00050   let fntop(program$,"CHANGE_ME")
00060 L60: open #101: "SROW=9,SCOL=23,EROW=11,ECOL=60,BORDER=DR,CAPTION=Convert G/L Master for GASB",display,outin 
00070   print fields "10,24,C 32": "ENTER COMPANY NUMBER TO CONVERT:"
00080   print fields "12,32,C 16,R,N": "PRESS F5 TO STOP"
00090 L90: input fields "10,57,N 2,UE,N",attr "R": cno conv L90
00100   if cmdkey=5 then goto L230
00110 ! 
00120   execute "COPY Q:\GLmstr\GLmstr.h"&str$(cno)&" Work."&session$&" -416" ioerr L220
00130   execute "COPY  Work."&session$&" Q:\GLmstr\GLmstr.h"&str$(cno)
00140   open #1: "Name=Q:\GLmstr\GLmstr.h"&str$(cno)&",KFName=Q:\GLmstr\GLIndex.h"&str$(cno),internal,outin,keyed 
00150 L150: read #1,using L180: mat revb eof END1
00160   mat revb=(0)
00170   rewrite #1,using L180: mat revb
00180 L180: form pos 339,13*pd 6.2
00190   goto L150
00200 END1: close #1: 
00210   execute "INDEX Q:\GLmstr\GLmstr.h"&str$(cno)&" Q:\GLmstr\GLIndex.h"&str$(cno)&" 1 12 REPLACE DupKeys"
00220 L220: ! Goto 30
00230 L230: chain "R:\acsGL\acglblds"
