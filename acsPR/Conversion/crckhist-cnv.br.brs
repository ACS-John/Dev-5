00010 ! Replace S:\acsPR\Conversion\CrCkHist-cnv
00020 ! does something to prCkHist
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   pr newpage
00080   pr f "10,15,C 50": "ENTER COMPANY # TO CONVERT OR 0 TO STOP:"
00090 L90: input fields "10,55,N 5,UE,N": cno conv L90
00100   if cno=0 then stop 
00110 ! 
00120   open #4: "Name=[Q]\PRmstr\PRCkHist.h[cno],RecL=150,USE",internal,output 
00130   close #4: 
00140   execute "Index [Q]\PRmstr\PRCkHist.h[cno]"&' '&"[Q]\PRmstr\PRCKINDX.h[cno] 1 14 Replace DupKeys -n"
00150   chain "S:\acsPR\company"
00160 ! ______________________________________________________________________
00170 XIT: stop 
00180 ! ______________________________________________________________________
00190 ! <updateable region: ertn>
00200 ERTN: fnerror(program$,err,line,act$,"xit")
00210   if uprc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00220   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00230   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00240 ERTN_EXEC_ACT: execute act$ : goto ERTN
00250 ! /region
00260 ! ______________________________________________________________________
