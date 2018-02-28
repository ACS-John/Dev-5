00020 ! 
00030 L30: pr newpage
00040   close #101: ioerr L50
00050 L50: open #101: "SROW=9,SCOL=4,EROW=11,ECOL=65,BORDER=DR,CAPTION=CONVERT TRANSACTION ALLOCATION FILE",display,outIn 
00060   pr f "10,5,C 60": "ENTER COMPANY NUMBER TO CONVERT OR 0 TO STOP:"
00070   input fields "10,51,N 2,UE,N": cno
00080   if cno=0 then stop 
00082 ! 
00090   execute "Copy [Q]\CLmstr\TRALLOC.h[cno] X -80"
00100   execute "Free [Q]\CLmstr\TRALLOC.h[cno]"
00110   execute "Copy X [Q]\CLmstr\TRALLOC.h[cno]"
00120   goto L30
