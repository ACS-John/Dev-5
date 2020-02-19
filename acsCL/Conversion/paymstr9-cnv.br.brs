00010 ! Replace S:\acsCL\Conversion\PayMstr9-CNV
00020 ! converts any length PayMstr file to 189 length and recreateing indexes
00030 !
00040   library 'S:\Core\Library': fnerror,fncno,fnxit
00050   on error goto Ertn
00060 !
00070   dim gl(3),ta(2)
00080 !
00090   fncno(cno)
00100 L100: gosub WIN
00110   pr f "11,25,Cr 27": "Company Number to Convert:"
00120   pr f "13,18,Cc 44,R,N": "F5: Stop"
00130 L130: rinput fields "11,52,Nz 2,U,N": cno
00140   if cmdkey=5 then goto XIT
00150   if cno=0 then pr f "23,1,c 7,n": bell$ : goto L130
00160 !
00170   gosub WIN
00180   pr f "11,19,Cc 42,N": "Conversion in Progress: Please wait..."
00190   pr f "13,18,Cc 44,R,N": "Do Not Stop"
00200   pause 
00210 ! 
00220   execute "Copy [Q]\CLmstr\PayMstr.h[cno],X -D -189"
00230   execute "Free [Q]\CLmstr\PayMstr.h[cno]"
00240   execute "Copy X [Q]\CLmstr\PayMstr.h[cno]"
00250   execute "Index [Q]\CLmstr\PayMstr.h[cno],[Q]\CLmstr\PayIndx1.H[cno],1,8,Replace,DupKeys"
00260   execute "Index [Q]\CLmstr\PayMstr.h[cno],[Q]\CLmstr\PayIndx2.H[cno],9,28,Replace,DupKeys"
00270 !
00280   gosub WIN
00290   pr f "11,19,Cc 42,N": "Conversion Completed for Company Number [cno]"
00300   pr f "13,18,Cc 44,R,N": "Enter: Continue to Next Company  F5: Stop"
00310   input fields "12,18,C 1,AE,N": pause$
00320   if cmdkey=5 then goto DONE
00330   goto L100
00340 !
00350 DONE: close #101: ioerr XIT
00360 XIT: fnxit
00370 !
00380 WIN: close #101: ioerr L390
00390 L390: open #101: "SRow=10,SCol=18,ERow=12,ECol=61,Border=DR,Caption=PayMstr Record Length Conversion",display,outIn 
00400   pr #101: newpage
00410   return 
00420 !
00430 ! <Updateable Region: ERTN>
00440 ERTN: fnerror(program$,err,line,act$,"xit")
00450   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00460   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00470   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00480 ERTN_EXEC_ACT: execute act$ : goto ERTN
00490 ! /region
00500 !
