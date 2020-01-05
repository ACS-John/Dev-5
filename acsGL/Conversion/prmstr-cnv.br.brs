00010 ! Replace S:\acsGL\Conversion\PRmstr-CNV
00020 ! CONVERT GL PAYROLL MASTER FILE !:
        ! from a recl of 190 to 280
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnputcno,fnerror
00050   fntop(program$,"CHANGE_ME")
00060   on error goto Ertn
00070 ! ______________________________________________________________________
00080   dim pr1$*90,pr1(18),pr2(36)
00090 ! ______________________________________________________________________
00100   pr newpage
00110   pr f "08,08,C 32,R,N": " CONVERT GL PAYROLL MASTER FILE"
00120   pr f "10,5,C 60": "ENTER COMPANY NUMBER TO BE CONVERTED:"
00130   pr f "12,15,C 16,B,5": "PRESS F5 TO STOP"
00140 L140: input fields "10,43,N 2,UE,N": cno conv L140
00150   if cmdkey=5 then goto XIT
00160 ! 
00170   open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno]",internal,outIn,keyed ioerr L140
00180   open #2: "Name="&env$('Temp')&"\Work."&session$&",SIZE=0,RecL=280,Replace",internal,output 
00190 L190: read #1,using L200: pr1$,mat pr1 eof END1
00200 L200: form pos 1,c 90,18*pd 5.2,2*n 5
00210   for j=1 to 11: pr2(j)=pr1(j): next j
00220   pr2(13)=pr1(12)
00230   for j=13 to 18: pr2(j+18)=pr1(j): next j
00240   write #2,using L250: pr1$,mat pr2
00250 L250: form pos 1,c 90,36*pd 5.2,2*n 5
00260   goto L190
00270 ! ______________________________________________________________________
00280 END1: close #1: 
00290   close #2: 
00300   execute "COPY "&env$('Temp')&"\Work."&session$&", [Q]\GLmstr\PRmstr.h[cno] -n"
00310   execute "Index [Q]\GLmstr\PRmstr.h[cno],[Q]\GLmstr\PRIndex.h[cno],1,4,Replace,DupKeys -n"
00320   fnputcno(cno)
00330   chain "S:\acsGL\Company"
00340 ! ______________________________________________________________________
00350 XIT: stop 
00360 ! ______________________________________________________________________
00370 ! <Updateable Region: ERTN>
00380 ERTN: fnerror(program$,err,line,act$,"xit")
00390   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00400   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00410   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00420 ERTN_EXEC_ACT: execute act$ : goto ERTN
00430 ! /region
00440 ! ______________________________________________________________________
