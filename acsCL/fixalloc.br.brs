00010 ! Replace S:\acsCL\fixalloc
00020 ! readd check amount using allocations
00030 ! GLT: 1=Post  2=Print Only
00040 ! ______________________________________________________________________
00050   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror
00060 ! ______________________________________________________________________
00070   dim dat$*20,cnam$*40,vnam$*30,de$*30,tr(2),tbc(99,2),io1$(8),pde$*30
00080   dim apc(99,3),td$*30,prd(23),cap$*128,glwk$*20,item1$(2)*12
00090 ! ______________________________________________________________________
00100   if glt=2 then !:
          cap$="Fix check amounts"
00110   fncno(cno,cnam$)
00120   cancel=99
00130 ! ______________________________________________________________________
00140   open #trmstr=1: "Name="&env$('Q')&"\CLmstr\TrMstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
00150   open #tralloc=3: "Name="&env$('Q')&"\CLmstr\TrAlloc.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\tralloc-idx.h"&str$(cno)&",Shr",internal,outin,keyed 
00160   open #bankmstr=4: "Name="&env$('Q')&"\CLmstr\BankMstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\BankIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
00170   open #work=5: "Name="&env$('Temp')&"\Work."&session$&",SIZE=0,RecL=76,Replace",internal,output 
00180 READ_TRMSTR: ! 
00190   holdck$=ck$
00200 L200: read #trmstr,using 'Form POS 1,n 2,n 1,C 8,N 6,PD 10.2,POS 28,C 8,C 30,POS 71,N 1,X 6,N 1': trbank_code,trtcde,ck$,pd,ca1,vn$,de$,pcde,scd eof END1
00210   if ck$=holdck$ then delete #trmstr: : goto L200
00220   restore #tralloc,key>=cnvrt$("pic(zz)",trbank_code)&cnvrt$("pic(#)",trtcde)&ck$: ! Nokey 210
00230   totalalloc=0
00240 READ_TRALLOC: ! 
00250 L250: read #tralloc,using 'Form POS 1,N 2,N 1,c 8,C 12,PD 5.2,C 12,X 18,N 6,POS 80,N 1': bank_code,tcde,trck$,gl$,amt,iv$,ivd,gde eof READ_TRMSTR
00260   if trbank_code=bank_code and tcde=trtcde and ck$=trck$ then goto L270 else goto L300
00270 L270: totalalloc+=amt
00280   if amt=ca1 then foundone=1
00290   goto L250
00300 L300: if totalalloc<>ca1 and foundone=1 then ca1=totalalloc
00310   rewrite #trmstr,using 'Form POS 1,n 2,n 1,C 8,N 6,PD 10.2,POS 28,C 8,C 30,POS 71,N 1,X 6,N 1': trbank_code,trtcde,ck$,pd,ca1,vn$,de$,pcde,scd
00320   foundone=0
00330   goto READ_TRMSTR
00340 END1: ! 
00350 XIT: fnxit
