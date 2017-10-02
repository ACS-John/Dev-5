00010 ! Replace S:\acsCL\checkalloc
00020 ! GL Distribution Report     AND/OR     Post to General Ledger
00030 ! GLT: 1=Post  2=Print Only
00040 ! ______________________________________________________________________
00050   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnputcno,fntop,fnxit,fndate_mmddyy_to_ccyymmdd,fnpgnum,fnprocess,fnchain,fntos,fnlbl,fntxt,fnchk,fncmdset,fnacs,fndate_mmddyy_to_ccyymmdd
00060 ! ______________________________________________________________________
00070   dim dat$*20,cnam$*40,vnam$*30,de$*30,tr(2),tbc(99,2),io1$(8),pde$*30
00080   dim apc(99,3),td$*30,prd(23),cap$*128,glwk$*20,item1$(2)*12
00090 ! ______________________________________________________________________
00100   if glt=2 then !:
          cap$="GL Distribution Report" else !:
          cap$="Post to General Ledger"
00110   let fncno(cno,cnam$)
00120   cancel=99
00130 ! ______________________________________________________________________
00140 ! determine if cash or accrual by checking for any !:
        ! accounts payable numbers in the general ledger control file
00150   let fnopenprn
00160   open #trmstr=1: "Name="&env$('Q')&"\CLmstr\TrMstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
00170   open #tralloc=3: "Name="&env$('Q')&"\CLmstr\TrAlloc.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\tralloc-idx.h"&str$(cno)&",Shr",internal,outin,keyed 
00180   open #bankmstr=4: "Name="&env$('Q')&"\CLmstr\BankMstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\BankIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
00190   open #work=5: "Name="&env$('Temp')&"\Work."&session$&",SIZE=0,RecL=76,Replace",internal,output 
00200 READ_TRMSTR: ! 
00210 L210: read #trmstr,using 'Form POS 1,n 2,n 1,C 8,N 6,PD 10.2,POS 28,C 8,C 30,POS 71,N 1,X 6,N 1': trbank_code,trtcde,ck$,pd,ca1,vn$,de$,pcde,scd eof END1
00215   if fndate_mmddyy_to_ccyymmdd(pd)<20050530 then goto L210 ! skip everything before this year
00216 ! If fndate_mmddyy_to_ccyymmdd(PD)<20050701 or fndate_mmddyy_to_ccyymmdd(pd)>20050731 Then Goto 210 ! skip everything before this year
00220   restore #tralloc,key>=cnvrt$("pic(zz)",trbank_code)&cnvrt$("pic(#)",trtcde)&ck$: ! Nokey 210
00230   let totalalloc=0
00240 READ_TRALLOC: ! 
00250 L250: read #tralloc,using 'Form POS 1,N 2,N 1,c 8,C 12,PD 5.2,C 12,X 18,N 6,POS 80,N 1': bank_code,tcde,trck$,gl$,amt,iv$,ivd,gde eof READ_TRMSTR
00254 ! pr TRBANK_CODE,BANK_CODE,TCDE,TRTCDE,CK$,TRCK$
00260   if trbank_code=bank_code and tcde=trtcde and ck$=trck$ then goto L270 else goto L290
00270 L270: let totalalloc+=amt
00280   goto L250
00290 L290: if totalalloc<>ca1 then pr #255: "Check # "&ck$ &"  total check="&str$(ca1)&" total allocations ="&str$(totalalloc)&" date: "&str$(pd)
00300   goto L210
00310 END1: ! 
00320   let fncloseprn
00330 XIT: let fnxit
