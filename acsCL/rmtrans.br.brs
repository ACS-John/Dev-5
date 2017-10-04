00010 ! Replace S:\acsCL\RmTrans
00020 ! Remove Transactions
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno,fnacs,fntos,fntxt,fndate_mmddyy_to_ccyymmdd,fncmdset,fnlbl
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim de$*30,cap$*128,tr$(5)*35
00080 ! ______________________________________________________________________
00090   fncno(cno)
00100   fntop(program$,"Remove Old Transactions")
00110   cancel=99 : let right=1 : center=2 : on=1 : off=0 !:
        left=0
00120   open #20: "Name="&env$('Q')&"\CLmstr\Company.h"&str$(cno)&",Shr",internal,input  !:
        read #20,using 'Form POS 417,N 1': rcn !:
        close #20: 
00130   open #trmstr:=1: "Name="&env$('Q')&"\CLmstr\TrMstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrIdx1.H"&str$(cno),internal,outin,keyed 
00140   open #work1:=2: "Name="&env$('Q')&"\CLmstr\Work1."&wsid$&",version=2,Size=0,RecL=84,Replace",internal,outin,relative 
00150   open #tralloc:=3: "Name="&env$('Q')&"\CLmstr\TrAlloc.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrAlloc-idx.h"&str$(cno),internal,input,keyed 
00160   open #work2=4: "Name="&env$('Q')&"\CLmstr\Work2."&wsid$&",version=2,Size=0,RecL=80,Replace",internal,outin,relative 
00170   fntos(sn$='RmTrans-'&str$(rcn)) !:
        let mylen=21 : let mypos=mylen+2 : lc=0
00180   fnlbl(lc+=1,1,"Oldest Retained Date:",mylen,right)
00190   fntxt(lc,mypos,10,0,0,'1003') !:
        let resp$(1)=str$(date('ccyymmdd')-50000)
00200   lc+=1
00210   if rcn=1 then !:
          fnlbl(lc+=1,1,"All cleared transactions with a",mylen*2,center)
00220   if rcn><1 then !:
          fnlbl(lc+=1,1,"All transactions with a",mylen*2,center)
00230   fnlbl(lc+=1,1,"date prior to this date will be removed.",mylen*2,center)
00240   fncmdset(2)
00250   fnacs(sn$,0,mat resp$,ckey)
00260   if ckey=5 or ckey=cancel then goto XIT else !:
          let rd1=val(resp$(1))
00270 ! fnwait
00280 READ_TRMSTR: ! 
00290   read #trmstr,using 'Form POS 1,G 2,G 1,C 8,G 6,PD 10.2,C 8,C 35,G 1,G 6,G 1': bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof END1
00300   if fndate_mmddyy_to_ccyymmdd(val(tr$(2)))>=rd1 then goto KEEP
00310   if tr3=0 and uprc$(trim$(tr$(5)))<>"VOID" then delete #trmstr: : goto READ_TRMSTR
00320   if rcn><1 then goto READ_TRMSTR
00330   if clr=0 then goto KEEP
00340   goto READ_TRMSTR
00350 ! ______________________________________________________________________
00360 KEEP: ! 
00370   write #work1,using 'Form POS 1,G 2,G 1,C 8,G 6,pd 10.2,C 8,C 35,G 1,G 6,G 1,2*PD 3': bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd
00380   restore #tralloc: 
00390   let key$=cnvrt$('Pic(ZZ)',bank_code)&str$(tcde)&tr$(1) !:
        restore #tralloc,key>=key$: nokey EO_TRALLOC
00400 READ_TRALLOC: ! 
00410   read #tralloc,using 'Form POS 1,C 11,C 12,PD 5.2,C 30,G 6,X 3,C 12,N 1': newkey$,gl$,amt,de$,ivd,po$,postd eof EO_TRALLOC !:
        if key$<>newkey$ then goto EO_TRALLOC
00420   write #work2,using 'Form POS 1,C 11,C 12,PD 5.2,C 30,G 6,PD 3,C 12,N 1': newkey$,gl$,amt,de$,ivd,0,po$,postd
00430   goto READ_TRALLOC
00440 EO_TRALLOC: ! 
00450   goto READ_TRMSTR
00460 ! ______________________________________________________________________
00470 END1: ! 
00480   close #work1: 
00490   close #work2: 
00500   close #trmstr,free: 
00510   close #tralloc,free: 
00520   execute "Rename "&env$('Q')&"\CLmstr\Work1."&wsid$&' '&env$('Q')&"\CLmstr\TRmstr.H"&str$(cno)&" -n"
00530   execute "Rename "&env$('Q')&"\CLmstr\Work2."&wsid$&' '&env$('Q')&"\CLmstr\TrAlloc.H"&str$(cno)&" -n"
00540   execute "Index "&env$('Q')&"\CLmstr\TrMstr.H"&str$(cno)&' '&env$('Q')&"\CLmstr\TrIdx1.H"&str$(cno)&" 1 11 Replace DupKeys -n"
00550   execute "Index "&env$('Q')&"\CLmstr\TrMstr.H"&str$(cno)&' '&env$('Q')&"\CLmstr\TrIdx2.H"&str$(cno)&" 28/1 8/11 Replace DupKeys -n"
00560   execute "Index "&env$('Q')&"\CLmstr\TrMstr.H"&str$(cno)&' '&env$('Q')&"\CLmstr\TrIdx3.H"&str$(cno)&" 16/12/4 2/4/8 Replace DupKeys -n"
00570   execute "Index "&env$('Q')&"\CLmstr\TrAlloc.H"&str$(cno)&' '&env$('Q')&"\CLmstr\TrAlloc-idx.H"&str$(cno)&" 1 11 Replace DupKeys -n"
00580   goto XIT
00590 ! ______________________________________________________________________
00600 XIT: let fnxit
00610 ! ______________________________________________________________________
00620 ! <Updateable Region: ERTN>
00630 ERTN: let fnerror(program$,err,line,act$,"xit")
00640   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00650   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00660   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00670 ERTN_EXEC_ACT: execute act$ : goto ERTN
00680 ! /region
00690 ! ______________________________________________________________________
