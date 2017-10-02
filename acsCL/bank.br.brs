00010 ! Replace S:\acsCL\Bank
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00040   on error goto ERTN
00050   let fntop(program$,cap$="Bank")
00060 ! ______________________________________________________________________
00070   dim lbl$(6)*24,tln(6),p$(6)*160,fltyp$(6),sln(6),mask(6)
00080   dim c$(6,8)*256
00090 ! ______________________________________________________________________
00100   let fncno(cno)
00110   let lbl$(1)="Bank Code" : let lbl$(2)="Bank Name" !:
        let lbl$(3)="General Ledger Number" : let lbl$(4)="Bank Balance" !:
        let lbl$(5)="Unpaid Invoices" : let lbl$(6)="Last Check Number"
00120   let tln(1)=2 : let tln(2)=30 : let tln(3)=12 : let tln(4)=11 !:
        let tln(5)=11 : let tln(6)=8
00130   let fltyp$(1)="N" : let fltyp$(2)="C" : let fltyp$(3)="C" !:
        let fltyp$(4)="PD" : let fltyp$(5)="PD" : let fltyp$(6)="C"
00140   let sln(3)=12 : let sln(4)=sln(5)=6.2
00150   let mask(1)=1030 !:
        let mask(4)=32 : let mask(5)=30 : let mask(6)=30
00160   cl=3: c$(cl,1)='ComboF' !:
        c$(cl,2)=env$('Q')&"\CLmstr\GLmstr.h"&str$(cno) !:
        c$(cl,3)='1' : c$(cl,4)='12' !:
        c$(cl,5)='13' : c$(cl,6)='50' !:
        c$(cl,7)=env$('Q')&"\CLmstr\GLIndex.h"&str$(cno) : c$(cl,8)='1'
00170   open #1: "Name="&env$('Q')&"\CLmstr\BankMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\BankIdx1.h"&str$(cno)&",Use,RecL=64,KPs=1,KLn=2,Shr",internal,outin,keyed 
00180   close #1: 
00190   open #1: "Name="&env$('Q')&"\CLmstr\BankMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\BankIdx1.h"&str$(cno)&",Use,RecL=64,KPs=1,KLn=2,Shr",internal,outin,keyed 
00200   let fnhamster("Bank",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00210   close #1: 
00220   execute "Index "&env$('Q')&"\CLmstr\BankMstr.h"&str$(cno)&' '&env$('Q')&"\CLmstr\BankIdx1.h"&str$(cno)&" 1 2 DupKeys Replace Shr -n" ioerr XIT
00230   gosub FIX_GL_NUMBERS
00240 XIT: let fnxit
00250 ! ______________________________________________________________________
00260 ! <Updateable Region: Ertn>
00270 ERTN: let fnerror(program$,err,line,act$,"xit")
00280   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00290   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00300   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00310 ERTN_EXEC_ACT: execute act$ : goto ERTN
00320 ! /region
00330 ! ______________________________________________________________________
00340 FIX_GL_NUMBERS: ! 
00350   open #1: "Name="&env$('Q')&"\CLmstr\BankMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\BankIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00360 L360: read #1,using "form pos 33,c 12": gl$ eof L410
00370   let gl$=lpad$(rtrm$(gl$),12)
00380   let gl$(1:3)=lpad$(rtrm$(gl$(1:3)),3)
00390   rewrite #1,using "form pos 33,c 12": gl$
00400   goto L360
00410 L410: close #1: 
00420   return 
