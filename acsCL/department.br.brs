00010 ! Replace S:\acsCL\Department
00020 ! __Departmental breakdown file for monticello and others for claims report
00030   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00040   on error goto ERTN
00050   fntop(program$,cap$="Department Breakdown")
00060 ! ______________________________________________________________________
00070   dim lbl$(3)*24,tln(3),p$(3)*160,fltyp$(3),sln(3),mask(3)
00080   dim c$(6,8)*40,cap$*128
00090 ! ______________________________________________________________________
00100   fncno(cno)
00110   let lbl$(1)="Fund Number" : let lbl$(2)="Department Number" !:
        let lbl$(3)="Description"
00120   let tln(1)=3 : let tln(2)=2 : let tln(3)=30
00130   let fltyp$(1)="N" : let fltyp$(2)="n" : let fltyp$(3)="C"
00140   let mask(1)=30 : let mask(2)=30 : let mask(3)=0
00150   open #1: "Name="&env$('Q')&"\CLmstr\dptmstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\dptidx1.h"&str$(cno)&",Use,RecL=35,KPs=1,KLn=5,Shr",internal,outin,keyed 
00160   close #1: 
00170   open #1: "Name="&env$('Q')&"\CLmstr\dptmstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\dptidx1.h"&str$(cno)&",Use,RecL=35,KPs=1,KLn=5,Shr",internal,outin,keyed 
00180   fnhamster("Bank",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00190   close #1: 
00200   execute "Index "&env$('Q')&"\CLmstr\DPTMSTR.h"&str$(cno)&' '&env$('Q')&"\CLmstr\DPTIDX1.h"&str$(cno)&" 1 5 Replace DupKeys -n"
00210 XIT: let fnxit
00220 ! ______________________________________________________________________
00230 ! <Updateable Region: ERTN>
00240 ERTN: let fnerror(program$,err,line,act$,"xit")
00250   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00260   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00270   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00280 ERTN_EXEC_ACT: execute act$ : goto ERTN
00290 ! /region
00300 ! ______________________________________________________________________
