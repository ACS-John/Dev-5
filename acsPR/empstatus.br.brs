00010 ! Replace S:\acsPR\EmpStatus
00020 ! Employment status for payroll
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnHamster
00050   fntop(program$,cap$="Employment Status")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim cap$*128,mask(2),p$(2)*25,lbl$(2)*21
00090 ! ______________________________________________________________________
00100   fncno(cno)
00110   lbl$(1)="Code" : lbl$(2)="Name"
00120   fln(1)=2 : fln(2)=25
00130   mask(1)=30 : mask(2)=0
00140   open #1: "Name="&env$('Q')&"\PRmstr\EmpStatus.dat,KFName="&env$('Q')&"\PRmstr\Empstatus.idx,Use,RecL=32,KPs=1,KLn=2,Shr",internal,outIn,keyed 
00150   fnHamster("County",mat lbl$,mat fln,1,mat p$,mat fltyp$,mat sln,mat mask)
00160   close #1: !:
        execute "Index "&env$('Q')&"\PRmstr\EmpStatus "&env$('Q')&"\PRmstr\EmpStatus.idx 1 2,Replace" ioerr XIT
00170 XIT: fnxit
00180 ! ______________________________________________________________________
00190 ! <Updateable Region: ERTN>
00200 ERTN: fnerror(program$,err,line,act$,"xit")
00210   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00220   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00230   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00240 ERTN_EXEC_ACT: execute act$ : goto ERTN
00250 ! /region
00260 ! ______________________________________________________________________
