00010 ! Replace S:\acsCL\Department
00020 ! __Departmental breakdown file for monticello and others for claims report
00030   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnHamster
00040   on error goto Ertn
00050   fntop(program$,cap$="Department Breakdown")
00060 !
00070   dim lbl$(3)*24,tln(3),p$(3)*160,fltyp$(3),sln(3),mask(3)
00080   dim c$(6,8)*40,cap$*128
00090 !
00100   fncno(cno)
00110   lbl$(1)="Fund Number" : lbl$(2)="Department Number" !:
        lbl$(3)="Description"
00120   tln(1)=3 : tln(2)=2 : tln(3)=30
00130   fltyp$(1)="N" : fltyp$(2)="n" : fltyp$(3)="C"
00140   mask(1)=30 : mask(2)=30 : mask(3)=0
00150   open #1: "Name=[Q]\CLmstr\dptmstr.h[cno],KFName=[Q]\CLmstr\dptidx1.h[cno],Use,RecL=35,KPs=1,KLn=5,Shr",internal,outIn,keyed 
00160   close #1: 
00170   open #1: "Name=[Q]\CLmstr\dptmstr.h[cno],KFName=[Q]\CLmstr\dptidx1.h[cno],Use,RecL=35,KPs=1,KLn=5,Shr",internal,outIn,keyed 
00180   fnHamster("Bank",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00190   close #1: 
00200   execute "Index [Q]\CLmstr\DPTMSTR.h[cno]"&' '&"[Q]\CLmstr\DPTIDX1.h[cno] 1 5 Replace DupKeys -n"
00210 XIT: fnxit
00220 !
00230 ! <Updateable Region: ERTN>
00240 ERTN: fnerror(program$,err,line,act$,"xit")
00250   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00260   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00270   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00280 ERTN_EXEC_ACT: execute act$ : goto ERTN
00290 ! /region
00300 !
