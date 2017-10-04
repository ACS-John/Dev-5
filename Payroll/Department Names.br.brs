00010 ! formerly S:\acsPR\DepartmentName
00020 ! Department names for payroll
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnhamster
00050   fntop(program$)
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim mask(2),p$(2)*25,lbl$(2)*21
00090 ! ______________________________________________________________________
00110   lbl$(1)="Code" : lbl$(2)="Name"
00120   fln(1)=3 : fln(2)=25
00130   let mask(1)=30 : let mask(2)=0
00140   fn_openfiles
00150   fnhamster("Deptname",mat lbl$,mat fln,1,mat p$,mat fltyp$,mat sln,mat mask)
00160   close #1: !:
        execute "Index "&env$('Q')&"\PRmstr\deptname.h"&env$('cno')&' '&env$('Q')&"\PRmstr\Depnameidx.h"&env$('cno')&" 1 3,Replace" ioerr ignore
00170 XIT: let fnxit
00180 ! ______________________________________________________________________
00190 ! <Updateable Region: ERTN>
00200 ERTN: let fnerror(program$,err,line,act$,"xit")
00210   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00220   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00230   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00240 ERTN_EXEC_ACT: execute act$ : goto ERTN
00250 ! /region
00260 ! ______________________________________________________________________
46000 def fn_openfiles
46020   if ~openFiles then
46040     openFiles=1
46060     open #1: "Name="&env$('Q')&"\PRmstr\DeptName.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptNameIdx.h"&env$('cno')&",use,RecL=32,kps=1,kln=3,Shr",internal,outin,keyed 
46080   end if
46100 fnend
46120 def fn_addMissingDepartments
46140   if ~openFiles then let fn_openfiles
46160 fnend
      