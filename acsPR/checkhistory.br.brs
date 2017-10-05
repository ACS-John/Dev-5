00010 ! Replace S:\acsPR\checkhistory
00020 ! Payroll Check History
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncombo1,fncno,fnerror,fndate_mmddyy_to_ccyymmdd,fnhours,fntos,fnlbl,fncmbemp,fncmdkey,fnacs,fncombof,fntxt,fnbutton,fnmsgbox,fnpic,fnfra,fnrgl$,fnqgl,fnagl$,fncheckfile,fnemployee_srch
00050   on error goto ERTN
00060   dim cnam$*40,cap$*128
00070 ! ______________________________________________________________________
00080   fntop(program$,cap$="Payroll Check History")
00090   fncno(cno,cnam$)
00100 ! 
00110 ! ______________________________________________________________________
00120   open #1: "Name="&env$('Q')&"\PRmstr\RPMstr.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00130   if exists(env$('Q')&"\PRmstr\PayrollChecks.h"&str$(cno))=0 then goto SETUP_PAYROLLCHECKS
00135   if exists(env$('Q')&"\PRmstr\checkidx.h"&str$(cno)&",Shr")=0 then goto L280
00140 L140: open #4: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&str$(cno)&",Shr",internal,outin,keyed 
00141   open #44: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\checkidx3.h"&str$(cno)&",Shr",internal,outin,keyed 
00150   hact$="": filnum=44
00160   fncheckfile(hact$,filnum)
00170 XIT: fnxit
00180 ! <Updateable Region: ERTN>
00190 ERTN: fnerror(program$,err,line,act$,"xit")
00200   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00210   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00220   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00230 ERTN_EXEC_ACT: execute act$ : goto ERTN
00240 ! /region
00250 SETUP_PAYROLLCHECKS: ! 
00260   open #4: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\checkidx3.h"&str$(cno)&",RecL=224,use",internal,outin,keyed 
00270   close #4: 
00280 L280: execute "Index "&env$('Q')&"\PRmstr\PayrollChecks.h"&str$(cno)&' '&env$('Q')&"\PRmstr\checkidx3.h"&str$(cno)&" 1/12/9 8/6/3 Replace DupKeys -n"
00282   execute "Index "&env$('Q')&"\PRmstr\PayrollChecks.h"&str$(cno)&' '&env$('Q')&"\PRmstr\checkidx.h"&str$(cno)&" 1 17 Replace DupKeys -n"
00290   goto L140
