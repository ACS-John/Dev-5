! Replace S:\acsPR\checkhistory
! Payroll Check History

library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncombo1,fnerror,fndate_mmddyy_to_ccyymmdd,fnhours,fnTos,fnLbl,fncmbemp,fnCmdKey,fnAcs,fncombof,fnTxt,fnButton,fnmsgbox,fnpic,fnFra,fnrgl$,fnqgl,fnagl$,fncheckfile,fnemployee_srch
on error goto ERTN

fntop(program$,"Payroll Check History")

open #1: "Name=[Q]\PRmstr\RPMstr.h[cno],KFName=[Q]\PRmstr\RPIndex.h[cno],Shr",internal,outIn,keyed 
if ~exists("[Q]\PRmstr\PayrollChecks.h[cno]") then 
	open #4: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx3.h[cno],RecL=224,use",internal,outIn,keyed 
	close #4: 
	gosub ReIndexPayrollChecks
end if
if ~exists("[Q]\PRmstr\checkidx.h[cno],Shr") then gosub ReIndexPayrollChecks
open #4: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],Shr",internal,outIn,keyed 
open #filnum:=44: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx3.h[cno],Shr",internal,outIn,keyed 
hact$=""
fncheckfile(hact$,filnum)
ReIndexPayrollChecks: ! r:
	execute "Index [Q]\PRmstr\PayrollChecks.h[cno]"&' '&"[Q]\PRmstr\checkidx3.h[cno] 1/12/9 8/6/3 Replace DupKeys -n"
	execute "Index [Q]\PRmstr\PayrollChecks.h[cno]"&' '&"[Q]\PRmstr\checkidx.h[cno] 1 17 Replace DupKeys -n"
return ! /r
XIT: fnxit
include: ertn