autoLibrary
fixPayrollDate=20201220 ! ccyymmdd
fixCheckNo=1001
open #hCheck=fnH: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed
do
	read #hCheck,using "form pos 1,N 8,n 3,PD 6,N 7": heno,tdn,prd,ckno eof EoCheck ioerr CheckReadErr
	NextCheck: !
loop
EoCheck: !
close #hCheck:
pr 'fixed ';errCnt
end

CheckReadErr: !
errCnt+=1
  pr err,variable$,prd,ckno
	rewrite #hCheck,using "form pos 12,pd 6,n 7": fixPayrollDate,fixCheckNo
	fixCheckNo+=1
! pause
goto NextCheck