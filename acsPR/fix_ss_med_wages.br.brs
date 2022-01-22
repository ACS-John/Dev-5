autoLibrary
fnTop(program$,"Fix WH Wages in Check History")
on error goto Ertn
ssrate1=fnss_employee*.01
ssrate2=fnss_employer*.01
open #1: "Name=[Q]\PRmstr\Company.h[cno]",i,outi,r 
read #1,using 'form pos 1,3*x 40,x 12,PD 6.3',rec=1: mcr
close #1:
mcr=mcr*.01
open #4: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],Shr",i,outIn,k 
do
	dim tdc(10),tcp(32)
	read #4,using F_HIST: heno,tdn,prdate,ckno,mat tdc,mat tcp eof Finis
	F_HIST: form pos 1,n 8,n 3,pd 6,n 7,5*pd 3.2,37*pd 5.2
	if tcp(2)=round(tcp(31)*ssrate1,2) then tdc(7)=tcp(31) else tdc(7)=round(tcp(2)/ssrate1,2)
	if tcp(3)=round(tcp(31)*mcr,2) then tdc(8)=tcp(31) else tdc(8)=round(tcp(3)/mcr,2) ! calculate medicare wages
	rewrite #4,using F_HIST: heno,tdn,prdate,ckno,mat tdc,mat tcp
loop
Finis: ! 
Xit: fnxit
include: ertn
