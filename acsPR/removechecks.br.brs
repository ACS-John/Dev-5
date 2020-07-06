! Replace S:\acsPR\RemoveChecks
! Remove Transactions
 
	autoLibrary
	on error goto Ertn
 
	dim de$*30,cap$*128,tr$(5)*35,cp(32),tdc(10)
 
	fncno(cno)
	fnTop(program$,"Remove Old Payroll Checks")
	cancel=99 : right=1 : center=2 : on=1 : off=0 : _
	left=0
	open #1: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],NoShr",internal,outIn,keyed
	open #work1:=2: "Name=[Q]\PRmstr\Work1."&wsid$&",Size=0,RecL=224,replace",internal,outIn,relative
	fnTos(sn$='RemoveChecks') : _
	mylen=22 : mypos=mylen+3 : lc=0
	fnLbl(lc+=1,1,"Oldest Date to Retain:",mylen,1)
	fnTxt(lc,mypos,10,0,0,'1003') : _
	resp$(1)=str$(date('ccyymmdd')-50000)
	lc+=1
	fnLbl(lc+=1,1,"All transactions with a",mylen*2,center)
	fnLbl(lc+=1,1,"date prior to this date will be removed.",mylen*2,center)
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 or ckey=cancel then goto Xit else : _
		rd1=val(resp$(1))
READ_CHECKS: !
	read #1,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,realckno,mat tdc,mat cp eof END1
	if prd>=rd1 then goto KEEP
	goto READ_CHECKS
 
KEEP: !
	write #work1,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,realckno,mat tdc,mat cp
	goto READ_CHECKS
 
END1: !
	close #work1:
	close #1,free:
	execute "Rename [Q]\PRmstr\Work1."&wsid$&' '&"[Q]\PRmstr\PayrollChecks.h[cno] -n"
	execute "Index [Q]\PRmstr\PayrollChecks.h[cno]"&' '&"[Q]\PRmstr\checkidx.h[cno] 1 17 Replace DupKeys -n"
	goto Xit
 
Xit: fnXit
 
include: Ertn
 
