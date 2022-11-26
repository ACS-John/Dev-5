! Replace S:\acsPR\RemoveChecks
! Remove Transactions

autoLibrary
on error goto Ertn
fnTop(program$,"Remove Old Payroll Checks")

	dim de$*30,cap$*128,tr$(5)*35,cp(32),tdc(10)

	open #1: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],NoShr",i,outIn,k
	open #work1:=2: "Name=[Q]\PRmstr\Work1."&wsid$&",Size=0,RecL=224,replace",i,outi,r
	fnTos
	mylen=22 : mypos=mylen+3 : lc=0
	fnLbl(lc+=1,1,"Oldest Date to Retain:",mylen,1)
	fnTxt(lc,mypos,10,0,0,'1003')
	resp$(1)=str$(date('ccyymmdd')-50000)
	lc+=1
	fnLbl(lc+=1,1,"All transactions with a",mylen*2,2)
	fnLbl(lc+=1,1,"date prior to this date will be removed.",mylen*2,2)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 or ckey=99 then goto Xit
	rd1=val(resp$(1))
	READ_CHECKS: !
		read #1,using "form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,realckno,mat tdc,mat cp eof END1
		if prd>=rd1 then goto KEEP
	goto READ_CHECKS
	KEEP: !
		write #work1,using "form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,realckno,mat tdc,mat cp
	goto READ_CHECKS

	END1: !
	close #work1:
	close #1,free:
	execute "Rename [Q]\PRmstr\Work1."&wsid$&' '&"[Q]\PRmstr\PayrollChecks.h[cno] -n"
	execute "Index [Q]\PRmstr\PayrollChecks.h[cno]"&' '&"[Q]\PRmstr\checkidx.h[cno] 1 17 Replace DupKeys -n"
goto Xit

Xit: fnXit

include: ertn

