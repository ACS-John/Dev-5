! Replace S:\acsGL\AcGlClr
! -- Clear Accumulated Transactions
 
	autoLibrary
	on error goto Ertn
 
	dim cnam$*40,tr(7),tr$*12,td$*30,cap$*128
 
	right=1 : center=2 : left=0
	fnTop(program$,cap$="Clear Accumulated Transactions")
	fnconsole(off=0)
	fncno(cno,cnam$)
	fnTos(sn$='ClrAccTrans') : _
	lc=0 : _
	mylen=50 : mypos=mylen+2 : width=80
	fnLbl(lc+=1,1,"* * *   Warning   * * *",width,center)
	fnLbl(lc+=1,1,"This selection will remove all records from the",width,center)
	fnLbl(lc+=1,1,"General Ledger Accumulated Transactions File ",width,center)
	fnLbl(lc+=1,1,"older than the date entered.",width,center)
	fnLbl(lc+=1,1,"Enter the Removal Date:",mylen,right)
	fnTxt(lc,mypos,8,0,0,'ccyymmdd') : _
	resp$=''
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	rd1=val(resp$(1))
 
	open #2: "Name=[Temp]\Work.[Session],RecL=72,Replace",internal,output
	open #1: "Name=[Q]\GLmstr\ACTRANS.h[cno]",internal,input
L270: read #1,using L280: mat tr,tr$,td$,pcde eof END1
L280: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
	if fndate_mmddyy_to_ccyymmdd(tr(4))<rd1 then goto L270
	write #2,using L280: mat tr,tr$,td$,pcde
	goto L270
 
END1: close #2:
	close #1:
	execute "COPY [Temp]\Work."&session$&' '&"[Q]\GLmstr\ACTRANS.h[cno] -n"
	execute "Index [Q]\GLmstr\ACTRANS.h[cno]"&' '&"[Q]\GLmstr\ACTRIDX.h[cno] 1/71/17/13 12/2/2/4 Replace DupKeys"
	execute "free [Temp]\Work."&session$
	goto Xit
 
Xit: fnXit
 
include: ertn
