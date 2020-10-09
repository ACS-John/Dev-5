! Replace S:\acsCL\RmTrans
! Remove Transactions
 
	autoLibrary
	on error goto Ertn
 
	dim de$*30,cap$*128,tr$(5)*35
 
	fncno(cno)
	fnTop(program$,"Remove Old Transactions")
	cancel=99 : right=1 : center=2 : on=1 : off=0 : _
	left=0
	open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",internal,input  : _
	read #20,using 'Form POS 417,N 1': rcn : _
	close #20:
	open #trmstr:=1: "Name=[Q]\CLmstr\TrMstr.H[cno],KFName=[Q]\CLmstr\TrIdx1.H[cno]",internal,outIn,keyed
	open #work1:=2: "Name=[Q]\CLmstr\Work1."&wsid$&",version=2,Size=0,RecL=84,Replace",internal,outIn,relative
	open #tralloc:=3: "Name=[Q]\CLmstr\TrAlloc.H[cno],KFName=[Q]\CLmstr\TrAlloc-idx.h[cno]",internal,input,keyed
	open #work2=4: "Name=[Q]\CLmstr\Work2."&wsid$&",version=2,Size=0,RecL=80,Replace",internal,outIn,relative
	fnTos
	mylen=21 : mypos=mylen+2 : lc=0
	fnLbl(lc+=1,1,"Oldest Retained Date:",mylen,right)
	fnTxt(lc,mypos,10,0,0,'1003') : _
	resp$(1)=str$(date('ccyymmdd')-50000)
	lc+=1
	if rcn=1 then : _
		fnLbl(lc+=1,1,"All cleared transactions with a",mylen*2,center)
	if rcn><1 then : _
		fnLbl(lc+=1,1,"All transactions with a",mylen*2,center)
	fnLbl(lc+=1,1,"date prior to this date will be removed.",mylen*2,center)
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 or ckey=cancel then goto Xit else : _
		rd1=val(resp$(1))
! fnwait
READ_TRMSTR: !
	read #trmstr,using 'Form POS 1,G 2,G 1,C 8,G 6,PD 10.2,C 8,C 35,G 1,G 6,G 1': bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof END1
	if fndate_mmddyy_to_ccyymmdd(val(tr$(2)))>=rd1 then goto KEEP
	if tr3=0 and uprc$(trim$(tr$(5)))<>"VOID" then delete #trmstr: : goto READ_TRMSTR
	if rcn><1 then goto READ_TRMSTR
	if clr=0 then goto KEEP
	goto READ_TRMSTR
 
KEEP: !
	write #work1,using 'Form POS 1,G 2,G 1,C 8,G 6,pd 10.2,C 8,C 35,G 1,G 6,G 1,2*PD 3': bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd
	restore #tralloc:
	key$=cnvrt$('Pic(ZZ)',bank_code)&str$(tcde)&tr$(1) : _
	restore #tralloc,key>=key$: nokey EO_TRALLOC
READ_TRALLOC: !
	read #tralloc,using 'Form POS 1,C 11,C 12,PD 5.2,C 30,G 6,X 3,C 12,N 1': newkey$,gl$,amt,de$,ivd,po$,postd eof EO_TRALLOC : _
	if key$<>newkey$ then goto EO_TRALLOC
	write #work2,using 'Form POS 1,C 11,C 12,PD 5.2,C 30,G 6,PD 3,C 12,N 1': newkey$,gl$,amt,de$,ivd,0,po$,postd
	goto READ_TRALLOC
EO_TRALLOC: !
	goto READ_TRMSTR
 
END1: !
	close #work1:
	close #work2:
	close #trmstr,free:
	close #tralloc,free:
	execute "Rename [Q]\CLmstr\Work1."&wsid$&' '&"[Q]\CLmstr\TRmstr.H[cno] -n"
	execute "Rename [Q]\CLmstr\Work2."&wsid$&' '&"[Q]\CLmstr\TrAlloc.H[cno] -n"
	execute "Index [Q]\CLmstr\TrMstr.H[cno]"&' '&"[Q]\CLmstr\TrIdx1.H[cno] 1 11 Replace DupKeys -n"
	execute "Index [Q]\CLmstr\TrMstr.H[cno]"&' '&"[Q]\CLmstr\TrIdx2.H[cno] 28/1 8/11 Replace DupKeys -n"
	execute "Index [Q]\CLmstr\TrMstr.H[cno]"&' '&"[Q]\CLmstr\TrIdx3.H[cno] 16/12/4 2/4/8 Replace DupKeys -n"
	execute "Index [Q]\CLmstr\TrAlloc.H[cno]"&' '&"[Q]\CLmstr\TrAlloc-idx.H[cno] 1 11 Replace DupKeys -n"
	goto Xit
 
Xit: fnXit
 
include: ertn
