! Replace S:\acsCL\RENUMCK
! ReNUMBER A SERIES OF CHECKS
 
	autoLibrary
	on error goto Ertn
 
	dim de$*30,cap$*128,tr$(5)*35
 
	fncno(cno)
	fnTop(program$,"Renumber Checks")
	cancel=99 : right=1 : center=2 : on=1 : off=0 : _
	left=0
	open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",internal,input  : _
	read #20,using 'Form POS 417,N 1': rcn : _
	close #20:
	open #trmstr:=1: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno]",internal,outIn,keyed
	open #tralloc:=3: "Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\TrAlloc-idx.h[cno]",internal,outIn,keyed
L150: fnTos
	mylen=30 : mypos=mylen+3 : lc=0
	fnLbl(lc+=1,1,"First Check Number to Renumber:",mylen,right)
	fnTxt(lc,mypos,10,0,0,'30') : _
	resp$(1)=""
	fnLbl(lc+=1,1,"Last Check Number to Renumber:",mylen,right)
	fnTxt(lc,mypos,10,0,0,'30') : _
	resp$(2)=""
	fnLbl(lc+=1,1,"First New Check Number to Use:",mylen,right)
	fnTxt(lc,mypos,10,0,0,'30') : _
	resp$(3)=""
	fnLbl(lc+=1,1,"Bank Account Number:",mylen,right)
	fnTxt(lc,mypos,2,0,0,'30') : _
	resp$(4)=""
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 or ckey=cancel then goto Xit else : _
		firstold=val(resp$(1)) : _
		lastold=val(resp$(2)) : _
		newnumber=firstnew=val(resp$(3)) : _
		bankaccount=val(resp$(4))
	if firstold=0 or lastold=0 or newnumber=0 or bankaccount=0 then goto L150
READ_TRMSTR: !
	restore #trmstr,key>=cnvrt$("pic(zz)",bankaccount)&"1"&cnvrt$("pic(zzzzzzzz",firstold): nokey L150
L300: read #trmstr,using 'Form POS 1,G 2,G 1,C 8,G 6,PD 10.2,C 8,C 35,G 1,G 6,G 1': bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof END1
	x=val(tr$(1)) conv L300
	if x<firstold or x>lastold then goto END1
	if bank_code<>bankaccount then goto L300
	if tcde<>1 then goto L300
	rewrite #trmstr,using 'Form POS 1,G 2,G 1,n 8': bank_code,tcde,newnumber
	restore #tralloc:
	key$=cnvrt$('Pic(ZZ)',bank_code)&str$(tcde)&tr$(1) : _
	restore #tralloc,key>=key$: nokey EO_TRALLOC
READ_TRALLOC: !
	read #tralloc,using 'Form POS 1,C 11,C 12,PD 5.2,C 30,G 6,X 3,C 12,N 1': newkey$,gl$,amt,de$,ivd,po$,postd eof EO_TRALLOC
	if newkey$=key$ then goto L410 else goto EO_TRALLOC
L410: rewrite #tralloc,using 'Form POS 4,n 8': newnumber
	goto READ_TRALLOC
EO_TRALLOC: !
	newnumber+=1
	goto READ_TRMSTR
 
END1: !
	close #trmstr:
	close #tralloc:
	execute "Index [Q]\CLmstr\TrMstr.h[cno]"&' '&"[Q]\CLmstr\TrIdx1.h[cno] 1 11 Replace DupKeys -n"
	execute "Index [Q]\CLmstr\TrMstr.h[cno]"&' '&"[Q]\CLmstr\TrIdx2.h[cno] 28/1 8/11 Replace DupKeys -n"
	execute "Index [Q]\CLmstr\TrMstr.h[cno]"&' '&"[Q]\CLmstr\TrIdx3.h[cno] 16/12/4 2/4/8 Replace DupKeys -n"
	execute "Index [Q]\CLmstr\TrAlloc.h[cno]"&' '&"[Q]\CLmstr\TrAlloc-idx.h[cno] 1 11 Replace DupKeys -n"
	goto Xit
 
Xit: fnXit
 
include: ertn
