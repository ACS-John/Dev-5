! Replace S:\acsCL\RemovePaidInvoices
! Remove Paid Invoices
 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128
 
	fnTop(program$,cap$="Remove Paid Invoices")
	cancel=99 : right=1 : center=2 : on=1 : off=0 : _
	left=0
	fncno(cno)
 
	fnTos(sn$='RmvPdInv') : _
	lc=0 : mylen=13 : mypos=mylen+2 : _
	mywidth=42
	fnLbl (lc+=1,1,"Removal Date:",mylen,right)
	fnTxt(lc,mypos,10,0,left,'1003') : _
	resp$(1)=""
	lc+=1
	fnLbl(lc+=1,1,"All transactions with a date equal to",mywidth,center)
	fnLbl(lc+=1,1,"or older than this date will be removed.",mywidth,center)
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 or ckey=cancel then goto Xit else : _
		rd1=val(resp$(1))
! fnwait
	open #ivpaid=1: "Name=[Q]\CLmstr\IvPaid.H[cno],KFName=[Q]\CLmstr\IVIndex.H[cno],Shr",internal,outIn,keyed
	open #work=2: "Name=[Q]\CLmstr\Work."&session$&",Size=0,RecL=34,Replace",internal,output
READ_IVPAID: !
	read #ivpaid,using 'Form POS 1,C 8,C 12,G 6,G 8': vn$,iv$,dp,ckn eof EO_IVPAID
	if fndate_mmddyy_to_ccyymmdd(dp)<=rd1 then goto READ_IVPAID
	write #work,using 'Form POS 1,C 8,C 12,G 6,G 8': vn$,iv$,dp,ckn
	goto READ_IVPAID
 
EO_IVPAID: !
	close #ivpaid:
	close #work:
	fnFree("[Q]\CLmstr\IvPaid.H[cno]")
	fnRename("[Q]\CLmstr\Work."&session$,"[Q]\CLmstr\IvPaid.H[cno]")
	goto Xit
 
Xit: fnXit
 
include: Ertn
