! Replace S:\acsCL\unclbank
! Unclear All Entries by Bank
 
	autoLibrary
 
	dim cnam$*40,cap$*128,resp$(2)*40
 
	fnTop(program$, cap$="Unclear All Entries by Bank")
	fncno(cno,cnam$)
	cancel=99 : right=1 : center=2 : on=1 : off=0 : _
	limit_to_list=1
 
	fnTos(sn$='UnClBank1') : _
	mylen=15 : mypos=mylen+3
	fnLbl(1,1,"Bank:",mylen,right)
	fncombof('Bank',1,mypos,32,"[Q]\CLmstr\BankMstr.h[cno]",1,2,3,30,"[Q]\CLmstr\BankIdx1.h[cno]",limit_to_list)
	fnLbl(2,1,"Cleared Date:",mylen,right)
	fnTxt(2,mypos,10,0,0,"3") : _
	resp$(2)=""
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	bc1=val(resp$(1)(1:2))
	clrdate=val(resp$(2))
 
	open #trmstr=2: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr",internal,outIn,keyed
READ_2: !
L250: read #trmstr,using "Form Pos 1,N 2,pos 72,n 6": bank_code,olddate eof Xit
	if fndate_mmddyy_to_ccyymmdd(olddate)<>clrdate then goto L250 ! clear dates must match
	if bc1=bank_code then : _
		rewrite #trmstr,using "Form Pos 72,N 6": 0
	goto READ_2
 
Xit: fnXit
 
include: Ertn
