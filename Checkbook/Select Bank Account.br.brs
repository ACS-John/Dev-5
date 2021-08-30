autoLibrary
on error goto Ertn
fnTop(program$)

open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",i,outi,r
read #20,using 'Form POS 152,N 2',rec=1,release: wbc
close #20:
ASK1: !
	dim resp$(1)*60
	fnTos
	lc=0 : mylen=20 : mypos=mylen+2
	fnLbl(lc+=1,1,"Working Bank:",mylen,1)
	fncombof('bank',lc,mypos,33,"[Q]\CLmstr\BankMstr.h[cno]",1,2,3,30,"[Q]\CLmstr\BankIdx1.h[cno]",1)
	resp$(1)=str$(wbc)
	fnCmdKey('&Save',2,1,0)
	fnCmdKey('&Add',1,0,0,'This takes you to the Bank File')
	fnCmdKey('&Cancel',5,0,1)
	ckey=fnAcs(mat resp$)
	if ckey=5 or ckey=99 then
		goto Xit
	else if ckey=1 then
		fnchain("S:\Checkbook\Bank")
	else if ckey=2 then
		wbc=val(resp$(1)(1:2))
	end if
	open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",i,outi,r
	rewrite #20,using 'Form POS 152,N 2',rec=1: wbc
	close #20:
goto Xit

Xit: fnXit
include: ertn
