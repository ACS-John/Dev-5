autoLibrary
dim resp$(10)*128
fnTop(program$)

fnTos
mylen=15 : mypos=mylen+3
fnLbl(1,1,'Bank:',mylen,1)
fnComboF('Bank',1,mypos,32,'[Q]\CLmstr\BankMstr.h[cno]',1,2,3,30,'[Q]\CLmstr\BankIdx1.h[cno]',1)
fnLbl(2,1,'Cleared Date:',mylen,1)
fnTxt(2,mypos,10,0,0,'3')
resp$(2)=''
fnCmdSet(2)
ckey=fnAcs(mat resp$)
if ckey=5 then goto Xit
bc1=val(resp$(1)(1:2))
clrdate=val(resp$(2))
open #trmstr=fnH: 'Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr',internal,outIn,keyed
do
	read #trmstr,using 'form pos 1,N 2,pos 72,n 6': bank_code,olddate eof Finis
	if fndate_mmddyy_to_ccyymmdd(olddate)=clrdate and bc1=bank_code then  ! clear dates and bank code must match
		rewrite #trmstr,using 'form pos 72,N 6': 0
	end if
loop
Finis: !
close #trmstr: ioerr ignore
Xit: fnXit
include: ertn
