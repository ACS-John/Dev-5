autoLibrary
on error goto Ertn

fnTop(program$)

fnTos
fnLbl (1,1,"Removal Date:",13,1)
fnTxt(1,15,10,0,0,'1003')
resp$(1)=''
fnLbl(3,1,"All transactions with a date equal to",42,2)
fnLbl(4,1,"or older than this date will be removed.",42,2)
fnCmdSet(2)
fnAcs(mat resp$,ckey)
if ckey=5 or ckey=99 then 
	goto Xit 
else 
	rd1=val(resp$(1))
end if
fnAutomatedSavePoint('before')
open #ivpaid=fnH: "Name=[Q]\CLmstr\IvPaid.H[cno],KFName=[Q]\CLmstr\IVIndex.H[cno],Shr",internal,outIn,keyed
open #work=fnH: "Name=[Q]\CLmstr\Work.[session],Size=0,RecL=34,Replace",internal,output
do
	READ_IVPAID: !
	read #ivpaid,using 'Form POS 1,C 8,C 12,G 6,G 8': vn$,iv$,dp,ckn eof EO_IVPAID
	if fndate_mmddyy_to_ccyymmdd(dp)<=rd1 then goto READ_IVPAID
	write #work,using 'Form POS 1,C 8,C 12,G 6,G 8': vn$,iv$,dp,ckn
loop
EO_IVPAID: !
close #ivpaid:
close #work:
fnFree("[Q]\CLmstr\IvPaid.H[cno]")
fnRename("[Q]\CLmstr\Work.[session]","[Q]\CLmstr\IvPaid.H[cno]")
goto Xit
Xit: fnXit
include: ertn
