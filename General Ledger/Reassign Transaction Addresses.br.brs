! formerly S:\acsGL\glreass
autoLibrary
fnTop(program$)
on error goto Ertn

fn_reassignTrA(val(env$('cno')))

dim txt$(3)*128
txt$(1)='Transactions have successfully been reassigned'
txt$(2)='to their correct General Ledger accounts.'
txt$(3)='This process is also completed when indexes are recreated.'
fnmsgbox(mat txt$,resp$,'',64)

goto Xit

Xit: fnXit

def library fnReassignTransactionAddresses(cno)
	autoLibrary
	fnReassignTransactionAddresses=fn_reassignTrA(cno)
fnend
def fn_reassignTrA(cno)

	open #hAcct=fnH: 'Name=[Q]\GLmstr\GLmstr.h'&str$(cno)&',KFName=[Q]\GLmstr\GLIndex.H'&str$(cno)&',Shr',internal,outIn,keyed
	dim ta(2)
	open #hTrans=fnH: 'Name=[Q]\GLmstr\GLTrans.H'&str$(cno)&',Shr',internal,outIn,relative

	do
		read #hAcct,using Fmstr: mat ta eof EoTrans
		Fmstr: form pos 333,2*pd 3
		rewrite #hAcct,using Fmstr: 0,0
	loop
	EoTrans: !

	lr2=lrec(2)
	rewrite #hTrans,using Ftrans,rec=1: lr2
	for j=1 to lr2
		read #hTrans,using L400,rec=j: k$,nta noRec NextTrans
		L400: form pos 1,c 12,pos 71,pd 3
		read #hAcct,using Fmstr,key=k$: mat ta nokey NextTrans
		if ta(1)=0 then ta(1)=j
		if ta(2)>0 then rewrite #hTrans,using Ftrans,rec=ta(2): j
		ta(2)=j
		rewrite #hAcct,using Fmstr,key=k$: mat ta
		rewrite #hTrans,using Ftrans,rec=j: 0
		Ftrans: form pos 71,pd 3
		NextTrans: !
	next j

	close #hAcct: ioerr ignore
	close #hTrans: ioerr ignore

fnend

include: ertn no
